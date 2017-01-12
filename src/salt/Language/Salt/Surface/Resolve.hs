-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Language.Salt.Surface.Resolve(
       resolve
       ) where

import Control.Monad.Messages.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Array(Array, Ix)
import Data.Array.BitArray(BitArray)
import Data.Array.BitArray.IO(IOBitArray)
import Data.Array.IO(IOArray)
import Data.Default
import Data.Foldable
import Data.Hashable
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.PositionElement
import Data.Position.BasicPosition
import Data.ScopeID
import Data.Symbol
import Language.Salt.Message
import Language.Salt.Surface.Syntax

import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as IOArray
import qualified Data.Array.BitArray as BitArray
import qualified Data.Array.BitArray.IO as IOBitArray
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

-- | State of a resolved symbol.
data Access =
    -- | A valid resolution result.
    Valid {
      -- | The ID of the scope in which the result is defined.
      validScope :: !ScopeID
    }
    -- | An illegal access result.
  | Illegal {
      -- | True for private access, false for protected.
      illegalPrivate :: !Bool,
      -- | The scope in which the result is defined.
      illegalScope :: !ScopeID
    }
    -- | Access to an out-of-context element.
  | Inaccessible {
      -- | True for object context, false for local.
      inaccessibleObject :: !Bool,
      -- | The scope in which the result is defined.
      inaccessibleScope :: !ScopeID
    }
    deriving (Eq, Ord)

data Resolution =
    -- | Locally-defined symbol.  This is always valid, so no need for
    -- validity.
    Direct
    -- | An inconsistent resolution.
  | Inconsistent
    -- | A definition imported from another scope.
  | Imported {
      -- | The set of scopes from which this definition is imported.
      importedScopes :: !(HashMap ScopeID Word)
    }
    -- | An inherited definition.
  | Inherited {
      -- | The set of scopes from which the definition is inherited.
      inheritedScopes :: !(HashMap ScopeID Word)
    }
    -- | An undefined symbol.
  | Undefined
    deriving (Eq)

-- | Placeholders for anything that might be a static expression.
-- These are substituted into expressions in a manner similar to
-- common subexpression elimination at the beginning of resolution,
-- then the resolved values are substituted back in at the end.
newtype TempRef = TempRef { tempRefID :: Word }
  deriving (Eq, Ord, Ix)

instance Default TempRef where
  def = TempRef { tempRefID = 0 }

instance Enum TempRef where
  fromEnum = fromEnum . tempRefID
  toEnum = TempRef . toEnum

data TempScope =
  TempScope {
    -- Unaltered content from the original scope.
    tempScopeNames :: !(Array Visibility (HashMap Symbol [DefID])),
    tempScopeEnclosing :: !(Maybe ScopeID),
    -- Static expression-based content from the original scope.
    tempScopeProofs :: ![Proof TempRef],
    tempScopeImports :: ![Import TempRef],
    tempScopeInherits :: ![TempRef],
    -- Renamed expression-based content from the original Scope.
    tempScopeBuilders :: !(HashMap Symbol (Builder (Exp Seq TempRef))),
    tempScopeSyntax :: !(HashMap Symbol (Syntax (Exp Seq TempRef))),
    tempScopeTruths :: !(HashMap Symbol (Truth (Exp Seq TempRef))),
    tempScopeDefs :: !(Array DefID (Def (Exp Seq TempRef))),
    tempScopeEval :: ![Compound (Exp Seq TempRef)],

    -- | Expressions to resolve
    tempScopeRenameTab :: !(Array TempRef TempExp),
    -- | Local symbols that need to be resolved.
    tempScopeSyms :: !(HashMap Symbol Resolution),
    -- | The enclosed scope.
    tempScopeEnclosed :: ![ScopeID]
  }

data TempExp =
    TempExp {
      tempExp :: !(Exp Seq TempRef)
    }
  | TempSym {
      tempSym :: !Symbol
    }
    deriving (Eq, Ord)

instance Hashable TempExp where
  hashWithSalt s TempExp { tempExp = ex } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` ex
  hashWithSalt s TempSym { tempSym = sym } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym

-- | Resolution state.
data ResolveState =
  ResolveState {
    -- | The current resolution state of all scopes.
    resolveStateScopes :: !(Array ScopeID TempScope),
    -- | The scopes that need to be recalculated.
    resolveStateWorkset :: !(BitArray ScopeID),
    -- | The workset for the next round.
    resolveStateNextWorkset :: !(IOBitArray ScopeID)
  }

type ResolveT m = ReaderT ResolveState m

data RenameState =
  RenameState {
    renameStateCurr :: !TempRef,
    renameStateTab :: !(HashMap TempExp TempRef)
  }

type RenameT m = StateT RenameState m

-- What we want to do is go through the source scopes and pluck out
-- all the 'Exp's that we want to resolve, starting from the leaves
-- down.  We generate placeholder names (TempRef's) for these, which
-- serve as the ref type.
--
-- Once resolution is done, we should have terms that we can sub in
-- for every TempRef.  First, we'll convert all the terms we're
-- resolving into their resolved forms, then, we'll convert the
-- scopes.

renameSym :: (Monad m) =>
             Symbol
          -> RenameT m TempRef
renameSym ref =
  let
    tempsym = TempSym { tempSym = ref }
  in do
    -- Check the renaming table to see if we've seen this one before
    st @ RenameState { renameStateCurr = curr, renameStateTab = tab } <- get
    case HashMap.lookup tempsym tab of
      -- If we have, return its TempRef
      Just tempref -> return tempref
      -- Otherwise, generate a new one
      Nothing ->
        let
          newidx = succ curr
        in do
          put st { renameStateTab = HashMap.insert tempsym newidx tab,
                   renameStateCurr = succ newidx }
          return newidx

renameSubExp :: (Monad m) =>
                Exp Seq TempRef
             -> RenameT m TempRef
renameSubExp subexp =
  let
    tempexp = TempExp { tempExp  = subexp }
  in do
    -- Check the renaming table to see if we've seen this one before
    st @ RenameState { renameStateCurr = curr,
                       renameStateTab = tab } <- get
    case HashMap.lookup tempexp tab of
      -- If we have, return its TempRef
      Just tempref -> return tempref
      -- Otherwise, generate a new one
      Nothing ->
        let
          newidx = succ curr
        in do
          put st { renameStateTab = HashMap.insert tempexp newidx tab,
                   renameStateCurr = newidx }
          return newidx

renameExp :: (Monad m) =>
             Exp Seq Symbol
          -> RenameT m (Exp Seq TempRef)
-- Replace an Id's symbol with a TempRef
renameExp i @ Id { idRef = ref } =
  do
    tempref <- renameSym ref
    return i { idRef = tempref }
-- Translate some Project's into temprefs
renameExp p @ Project { projectVal = val, projectPos = pos } =
  do
    newval <- renameExp val
    -- Check the value
    case newval of
      -- If we're projecting from an identifier, turn this expression
      -- into an identifier too.
      Id {} ->
        do
          tempref <- renameSubExp p { projectVal = newval }
          return Id { idRef = tempref, idPos = pos }
      -- Otherwise, it's just compositional
      _ -> return p { projectVal = newval }
-- These are compositional
renameExp a @ Abs { absCases = cases } =
  do
    newcases <- mapM (mapM renameExp) cases
    return a { absCases = newcases }
renameExp m @ Match { matchVal = val, matchCases = cases } =
  do
    newval <- renameExp val
    newcases <- mapM (mapM renameExp) cases
    return m { matchVal = newval, matchCases = newcases }
renameExp a @ Ascribe { ascribeVal = val, ascribeType = ty } =
  do
    newval <- renameExp val
    newty <- renameExp ty
    return a { ascribeVal = newval, ascribeType = newty }
renameExp c @ Call { callInfo = info } =
  do
    newinfo <- mapM renameExp info
    return c { callInfo = newinfo }
renameExp r @ Record { recordFields = fields } =
  do
    newfields <- mapM renameExp fields
    return r { recordFields = newfields }
renameExp r @ RecordType { recordTypeFields = fields } =
  do
    newfields <- mapM renameExp fields
    return r { recordTypeFields = newfields }
renameExp t @ Tuple { tupleFields = fields } =
  do
    newfields <- mapM renameExp fields
    return t { tupleFields = newfields }
renameExp w @ With { withVal = val, withArgs = args } =
  do
    newval <- renameExp val
    newargs <- renameExp args
    return w { withVal = newval, withArgs = newargs }
renameExp w @ Where { whereVal = val, whereProp = prop } =
  do
    newval <- renameExp val
    newprop <- renameExp prop
    return w { whereVal = newval, whereProp = newprop }
renameExp a @ Anon { anonParams = params } =
  do
    newparams <- mapM renameExp params
    return a { anonParams = newparams }
-- These are an exact translation
renameExp Compound { compoundScope = scopeid, compoundPos = pos } =
  return Compound { compoundScope = scopeid, compoundPos = pos }
renameExp Literal { literalVal = lit } = return Literal { literalVal = lit }
renameExp Bad { badPos = pos } = return Bad { badPos = pos }

renameScope :: (MonadMessages Message m) =>
               Scope (Exp Seq Symbol)
            -> m ([Proof TempRef],
                  [Import TempRef],
                  [TempRef],
                  HashMap Symbol (Builder (Exp Seq TempRef)),
                  HashMap Symbol (Syntax (Exp Seq TempRef)),
                  HashMap Symbol (Truth (Exp Seq TempRef)),
                  Array DefID (Def (Exp Seq TempRef)),
                  [Compound (Exp Seq TempRef)],
                  Array TempRef TempExp)
renameScope Scope { scopeBuilders = builders, scopeSyntax = syntax,
                    scopeTruths = truths, scopeDefs = defs,
                    scopeEval = eval, scopeProofs = proofs,
                    scopeImports = imports, scopeInherits = inherits } =
  let
    -- Start with def, that way we never end up assigning it
    initstate = RenameState { renameStateCurr = def,
                              renameStateTab = HashMap.empty }

    -- Imports, inherits, and proofs are static expressions.  These
    -- should always be converted into TempRefs
    renameStaticExp :: (MonadMessages Message m) =>
                       Exp Seq Symbol
                    -> RenameT m TempRef
    renameStaticExp ex =
      do
        -- Do the renaming
        renamed <- renameExp ex
        -- We should always see an Id.  It's an internal error if we don't.
        case renamed of
          Id { idRef = ref } -> return ref
          _ ->
            do
              internalError "Expected static expression here" [position ex]
              -- Return the default symbol
              return def

    renameScope' =
      let
      in do
        newproofs <- mapM (mapM renameStaticExp) proofs
        newimports <- mapM (mapM renameStaticExp) imports
        newinherits <- mapM renameStaticExp inherits
        newbuilders <- mapM (mapM renameExp) builders
        newsyntax <- mapM (mapM renameExp) syntax
        newtruths <- mapM (mapM renameExp) truths
        newdefs <- mapM (mapM renameExp) defs
        neweval <- mapM (mapM renameExp) eval
        return (newproofs, newimports, newinherits, newbuilders,
                newsyntax, newtruths, newdefs, neweval)

    renametab endidx tab =
      let
        elems = map (\(a, b) -> (b, a)) (HashMap.toList tab)
        arr = Array.array (succ def, endidx) elems
      in
        arr
  in do
    ((proofs, imports, inherits, builders, syntax, truths, defs, eval),
     RenameState { renameStateCurr = endidx, renameStateTab = tab }) <-
      runStateT renameScope' initstate
    return (proofs, imports, inherits, builders, syntax,
            truths, defs, eval, renametab endidx tab)

-- | Convert a 'TempScope' into a 'Resolved' scope.
finishScope :: (MonadIO m) =>
               (ScopeID, TempScope)
            -> m (ScopeID, Resolved (Exp Seq Ref))
finishScope (scopeid, TempScope { tempScopeBuilders = builders,
                                  tempScopeSyntax = syntax,
                                  tempScopeTruths = truths,
                                  tempScopeDefs = defs,
                                  tempScopeEval = eval,
                                  tempScopeNames = names,
                                  tempScopeEnclosing = enclosing,
                                  tempScopeProofs = proofs }) =
  let
    subst tmprefvals = (>>= (tmprefvals Array.!))
  in do
    tmprefvals <- _
    return (scopeid,
            Resolved {
              -- Names and enclosing scopes carry over directly
              resolvedNames = names,
              resolvedEnclosing = enclosing,

              resolvedProofs = _,
              resolvedImports = _,
              resolvedInherits = _,
              -- For the rest, substitute the expression back in
              resolvedBuilders = fmap (fmap (subst tmprefvals)) builders,
              resolvedSyntax = fmap (fmap (subst tmprefvals)) syntax,
              resolvedTruths = fmap (fmap (subst tmprefvals)) truths,
              resolvedDefs = fmap (fmap (subst tmprefvals)) defs,
              resolvedEval = fmap (fmap (subst tmprefvals)) eval
            })

-- | Core resolution algorithm.
resolveScopes :: (MonadIO m) =>
                 Array ScopeID TempScope
              -- ^ The current scope state
              -> BitArray ScopeID
              -- ^ The current workset
              -> IOBitArray ScopeID
              -- ^ The array into which to accumulate the next workset
              -> m (Array ScopeID TempScope)
resolveScopes tempscopes workset nextworkset =
  let
    arrbounds = Array.bounds tempscopes

    resolveScope :: (MonadIO m) =>
                    (ScopeID, TempScope)
                 -> m (ScopeID, TempScope)
    -- Skip this scope if it's not in the workset
    resolveScope elem @ (idx, _) | workset BitArray.! idx = return elem
    resolveScope curr @ (scopeid,
                         TempScope { tempScopeSyms = resolutions }) =
      let
        syms = HashMap.keys resolutions

        -- | Resolve one symbol
        resolveSymbol :: (MonadIO m) =>
                         Symbol
                      -- ^ The symbol to resolve
                      -> m (Symbol, Resolution)
        resolveSymbol sym = _

        finishScope :: (MonadIO m) =>
                       HashMap Symbol Resolution
                    -> m (ScopeID, TempScope)
        finishScope newresolutions
          | resolutions == newresolutions = return curr
          | otherwise =
            do
              -- Resolve all symbols
              -- Use the new resolutions to resolve all imports

              -- Use the new resoultions to resolve all inherits

              -- Propagate this change to all the scopes that
              -- depend on this one
              _
      in do
        -- Resolve all symbols using the previous scope
        newresolutions <- mapM resolveSymbol syms
        finishScope (HashMap.fromList newresolutions)

    resolveRound :: (MonadIO m) =>
                    m (Array ScopeID TempScope)
    resolveRound =
      do
        -- Attempt to resolve all the external symbols
        newtempscopes <- mapM resolveScope (Array.assocs tempscopes)
        return (Array.array arrbounds newtempscopes)
  in do
    -- Clear the next workset array
    liftIO (IOBitArray.fill nextworkset False)
    -- Run one round
    newtempscopes <- resolveRound
    -- Check to see if we're done
    done <- liftIO (IOBitArray.and nextworkset)
    if done
      -- If the next workset is empty, then we're done
      then return newtempscopes
      -- Otherwise, recurse
      else do
        -- Make the next workset
        newworkset <- liftIO (IOBitArray.freeze nextworkset)
        -- Recurse with the new tempscopes and workset array
        resolveScopes newtempscopes newworkset nextworkset

-- | Resolve all references in a 'Surface' syntax structure and link
-- all scopes together.
resolve :: (MonadMessages Message m, MonadIO m) =>
           Surface (Scope (Exp Seq Symbol))
        -- ^ The syntax structure to resolve.
        -> m (Surface (Resolved (Exp Seq Ref)))
        -- ^ The resolved syntax structure.
resolve surface @ Surface { surfaceScopes = scopes } =
  let
    arrbounds = Array.bounds scopes

    -- Create the initial state for resolution
    initialState :: (MonadMessages Message m, MonadIO m) =>
                    m (Array ScopeID TempScope)
    initialState =
      let
        mapfun :: (MonadMessages Message m, MonadIO m) =>
                  IOArray ScopeID [ScopeID]
               -> (ScopeID, Scope (Exp Seq Symbol))
               -> m (ScopeID, HashMap Symbol Resolution,
                     [Proof TempRef], [Import TempRef], [TempRef],
                     HashMap Symbol (Builder (Exp Seq TempRef)),
                     HashMap Symbol (Syntax (Exp Seq TempRef)),
                     HashMap Symbol (Truth (Exp Seq TempRef)),
                     Array DefID (Def (Exp Seq TempRef)),
                     [Compound (Exp Seq TempRef)],
                     Array TempRef TempExp)
        mapfun enclosedarr (scopeid, scope) =
          let
            -- Gather up all the references
            syms = foldl (foldl (flip HashSet.insert)) HashSet.empty scope
            -- Make everything undefined initially
            ents = zip (HashSet.toList syms) (repeat Undefined)
          in do
            case scope of
              Scope { scopeEnclosing = Just enclosing } ->
                do
                  enclosed <- liftIO (IOArray.readArray enclosedarr enclosing)
                  liftIO (IOArray.writeArray enclosedarr enclosing
                                             (scopeid : enclosed))
              _ -> return ()
            (proofs, imports, inherits, builders,
             syntax, truths, defs, eval, renametab) <- renameScope scope
            return (scopeid, HashMap.fromList ents, proofs, imports, inherits,
                    builders, syntax, truths, defs, eval, renametab)

        -- Create the 'TempScope's out of their components
        makeTempScopes :: MonadIO m =>
                          Array ScopeID [ScopeID]
                       -> [(ScopeID, HashMap Symbol Resolution,
                            [Proof TempRef], [Import TempRef], [TempRef],
                            HashMap Symbol (Builder (Exp Seq TempRef)),
                            HashMap Symbol (Syntax (Exp Seq TempRef)),
                            HashMap Symbol (Truth (Exp Seq TempRef)),
                            Array DefID (Def (Exp Seq TempRef)),
                            [Compound (Exp Seq TempRef)],
                            Array TempRef TempExp)]
                       -> m (Array ScopeID TempScope)
        makeTempScopes enclosedarr scopedata =
          let
            -- Map function
            makeTempScope (scopeid, syms, proofs, imports, inherits,
                           builders, syntax, truths, defs, eval, renametab) =
              let
                Scope { scopeEnclosing = enclosing,
                        scopeNames = names } = scopes Array.! scopeid
                -- Lookup the enclosed scopes
                enclosed = enclosedarr Array.! scopeid
              in do
                return (scopeid, TempScope { tempScopeNames = names,
                                             tempScopeEnclosing = enclosing,
                                             tempScopeProofs = proofs,
                                             tempScopeImports = imports,
                                             tempScopeInherits = inherits,
                                             tempScopeBuilders = builders,
                                             tempScopeSyntax = syntax,
                                             tempScopeTruths = truths,
                                             tempScopeDefs = defs,
                                             tempScopeEval = eval,
                                             tempScopeRenameTab = renametab,
                                             tempScopeSyms = syms,
                                             tempScopeEnclosed = enclosed })
          in do
            tempscopes <- mapM makeTempScope scopedata
            return (Array.array arrbounds tempscopes)

      in do
        -- Run through the scopes, extract the referenced symbols,
        -- link up the encloded scopes.
        enclosedarr <- liftIO (IOArray.newArray arrbounds [])
        tempscopes <- mapM (mapfun enclosedarr) (Array.assocs scopes)
        frozenenclosed <- liftIO (IOArray.unsafeFreeze enclosedarr)
        makeTempScopes frozenenclosed tempscopes

    initworkset = (BitArray.true arrbounds)
  in do
    -- Set up the initial state
    nextworkset <- liftIO (IOBitArray.newArray arrbounds True)
    initscopes <- initialState
    -- Run resolution
    finalscopes <- resolveScopes initscopes initworkset nextworkset
    -- Finalize the results and report errors
    resolvedscopes <- mapM finishScope (Array.assocs finalscopes)
    return surface { surfaceScopes = Array.array arrbounds resolvedscopes }

{-
import Control.Monad.Messages
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Array.IO(IOArray)
import Data.Semigroup
import Language.Salt.Message
import Language.Salt.Surface.Common

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.Array.BitArray.IO as BitArray
import qualified Data.Array.IO as Array

instance Semigroup Dependence where
  Imported { importedSyms = symsa, importedInherit = inherita } <>
  Imported { importedSyms = symsb, importedInherit = inheritb } =
    Imported { importedSyms = symsa <> symsb,
               importedInherit = inherita || inheritb }
  i @ Imported {} <> Inherited = i { importedInherit = True }
  Inherited <> i @ Imported {} = i { importedInherit = True }
  Inherited <> Inherited = Inherited
  En


-- | State of elaboration of scopes.
data ElaborationState =
  ElaborationState {
    -- | A bitmap of the pending scope completions.  Used to catch cycles.
    elaboratePending :: !(IOBitArray ScopeID),
    -- | A bitmap of all the completed scopes.
    elaborateDone :: !(IOBitArray ScopeID),
    -- | Array mapping @ScopeID@s to completed scopes.
    elaborateScopes :: !(IOArray ScopeID Scope.Scope)
  }

data TempScope =
  TempScope {
    -- | Inherited scopes.
    scopeInherits :: !(HashMap ScopeID Scope.Scope),
    -- | Truths.
    scopeTruths :: !(HashMap Symbol Truth),
  }

-- | A reference to a scope.  This is what is stored in the stack of
-- scopes.
data ScopeRef =
  ScopeRef {
    -- | Whether or not local definitions are accessible from this scope.
    refLocalAccess :: !Bool,
    -- | Whether or not object definitions are accessible from this
    -- scope reference.
    refObjectAccess :: !Bool,
    -- | The ID of the scope.
    refID :: !ScopeID,
    -- | The actual scope.
    refScope :: !Bindings.Scope
  }

type CompleteT = ReaderT CompletionState

-- | Get the completed form of a scope
scope :: (Monad m) =>
         ScopeID
      -- ^ The ID of the scope to get.
      -> CompleteT m Bindings.Scope
      -- ^ A monad that generates the completed scope.
      -> CompleteT m Bindings.Scope
      -- ^ A monad that generates an error value upon detection of a cycle.
      -> CompleteT m Bindings.Scope
scope scopeid create cycle =
  do
    CompleteState { completePending = pendingarr, completeDone = donearr,
                    completeScopes = scopesarr } <- ask
    -- Check the pending bits.
    pending <- BitArray.readArray pendingarr scopeid
    if pending
      -- Call the error action on a cycle.
      then cycle
      -- Otherwise, lookup or create the scope.
      else do
        -- Check the completed bit.
        completed <- Bitarray.readArray donearr scopeid
        if completed
          -- If the completed flag is set, then get the scope out of
          -- the completed scopes array.
          then Array.readArray scopesarr scopeid
          else do
            -- Set the pending bit.
            BitArray.writeArray pendingarr scopeid True
            -- Create the result.
            out <- create
            -- Clear the pending bit and set the done bit.
            BitArray.writeArray pendingarr scopeid False
            BitArray.writeArray donearr scopeid True
            -- Store the result.
            Array.writeArray scopesarr out
            -- Finally return it.
            return out

-- Resolution results are a monoid, so that we can combine results
-- from resolution in multiple scopes.
instance Monoid (Resolve a) where
  mempty = Undef

  -- Out-of-context accesses beat everything else if they come first.
  mappend c @ Context { contextObject = False } _ = c
  mappend c @ Context { contextObject = True } _ = c
  -- The first valid reference beats everything but an out-of-context access.
  mappend v @ Valid {} _ = v
  -- Later out-of-context accesses beat everything but a valid response.
  mappend _ c @ Context { contextObject = False } = c
  mappend _ c @ Context { contextObject = True } = c
  -- A later valid reference beats everything but an out-of-context access.
  mappend _ v @ Valid {} = v
  -- If there's no valid reference or out-of-context access, take the first
  -- illegal access.
  mappend i @ Illegal { illegalPrivate = False } _ = i
  mappend _ i @ Illegal {illegalPrivate = False } = i
  mappend i @ Illegal { illegalPrivate = True } _ = i
  mappend _ i @ Illegal {illegalPrivate = True } = i
  -- Undefs get ignored unless that's all there is.
  mappend Undef Undef = Undef

-- | Lookup a local definition in a scope.  This does not chase down
-- inherited scopes.
localValueSym :: Symbol
              -- ^ The symbol to lookup.
              -> Visibility
              -- ^ The expected visibility level of the lookup.
              -> ScopeRef
              -- ^ The 'ScopeRef' in which to do the lookup.
              -> Resolve [Bindings.Bind]
localValueSym sym vis ScopeRef { refContext = ctx, refID = scopeid,
                                 refLocalAccess = localaccess,
                                 refObjectAccess = objectaccess,
                                 refScope = Scope { scopeDefs = localdefs } } =
  let
    -- Check to see if an access is illegal
    checkIllegal =
      case vis of
        -- Check for protected or private binds.
        Public -> case (bindarr ! Protected, bindarr ! Private) of
          -- There aren't any private or protected defs, so the only
          -- other ones must be hidden.  Report undef.
          ([], []) -> Undef
          -- We found private binds, so report an illegal access
          ([], _) -> Illegal { illegalScope = scopeid, illegalPrivate = True }
          -- We found protected bindings, so report an illegal access
          (_, _) -> Illegal { illegalScope = scopeid, illegalPrivate = False }
        -- For protected, see if there are any private defs
        Protected -> case bindarr ! Private of
          -- There aren't so the only other defs must be hidden
          [] -> Undef
          -- We found binds, so report an illegal access
          _ -> Illegal { illegalScope = scopeid, illegalPrivate = True }
        -- We're looking at private level: the only other
        -- definitions must be Hidden, so we don't report errors for
        -- them.
        Private -> Undef
        -- This case should never happen
        Hidden -> error "Impossible case"

  in
    -- Lookup the bindings in the table
    case HashMap.lookup sym vals of
      -- If we get an object binding and object bindings aren't
      -- accessible, then report out of context.
      Just Binds { bindsContext = Object } | not objectaccess ->
        Context { contextObject = True, contextScope = scopeid }
      -- If we get a local binding and local bindings aren't
      -- accessible, then report out of context.
      Just Binds { bindsContext = Local } | not localaccess ->
        Context { contextObject = False, contextScope = scopeid }
      -- Otherwise, we have an accessible right context, so figure out
      -- what to do.
      Just Binds { bindsViews = bindarr } ->
        -- Look up the binds for this visibility level (remember,
        -- binds are a telescoping list)
        case bindarr ! vis of
          -- If we get nothing, check for possible illegal access
          [] -> checkIllegal
          -- If there's a non-empty list at the given visibility,
          -- return a Valid binding.
          out -> Valid { validScope = scopeid, validContent = out }
      -- There are no bindings, so return Undef
      Nothing -> Undef

lookupValueSym :: Symbol
               -- ^ The symbol to look up.
               -> ScopeRef
               -- ^ The scope in which to do the lookup
               -> Resolve [Bindings.Bind]
lookupValueSym sym ref @ ScopeRef {
                           refID = localid,
                           refScope = Scope { scopeInherits = localinherits }
                         } =
  let
    lookupInheritedValue :: ScopeID -> Scope -> Resolve [Bindings.Bind]
    lookupInheritedValue scopeid Scope { scopeInherits = inherits,
                                         scopeDefs = defs } =
      localValueSym sym Protected scopeid defs <>
      foldMapWithKey (lookupInheritedValue

    -- Lookup any symbol in the local scope, then try the inherited scopes.
    result = localValueSym sym Hidden localid localdefs
  in

-- | Attempt to resolve a value symbol in a given scope.
resolveValueSym :: (MonadMessages Message m) =>
                   Symbol
                -- ^ The symbol being resolved.
                -> DWARFPosition
                -- ^ The position at which the reference occurred.
                -> [ScopeRef]
                -- ^ The stack of scopes.
                -> m Scope.Exp
resolveValueSym sym pos scopes =
  -- Convert the Resolve into an expression and report any errors.
  -- Note: this depends on laziness to be efficient!
  case mconcat lookupValueSym scopes of
    -- For valid results, convert it into a Sym expression
    Valid { validScope = scopeid } ->
      return Bindings.Sym { symName = sym, symScope = scopeid, symPos = pos }
    -- For the rest, we have an error.  Report the error and return a
    -- bad expression.
    Illegal { illegalPrivate = private } ->
      do
        if private
          then privateAccess sym pos
          else protectedAccess sym pos
        return Bindings.BadExp { badExpPos = pos }
    Context { contextObject = object } ->
      do
        if object
          then objectAccess sym pos
          else localAccess sym pos
        return Bindings.BadExp { badExpPos = pos }
    Undef ->
      do
        undefSymbol sym pos
        return Bindings.BadExp { badExpPos = pos }

badProject :: Strict.ByteString
badProject = Strict.fromString "Empty field list in projection"

-- | Resolve a set of imports.
elaborateImports :: (MonadMessages Message m, MonadIO m) =>
                   Syntax.Exp
                -- ^ The import expression.
                -> m [Scope.Scope]
                -- ^ The scope referred to by the import
elaborateImports =
  let
    -- This is a worklist-fixedpoint algorithm.  We run over
    -- unresolved imports until we hit a fixed point.  Once we do, we
    -- convert any unresolved imports into error messages.

    -- | Resolve a single import.  Returns a scope if it could be
    -- resolved, or Nothing if it couldn't.
    completeImport :: (MonadMessages Message m, MonadIO m) =>
                     ([Syntax.Exp], [Syntax.Exp]) -> Syntax.Exp ->
                     m ([Syntax.Exp], [Syntax.Exp])
                  -- ^ The scope referred to by the import
    completeImport accum Syntax.With { Syntax.withPos = pos } = ()
    -- We can't import from a with expression
    completeImport accum Syntax.Seq { Syntax.seqExps = func : args } = ()
    -- If importing from a call, check all the arguments, and pull in the
    -- scope from the "function".

    -- Use elaborateStaticExp for the function and arguments.  This is OK;
    -- syntax directives don't apply to static expressions.

    -- For a symbol, look for a builder of that name.  If we find it, pull
    -- in the scope.
    completeImport accum Syntax.Sym { } = ()

    -- XXX A split could be tricky.  If it ends up referring to a
    -- definition in our own (incomplete) scope, then we've got a cycle.
    completeImport accum Syntax.Project { Syntax.projectFields = fields } =
      case HashMap.assocs fields of
        -- An empty project list can't possibly happen
        [] -> internalError badProject pos >> return accum
        -- Importing one field
        [(fname, ent)] -> ()
        -- Check that the base expression isn't referring to a builder that we
        -- ourselves define

        -- For multiple fields,
        multi -> ()

    -- An empty fields list is an internal error
    -- You can't import an anonymous builder instance.
    completeImport accum Syntax.Anon { Syntax.anonPos = pos } =
      do
        importNestedScope pos
        return accum
    -- The rest of these can't occur in a static expression.
    completeImport nonstatic =
      let
        pos = Syntax.expPosition nonstatic
        badStatic = Strict.fromString "Expression is not a static expression"
      in do
        internalError badStatic pos
        return accum

    -- | Run over unresolved imports once.
    resolvePass :: (MonadMessages Message m, MonadIO m) =>
                   ([Syntax.Exp], [Syntax.Exp]) ->
                   m ([Syntax.Exp], [Syntax.Exp])
    resolvePass (resolved, unresolved) =
      foldM completeImport (resolved, []) unresolved

    -- | Run passes until we reach a fixed point.
  in
    -- Now convert all unresolved imports into an error message.

-- | Construct a scope by pulling in all definitions.
elaborateScope :: (MonadMessages Message m, MonadIO m) =>
                  Surface.Scope
               -- ^ The scope being elaborated.
               -> m Bindings.Scope
elaborateScope Syntax.Scope { Syntax.scopeID = scopeID } = _
-- First, pull in all the all the imports

-- | Elaborate one component.  This generates the top-level scope and
-- then checks that the expected definition is actually present.
elaborateComponent :: (MonadMessages Message m, MonadIO m) =>
                      Syntax.Component
                   -- ^ The component being elaborated.
                   -> m Scope.Scope
-- First, enter a new scope for the component.
-- Once all the defs are in, check that the expected definition exists
-- At the end, pop the scope and turn it into a top-level.

convert :: STArray s ScopeID Scope
        -> [Syntax.Component]
        -> ST s (STArray s ScopeID Scope)
convert arr compontents =
  let
  in do
    mapM_ convertComponent components
    return arr

-- | Bind a set of components.  The components are expected to
-- represent a closed set, meaning no external dependencies.  Binding
-- turns them into a set of top-level definitions with inter-linked
-- scopes.
bind :: (MonadMessages Message m, MonadIO m) =>
        [Syntax.Component]
     -> m Scope.Scopes
bind components =
  let

  in
-- First, convert all the trees, with placeholders at all references
-- Now, run a fixed-point computation to resolve all references
-- Last, convert the tree to its final form
-- Check that all the expected definitions are present
-}
