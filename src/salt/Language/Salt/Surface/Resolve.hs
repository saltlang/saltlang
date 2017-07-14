-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
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

import Control.Monad.Except
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
import Data.List
import Data.PositionElement
import Data.Position.BasicPosition
import Data.ScopeID
import Data.Semigroup
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
    Valid
    -- | Access to an out-of-context element.
  | Inaccessible {
      -- | True for object context, false for local.
      inaccessibleObject :: !Bool
    }
    -- | An illegal access result.
  | Illegal {
      -- | True for private access, false for protected.
      illegalVisibility :: !Visibility
    }
    deriving (Eq, Ord)

-- | Resolution path element.  A sequence of these describe how an
-- indirect resolution is resolved.
data PathElem =
    -- | A definition imported from another scope.
    Imported {
      -- | The set of scopes from which this definition is imported.
      -- Non-singleton maps denate ambiguity.
      importedScope :: !ScopeID,
      -- | The expression for the import.
      importedScopeExp :: !TempExp
    }
    -- | An inherited definition.
  | Inherited {
      -- | The set of scopes from which the definition is inherited.
      inheritedScope :: !ScopeID,
      -- | The expression for the inherited scope.
      inheritedScopeExp :: !TempExp
    }
    -- | A definition in an enclosing scope.
  | Enclosing {
      -- | Enclosing scope from which this was captured.
      enclosingScope :: !ScopeID,
      -- | Depth of enclosing scope capture.
      enclosingDepth :: !Word
    }
    deriving (Eq, Ord)

-- | Non-Local resolution result.  This is separate, because we only
-- store non-local resolutions in the dependence set.
data NonLocal =
  NonLocal {
    -- | The scope in which the resolved definition is defined.
    nonLocalScope :: !ScopeID,
    -- | The name of the resolved definition in its defining scope.
    nonLocalSym :: !Symbol,
    -- | The source of the non-local resolution in the local scope
    -- (this is also teh first element of the full resolution path).
    nonLocalSrc :: !PathElem,
    -- | The rest of the resolution path by which this result was
    -- derived.
    nonLocalTail :: ![PathElem],
    -- | All resolution results on which this resolution depends.
    nonLocalDeps :: !(HashSet NonLocal)
  }
  deriving (Eq)

data Resolution =
    -- | Locally-defined symbol.  This is always valid, so no need for
    -- validity.
    Direct
    -- | Resolution from a different scope.
  | Indirect {
      -- | Resolution data.
      indirectRes :: !NonLocal
    }
    deriving (Eq, Ord)

data Error =
    Cyclic { cyclicRefs :: !(HashSet TempRef) }
  | Ambiguous { ambiguousRefs :: !(HashSet Resolution) }
    deriving (Eq)

-- | Placeholders for anything that might be a static expression.
-- These are substituted into expressions in a manner similar to
-- common subexpression elimination at the beginning of resolution,
-- then the resolved values are substituted back in at the end.
newtype TempRef = TempRef { tempRefID :: Word }
  deriving (Eq, Ord, Ix)

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
    tempScopeSyms :: !(HashMap Symbol (Maybe Resolution)),
    -- | All enclosed scopes
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

instance Ord NonLocal where
  compare NonLocal { nonLocalScope = scopeid1, nonLocalSym = sym1,
                     nonLocalSrc = src1, nonLocalTail = tail1,
                     nonLocalDeps = deps1 }
          NonLocal { nonLocalScope = scopeid2, nonLocalSym = sym2,
                     nonLocalSrc = src2, nonLocalTail = tail2,
                     nonLocalDeps = deps2 } =
    let
      sorteddeps1 = sort (HashSet.toList deps1)
      sorteddeps2 = sort (HashSet.toList deps2)
    in case compare scopeid1 scopeid2 of
      EQ -> case compare sym1 sym2 of
        EQ -> case compare src1 src2 of
          EQ -> case compare tail1 tail2 of
            EQ -> compare sorteddeps1 sorteddeps2
          out -> out
        out -> out
      out -> out

instance Hashable Access where
  hashWithSalt s Valid = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Inaccessible { inaccessibleObject = object } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` object
  hashWithSalt s Illegal { illegalVisibility = vis } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` vis

instance Hashable PathElem where
  hashWithSalt s Imported { importedScope = scope, importedScopeExp = ex } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` scope `hashWithSalt` ex
  hashWithSalt s Inherited { inheritedScope = scope, inheritedScopeExp = ex } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` scope `hashWithSalt` ex
  hashWithSalt s Enclosing { enclosingScope = scope, enclosingDepth = depth } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` scope `hashWithSalt` depth

instance Hashable NonLocal where
  hashWithSalt s NonLocal { nonLocalScope = scopeid, nonLocalSym = sym,
                            nonLocalSrc = src, nonLocalTail = tail,
                            nonLocalDeps = deps } =
    s `hashWithSalt` scopeid `hashWithSalt` sym `hashWithSalt`
    src `hashWithSalt` tail `hashWithSalt` deps

instance Hashable Resolution where
  hashWithSalt s Direct = s `hashWithSalt` (0 :: Int)
  hashWithSalt s Indirect { indirectRes = res } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` res

smallest :: Word -> Word -> Word
smallest a b
  | a <= b = a
  | otherwise = b

instance Semigroup NonLocal where
  out @ NonLocal { nonLocalSrc = Imported {} } <> _ = out
  _ <> out @ NonLocal { nonLocalSrc = Imported {} } = out
  out @ NonLocal { nonLocalSrc = Inherited {} } <> _ = out
  _ <> out @ NonLocal { nonLocalSrc = Inherited {} } = out
  out <> _ = out

instance Semigroup Resolution where
  Direct <> Direct = Direct
  Direct <> _ = Direct
  _ <> Direct = Direct
  Indirect { indirectRes = res1 } <> Indirect { indirectRes = res2 } =
    Indirect { indirectRes = res1 <> res2 }

instance Default TempRef where
  def = TempRef { tempRefID = 0 }

instance Enum TempRef where
  fromEnum = fromEnum . tempRefID
  toEnum = TempRef . toEnum

instance Hashable TempRef where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable TempExp where
  hashWithSalt s TempExp { tempExp = ex } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` ex
  hashWithSalt s TempSym { tempSym = sym } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym

-- | Get the full non-local resolution path
nonLocalPath :: NonLocal -> [PathElem]
nonLocalPath NonLocal { nonLocalSrc = first, nonLocalTail = rest } =
  first : rest

-- What we want to do is go through the source scopes and pluck out
-- all the 'Exp's that we want to resolve, starting from the leaves
-- down.  We generate placeholder names (TempRef's) for these, which
-- serve as the ref type.
--
-- Once resolution is done, we should have terms that we can sub in
-- for every TempRef.  First, we'll convert all the terms we're
-- resolving into their resolved forms, then, we'll convert the
-- scopes.

-- | Rename a symbol.  This generates a unique TempRef for each
-- 'Symbol'.
renameSym :: (Monad m) =>
             Symbol
          -- ^ The symbol to rename.
          -> RenameT m TempRef
          -- ^ A 'TempRef' representing this 'Symbol'.
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

-- | Rename an entire expression.  This generates a unique TempRef for
-- a given 'Exp' tree.  Identical 'Exp' trees will be renamed to the
-- same 'TempRef'.
renameSubExp :: (Monad m) =>
                Exp Seq TempRef
             -- ^ Expression to rename.
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
-- Always replace 'Id's symbol with a 'TempRef'
renameExp i @ Id { idRef = ref } =
  do
    tempref <- renameSym ref
    return i { idRef = tempref }
-- Translate 'Project's into 'TempRef's, as long as their inner
-- expression is also translated into a 'TempRef'
renameExp p @ Project { projectVal = val, projectPos = pos } =
  do
    newval <- renameExp val
    -- Check the value
    case newval of
      -- If we're projecting from a tempref, then turn this expression
      -- into a tempref as well
      Id {} ->
        do
          tempref <- renameSubExp p { projectVal = newval }
          return Id { idRef = tempref, idPos = pos }
      -- Otherwise, it's just compositional
      _ -> return p { projectVal = newval }

-- XXX We'll need to do something with calls.  Probably the thing to
-- do is have static calls come out of the parser.
--
-- On the other hand, if we match the form of calls coming out of the
-- parser, then we get potentially static calls in non-static
-- expressions as well.  Problem with that is that we might have
-- postfix or infix expressions referring to builders.
--
-- Obvervation: the only time builders in calls can affect another
-- scope is in a static expression, which we know is of the form
-- [builder, args].  Therefore, we can effectively delay handling of
-- builder calls in non-static expressions.
renameExp c @ Call { callInfo = s @ Seq { seqExps = [func, arg @ Record {}],
                                          seqPos = pos } } =
  do
    newfunc <- renameExp func
    newarg <- renameExp arg
    -- Check the function
    case newfunc of
      -- If we're calling a tempref, then turn this expression into a
      -- tempref as well
      Id {} ->
        let
          newcall = c { callInfo = s { seqExps = [newfunc, newarg ] } }
        in do
          tempref <- renameSubExp newcall
          return Id { idRef = tempref, idPos = pos }
      -- Otherwise, it's just compositional
      _ -> return c { callInfo = s { seqExps = [newfunc,  newarg ] } }
renameExp c @ Call { callInfo = s @ Seq { seqExps = [func, arg @ Tuple {}],
                                          seqPos = pos } } =
  do
    newfunc <- renameExp func
    newarg <- renameExp arg
    -- Check the function
    case newfunc of
      -- If we're calling a tempref, then turn this expression into a
      -- tempref as well
      Id {} ->
        let
          newcall = c { callInfo = s { seqExps = [newfunc, newarg ] } }
        in do
          tempref <- renameSubExp newcall
          return Id { idRef = tempref, idPos = pos }
      -- Otherwise, it's just compositional
      _ -> return c { callInfo = s { seqExps = [newfunc,  newarg ] } }
-- Otherwise function calls are just compositional
renameExp c @ Call { callInfo = info } =
  do
    newinfo <- mapM renameExp info
    return c { callInfo = newinfo }
-- The rest are compositional
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

-- Performs renaming on a scope, turning everything that could be a
-- static expression into a TempRef.
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
    -- | Substitute the term for a 'TempRef' back in
    subst :: Array TempRef (Exp Seq Ref)
          -> Exp Seq TempRef
          -> Exp Seq Ref
    subst tmprefvals = (>>= (tmprefvals Array.!))
  in do
    tmprefvals <- _
    return (scopeid,
            Resolved {
              -- Names and enclosing scopes carry over directly
              resolvedNames = names,
              resolvedEnclosing = enclosing,

              resolvedImports = _,
              resolvedInherits = _,
              -- For proofs, just look up the value
              resolvedProofs = fmap (fmap (tmprefvals Array.!)) proofs,
              -- For the rest, substitute the expression back in
              resolvedBuilders = fmap (fmap (subst tmprefvals)) builders,
              resolvedSyntax = fmap (fmap (subst tmprefvals)) syntax,
              resolvedTruths = fmap (fmap (subst tmprefvals)) truths,
              resolvedDefs = fmap (fmap (subst tmprefvals)) defs,
              resolvedEval = fmap (fmap (subst tmprefvals)) eval
            })

-- | Report an undefined symbol.
undef :: (Monad m) => ExceptT (Maybe Error) m a
undef = throwError Nothing

cyclic :: (Monad m) => HashSet TempRef -> ExceptT (Maybe Error) m a
cyclic refs = throwError $! Just Cyclic { cyclicRefs = refs }

-- | Report an ambiguous inherited symbol
ambiguous :: (Monad m) => HashSet Resolution -> ExceptT (Maybe Error) m a
ambiguous refs = throwError $! Just Ambiguous { ambiguousRefs = refs }

-- | Core resolution algorithm.
resolveScopes :: (MonadIO m, MonadMessages Message m) =>
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

    -- | Try to get the ScopeID of an Exp representing a builder reference.
    lookupScopeExp :: (MonadIO m, MonadMessages Message m) =>
                      TempScope
                   -> HashSet TempRef
                   -> Exp Seq TempRef
                   -> ExceptT (Maybe Error) m ScopeID
    -- For projects, look up the base scope, then do a lookup of the
    -- field name in the base scope.
    lookupScopeExp scope history Project { projectFields = fields,
                                           projectVal = base,
                                           projectPos = pos } =
      case HashSet.toList fields of
        [FieldName { fieldSym = sym }] ->
          do
            basescope <- lookupScopeExp scope history base
            lookupScopeSymbol (tempscopes Array.! basescope) history sym
        [] ->
          do
            internalError "Should not see empty projected fields set" [pos]
            undef
        _ ->
          do
            internalError "Should not see multiple projected fields" [pos]
            undef

    -- For Id, look up the temp ref (note that cycles will be handled
    -- by that function).
    lookupScopeExp scope history Id { idRef = ref } =
      lookupScopeTempRef scope history ref
    -- XXX Same as With, should this get turned into a separate scope?
    lookupScopeExp scope history Call { callInfo = Seq { seqExps = [inner, _]} } =
      lookupScopeExp scope history inner
    -- XXX What do we do here? Possibly create a separate scope for this?
    lookupScopeExp scope history With { withVal = val } =
      lookupScopeExp scope history val
    lookupScopeExp scope history Where { whereVal = val } =
      lookupScopeExp scope history val
    -- For a builder expression, return the scope id.
    lookupScopeExp _ _ Anon { anonScope = scopeid } = return scopeid
    -- Skip bad scopes
    lookupScopeExp _ _ Bad {} = undef
    -- Everything else is an internal error
    lookupScopeExp _ _ ex =
      do
        internalError "Expected static expression" [position ex]
        undef

    -- | Try to get the ScopeID of a Symbol representing a builder reference.
    lookupScopeSymbol :: (MonadIO m, MonadMessages Message m) =>
                         TempScope
                      -> HashSet TempRef
                      -> Symbol
                      -> ExceptT (Maybe Error) m ScopeID
    lookupScopeSymbol scope @ TempScope { tempScopeBuilders = builders }
                      history sym =
      case HashMap.lookup sym builders of
        Just Builder { builderContent = content } ->
          lookupScopeExp scope history content
        Nothing -> undef

    -- | Try to get the ScopeID of a TempExp representing a builder reference.
    lookupScopeTempExp :: (MonadIO m, MonadMessages Message m) =>
                           TempScope
                        -> HashSet TempRef
                        -> TempExp
                        -> ExceptT (Maybe Error) m ScopeID
    lookupScopeTempExp ctx @ TempScope { tempScopeSyms = syms }
                       history TempSym { tempSym = sym } =
      case join (HashMap.lookup sym syms) of
        -- For direct resolutions, look up in the local builders
        Just Direct -> lookupScopeSymbol ctx history sym
        -- For imported and inherited, only do the lookup if there
        -- is no ambiguity
        --
        -- XXX The blocker here is figuring out how to check for ambiguity
        Just Imported {} -> _
        Just Inherited {} -> _
        -- For enclosing resolutions, look up in the enclosing scope
        Just Enclosing { enclosingScope = encscopeid } ->
          lookupScopeSymbol (tempscopes Array.! encscopeid) history sym
        -- Anything else fails
        Nothing -> undef
    lookupScopeTempExp ctx history TempExp { tempExp = ex } =
      lookupScopeExp ctx history ex

    -- | Try to get the ScopeID of a TempRef representing a builder reference.
    lookupScopeTempRef :: (MonadIO m, MonadMessages Message m) =>
                           TempScope
                        -> HashSet TempRef
                        -> TempRef
                        -> ExceptT (Maybe Error) m ScopeID
    lookupScopeTempRef ctx @ TempScope { tempScopeRenameTab = renametab }
                       history tempref
        -- Detect cyclic references
      | HashSet.member tempref history = cyclic history
      | otherwise =
        let
          newhistory = HashSet.insert tempref history
          tempexp = renametab Array.! tempref
        in
          lookupScopeTempExp ctx newhistory tempexp

    -- | Check if the first scope inherits from the second, possibly
    -- excluding some scope from the set of possible paths.
    checkInherit :: (MonadIO m, MonadMessages Message m) =>
                     Maybe ScopeID
                  -- ^ Excluded scope
                  -> ScopeID
                  -- ^ Source scope
                  -> ScopeID
                  -- ^ Destination scope
                  -> m Bool
    -- Sanity check cases
    checkInherit (Just exclude) src dst
      | exclude == dst =
        do
          internalError "Destination is excluded in inheritence check" []
          return False
      | exclude == src =
        do
          internalError "Source is excluded in inheritence check" []
          return False
    -- The actual behavior
    checkInherit exclude src dst =
      let
        searchInherit :: (MonadIO m, MonadMessages Message m) =>
                         IOBitArray ScopeID
                      -- ^ The visited bits on all scopes
                      -> ScopeID
                      -- ^ The current scope
                      -> m Bool
        searchInherit excludes currscope
          | currscope == dst = return True
          | otherwise =
            let
              tempscope @ TempScope { tempScopeInherits = inherits } =
                tempscopes Array.! currscope

              -- Foldable function to visit superscopes
              foldfun True _ = return True
              foldfun False tempref =
                do
                  res <- runExceptT $! lookupScopeTempRef tempscope
                                                          HashSet.empty tempref
                  case res of
                    Left _ -> return False
                    Right superscope -> searchInherit excludes superscope
            in do
              -- Check if this node is excluded
              excluded <- liftIO $! IOBitArray.readArray excludes currscope
              if excluded
                then return False
                else do
                  -- Mark this node as excluded, then visit all the superscopes
                liftIO $! IOBitArray.writeArray excludes currscope True
                foldM foldfun False inherits
      in do
        excludes <- case exclude of
          -- If we're excluding a scope, add it to the exclude set
          Just excluded ->
            do
              excludes' <- liftIO $! IOBitArray.newArray arrbounds False
              liftIO $! IOBitArray.writeArray excludes' excluded True
              return excludes'
          Nothing -> liftIO (IOBitArray.newArray arrbounds False)
        searchInherit excludes src

    -- | Try to look up a direct (local) definition in a scope by its
    -- symbol.
    resolveDirect :: (MonadIO m, MonadMessages Message m) =>
                     Visibility
                  -- ^ The visibility level at which to resolve
                  -> ScopeID
                  -- ^ The scope in which to resolve the symbol.
                  -> Symbol
                  -- ^ The symbol to resolve
                  -> ExceptT (Maybe Error) m Access
    resolveDirect expectvis scopeid sym =
      let
        TempScope { tempScopeBuilders = builders, tempScopeTruths = truths,
                    tempScopeNames = names } = tempscopes Array.! scopeid

        -- | Check the visibility of a definition we find.
        checkVisibility actualvis
            -- If we find it, and the expected visibility is lower
            -- or equal to the actual, then it's a valid access.
          | expectvis <= actualvis = Just Valid
            -- Don't report invalid accesses for Hidden visibility.
          | actualvis == Hidden = Nothing
            -- Otherwise report illegal access
          | otherwise = Just Illegal { illegalVisibility = actualvis }

        hiddenLookup
          | HashMap.member sym (names Array.! Hidden) = checkVisibility Hidden
          | otherwise = Nothing

        privateLookup
          | HashMap.member sym (names Array.! Private) = checkVisibility Private
          | otherwise = Nothing

        protectedLookup
          | HashMap.member sym (names Array.! Protected) =
            checkVisibility Protected
          | otherwise = Nothing

        publicLookup
          | HashMap.member sym (names Array.! Public) = checkVisibility Public
          | otherwise = Nothing

        defsLookup = msum [hiddenLookup, privateLookup,
                           protectedLookup, publicLookup]

        builderLookup = case HashMap.lookup sym builders of
          Just Builder { builderVisibility = actualvis } ->
            checkVisibility actualvis
          Nothing -> Nothing

        truthLookup = case HashMap.lookup sym truths of
          Just Truth { truthVisibility = actualvis } ->
            checkVisibility actualvis
          Nothing -> Nothing

        foldfun (Just out) (Just _) =
          do
            internalError "Should not see multiple kinds of definitions" []
            return (Just out)
        foldfun (Just out) Nothing = return (Just out)
        foldfun Nothing (Just out) = return (Just out)
        foldfun Nothing Nothing = return Nothing
      in do
        res <- foldM foldfun Nothing [defsLookup, builderLookup, truthLookup]
        case res of
          Just out -> return out
          Nothing -> undef

    -- | Full resolution on a symbol.
    resolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                     ScopeID
                  -> Word
                  -> Bool
                  -- ^ Whether or not to search enclosing scopes as well.
                  -> Symbol
                  -- ^ The symbol to resolve.
                  -> ExceptT (Maybe Error) m Resolution
    resolveSymbol scopeid depth withenclosing sym =
      let
        ctx @ TempScope { tempScopeEnclosing = enclosing,
                          tempScopeRenameTab = renametab,
                          tempScopeImports = imports,
                          tempScopeInherits = inherits,
                          tempScopeSyms = syms } = tempscopes Array.! scopeid

        -- | Attempt to resolve local definitions
        resolveLocal :: (MonadIO m, MonadMessages Message m) =>
                        ExceptT (Maybe Error) m Resolution
        resolveLocal =
          do
            res <- resolveDirect Hidden scopeid sym
            case res of
              -- We should only ever see valid resolutions here.
              Valid
                  -- If this is a local definition, return a direct definition.
                | depth == 0 -> return Direct
                  -- Otherwise, it's an enclosing definition.
                | otherwise -> return Enclosing { enclosingScope = scopeid,
                                                  enclosingDepth = depth }
              -- Report an internal error otherwise
              _ ->
                do
                  internalError "Should only see a Valid resolution" []
                  if depth == 0
                    then return Direct
                    else return Enclosing { enclosingScope = scopeid,
                                            enclosingDepth = depth }

        -- | Attempt to resolve the symbol from imports.
        resolveImports :: (MonadIO m, MonadMessages Message m) =>
                          ExceptT (Maybe Error) m Resolution
        resolveImports =
          let
            resolveImport :: (MonadIO m, MonadMessages Message m) =>
                             Import TempRef
                          -> ExceptT (Maybe Error) m Resolution
            resolveImport Import { importExp = tempref, importNames = names }
                -- Check if the symbol is one of the ones in the
                -- import list, or if the list is empty (in which case
                -- we import everything)
              | HashSet.null names || HashSet.member sym names =
                do
                  -- First, look up the scope from which we import
                  importscope <- lookupScopeTempRef ctx HashSet.empty tempref
                  -- Now do resolution from that scope, ignoring
                  -- enclosing scopes.
                  resolveSymbol importscope depth False sym
                  -- XXX We're not done here; we need to come back and add a
                  -- path element when we figure out how that works.

                  -- Otherwise, we can't possibly import the symbol here.
              | otherwise = undef
          in do
            resolved <- mapM resolveImport imports
            return (mconcat resolved)

        resolveInherits :: (MonadIO m, MonadMessages Message m) =>
                          ExceptT (Maybe Error) m Resolution
        resolveInherits =
          let
            resolveInherit :: (MonadIO m, MonadMessages Message m) =>
                              TempRef
                           -> ExceptT (Maybe Error) m Resolution
            resolveInherit inherit =
              do
                -- First, look up the scope from which we inherit
                importscope <- lookupScopeTempRef ctx HashSet.empty inherit
                -- Now do resolution from that scope, ignoring
                -- enclosing scopes.
                resolveSymbol importscope depth False sym
                -- XXX We're not done here; we need to come back and add a
                -- path element when we figure out how that works.

            -- | Take a raw set of inherited resolutions and eliminate
            -- all overridden resolutions.  If this reduces the set to
            -- a singleton, that is the result; otherwise, it's an
            -- ambiguous resolution.
            reconcileInherits :: (MonadIO m, MonadMessages Message m) =>
                                 [Resolution]
                              -> ExceptT (Maybe Error) m Resolution
            -- Easy case: we found nothing
            reconcileInherits [] = undef
            -- Easy case; we found exactly one, no reconciliation required
            reconcileInherits [out] = return out
            reconcileInherits resolutions =
              let
                -- Filter out all the resolutions that are overridden
                -- by other resolutions.
                filterOverrides accum [] = return accum
                filterOverrides accum _ = _
              in do
                nonoverrides <- filterOverrides HashSet.empty resolutions
                case HashSet.toList nonoverrides of
                  -- This case shouldn't happen, log an internal error
                  [] ->
                    do
                      internalError ("Should not see empty non-override set") []
                      undef
                  -- Exactly one resolution.  This is what we want.
                  [out] -> return out
                  -- Ambiguous resolution
                  _ -> ambiguous nonoverrides
          in do
            -- First resolve all inherits
            resolved <- mapM resolveInherit inherits
            -- Now reconcile all the results down to one
            reconcileInherits resolved

        -- | Attempt to resolve enclosing definitions, if we are
        -- allowed to look there.
        resolveEnclosing :: (MonadIO m, MonadMessages Message m) =>
                            ExceptT (Maybe Error) m Resolution
        resolveEnclosing
            -- Only resolve if we are actually checking enclosing scopes
          | withenclosing =
            case enclosing of
              -- If there is an enclosing scope, attempt to resolve inside it
              Just encscopeid ->
                do
                  res <- resolveSymbol encscopeid (depth + 1) True sym
                  case res of
                    -- This should be an enclosing result.
                    Enclosing {} -> return res
                    -- Otherwise, report an internal error.
                    _ ->
                      do
                        internalError "Should only see Enclosing" []
                        return Enclosing { enclosingScope = scopeid,
                                           enclosingDepth = depth }
              -- If there is no enclosing scope, report undefined
              Nothing -> undef
            -- Otherwise, report undefined.
          | otherwise = undef
      in
        resolveLocal `catchError`
          (\_ -> resolveImports `catchError`
                 (\_ -> resolveInherits `catchError`
                          (\_ -> resolveEnclosing)))

    -- | Attempt to resolve all symbols in a scope, if it's in the
    -- workset.
    resolveScope :: (MonadIO m, MonadMessages Message m) =>
                    (ScopeID, TempScope)
                 -> m (ScopeID, TempScope)
    -- Skip this scope if it's not in the workset
    resolveScope elem @ (idx, _) | workset BitArray.! idx = return elem
    resolveScope curr @ (scopeid, TempScope { tempScopeSyms = resolutions }) =
      let
        resolveScopeLinks :: (MonadIO m) =>
                             HashMap Symbol (Maybe Resolution)
                          -> m (ScopeID, TempScope)
        resolveScopeLinks newresolutions
          | resolutions == newresolutions = return curr
          | otherwise =
            do
              -- Use the new resolutions to resolve all imports

              -- Use the new resoultions to resolve all inherits

              -- Propagate this change to all the scopes that
              -- depend on this one
              _

        tryResolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                            Symbol
                         -> m (Symbol, Maybe Resolution)
        tryResolveSymbol sym =
          do
            -- Catch any errors
            res <- runExceptT (resolveSymbol scopeid 0 True sym)
            case res of
              Left _ -> return (sym, Nothing)
              -- Correct resolution adds an entry
              Right out -> return (sym, Just out)
      in do
        -- Resolve all symbols using the previous scope
        newresolutions <- mapM tryResolveSymbol (HashMap.keys resolutions)
        resolveScopeLinks (HashMap.fromList newresolutions)

    resolveRound :: (MonadIO m, MonadMessages Message m) =>
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
               -> m (ScopeID, HashMap Symbol (Maybe Resolution),
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
            ents = zip (HashSet.toList syms) (repeat Nothing)
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
                       -> [(ScopeID, HashMap Symbol (Maybe Resolution),
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

    -- The initial working set visits everything
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
