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

-- | Resolution path element.  A sequence of these describe how an
-- indirect resolution is resolved.
data PathElem =
    -- | A definition imported from another scope.
    Imported {
      -- | The set of scopes from which this definition is imported.
      -- Non-singleton maps denate ambiguity.
      importedScope :: !ScopeID,
      -- | The expression for the import.
      importedScopeExp :: !TempRef
    }
    -- | An inherited definition.
  | Inherited {
      -- | The set of scopes from which the definition is inherited.
      inheritedScope :: !ScopeID,
      -- | The expression for the inherited scope.
      inheritedScopeExp :: !TempRef
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
    nonLocalDeps :: !(HashMap Ref Ref)
  }
  deriving (Eq)

data Resolution =
    -- | Locally-defined symbol.  This is always valid, so no need for
    -- validity.
    Direct
    -- | Resolution from a different scope.
  | Indirect {
      -- | Resolution data.
      indirectSrc :: !NonLocal
    }
    deriving (Eq, Ord)

data Error =
    -- | Cyclic definitions.  This can happen with builder definitions
    -- that never "bottom out" at a concrete definition.
    Cyclic { cyclicRefs :: !(HashSet TempRef) }
    -- | Ambiguous inherited or imported definitions.
  | Ambiguous { ambiguousRefs :: !(HashSet Resolution) }
    -- | Illegal access (private from an inheriting scope, private or
    -- protected from an non-inheriting scope)
  | Illegal {
      -- | Visibility of the symbol.
      illegalVisibility :: !Visibility
    }
    -- | Access to a non-static definition from a static context.
  | Inaccessible {
      -- | True for object context, false for local.
      inaccessibleKind :: !Bool
    }
    -- | A resolution result which depends on itself resolving to
    -- something else.
  | Inconsistent {
      inconsistentRes :: !NonLocal
    }
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
      sorteddeps1 = sort (HashMap.toList deps1)
      sorteddeps2 = sort (HashMap.toList deps2)
    in case compare scopeid1 scopeid2 of
      EQ -> case compare sym1 sym2 of
        EQ -> case compare src1 src2 of
          EQ -> case compare tail1 tail2 of
            EQ -> compare sorteddeps1 sorteddeps2
          out -> out
        out -> out
      out -> out

instance Ord Error where
  compare Cyclic { cyclicRefs = refs1 } Cyclic { cyclicRefs = refs2 } =
    let
      sortedrefs1 = sort (HashSet.toList refs1)
      sortedrefs2 = sort (HashSet.toList refs2)
    in
      compare sortedrefs1 sortedrefs2
  compare Cyclic {} _ = LT
  compare _ Cyclic {} = GT
  compare Ambiguous { ambiguousRefs = refs1 }
          Ambiguous { ambiguousRefs = refs2 } =
    let
      sortedrefs1 = sort (HashSet.toList refs1)
      sortedrefs2 = sort (HashSet.toList refs2)
    in
      compare sortedrefs1 sortedrefs2
  compare Ambiguous {} _ = LT
  compare _ Ambiguous {} = GT
  compare Illegal { illegalVisibility = vis1 }
          Illegal { illegalVisibility = vis2 } = compare vis1 vis2
  compare Illegal {} _ = LT
  compare _ Illegal {} = GT
  compare Inaccessible { inaccessibleKind = kind1 }
          Inaccessible { inaccessibleKind = kind2 } =
    compare kind1 kind2

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
  hashWithSalt s Indirect { indirectSrc = res } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` res

instance Hashable Error where
  hashWithSalt s Cyclic { cyclicRefs = refs } =
    let
      sortedRefs = sort (HashSet.toList refs)
    in
      s `hashWithSalt` (0 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Ambiguous { ambiguousRefs = refs } =
    let
      sortedRefs = sort (HashSet.toList refs)
    in
      s `hashWithSalt` (1 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Illegal { illegalVisibility = vis } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` vis
  hashWithSalt s Inaccessible { inaccessibleKind = object } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` object
  hashWithSalt s Inconsistent { inconsistentRes = res } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` res

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
  Indirect { indirectSrc = res1 } <> Indirect { indirectSrc = res2 } =
    Indirect { indirectSrc = res1 <> res2 }

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
cyclic = throwError . Just . Cyclic

-- | Report an ambiguous inherited symbol
ambiguous :: (Monad m) => HashSet Resolution -> ExceptT (Maybe Error) m a
ambiguous = throwError . Just . Ambiguous

-- | Report an illegal access
illegal :: (Monad m) => Visibility -> ExceptT (Maybe Error) m a
illegal = throwError . Just . Illegal

-- | Report an inconsistent resolution
inconsistent :: (Monad m) => NonLocal -> ExceptT (Maybe Error) m a
inconsistent = throwError . Just . Inconsistent

-- Common pattern: go with the first valid resolution out of a list of
-- actions representing the precedence.
firstValid :: (MonadIO m, MonadMessages Message m) =>
              [ExceptT (Maybe Error) m a] ->
              ExceptT (Maybe Error) m a
firstValid [] =
  do
    internalError "Should not see empty action list" []
    undef
firstValid [out] = out
firstValid (first : rest) = first `catchError` const (firstValid rest)

allUndef :: (MonadIO m, MonadMessages Message m) =>
            ExceptT (Maybe Error) m a
         -- ^ Final action to perform
         -> [ExceptT (Maybe Error) m a]
         -- ^ List of actions expected to return undefined.
         -> ExceptT (Maybe Error) m a
allUndef final [] = final
allUndef final (first : rest) =
  let
    report =
      do
        internalError "Expected only one definite result here" []
        final

    reportSuccess =
      do
        _ <- first
        report

    expectUndef Nothing = allUndef final rest
    expectUndef _ = report
  in
    reportSuccess `catchError` expectUndef

-- | Another common pattern: only one valid result expected.  Do a
-- sanity check to enforce this.
oneValid :: (MonadIO m, MonadMessages Message m) =>
            [ExceptT (Maybe Error) m a] ->
            ExceptT (Maybe Error) m a
oneValid [] =
  do
    internalError "Should not see empty action list" []
    undef
oneValid [out] = out
oneValid (first : rest) =
  let
    acceptOne =
      do
        res <- first
        allUndef (return res) rest

    handler Nothing = oneValid rest
    handler err = allUndef (throwError err) rest
  in
    acceptOne `catchError` handler

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

    lookupResolvedScopeSymbol :: (MonadIO m, MonadMessages Message m) =>
                                 TempScope
                              -> HashSet TempRef
                              -> Symbol
                              -> ExceptT (Maybe Error) m ScopeID
    lookupResolvedScopeSymbol ctx @ TempScope { tempScopeSyms = syms }
                              history sym =
      let
        expectDefined :: (MonadIO m, MonadMessages Message m) =>
                         Maybe Error -> ExceptT (Maybe Error) m ScopeID
        expectDefined Nothing =
          do
            internalError "Should not see undefined for a resolved symbol" []
            undef

        lookupResolvedScopeSymbol' =
          case join (HashMap.lookup sym syms) of
            -- For direct resolutions, look up in the local scope.
            Just Direct -> lookupScopeSymbol ctx history sym
            -- For indirect resolutions, look up in the remote scope.
            Just Indirect { indirectSrc = NonLocal { nonLocalScope = indscope,
                                                     nonLocalSym = indsym } } ->
              lookupScopeSymbol (tempscopes Array.! indscope) history indsym
            -- Anything else fails
            Nothing -> undef
      in
        lookupResolvedScopeSymbol' `catchError` expectDefined


    -- | Try to get the ScopeID of a TempExp representing a builder reference.
    lookupScopeTempExp :: (MonadIO m, MonadMessages Message m) =>
                           TempScope
                        -> HashSet TempRef
                        -> TempExp
                        -> ExceptT (Maybe Error) m ScopeID
    lookupScopeTempExp ctx history TempSym { tempSym = sym } =
      lookupResolvedScopeSymbol ctx history sym
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

    -- | Full resolution on a symbol.
    resolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                     ScopeID
                  -> Visibility
                  -> Bool
                  -- ^ Whether or not to search enclosing scopes as well.
                  -> Symbol
                  -- ^ The symbol to resolve.
                  -> ExceptT (Maybe Error) m Resolution
    resolveSymbol scopeid expectvis withenclosing sym =
      let
        ctx @ TempScope { tempScopeEnclosing = enclosing,
                          tempScopeTruths = truths,
                          tempScopeBuilders = builders,
                          tempScopeRenameTab = renametab,
                          tempScopeImports = imports,
                          tempScopeInherits = inherits,
                          tempScopeNames = names,
                          tempScopeSyms = syms } = tempscopes Array.! scopeid

        -- | Check that the resolution result is consistent.
        checkConsistent :: (MonadIO m, MonadMessages Message m) =>
                           Resolution
                        -> ExceptT (Maybe Error) m Resolution
        -- Direct resolutions are always consistent
        checkConsistent Direct = return Direct
        checkConsistent out @ Indirect {
                                indirectSrc =
                                  src @ NonLocal { nonLocalDeps = deps,
                                                   nonLocalScope = expectscope,
                                                   nonLocalSym = expectsym }
                              } =
          let
            key = Ref { refScopeID = scopeid, refSymbol = sym }
          in case HashMap.lookup key deps of
            -- If we depend on our own resolution being something
            -- different, then this is an inconsistent resolution
            -- result.
            Just Ref { refScopeID = actualscope, refSymbol = actualsym }
              | expectscope /= actualscope ||
                expectsym /= actualsym -> inconsistent src
            -- Otherwise the result is fine.
            _ -> return out

        -- | Try to look up a direct (local) definition in a scope by its
        -- symbol.
        resolveDirect :: (MonadIO m, MonadMessages Message m) =>
                         ExceptT (Maybe Error) m Resolution
        resolveDirect =
          let
            checkVisibility :: (Monad m) =>
                               Visibility
                            -> ExceptT (Maybe Error) m Resolution
            -- | Check the visibility of a definition we find.
            checkVisibility actualvis
                -- If we find it, and the expected visibility is lower
                -- or equal to the actual, then it's a valid access.
              | expectvis <= actualvis = return Direct
                -- Don't report invalid accesses for Hidden visibility.
              | actualvis == Hidden = undef
                -- Otherwise report illegal access
              | otherwise = illegal actualvis

            hiddenLookup
              | HashMap.member sym (names Array.! Hidden) =
                checkVisibility Hidden
              | otherwise = undef

            privateLookup
              | HashMap.member sym (names Array.! Private) =
                checkVisibility Private
              | otherwise = undef

            protectedLookup
              | HashMap.member sym (names Array.! Protected) =
                checkVisibility Protected
              | otherwise = undef

            publicLookup
              | HashMap.member sym (names Array.! Public) =
                checkVisibility Public
              | otherwise = undef

            defsLookup = firstValid [hiddenLookup, privateLookup,
                                     protectedLookup, publicLookup]

            builderLookup = case HashMap.lookup sym builders of
              Just Builder { builderVisibility = actualvis } ->
                checkVisibility actualvis
              Nothing -> undef

            truthLookup = case HashMap.lookup sym truths of
              Just Truth { truthVisibility = actualvis } ->
                checkVisibility actualvis
              Nothing -> undef

            foldfun (Just out) (Just _) =
              do
                internalError "Should not see multiple kinds of definitions" []
                return (Just out)
            foldfun (Just out) Nothing = return (Just out)
            foldfun Nothing (Just out) = return (Just out)
            foldfun Nothing Nothing = return Nothing
          in do
            oneValid [defsLookup, builderLookup, truthLookup]

        -- | Attempt to resolve the symbol from imports.
        resolveFromImports :: (MonadIO m, MonadMessages Message m) =>
                          ExceptT (Maybe Error) m Resolution
        resolveFromImports =
          let
            resolveFromImport :: (MonadIO m, MonadMessages Message m) =>
                             Import TempRef
                          -> ExceptT (Maybe Error) m Resolution
            resolveFromImport Import { importExp = tempref, importNames = names }
                -- Check if the symbol is one of the ones in the
                -- import list, or if the list is empty (in which case
                -- we import everything)
              | HashSet.null names || HashSet.member sym names =
                let
                  addPathElem impscope deps Direct =
                    let
                      -- For direct resolutions, synthesize a new
                      -- indirect resolution.
                      imported = Imported { importedScopeExp = tempref,
                                            importedScope = impscope }
                      nonlocal = NonLocal { nonLocalSym = sym,
                                            nonLocalScope = impscope,
                                            nonLocalSrc = imported,
                                            nonLocalTail = [],
                                            nonLocalDeps = deps }
                    in
                      return Indirect { indirectSrc = nonlocal }
                  -- For indirect resolutions, add a path element and
                  -- union the dependencies.
                  addPathElem impscope deps
                              ind @ Indirect {
                                      indirectSrc =
                                        nl @ NonLocal { nonLocalTail = tail,
                                                        nonLocalSrc = src,
                                                        nonLocalDeps = olddeps }
                                    } =
                    let
                      imported = Imported { importedScopeExp = tempref,
                                            importedScope = impscope }
                      newnonlocal = nl { nonLocalSrc = imported,
                                         nonLocalTail = src : tail,
                                         nonLocalDeps = olddeps <> deps }
                    in
                      return ind { indirectSrc = newnonlocal }
                in do
                  -- First, look up the scope from which we import
                  importscope <- lookupScopeTempRef ctx HashSet.empty tempref
                  -- Now do resolution from that scope, ignoring
                  -- enclosing scopes.
                  res <- resolveSymbol importscope Public False sym
                  -- Add the import path element.
                  addPathElem importscope _ res
                  -- Otherwise, we can't possibly import the symbol here.
              | otherwise = undef
          in do
            resolved <- mapM resolveFromImport imports
            return (mconcat resolved)

        resolveInherits :: (MonadIO m, MonadMessages Message m) =>
                          ExceptT (Maybe Error) m Resolution
        resolveInherits =
          let
            resolveInherit :: (MonadIO m, MonadMessages Message m) =>
                              TempRef
                           -> ExceptT (Maybe Error) m Resolution
            resolveInherit inherit =
              let
                  addPathElem inheritscope deps Direct =
                    let
                      -- For direct resolutions, synthesize a new
                      -- indirect resolution.
                      inherited = Inherited { inheritedScopeExp = inherit,
                                              inheritedScope = inheritscope }
                      nonlocal = NonLocal { nonLocalSym = sym,
                                            nonLocalScope = inheritscope,
                                            nonLocalSrc = inherited,
                                            nonLocalTail = [],
                                            nonLocalDeps = deps }
                    in
                      return Indirect { indirectSrc = nonlocal }
                  -- For indirect resolutions, add a path element and
                  -- union the dependencies.
                  addPathElem impscope deps
                              ind @ Indirect {
                                      indirectSrc =
                                        nl @ NonLocal { nonLocalTail = tail,
                                                        nonLocalSrc = src,
                                                        nonLocalDeps = olddeps }
                                    } =
                    let
                      inherited = Inherited { inheritedScopeExp = inherit,
                                              inheritedScope = impscope }
                      newnonlocal = nl { nonLocalSrc = inherited,
                                         nonLocalTail = src : tail,
                                         nonLocalDeps = olddeps <> deps }
                    in
                      return ind { indirectSrc = newnonlocal }
              in do
                -- First, look up the scope from which we inherit
                inheritscope <- lookupScopeTempRef ctx HashSet.empty inherit
                -- Now do resolution from that scope at protected
                -- visibility, ignoring enclosing scopes.
                res <- resolveSymbol inheritscope Protected False sym
                -- Add the inherit path element.
                addPathElem inheritscope _ res

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
                  res <- resolveSymbol encscopeid expectvis True sym
                  case res of
                    -- If the result is a direct definition, construct
                    -- a non-local resolution result.
                    Direct ->
                      let
                        pathelem = Enclosing { enclosingScope = encscopeid,
                                               enclosingDepth = 1 }
                        nonlocal = NonLocal { nonLocalSym = sym,
                                              nonLocalScope = encscopeid,
                                              nonLocalSrc = pathelem,
                                              nonLocalTail = [],
                                              nonLocalDeps = HashMap.empty }
                      in
                        return Indirect { indirectSrc = nonlocal }
                    -- If the result is an enclosing scope definition,
                    -- increment the depth.
                    ind @ Indirect {
                            indirectSrc =
                              nl @ NonLocal {
                                     nonLocalSrc =
                                       enc @ Enclosing {
                                               enclosingDepth = depth
                                             }
                                   }
                          } ->
                      return ind {
                               indirectSrc =
                                 nl {
                                   nonLocalSrc =
                                     enc { enclosingDepth = depth + 1 }
                                 }
                             }
                    -- Otherwise, add an enclosing scope path element
                    ind @ Indirect {
                            indirectSrc = nl @ NonLocal { nonLocalTail = tail,
                                                          nonLocalSrc = src }
                          } ->
                      let
                        newsrc = Enclosing { enclosingScope = encscopeid,
                                             enclosingDepth = 1 }
                        newnonlocal = nl { nonLocalSrc = newsrc,
                                           nonLocalTail = src : tail }
                      in
                      return ind { indirectSrc = newnonlocal }
              -- If there is no enclosing scope, report undefined
              Nothing -> undef
            -- Otherwise, report undefined.
          | otherwise = undef
      in do
        -- Search for results in precedence order
        res <- firstValid [resolveDirect, resolveFromImports,
                           resolveInherits, resolveEnclosing]
        -- Check consistency
        checkConsistent res

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
            res <- runExceptT (resolveSymbol scopeid Hidden True sym)
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
