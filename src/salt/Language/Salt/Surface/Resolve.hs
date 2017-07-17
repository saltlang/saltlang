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
import Control.Monad.Symbols
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
      importedScope :: !ScopeID
    }
    -- | An inherited definition.
  | Inherited {
      -- | The set of scopes from which the definition is inherited.
      inheritedScope :: !ScopeID
    }
    -- | A definition in an enclosing scope.
  | Enclosing {
      -- | Enclosing scope from which this was captured.
      enclosingScope :: !ScopeID,
      -- | Depth of enclosing scope capture.
      enclosingDepth :: !Word
    }
    deriving (Eq, Ord)

data ResolvedScope =
    ResolvedScope {
      resolvedScopeID :: !ScopeID,
      resolvedScopeDeps :: !(HashMap (ScopeID, TempRef) ScopeID)
    }
  | NotScope
    deriving (Eq)

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
    nonLocalDeps :: !(HashMap (ScopeID, TempRef) ScopeID),
    -- | If this resolution points to a builder and we can resolve
    -- the scope, it is stored here.
    nonLocalResolvedScope :: !(Maybe ResolvedScope)

  }
  deriving (Eq)

data Resolution =
    -- | Locally-defined symbol.  This is always valid, so no need for
    -- validity.
    Direct {
      -- | If this resolution points to a builder and we can resolve
      -- the scope, it is stored here.
      directResolvedScope :: !(Maybe ResolvedScope)
    }
    -- | Resolution from a different scope.
  | Indirect {
      -- | Resolution data.
      indirectSrc :: !NonLocal
    }
    deriving (Eq, Ord)

data Error =
    -- | Cyclic definitions.  This can happen with builder definitions
    -- that never "bottom out" at a concrete definition.
    Cyclic { cyclicRefs :: !(HashSet (ScopeID, TempRef)) }
    -- | Ambiguous inherited or imported definitions.
  | Ambiguous {
      ambiguousRefs :: !(HashMap ScopeID NonLocal)
    }
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
  | Undefined
    deriving (Eq)

-- | Placeholders for anything that might be a static expression.
-- These are substituted into expressions in a manner similar to
-- common subexpression elimination at the beginning of resolution,
-- then the resolved values are substituted back in at the end.
newtype TempRef = TempRef { tempRefID :: Word }
  deriving (Eq, Ord, Ix)

data TempInherit =
  TempInherit {
    tempInheritScope :: !ScopeID,
    tempInheritDeps :: !(HashMap (ScopeID, TempRef) ScopeID)
  }

data TempImport =
  TempImport {
    tempImportData :: !(Import ScopeID),
    tempImportDeps :: !(HashMap (ScopeID, TempRef) ScopeID)
  }

data TempScope =
  TempScope {
    -- Unaltered content from the original scope.
    tempScopeNames :: !(Array Visibility (HashMap Symbol [DefID])),
    tempScopeEnclosing :: !(Maybe ScopeID),
    -- Static expression-based content from the original scope.
    tempScopeProofs :: ![Proof (Exp Seq TempRef)],
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
    tempScopeSyms :: !(HashMap Symbol (Either Error Resolution)),
    tempScopeResolvedImports :: ![Either Error TempImport],
    tempScopeResolvedInherits :: ![Either Error TempInherit],
    -- | All enclosed scopes
    tempScopeEnclosed :: ![ScopeID],
    tempScopeSymbolTempRefs :: !(HashMap Symbol TempRef),
    tempScopeSymbolPos :: !(HashMap Symbol [Position])
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
    renameStateTab :: !(HashMap TempExp TempRef),
    renameSymTab :: !(HashMap Symbol TempRef),
    renameSymPos :: !(HashMap Symbol [Position])
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

instance Ord ResolvedScope where
  compare ResolvedScope { resolvedScopeID = scopeid1,
                          resolvedScopeDeps = deps1 }
          ResolvedScope { resolvedScopeID = scopeid2,
                          resolvedScopeDeps = deps2 } =
    let
      sorteddeps1 = sort (HashMap.toList deps1)
      sorteddeps2 = sort (HashMap.toList deps2)
    in
      case compare scopeid1 scopeid2 of
        EQ -> compare sorteddeps1 sorteddeps2
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
      sortedrefs1 = sort (HashMap.toList refs1)
      sortedrefs2 = sort (HashMap.toList refs2)
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
  hashWithSalt s Imported { importedScope = scope } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` scope
  hashWithSalt s Inherited { inheritedScope = scope } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` scope
  hashWithSalt s Enclosing { enclosingScope = scope, enclosingDepth = depth } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` scope `hashWithSalt` depth

instance Hashable ResolvedScope where
  hashWithSalt s ResolvedScope { resolvedScopeID = scopeid,
                                 resolvedScopeDeps = deps } =
    let
      sorteddeps = sort (HashMap.toList deps)
    in
      s `hashWithSalt` scopeid `hashWithSalt` sorteddeps

instance Hashable NonLocal where
  hashWithSalt s NonLocal { nonLocalScope = scopeid, nonLocalSym = sym,
                            nonLocalSrc = src, nonLocalTail = tail,
                            nonLocalDeps = deps } =
    s `hashWithSalt` scopeid `hashWithSalt` sym `hashWithSalt`
    src `hashWithSalt` tail `hashWithSalt` deps

instance Hashable Resolution where
  hashWithSalt s Direct { directResolvedScope = scope} =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` scope
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
      sortedRefs = sort (HashMap.toList refs)
    in
      s `hashWithSalt` (1 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Illegal { illegalVisibility = vis } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` vis
  hashWithSalt s Inaccessible { inaccessibleKind = object } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` object
  hashWithSalt s Inconsistent { inconsistentRes = res } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` res
  hashWithSalt s Undefined = s `hashWithSalt` (5 :: Word)

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
  out @ Direct {} <> _ = out
  _ <> out @ Direct {} = out
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
          -> Position
          -- ^ Position at which the symbol was referenced.
          -> RenameT m TempRef
          -- ^ A 'TempRef' representing this 'Symbol'.
renameSym sym pos =
  let
    tempsym = TempSym { tempSym = sym }
  in do
    -- Check the renaming table to see if we've seen this one before
    st @ RenameState { renameStateCurr = curr, renameStateTab = tab,
                       renameSymTab = symtab, renameSymPos = sympos } <- get
    case HashMap.lookup tempsym tab of
      -- If we have, return its TempRef
      Just tempref -> return tempref
      -- Otherwise, generate a new one
      Nothing ->
        let
          newidx = succ curr
        in do
          put st { renameStateTab = HashMap.insert tempsym newidx tab,
                   renameSymTab = HashMap.insert sym newidx symtab,
                   renameSymPos = HashMap.insertWith (++) sym [pos] sympos,
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
renameExp i @ Id { idRef = ref, idPos = pos } =
  do
    tempref <- renameSym ref pos
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
            -> m ([Proof (Exp Seq TempRef)],
                  [Import TempRef],
                  [TempRef],
                  HashMap Symbol (Builder (Exp Seq TempRef)),
                  HashMap Symbol (Syntax (Exp Seq TempRef)),
                  HashMap Symbol (Truth (Exp Seq TempRef)),
                  Array DefID (Def (Exp Seq TempRef)),
                  [Compound (Exp Seq TempRef)],
                  Array TempRef TempExp,
                  HashMap Symbol TempRef,
                  HashMap Symbol [Position])
renameScope Scope { scopeBuilders = builders, scopeSyntax = syntax,
                    scopeTruths = truths, scopeDefs = defs,
                    scopeEval = eval, scopeProofs = proofs,
                    scopeImports = imports, scopeInherits = inherits } =
  let
    -- Start with def, that way we never end up assigning it
    initstate = RenameState { renameStateCurr = def,
                              renameStateTab = HashMap.empty,
                              renameSymTab = HashMap.empty,
                              renameSymPos = HashMap.empty }

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
      do
        newproofs <- mapM (mapM renameExp) proofs
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
     RenameState { renameStateCurr = endidx, renameStateTab = tab,
                   renameSymTab = symtab, renameSymPos = sympos }) <-
      runStateT renameScope' initstate
    return (proofs, imports, inherits, builders, syntax, truths,
            defs, eval, renametab endidx tab, symtab, sympos)

-- | Core resolution algorithm.
finishScopes :: (MonadIO m, MonadMessages Message m, MonadSymbols m) =>
                 Array ScopeID TempScope
             -> m (Array ScopeID (Resolved (Exp Seq Ref)))
finishScopes finalscopes =
  let
    arrbounds = Array.bounds finalscopes

    -- | Convert a 'TempScope' into a 'Resolved' scope.
    finishScope :: (MonadIO m, MonadMessages Message m, MonadSymbols m) =>
                   Array ScopeID TempScope
                -- ^ The final scope state.
                -> (ScopeID, TempScope)
                -> m (ScopeID, Resolved (Exp Seq Ref))
    finishScope tempscopes
                (scopeid,
                 tempscope @ TempScope { tempScopeBuilders = builders,
                                         tempScopeSyntax = syntax,
                                         tempScopeTruths = truths,
                                         tempScopeDefs = defs,
                                         tempScopeEval = eval,
                                         tempScopeNames = names,
                                         tempScopeEnclosing = enclosing,
                                         tempScopeSyms = syms,
                                         tempScopeSymbolPos = sympos,
                                         tempScopeRenameTab = renametab,
                                         tempScopeResolvedInherits = inherits,
                                         tempScopeResolvedImports = imports,
                                         tempScopeProofs = proofs }) =
      let
        -- | Substitute the term for a 'TempRef' back in
        subst :: Array TempRef (Position -> Exp Seq Ref)
              -> Exp Seq TempRef
              -> Exp Seq Ref
        subst _ Compound { compoundScope = scopeid, compoundPos = pos } =
          Compound { compoundScope = scopeid, compoundPos = pos }
        subst arr a @ Abs { absCases = cases } =
          a { absCases = fmap (fmap (subst arr)) cases  }
        subst arr m @ Match { matchVal = val, matchCases = cases } =
          m { matchCases = fmap (fmap (subst arr)) cases,
              matchVal = subst arr val }
        subst arr a @ Ascribe { ascribeVal = val, ascribeType = ty } =
          a { ascribeVal = subst arr val, ascribeType = subst arr ty }
        subst arr c @ Call { callInfo = info } =
          c { callInfo = fmap (subst arr) info }
        subst arr r @ RecordType { recordTypeFields = fields } =
          r { recordTypeFields = fmap (subst arr) fields }
        subst arr r @ Record { recordFields = fields } =
          r { recordFields = fmap (subst arr) fields }
        subst arr t @ Tuple { tupleFields = fields } =
          t { tupleFields = fmap (subst arr) fields }
        subst arr p @ Project { projectVal = val } =
          p { projectVal = subst arr val }
        subst arr i @ Id { idRef = ref, idPos = pos } = (arr Array.! ref) pos
        subst arr w @ With { withVal = val, withArgs = args } =
          w { withVal = subst arr val, withArgs = subst arr args }
        subst arr w @ Where { whereVal = val, whereProp = prop } =
          w { whereVal = subst arr val, whereProp = subst arr prop }
        subst arr a @ Anon { anonParams = params } =
          a { anonParams = fmap (subst arr) params }
        subst _ Literal { literalVal = lit } = Literal { literalVal = lit }
        subst _ Bad { badPos = pos } = Bad { badPos = pos }
    {-
        getScopeID :: (MonadIO m, MonadMessages Message m) =>
                      TempRef
                   -> m (Maybe ScopeID)
        getScopeID tempref =
          do

res <- runExceptT $! lookupScopeTempRef tempscopes tempscope tempref
            case res of
              Left _ -> return Nothing
              Right (out, _) -> return $! Just out

        foldinherits :: (MonadIO m, MonadMessages Message m) =>
                        [ScopeID]
                     -> TempRef
                     -> m [ScopeID]
        foldinherits accum tempref =
          do
            res <- getScopeID tempref
            case res of
              Just scopeid -> return (scopeid : accum)
              Nothing -> return accum

        foldimports :: (MonadIO m, MonadMessages Message m) =>
                       [Import ScopeID]
                    -> Import TempRef
                    -> m [Import ScopeID]
        foldimports accum imp @ Import { importExp = tempref } =
          do
            res <- getScopeID tempref
            case res of
              Just scopeid -> return (imp { importExp = scopeid } : accum)
              Nothing -> return accum
    -}
        foldinherits :: [ScopeID]
                     -> Either Error TempInherit
                     -> [ScopeID]
        foldinherits accum (Left _) = accum
        foldinherits accum (Right TempInherit { tempInheritScope = scope }) =
          scope : accum

        foldimports :: [Import ScopeID]
                    -> Either Error TempImport
                    -> [Import ScopeID]
        foldimports accum (Left _) = accum
        foldimports accum (Right TempImport { tempImportData = out }) =
          out : accum

        -- | Report resolution errors for symbols
        finishSym :: (MonadIO m, MonadMessages Message m, MonadSymbols m) =>
                     HashMap Symbol Ref
                  -> (Symbol, Either Error Resolution)
                  -> m (HashMap Symbol Ref)
        finishSym accum (sym, Left Undefined) =
          case HashMap.lookup sym sympos of
            Just poslist ->
              do
                undefSymbol sym poslist
                return accum
            Nothing ->
              do
                internalError "Expected position list for symbol" []
                return accum
        -- Direct resolutions
        finishSym accum (sym, Right Direct {}) =
          return $! HashMap.insert sym Ref { refScopeID = scopeid,
                                             refSymbol = sym } accum
        finishSym accum
                  (sym,
                   Right Indirect {
                           indirectSrc = NonLocal {
                                           nonLocalScope = resscope,
                                           nonLocalSym = ressym
                                         }
                         }) =
          return $! HashMap.insert sym Ref { refScopeID = resscope,
                                             refSymbol = ressym } accum

        finishRename :: (MonadIO m, MonadMessages Message m) =>
                        HashMap Symbol Ref
                     -> m (Array TempRef (Position -> Exp Seq Ref))
        finishRename finishedsyms =
          let
            arrbounds @ (lo, hi) = Array.bounds renametab

            finishEnt :: (MonadIO m, MonadMessages Message m) =>
                         IOBitArray TempRef
                      -> IOBitArray TempRef
                      -> IOArray TempRef (Position -> Exp Seq Ref)
                      -> TempRef
                      -> m ()
            finishEnt finishmask visitmask finishedarr idx' =
              let
                finishExp  :: (MonadIO m, MonadMessages Message m) =>
                              Exp Seq TempRef
                           -> m (Position -> Exp Seq Ref)
                finishExp Compound { compoundScope = scope,
                                     compoundPos = pos } =
                  return (const Compound { compoundScope = scope,
                                           compoundPos = pos })
                finishExp a @ Abs { absCases = cases } =
                  do
                    newcases <- mapM (mapM finishExp) cases
                    return (\pos -> a { absCases = fmap (fmap (\f -> f pos))
                                                         newcases })
                finishExp m @ Match { matchVal = val, matchCases = cases } =
                  do
                    newval <- finishExp val
                    newcases <- mapM (mapM finishExp) cases
                    return (\pos -> m { matchVal = newval pos,
                                        matchCases = fmap (fmap (\f -> f pos))
                                                     newcases })
                finishExp a @ Ascribe { ascribeVal = val, ascribeType = ty } =
                  do
                    newval <- finishExp val
                    newty <- finishExp ty
                    return (\pos -> a { ascribeVal = newval pos,
                                        ascribeType = newty pos})
                finishExp c @ Call { callInfo = info } =
                  do
                    newinfo <- mapM finishExp info
                    return (\pos -> c { callInfo = fmap (\f -> f pos) newinfo })
                finishExp r @ RecordType { recordTypeFields = fields } =
                  do
                    newfields <- mapM finishExp fields
                    return (\pos -> r { recordTypeFields = fmap (\f -> f pos)
                                                                newfields })
                finishExp r @ Record { recordFields = fields } =
                  do
                    newfields <- mapM finishExp fields
                    return (\pos -> r { recordFields = fmap (\f -> f pos)
                                                            newfields })
                finishExp t @ Tuple { tupleFields = fields } =
                  do
                    newfields <- mapM finishExp fields
                    return (\pos -> t { tupleFields = fmap (\f -> f pos)
                                                           newfields })
                finishExp p @ Project { projectVal = val } =
                  do
                    newval <- finishExp val
                    return (\pos -> p { projectVal = newval pos })
                finishExp Id { idRef = tempref, idPos = pos } =
                  do
                    out <- finishTempRef tempref
                    return (const (out pos))
                finishExp w @ Where { whereVal = val, whereProp = prop } =
                  do
                    newval <- finishExp val
                    newprop <- finishExp prop
                    return (\pos -> w { whereVal = newval pos,
                                        whereProp = newprop pos})
                finishExp w @ With { withVal = val, withArgs = args } =
                  do
                    newval <- finishExp val
                    newargs <- finishExp args
                    return (\pos -> w { withVal = newval pos,
                                        withArgs = newargs pos})
                finishExp Literal { literalVal = val } =
                  return (const Literal { literalVal = val })
                finishExp Bad { badPos = pos } =
                  return (const Bad { badPos = pos })

                finishTempExp :: (MonadIO m, MonadMessages Message m) =>
                                 TempExp
                              -> m (Position -> Exp Seq Ref)
                finishTempExp TempSym { tempSym = sym } =
                  case HashMap.lookup sym finishedsyms of
                    Just ref -> return (\pos -> Id { idRef = ref, idPos = pos })
                    Nothing ->
                      do
                        internalError "Symbol not present in resolved syms" []
                        return (\pos -> Bad { badPos = pos })
                finishTempExp TempExp { tempExp = ex } = finishExp ex

                finishTempRef :: (MonadIO m, MonadMessages Message m) =>
                                 TempRef
                              -> m (Position -> Exp Seq Ref)
                finishTempRef idx =
                  let
                    returnFinished = liftIO $! IOArray.readArray finishedarr idx

                    markCyclic =
                      do
                        -- Mark as done
                        liftIO $! IOBitArray.writeArray finishmask idx True
                        return Bad

                    convert =
                      let
                        tempexp = renametab Array.! idx
                      in do
                        -- Mark as visited
                        liftIO $! IOBitArray.writeArray visitmask idx True
                        out <- finishTempExp tempexp
                        -- Store result
                        liftIO $! IOArray.writeArray finishedarr idx out
                        -- Clear visited
                        liftIO $! IOBitArray.writeArray visitmask idx False
                        -- Mark as done
                        liftIO $! IOBitArray.writeArray finishmask idx True
                        return out

                  in do
                    finished <- liftIO $! IOBitArray.readArray finishmask idx
                    if finished
                      -- If this one is finished, read it out of the array
                      then returnFinished
                      else do
                        visited <- liftIO $! IOBitArray.readArray finishmask idx
                        if visited
                           then markCyclic
                           else convert
              in do
                _ <- finishTempRef idx'
                return ()

          in do
            finishmask <- liftIO $! IOBitArray.newArray arrbounds False
            visitmask <- liftIO $! IOBitArray.newArray arrbounds False
            finishedarr <- liftIO $! IOArray.newArray_ arrbounds
            mapM_ (finishEnt finishmask visitmask finishedarr) [lo..hi]
            liftIO $! IOArray.unsafeFreeze finishedarr


      in do
        finishedsyms <- foldM finishSym HashMap.empty (HashMap.toList syms)
        tmprefvals <- finishRename finishedsyms
        return (scopeid,
                Resolved {
                  -- Names and enclosing scopes carry over directly
                  resolvedNames = names,
                  resolvedEnclosing = enclosing,
                  resolvedImports = foldl foldimports [] imports,
                  resolvedInherits = foldl foldinherits [] inherits,
                  -- For proofs, just look up the value
                  resolvedProofs = fmap (fmap (subst tmprefvals)) proofs,
                  -- For the rest, substitute the expression back in
                  resolvedBuilders = fmap (fmap (subst tmprefvals)) builders,
                  resolvedSyntax = fmap (fmap (subst tmprefvals)) syntax,
                  resolvedTruths = fmap (fmap (subst tmprefvals)) truths,
                  resolvedDefs = fmap (fmap (subst tmprefvals)) defs,
                  resolvedEval = fmap (fmap (subst tmprefvals)) eval
                })
  in do
    resolvedscopes <- mapM (finishScope finalscopes) (Array.assocs finalscopes)
    return $! Array.array arrbounds resolvedscopes

-- | Report an undefined symbol.
undef :: (Monad m) => ExceptT Error m a
undef = throwError Undefined

cyclic :: (Monad m) => HashSet (ScopeID, TempRef) -> ExceptT Error m a
cyclic = throwError . Cyclic

-- | Report an ambiguous inherited symbol
ambiguous :: (Monad m) => HashMap ScopeID NonLocal -> ExceptT Error m a
ambiguous = throwError . Ambiguous

-- | Report an illegal access
illegal :: (Monad m) => Visibility -> ExceptT Error m a
illegal = throwError . Illegal

-- | Report an inconsistent resolution
inconsistent :: (Monad m) => NonLocal -> ExceptT Error m a
inconsistent = throwError . Inconsistent

-- Common pattern: go with the first valid resolution out of a list of
-- actions representing the precedence.
firstValid :: (MonadIO m, MonadMessages Message m) =>
              [ExceptT Error m a] ->
              ExceptT Error m a
firstValid [] =
  do
    internalError "Should not see empty action list" []
    undef
firstValid [out] = out
firstValid (first : rest) = first `catchError` const (firstValid rest)

allUndef :: (MonadIO m, MonadMessages Message m) =>
            ExceptT Error m a
         -- ^ Final action to perform
         -> [ExceptT Error m a]
         -- ^ List of actions expected to return undefined.
         -> ExceptT Error m a
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

    expectUndef Undefined = allUndef final rest
    expectUndef _ = report
  in
    reportSuccess `catchError` expectUndef

-- | Another common pattern: only one valid result expected.  Do a
-- sanity check to enforce this.
oneValid :: (MonadIO m, MonadMessages Message m) =>
            [ExceptT Error m a] ->
            ExceptT Error m a
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

    handler Undefined = oneValid rest
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

    -- | Try to get the ScopeID of a Symbol representing a builder
    -- reference.
    lookupScopeSymbol :: (MonadIO m, MonadMessages Message m) =>
                         HashSet (ScopeID, TempRef)
                      -> HashMap (ScopeID, TempRef) ScopeID
                      -> TempScope
                      -> Symbol
                      -> ExceptT Error m (ScopeID,
                                          HashMap (ScopeID, TempRef) ScopeID)
    lookupScopeSymbol history deps TempScope { tempScopeSyms = syms } sym =
      case HashMap.lookup sym syms of
        Just (Right Direct {
                      directResolvedScope =
                        Just ResolvedScope { resolvedScopeID = resscope,
                                             resolvedScopeDeps = resdeps }
                      }) ->
          -- XXX Union deps and resdeps
          return (resscope, deps)
        Just (Right Indirect {
                      indirectSrc =
                        NonLocal {
                          nonLocalResolvedScope =
                            Just ResolvedScope { resolvedScopeID = resscope,
                                                 resolvedScopeDeps = resdeps }
                                 }
                    }) ->
          -- XXX Union deps and resdeps
          return (resscope, deps)
        -- If we can't find a resolved scope, report an undefined
        _ -> undef

    -- | Try to get the ScopeID of an Exp representing a builder reference.
    lookupScopeExpTail :: (MonadIO m, MonadMessages Message m) =>
                          HashSet (ScopeID, TempRef)
                       -> HashMap (ScopeID, TempRef) ScopeID
                       -> ScopeID
                       -> TempRef
                       -> Exp Seq TempRef
                       -> ExceptT Error m (ScopeID,
                                           HashMap (ScopeID, TempRef) ScopeID)
    -- For projects, look up the base scope, then do a lookup of the
    -- field name in the base scope.
    lookupScopeExpTail history deps scopeid tempref
                       Project { projectFields = fields,
                                 projectVal = base,
                                 projectPos = pos } =
      case HashSet.toList fields of
        [FieldName { fieldSym = sym }] ->
          do
            (basescope, basedeps) <- lookupScopeExpTail history deps scopeid
                                                        tempref base
            lookupScopeSymbol history basedeps (tempscopes Array.! basescope)
                              sym
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
    lookupScopeExpTail history deps scopeid tempref Id { idRef = ref } =
      lookupScopeTempRefTail history deps scopeid ref
    -- XXX Same as With, should this get turned into a separate scope?
    lookupScopeExpTail history deps scopeid tempref
                       Call { callInfo = Seq { seqExps = [inner, _]} } =
      lookupScopeExpTail history deps scopeid tempref inner
    -- XXX What do we do here? Possibly create a separate scope for this?
    lookupScopeExpTail history deps scopeid tempref With { withVal = val } =
      lookupScopeExpTail history deps scopeid tempref val
    lookupScopeExpTail history deps scopeid tempref Where { whereVal = val } =
      lookupScopeExpTail history deps scopeid tempref val
    -- For a builder expression, return the scope id.
    lookupScopeExpTail _ deps scopeid tempref Anon { anonScope = resscopeid } =
      return (resscopeid, HashMap.insert (scopeid, tempref) resscopeid deps)
    -- Skip bad scopes
    lookupScopeExpTail _ _ _ _ Bad {} = undef
    -- Everything else is an internal error
    lookupScopeExpTail _ _ _ _ ex =
      do
        internalError "Expected static expression" [position ex]
        undef

    lookupScopeExp :: (MonadIO m, MonadMessages Message m) =>
                      ScopeID
                   -> TempRef
                   -> Exp Seq TempRef
                   -> ExceptT Error m (ScopeID,
                                       HashMap (ScopeID, TempRef) ScopeID)
    lookupScopeExp = lookupScopeExpTail HashSet.empty HashMap.empty

    lookupScopeTempRefTail :: (MonadIO m, MonadMessages Message m) =>
                              HashSet (ScopeID, TempRef)
                           -> HashMap (ScopeID, TempRef) ScopeID
                           -> ScopeID
                           -> TempRef
                           -> ExceptT Error m
                                      (ScopeID,
                                       HashMap (ScopeID, TempRef) ScopeID)
    lookupScopeTempRefTail history deps scopeid tempref
        -- Detect cyclic references
      | HashSet.member (scopeid, tempref) history = cyclic history
      | otherwise =
        let
          TempScope { tempScopeRenameTab = renametab } =
            tempscopes Array.! scopeid

          -- | Try to get the ScopeID of a TempExp representing a
          -- builder reference.
          lookupScopeTempExp :: (MonadIO m, MonadMessages Message m) =>
                                 TempExp
                              -> ExceptT Error m
                                         (ScopeID,
                                          HashMap (ScopeID, TempRef) ScopeID)
          lookupScopeTempExp TempSym { tempSym = sym } =
            lookupScopeSymbol history deps (tempscopes Array.! scopeid) sym
          lookupScopeTempExp TempExp { tempExp = ex } =
            lookupScopeExpTail history deps scopeid tempref ex

          newhistory = HashSet.insert (scopeid, tempref) history
          tempexp = renametab Array.! tempref
        in
          lookupScopeTempExp tempexp

    lookupScopeTempRef :: (MonadIO m, MonadMessages Message m) =>
                          ScopeID
                       -> TempRef
                       -> ExceptT Error m (ScopeID,
                                           HashMap (ScopeID, TempRef) ScopeID)
    lookupScopeTempRef = lookupScopeTempRefTail HashSet.empty HashMap.empty

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
                  res <- runExceptT $! lookupScopeTempRef currscope tempref
                  case res of
                    Left _ -> return False
                    Right (superscope, _) -> searchInherit excludes superscope
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
                  -> ExceptT Error m Resolution
    resolveSymbol scopeid expectvis withenclosing sym =
      let
        ctx @ TempScope { tempScopeEnclosing = enclosing,
                          tempScopeTruths = truths,
                          tempScopeBuilders = builders,
                          tempScopeRenameTab = renametab,
                          tempScopeResolvedImports = imports,
                          tempScopeResolvedInherits = inherits,
                          tempScopeNames = names,
                          tempScopeSyms = syms,
                          tempScopeSymbolTempRefs = symtemprefs } =
          tempscopes Array.! scopeid
        -- | Try to look up a direct (local) definition in a scope by its
        -- symbol.
        resolveDirect :: (MonadIO m, MonadMessages Message m) =>
                         ExceptT Error m Resolution
        resolveDirect =
          let
            checkVisibility :: (Monad m) =>
                               Maybe ResolvedScope
                            -- ^ The resolved scope ID
                            -> Visibility
                            -- ^ The actual visibility level
                            -> ExceptT Error m Resolution
            -- | Check the visibility of a definition we find.
            checkVisibility resscope actualvis
                -- If we find it, and the expected visibility is lower
                -- or equal to the actual, then it's a valid access.
              | expectvis <= actualvis =
                return Direct { directResolvedScope = resscope }
                -- Don't report invalid accesses for Hidden visibility.
              | actualvis == Hidden = undef
                -- Otherwise report illegal access
              | otherwise = illegal actualvis

            hiddenLookup
              | HashMap.member sym (names Array.! Hidden) =
                checkVisibility Nothing Hidden
              | otherwise = undef

            privateLookup
              | HashMap.member sym (names Array.! Private) =
                checkVisibility Nothing Private
              | otherwise = undef

            protectedLookup
              | HashMap.member sym (names Array.! Protected) =
                checkVisibility Nothing Protected
              | otherwise = undef

            publicLookup
              | HashMap.member sym (names Array.! Public) =
                checkVisibility Nothing Public
              | otherwise = undef

            defsLookup = firstValid [hiddenLookup, privateLookup,
                                     protectedLookup, publicLookup]

            builderLookup = case HashMap.lookup sym builders of
              Just Builder { builderVisibility = actualvis,
                             builderContent = content } ->
                -- There should be a tempref for this symbol
                case HashMap.lookup sym symtemprefs of
                  Just tempref ->
                    do
                      -- Attempt to resolve the scope expression.
                      (resscope, resdeps) <- lookupScopeExp scopeid tempref
                                                            content
                      checkVisibility (Just ResolvedScope {
                                              resolvedScopeID = resscope,
                                              resolvedScopeDeps = resdeps
                                            })
                                      actualvis
                  Nothing ->
                    do
                      internalError "No tempref mapping for symbol" []
                      undef
              Nothing -> undef

            truthLookup = case HashMap.lookup sym truths of
              Just Truth { truthVisibility = actualvis } ->
                checkVisibility Nothing actualvis
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
                              ExceptT Error m Resolution
        resolveFromImports =
          let
            resolveFromImport :: (MonadIO m, MonadMessages Message m) =>
                                 TempImport
                              -> ExceptT Error m Resolution
            resolveFromImport TempImport {
                                tempImportData = Import {
                                                   importExp = importscope,
                                                   importNames = names
                                                 },
                                tempImportDeps = deps
                              }
                -- Check if the symbol is one of the ones in the
                -- import list, or if the list is empty (in which case
                -- we import everything)
              | HashSet.null names || HashSet.member sym names =
                let
                  addPathElem impscope deps
                              Direct { directResolvedScope = resscope } =
                    let
                      -- For direct resolutions, synthesize a new
                      -- indirect resolution.
                      imported = Imported { importedScope = impscope }
                      nonlocal = NonLocal { nonLocalSym = sym,
                                            nonLocalScope = impscope,
                                            nonLocalSrc = imported,
                                            nonLocalTail = [],
                                            nonLocalDeps = deps,
                                            nonLocalResolvedScope = resscope }
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
                      imported = Imported { importedScope = impscope }
                      newnonlocal = nl { nonLocalSrc = imported,
                                         nonLocalTail = src : tail,
                                         nonLocalDeps = olddeps <> deps }
                    in
                      return ind { indirectSrc = newnonlocal }
                in do
                  -- Do resolution from the imported scope, ignoring
                  -- enclosing scopes.
                  res <- resolveSymbol importscope Public False sym
                  -- Add the import path element.
                  addPathElem importscope deps res
                  -- Otherwise, we can't possibly import the symbol here.
              | otherwise = undef

            mapfun (Left _) = undef
            mapfun (Right imp) = resolveFromImport imp
          in do
            resolved <- mapM mapfun imports
            return (mconcat resolved)

        resolveFromInherits :: (MonadIO m, MonadMessages Message m) =>
                               ExceptT Error m Resolution
        resolveFromInherits =
          let
            resolveFromInherit :: (MonadIO m, MonadMessages Message m) =>
                                  TempInherit
                               -> ExceptT Error m Resolution
            resolveFromInherit TempInherit { tempInheritScope = inheritscope,
                                             tempInheritDeps = deps } =
              let
                  addPathElem inheritscope deps
                              Direct { directResolvedScope = resscope } =
                    let
                      -- For direct resolutions, synthesize a new
                      -- indirect resolution.
                      inherited = Inherited { inheritedScope = inheritscope }
                      nonlocal = NonLocal { nonLocalSym = sym,
                                            nonLocalScope = inheritscope,
                                            nonLocalSrc = inherited,
                                            nonLocalTail = [],
                                            nonLocalDeps = deps,
                                            nonLocalResolvedScope = resscope }
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
                      inherited = Inherited { inheritedScope = impscope }
                      newnonlocal = nl { nonLocalSrc = inherited,
                                         nonLocalTail = src : tail,
                                         nonLocalDeps = olddeps <> deps }
                    in
                      return ind { indirectSrc = newnonlocal }
              in do
                -- Do resolution from that scope at protected
                -- visibility, ignoring enclosing scopes.
                res <- resolveSymbol inheritscope Protected False sym
                -- Add the inherit path element.
                addPathElem inheritscope deps res

            -- | Take a raw set of inherited resolutions and eliminate
            -- all overridden resolutions.  If this reduces the set to
            -- a singleton, that is the result; otherwise, it's an
            -- ambiguous resolution.
            reconcileInherits :: (MonadIO m, MonadMessages Message m) =>
                                 [Resolution]
                              -> ExceptT Error m Resolution
            -- Easy case: we found nothing
            reconcileInherits [] = undef
            -- Easy case; we found exactly one, no reconciliation required
            reconcileInherits [out] = return out
            reconcileInherits resolutions =
              let
                -- Filter out all the resolutions that are overridden
                -- by other resolutions.
                filterOverrides accum [] = return accum
                filterOverrides accum
                                (first @ Indirect {
                                           indirectSrc =
                                             nl @ NonLocal {
                                                    nonLocalSrc =
                                                      Inherited {
                                                        inheritedScope =
                                                          currscope
                                                      }
                                                  }
                                         } : rest) =
                  let
                    skipscope = Just scopeid

                    mapfun Indirect {
                             indirectSrc =
                               NonLocal {
                                 nonLocalSrc =
                                   Inherited { inheritedScope = scope2 }
                               }
                           } =
                      do
                        overridden <- checkInherit skipscope scope2 currscope
                        if overridden
                          then
                            -- Check for an inheritence cycle.
                            do
                              cyclic <- checkInherit skipscope currscope scope2
                              -- Only drop the current if it's
                              -- overridden AND we don't have a cyclic
                              -- inheritence.
                              return (not cyclic)
                          else
                            -- If we're not overridden, keep the current.
                            return False

                    -- Invalid resolution we haven't reported yet, but
                    -- we will.
                    mapfun _ = return False
                  in do
                    -- Check if the current definition is overridden
                    -- by any of the rest.
                    checks <- mapM mapfun rest
                    if any id checks
                      -- If it is, drop it
                      then filterOverrides accum rest
                      else
                        let
                          -- XXX Should we collect all paths?
                          newaccum = HashMap.insert currscope nl accum
                        in
                          filterOverrides newaccum rest
                -- We shouldn't see non-inherited resolutions.  Report
                -- them and drop them.
                filterOverrides accum _ =
                  do
                    internalError "Invalid resolution in inherits" []
                    return accum
              in do
                nonoverrides <- filterOverrides HashMap.empty resolutions
                case HashMap.toList nonoverrides of
                  -- This case shouldn't happen, log an internal error
                  [] ->
                    do
                      internalError ("Should not see empty non-override set") []
                      undef
                  -- Exactly one resolution.  This is what we want.
                  [(_, out)] -> return Indirect { indirectSrc = out }
                  -- Ambiguous resolution
                  _ -> ambiguous nonoverrides

            mapfun (Left _) = undef
            mapfun (Right inherit) = resolveFromInherit inherit
          in do
            -- First resolve all inherits
            resolved <- mapM mapfun inherits
            -- Now reconcile all the results down to one
            reconcileInherits resolved

        -- | Attempt to resolve enclosing definitions, if we are
        -- allowed to look there.
        resolveFromEnclosing :: (MonadIO m, MonadMessages Message m) =>
                                ExceptT Error m Resolution
        resolveFromEnclosing
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
                    Direct { directResolvedScope = resscope } ->
                      let
                        pathelem = Enclosing { enclosingScope = encscopeid,
                                               enclosingDepth = 1 }
                        nonlocal = NonLocal { nonLocalSym = sym,
                                              nonLocalScope = encscopeid,
                                              nonLocalSrc = pathelem,
                                              nonLocalTail = [],
                                              nonLocalDeps = HashMap.empty,
                                              nonLocalResolvedScope = resscope }
                      in
                        return Indirect { indirectSrc = nonlocal }
                    -- If the result is an enclosing scope definition,
                    -- increment the depth.
                    ind @ Indirect {
                            indirectSrc =
                              src @ NonLocal {
                                      nonLocalSrc =
                                        enc @ Enclosing {
                                                enclosingDepth = depth
                                              }
                                    }
                          } ->
                      let
                        incdepth = enc { enclosingDepth = depth + 1 }
                        newsrc = src { nonLocalSrc = incdepth }
                      in
                        return ind { indirectSrc = newsrc }
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

        -- | Check that the resolution result is consistent.
        checkConsistent :: (MonadIO m, MonadMessages Message m) =>
                           Resolution
                        -> ExceptT Error m Resolution
        checkConsistent out @ Indirect {
                                indirectSrc =
                                  src @ NonLocal {
                                          nonLocalDeps = deps,
                                          nonLocalResolvedScope =
                                            Just ResolvedScope {
                                                   resolvedScopeID = expectscope
                                                 }
                                        }
                              } =
          -- See if there's a tempref for this symbol.
          case HashMap.lookup sym symtemprefs of
            -- If there's not, then there's nothing to do.
            Nothing -> return out
            -- If there is, then we do the consistency check
            Just tempref ->
              -- Check if the scope, tempref pair is in the dependencies
              case HashMap.lookup (scopeid, tempref) deps of
                -- If we depend on our own resolution being something
                -- different, then this is an inconsistent resolution
                -- result.
                Just actualscope
                  | expectscope /= actualscope -> inconsistent src
                -- Otherwise the result is fine.
                _ -> return out
        -- In all other cases, resolution is consistent
        checkConsistent out = return out
      in do
        -- Search for results in precedence order
        res <- firstValid [resolveDirect, resolveFromImports,
                           resolveFromInherits, resolveFromEnclosing]
        -- Check consistency
        checkConsistent res

    -- | Attempt to resolve all symbols in a scope, if it's in the
    -- workset.
    resolveScope :: (MonadIO m, MonadMessages Message m) =>
                    (ScopeID, TempScope)
                 -> m (ScopeID, TempScope)
    -- Skip this scope if it's not in the workset
    resolveScope elem @ (idx, _) | workset BitArray.! idx = return elem
    resolveScope curr @ (scopeid,
                         scope @ TempScope { tempScopeSyms = resolutions }) =
      let
        tryResolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                            Symbol
                         -> m (Symbol, Either Error Resolution)
        tryResolveSymbol sym =
          do
            -- Catch any errors
            res <- runExceptT $! resolveSymbol scopeid Hidden True sym
            return (sym, res)
      in do
        -- Resolve all symbols using the previous scope
        newres <- mapM tryResolveSymbol (HashMap.keys resolutions)
        return (scopeid, scope { tempScopeSyms = HashMap.fromList newres })

    resolveScopeLinks :: (MonadIO m) =>
                         (ScopeID, TempScope)
                      -> HashMap Symbol (Either Error Resolution)
                      -> m (ScopeID, TempScope)
    resolveScopeLinks newresolutions
      | resolutions == newresolutions = return curr
      | otherwise =
        let
        in do
          -- Use the new resolutions to resolve all imports

          -- Use the new resoultions to resolve all inherits

          -- Propagate this change to all the scopes that
          -- depend on this one
          _

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
resolve :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
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
               -> m (ScopeID, HashMap Symbol (Either Error Resolution),
                     [Proof (Exp Seq TempRef)], [Import TempRef], [TempRef],
                     HashMap Symbol (Builder (Exp Seq TempRef)),
                     HashMap Symbol (Syntax (Exp Seq TempRef)),
                     HashMap Symbol (Truth (Exp Seq TempRef)),
                     Array DefID (Def (Exp Seq TempRef)),
                     [Compound (Exp Seq TempRef)],
                     Array TempRef TempExp,
                     HashMap Symbol TempRef,
                     HashMap Symbol [Position])
        mapfun enclosedarr (scopeid, scope) =
          let
            -- Gather up all the references
            syms = foldl (foldl (flip HashSet.insert)) HashSet.empty scope
            -- Make everything undefined initially
            ents = zip (HashSet.toList syms) (repeat (Left Undefined))
          in do
            case scope of
              Scope { scopeEnclosing = Just enclosing } ->
                do
                  enclosed <- liftIO (IOArray.readArray enclosedarr enclosing)
                  liftIO (IOArray.writeArray enclosedarr enclosing
                                             (scopeid : enclosed))
              _ -> return ()
            (proofs, imports, inherits, builders, syntax, truths,
             defs, eval, renametab, symtab, sympos) <- renameScope scope
            return (scopeid, HashMap.fromList ents, proofs, imports, inherits,
                    builders, syntax, truths, defs, eval, renametab, symtab,
                    sympos)

        -- Create the 'TempScope's out of their components
        makeTempScopes :: MonadIO m =>
                          Array ScopeID [ScopeID]
                       -> [(ScopeID, HashMap Symbol (Either Error Resolution),
                            [Proof (Exp Seq TempRef)],
                            [Import TempRef], [TempRef],
                            HashMap Symbol (Builder (Exp Seq TempRef)),
                            HashMap Symbol (Syntax (Exp Seq TempRef)),
                            HashMap Symbol (Truth (Exp Seq TempRef)),
                            Array DefID (Def (Exp Seq TempRef)),
                            [Compound (Exp Seq TempRef)],
                            Array TempRef TempExp,
                            HashMap Symbol TempRef,
                            HashMap Symbol [Position])]
                       -> m (Array ScopeID TempScope)
        makeTempScopes enclosedarr scopedata =
          let
            -- Map function
            makeTempScope (scopeid, syms, proofs, imports, inherits,
                           builders, syntax, truths, defs, eval, renametab,
                           symtab, sympos) =
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
                                             tempScopeResolvedImports = [],
                                             tempScopeResolvedInherits = [],
                                             tempScopeEnclosed = enclosed,
                                             tempScopeSymbolTempRefs = symtab,
                                             tempScopeSymbolPos = sympos })
          in do
            tempscopes <- mapM makeTempScope scopedata
            return (Array.array arrbounds tempscopes)

      in do
        -- Run through the scopes, extract the referenced symbols,
        -- link up the encloded scopes.
        enclosedarr <- liftIO $! IOArray.newArray_ arrbounds
        tempscopes <- mapM (mapfun enclosedarr) (Array.assocs scopes)
        frozenenclosed <- liftIO $! IOArray.unsafeFreeze enclosedarr
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
    resolvedscopes <- finishScopes finalscopes
    return surface { surfaceScopes = resolvedscopes }
