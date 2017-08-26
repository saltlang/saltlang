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
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Positions
import Data.Array(Array, Ix)
import Data.Array.BitArray(BitArray)
import Data.Array.BitArray.IO(IOBitArray)
import Data.Array.IO(IOArray)
import Data.Default
import Data.Either
import Data.Hashable
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.List(sort, partition)
import Data.PositionElement
import Data.ScopeID
import Data.Semigroup
import Data.Symbol
import Language.Salt.Message
import Language.Salt.Surface.Resolve.Resolution
import Language.Salt.Surface.Syntax

import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as IOArray
import qualified Data.Array.BitArray as BitArray
import qualified Data.Array.BitArray.IO as IOBitArray
import qualified Data.ByteString as Strict
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

data TempRefError =
    -- | Cyclic definitions.  This can happen with builder definitions
    -- that never "bottom out" at a concrete definition.
    Cyclic { cyclicRefs :: !(HashSet (ScopeID, TempRef)) }
    -- | A resolution result which depends on itself resolving to
    -- something else.
  | Inconsistent { inconsistentDeps :: !(HashMap (ScopeID, TempRef) ScopeID) }
    -- | Resolution of the TempRef relies on a symbol which resolves
    -- to an error.
  | SymbolError { symbolError :: !SymbolError }
  | NotScope
    deriving (Eq)

data SymbolError =
    -- | Import Collision.
    Collision { collisionRefs :: !(HashMap Ref (NonLocal TempRef)) }
    -- | Ambiguous inherited definition.
  | Ambiguous { ambiguousRefs :: !(HashMap ScopeID (NonLocal TempRef)) }
    -- | Illegal access (private from an inheriting scope, private or
    -- protected from an non-inheriting scope)
  | Illegal {
      -- | Visibility of the symbol.
      illegalVisibility :: !Visibility
    }
    -- | Access to a non-static definition from a static context.
  | Inaccessible {
      -- | True for object context, false for local.
      inaccessibleKind :: !ContextKind
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
    tempImportData :: !ScopeID,
    tempImportDeps :: !(HashMap (ScopeID, TempRef) ScopeID)
  }

data TempScope =
  TempScope {
    -- Unaltered content from the original scope.
    tempScopeNames :: !(Array Visibility (HashMap Symbol [DefID])),
    tempScopeEnclosing :: !(Maybe ScopeID),
    -- Static expression-based content from the original scope.
    tempScopeProofs :: ![Proof (Exp Seq TempRef)],
    tempScopeImports :: ![Import (TempRef, Position)],
    tempScopeInherits :: ![(TempRef, Position)],
    -- Renamed expression-based content from the original Scope.
    tempScopeBuilders :: !(HashMap Symbol (Builder (TempRef, Position))),
    tempScopeSyntax :: !(HashMap Symbol (Syntax (Exp Seq TempRef))),
    tempScopeTruths :: !(HashMap Symbol (Truth (Exp Seq TempRef))),
    tempScopeDefs :: !(Array DefID (Def (Exp Seq TempRef))),
    tempScopeEval :: ![Compound (Exp Seq TempRef)],

    -- | Definitions of 'TempRef's
    tempScopeRenameTab :: !(Array TempRef TempExp),
    tempScopeSymbolTempRefs :: !(HashMap Symbol TempRef),
    tempScopeSymbolPos :: !(HashMap Symbol [Position]),
    -- | Map from 'TempRef's (possible static expressions) to
    -- resolution results.
    tempScopeResTab :: !(IOArray TempRef TempRefResult),
    tempScopeResTabMask :: !(IOBitArray TempRef)
  }

type ResolveDeps = HashMap (ScopeID, TempRef) ScopeID

type SymbolResult = Either SymbolError (Resolution TempRef)

type TempRefResult = Either TempRefError (ResolvedScope TempRef)

data TempScopeLinks =
  TempScopeLinks {
    -- | We carry around the import statement with a unit expression,
    -- so we can sub everything back in.
    tempScopeResolvedImports :: ![(Import (), Either TempRefError TempImport,
                                   Position)],
    tempScopeResolvedInherits :: ![(Either TempRefError TempInherit, Position)]
  }

data TempExp =
    TempExp {
      tempExp :: !(Exp Seq TempRef)
    }
  | TempSym {
      tempSym :: !Symbol
    }
    deriving (Eq, Ord)

data RenameState =
  RenameState {
    renameStateCurr :: !TempRef,
    renameStateTab :: !(HashMap TempExp TempRef),
    renameSymTab :: !(HashMap Symbol TempRef),
    renameSymPos :: !(HashMap Symbol [Position])
  }

type RenameT m = StateT RenameState m

instance Ord TempRefError where
  compare Cyclic { cyclicRefs = refs1 } Cyclic { cyclicRefs = refs2 } =
    let
      sortedrefs1 = sort (HashSet.toList refs1)
      sortedrefs2 = sort (HashSet.toList refs2)
    in
      compare sortedrefs1 sortedrefs2
  compare Cyclic {} _ = LT
  compare _ Cyclic {} = GT
  compare Inconsistent { inconsistentDeps = deps1 }
          Inconsistent { inconsistentDeps = deps2 } =
    let
      sorteddeps1 = sort (HashMap.toList deps1)
      sorteddeps2 = sort (HashMap.toList deps2)
    in
      compare sorteddeps1 sorteddeps2
  compare Inconsistent {} _ = LT
  compare _ Inconsistent {} = GT

instance Ord SymbolError where
  compare Ambiguous { ambiguousRefs = refs1 }
          Ambiguous { ambiguousRefs = refs2 } =
    let
      sortedrefs1 = sort (HashMap.toList refs1)
      sortedrefs2 = sort (HashMap.toList refs2)
    in
      compare sortedrefs1 sortedrefs2
  compare Ambiguous {} _ = LT
  compare _ Ambiguous {} = GT
  compare Collision { collisionRefs = refs1 }
          Collision { collisionRefs = refs2 } =
    let
      sortedrefs1 = sort (HashMap.toList refs1)
      sortedrefs2 = sort (HashMap.toList refs2)
    in
      compare sortedrefs1 sortedrefs2
  compare Collision {} _ = LT
  compare _ Collision {} = GT
  compare Illegal { illegalVisibility = vis1 }
          Illegal { illegalVisibility = vis2 } = compare vis1 vis2
  compare Illegal {} _ = LT
  compare _ Illegal {} = GT
  compare Inaccessible { inaccessibleKind = kind1 }
          Inaccessible { inaccessibleKind = kind2 } =
    compare kind1 kind2
  compare Inaccessible {} _ = LT
  compare _ Inaccessible {} = GT
  compare Undefined Undefined = EQ

instance Hashable TempRefError where
  hashWithSalt s Cyclic { cyclicRefs = refs } =
    let
      sortedRefs = sort (HashSet.toList refs)
    in
      s `hashWithSalt` (0 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Inconsistent { inconsistentDeps = deps } =
    let
      sorteddeps = sort (HashMap.toList deps)
    in
      s `hashWithSalt` (1 :: Word) `hashWithSalt` deps

instance Hashable SymbolError where
  hashWithSalt s Ambiguous { ambiguousRefs = refs } =
    let
      sortedRefs = sort (HashMap.toList refs)
    in
      s `hashWithSalt` (0 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Collision { collisionRefs = refs } =
    let
      sortedRefs = sort (HashMap.toList refs)
    in
      s `hashWithSalt` (1 :: Int) `hashWithSalt` sortedRefs
  hashWithSalt s Illegal { illegalVisibility = vis } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` vis
  hashWithSalt s Inaccessible { inaccessibleKind = object } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` object
  hashWithSalt s Undefined = s `hashWithSalt` (3 :: Word)

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
nonLocalPath :: NonLocal TempRef -> [PathElem]
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
renameScope :: (MonadIO m, MonadMessages Message m) =>
               Scope (Exp Seq Symbol)
            -> m TempScope
renameScope Scope { scopeBuilders = builders, scopeSyntax = syntax,
                    scopeTruths = truths, scopeDefs = defs,
                    scopeEval = eval, scopeProofs = proofs,
                    scopeImports = imports, scopeInherits = inherits,
                    scopeNames = names, scopeEnclosing = enclosing } =
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
                    -> RenameT m (TempRef, Position)
    renameStaticExp ex =
      do
        -- Do the renaming
        renamed <- renameExp ex
        -- We should always see an Id.  It's an internal error if we don't.
        case renamed of
          Id { idRef = ref, idPos = pos } -> return (ref, pos)
          _ ->
            do
              internalError "Expected static expression here" [position ex]
              -- Return the default symbol
              return (def, position ex)

    renameScope' =
      do
        newproofs <- mapM (mapM renameExp) proofs
        newimports <- mapM (mapM renameStaticExp) imports
        newinherits <- mapM renameStaticExp inherits
        newbuilders <- mapM (mapM renameStaticExp) builders
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
    ((newproofs, newimports, newinherits, newbuilders,
      newsyntax, newtruths, newdefs, neweval),
     RenameState { renameStateCurr = endidx, renameStateTab = tab,
                   renameSymTab = symtab, renameSymPos = sympos }) <-
      runStateT renameScope' initstate
    restab <- liftIO $! IOArray.newArray_ (succ def, endidx)
    restabmask <- liftIO $! IOBitArray.newArray (succ def, endidx) False
    return TempScope { tempScopeNames = names,
                       tempScopeEnclosing = enclosing,
                       tempScopeProofs = newproofs,
                       tempScopeImports = newimports,
                       tempScopeInherits = newinherits,
                       tempScopeBuilders = newbuilders,
                       tempScopeSyntax = newsyntax,
                       tempScopeTruths = newtruths,
                       tempScopeDefs = newdefs,
                       tempScopeEval = neweval,
                       tempScopeRenameTab = renametab endidx tab,
                       tempScopeSymbolTempRefs = symtab,
                       tempScopeSymbolPos = sympos,
                       tempScopeResTab = restab,
                       tempScopeResTabMask = restabmask }

tempScopeIsDirect :: Array ScopeID TempScope
                  -> ScopeID
                  -> Symbol
                  -> Bool
tempScopeIsDirect tempscopes scopeid sym =
  let
    TempScope { tempScopeNames = names,
                tempScopeBuilders = builders,
                tempScopeTruths = truths } = tempscopes Array.! scopeid
  in
    any (HashMap.member sym) (Array.elems names) ||
    HashMap.member sym builders || HashMap.member sym truths

-- | Core resolution algorithm.
finishScopes :: (MonadIO m, MonadMessages Message m,
                 MonadSymbols m, MonadPositions m) =>
                Array ScopeID TempScope
             -> Array ScopeID (HashMap Symbol SymbolResult)
             -> Array ScopeID TempScopeLinks
             -> m (Array ScopeID (Resolved (Exp Seq Ref)))
finishScopes finalscopes resolvetabs scopelinks =
  let
    arrbounds = Array.bounds finalscopes

    mapinherits :: (Either TempRefError TempInherit, Position)
                -> Link
    mapinherits (Left _, pos) = BadLink { badLinkPos = pos }
    mapinherits (Right TempInherit { tempInheritScope = scopeid }, pos) =
      Link { linkScopeID = scopeid, linkPos = pos }

    mapimports :: (Import (), Either TempRefError TempImport, Position)
               -> Import Link
    mapimports (imp, Left _, pos) =
      imp { importExp = BadLink { badLinkPos = pos } }
    mapimports (imp, Right TempImport { tempImportData = scopeid }, pos) =
      imp { importExp = Link { linkScopeID = scopeid, linkPos = pos } }

    importarr = fmap (map mapimports . tempScopeResolvedImports) scopelinks

    temprefPosition :: (MonadIO m, MonadMessages Message m,
                        MonadSymbols m, MonadPositions m) =>
                       [Position]
                    -> (ScopeID, TempRef)
                    -> m [Position]
    temprefPosition accum (scopeid, tempref) =
      let
        TempScope { tempScopeRenameTab = renametab,
                    tempScopeSymbolPos = sympos } =
          finalscopes Array.! scopeid
      in
        case renametab Array.! tempref of
          TempExp { tempExp = ex } -> return $! position ex : accum
          TempSym { tempSym = sym } ->
            case HashMap.lookup sym sympos of
              Just pos -> return $! pos ++ accum
              Nothing ->
                do
                  internalError "Should have position for symbol" []
                  return accum

    reportCyclic :: (MonadIO m, MonadMessages Message m,
                     MonadSymbols m, MonadPositions m) =>
                    HashSet (ScopeID, TempRef)
                 -- ^ The cyclic definitions.
                 -> m ()
    reportCyclic defs =
      do
        poslist <- foldM temprefPosition [] (HashSet.toList defs)
        cyclicDefs poslist

    -- | Convert a 'TempScope' into a 'Resolved' scope.
    finishScope :: (MonadIO m, MonadMessages Message m,
                    MonadSymbols m, MonadPositions m) =>
                -- ^ The final scope state.
                   ScopeID
                -> m (ScopeID, Resolved (Exp Seq Ref))
    finishScope scopeid =
      let
        isDirect = tempScopeIsDirect finalscopes scopeid

        TempScope { tempScopeBuilders = builders,
                    tempScopeSyntax = syntax,
                    tempScopeTruths = truths,
                    tempScopeDefs = defs,
                    tempScopeEval = eval,
                    tempScopeNames = names,
--                    tempScopeLocalRefs = localrefs,
                    tempScopeEnclosing = enclosing,
                    tempScopeSymbolPos = sympos,
                    tempScopeRenameTab = renametab,
                    tempScopeProofs = proofs,
                    tempScopeResTab = iorestab } = finalscopes Array.! scopeid

        syms = resolvetabs Array.! scopeid

        TempScopeLinks { tempScopeResolvedImports = imports,
                         tempScopeResolvedInherits = inherits } =
          scopelinks Array.! scopeid

        -- | Substitute the term for a 'TempRef' back in
        subst :: Array TempRef (Position -> Exp Seq Ref)
              -> Exp Seq TempRef
              -> Exp Seq Ref
        subst _ Compound { compoundScope = compscope, compoundPos = pos } =
          Compound { compoundScope = compscope, compoundPos = pos }
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
        subst arr Id { idRef = ref, idPos = pos } = (arr Array.! ref) pos
        subst arr w @ With { withVal = val, withArgs = args } =
          w { withVal = subst arr val, withArgs = subst arr args }
        subst arr w @ Where { whereVal = val, whereProp = prop } =
          w { whereVal = subst arr val, whereProp = subst arr prop }
        subst arr a @ Anon { anonParams = params } =
          a { anonParams = fmap (subst arr) params }
        subst _ Literal { literalVal = lit } = Literal { literalVal = lit }
        subst _ Bad { badPos = pos } = Bad { badPos = pos }

        -- | Report resolution errors for symbols
        finishSym :: (MonadIO m, MonadMessages Message m,
                      MonadSymbols m, MonadPositions m) =>
                     HashMap Symbol Ref
                  -> (Symbol, SymbolResult)
                  -> m (HashMap Symbol Ref)
        -- Report errors
        finishSym accum (sym, Left err) =
          case HashMap.lookup sym sympos of
            Just poslist ->
              do
                case err of
                  Illegal { illegalVisibility = vis } ->
                    illegalAccess sym vis poslist
                  Inaccessible { inaccessibleKind = ctx } ->
                    outOfContextAccess sym ctx poslist
                  Undefined -> undefSymbol sym poslist
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
            renamebounds @ (lo, hi) = Array.bounds renametab

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
                finishExp a @ Anon { anonParams = params } =
                  do
                    newparams <- mapM finishExp params
                    return (\pos -> a { anonParams = fmap (\f -> f pos)
                                                          newparams })
                finishExp Literal { literalVal = val } =
                  return (const Literal { literalVal = val })
                finishExp Bad { badPos = pos } =
                  return (const Bad { badPos = pos })

                finishTempExp :: (MonadIO m, MonadMessages Message m) =>
                                 TempExp
                              -> m (Position -> Exp Seq Ref)
                finishTempExp TempSym { tempSym = sym } =
                  case HashMap.lookup sym finishedsyms of
                    Just ref ->
                      return (\pos -> Id { idRef = ref, idPos = pos })
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
            finishmask <- liftIO $! IOBitArray.newArray renamebounds False
            visitmask <- liftIO $! IOBitArray.newArray renamebounds False
            finishedarr <- liftIO $! IOArray.newArray_ renamebounds
            mapM_ (finishEnt finishmask visitmask finishedarr) [lo..hi]
            liftIO $! IOArray.unsafeFreeze finishedarr

        finishLink :: (MonadIO m, MonadMessages Message m) =>
                      Array TempRef TempRefResult
                   -> (TempRef, Position)
                   -> m Link
        finishLink restab (tempref, pos) =
          case restab Array.! tempref of
            -- Direct or indirect resolutions that resolve to a scope
            -- can be turned into links.
            Right ResolvedScope { resolvedScopeID = scopeid } ->
              return Link { linkScopeID = scopeid, linkPos = pos }
            -- Anything else is a bad link.
            _ -> return BadLink { badLinkPos = pos }

        finishedinherits = fmap mapinherits inherits
      in do
        -- Pull in all the resolved symbols
        finishedsyms <- foldM finishSym HashMap.empty (HashMap.toList syms)
        tmprefvals <- finishRename finishedsyms
        restab <- liftIO $! IOArray.unsafeFreeze iorestab
        finishedbuilders <- mapM (mapM (finishLink restab)) builders
        return (scopeid,
                Resolved {
                  -- Names and enclosing scopes carry over directly
                  resolvedNames = names,
                  resolvedEnclosing = enclosing,
                  resolvedImports = fmap mapimports imports,
                  resolvedInherits = finishedinherits,
                  resolvedBuilders = finishedbuilders,
                  -- For proofs, just look up the value
                  resolvedProofs = fmap (fmap (subst tmprefvals)) proofs,
                  -- For the rest, substitute the expression back in
                  resolvedSyntax = fmap (fmap (subst tmprefvals)) syntax,
                  resolvedTruths = fmap (fmap (subst tmprefvals)) truths,
                  resolvedDefs = fmap (fmap (subst tmprefvals)) defs,
                  resolvedEval = fmap (fmap (subst tmprefvals)) eval
                })
  in do
    resolvedscopes <- mapM finishScope (Array.indices finalscopes)
    return $! Array.array arrbounds resolvedscopes

-- | Report an undefined symbol.
undef :: (Monad m) => ExceptT SymbolError m a
undef = throwError Undefined

cyclic :: (Monad m) =>
          HashSet (ScopeID, TempRef)
       -> ExceptT TempRefError m a
cyclic = throwError . Cyclic

-- | Report an ambiguous inherited symbol
ambiguous :: (Monad m) =>
             HashMap ScopeID (NonLocal TempRef)
          -> ExceptT SymbolError m a
ambiguous = throwError . Ambiguous

-- | Report an import collision
collision :: (Monad m) =>
             HashMap Ref (NonLocal TempRef)
          -> ExceptT SymbolError m a
collision = throwError . Collision

-- | Report an illegal access
illegal :: (Monad m) =>
           Visibility
        -> ExceptT SymbolError m a
illegal = throwError . Illegal

-- | Report an inconsistent resolution
inconsistent :: (Monad m) =>
                HashMap (ScopeID, TempRef) ScopeID
             -> ExceptT TempRefError m a
inconsistent = throwError . Inconsistent

symErr :: (Monad m) =>
          SymbolError
       -> ExceptT TempRefError m a
symErr = throwError . SymbolError

notScope :: (Monad m) => ExceptT TempRefError m a
notScope = throwError NotScope

-- Common pattern: go with the first valid resolution out of a list of
-- actions representing the precedence.
firstValid :: (MonadIO m, MonadMessages Message m) =>
              [ExceptT SymbolError m a] ->
              ExceptT SymbolError m a
firstValid [] =
  do
    internalError "Should not see empty action list" []
    undef
firstValid [out] = out
firstValid (first : rest) = first `catchError` const (firstValid rest)

allUndef :: (MonadIO m, MonadMessages Message m) =>
            ExceptT SymbolError m a
         -- ^ Final action to perform
         -> [ExceptT SymbolError m a]
         -- ^ List of actions expected to return undefined.
         -> ExceptT SymbolError m a
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
            [ExceptT SymbolError m a] ->
            ExceptT SymbolError m a
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
                 IOArray ScopeID (HashMap Symbol SymbolResult)
              -> IOArray ScopeID TempScopeLinks
              -> Array ScopeID TempScope
              -- ^ The current scope state
              -> BitArray ScopeID
              -- ^ The current workset
              -> IOBitArray ScopeID
              -- ^ The array into which to accumulate the next workset
              -> m ()
resolveScopes resolvetabs scopelinks tempscopes workset nextworkset =
  let
    arrbounds = Array.bounds tempscopes

    -- | Try to get the ScopeID of a Symbol representing a builder
    -- reference.
    resolveScopeSymbol :: (MonadIO m, MonadMessages Message m) =>
                          ResolveDeps
                       -> ScopeID
                       -> Symbol
                       -> ExceptT TempRefError m (ResolvedScope TempRef)
    resolveScopeSymbol deps scopeid sym =
      let
        foldfun :: (MonadIO m, MonadMessages Message m) =>
                   HashMap (ScopeID, TempRef) ScopeID
                -> ((ScopeID, TempRef), ScopeID)
                -> m (HashMap (ScopeID, TempRef) ScopeID)
        foldfun accum (key @ (scopeid, tempref), resscope)
          | not (HashMap.member key accum) =
            return (HashMap.insert key resscope accum)
          | otherwise =
            do
              internalError "Inconsistent dependencies" []
              return accum
      in do
        syms <- liftIO $! IOArray.readArray resolvetabs scopeid
        case HashMap.lookup sym syms of
          Just (Right Direct {
                        directResolvedScope = Just ResolvedScope {
                                                     resolvedScopeID = resscope,
                                                     resolvedScopeDeps = resdeps
                                                   }
                      }) ->
            do
              uniondeps <- foldM foldfun deps (HashMap.toList resdeps)
              return ResolvedScope { resolvedScopeID = resscope,
                                     resolvedScopeDeps = uniondeps }
          Just (Right Indirect {
                        indirectSrc =
                          NonLocal {
                            nonLocalResolvedScope =
                              Just ResolvedScope { resolvedScopeID = resscope,
                                                   resolvedScopeDeps = resdeps }
                                   }
                      }) ->
            do
              uniondeps <- foldM foldfun deps (HashMap.toList resdeps)
              return ResolvedScope { resolvedScopeID = resscope,
                                     resolvedScopeDeps = uniondeps }
          -- Report non-scopes
          Just (Right Direct { directResolvedScope = Nothing }) -> notScope
          Just (Right Indirect {
                        indirectSrc = NonLocal {
                                        nonLocalResolvedScope = Nothing
                                      }
                      }) -> notScope
          -- If the symbol is an error, report a symbol error
          Just (Left symerr) -> symErr symerr
          -- If we can't find a resolved scope, report an undefined
          Nothing -> symErr Undefined

    -- | Try to get the ScopeID of an Exp representing a builder reference.
    resolveScopeExpTail :: (MonadIO m, MonadMessages Message m) =>
                           HashSet (ScopeID, TempRef)
                        -> ResolveDeps
                        -> ScopeID
                        -> TempRef
                        -> Exp Seq TempRef
                        -> ExceptT TempRefError m (ResolvedScope TempRef)
    -- For projects, look up the base scope, then do a lookup of the
    -- field name in the base scope.
    resolveScopeExpTail history deps scopeid tempref
                       Project { projectFields = fields,
                                 projectVal = base,
                                 projectPos = pos } =
      case HashSet.toList fields of
        [FieldName { fieldSym = sym }] ->
          do
            ResolvedScope { resolvedScopeID = basescope,
                            resolvedScopeDeps = basedeps } <-
              resolveScopeExpTail history deps scopeid tempref base
            resolveScopeSymbol basedeps basescope sym
        [] ->
          do
            internalError "Should not see empty projected fields set" [pos]
            notScope
        _ ->
          do
            internalError "Should not see multiple projected fields" [pos]
            notScope

    -- For Id, look up the temp ref (note that cycles will be handled
    -- by that function).
    resolveScopeExpTail history deps scopeid _ Id { idRef = ref } =
      do
        res <- resolveScopeTempRefTail history deps scopeid ref
        case res of
          Left err -> notScope
          Right out -> return out
    -- XXX Same as With, should this get turned into a separate scope?
    resolveScopeExpTail history deps scopeid tempref
                       Call { callInfo = Seq { seqExps = [inner, _]} } =
      resolveScopeExpTail history deps scopeid tempref inner
    -- XXX What do we do here? Possibly create a separate scope for this?
    resolveScopeExpTail history deps scopeid tempref With { withVal = val } =
      resolveScopeExpTail history deps scopeid tempref val
    resolveScopeExpTail history deps scopeid tempref Where { whereVal = val } =
      resolveScopeExpTail history deps scopeid tempref val
    -- For a builder expression, return the scope id.
    resolveScopeExpTail _ deps scopeid tempref Anon { anonScope = resscopeid } =
      let
        key = (scopeid, tempref)
        newdeps = HashMap.insert key resscopeid deps
      in do
        when (HashMap.member (scopeid, tempref) deps)
             (internalError "Inconsistent dependencies" [])
        return ResolvedScope { resolvedScopeID = resscopeid,
                               resolvedScopeDeps = newdeps }
    -- Skip bad scopes
    resolveScopeExpTail _ _ _ _ Bad {} = notScope
    -- Everything else is an internal error
    resolveScopeExpTail _ _ _ _ ex =
      do
        internalError "Expected static expression" [position ex]
        notScope

    lookupScopeExp :: (MonadIO m, MonadMessages Message m) =>
                      ScopeID
                   -> TempRef
                   -> Exp Seq TempRef
                   -> ExceptT TempRefError m (ResolvedScope TempRef)
    lookupScopeExp = resolveScopeExpTail HashSet.empty HashMap.empty

    resolveScopeTempRefTail :: (MonadIO m, MonadMessages Message m) =>
                              HashSet (ScopeID, TempRef)
                           -> ResolveDeps
                           -> ScopeID
                           -> TempRef
                           -> m TempRefResult
    resolveScopeTempRefTail history deps scopeid tempref
        -- Detect cyclic references
      | HashSet.member (scopeid, tempref) history =
        return (Left Cyclic { cyclicRefs = history })
      | otherwise =
        let
          -- | Check that the resolution result is consistent.
          checkConsistent :: (MonadIO m, MonadMessages Message m) =>
                             (ResolvedScope TempRef)
                          -> ExceptT TempRefError m (ResolvedScope TempRef)
          checkConsistent out @ ResolvedScope { resolvedScopeID = expected,
                                                resolvedScopeDeps = deps } =
            case HashMap.lookup (scopeid, tempref) deps of
              -- If we depend on our own resolution being something
              -- different, then this is an inconsistent resolution
              -- result.
              Just actual
                | expected /= actual -> inconsistent deps
                  -- Otherwise the result is fine.
              _ -> return out
          -- In all other cases, resolution is consistent
          checkConsistent out = return out

          TempScope { tempScopeRenameTab = renametab,
                      tempScopeResTab = restab,
                      tempScopeResTabMask = mask } = tempscopes Array.! scopeid

          -- | Try to get the ScopeID of a TempExp representing a
          -- builder reference.
          resolveScopeTempExp :: (MonadIO m, MonadMessages Message m) =>
                                 TempExp
                              -> ExceptT TempRefError m (ResolvedScope TempRef)
          resolveScopeTempExp TempSym { tempSym = sym } =
            do
              res <- resolveScopeSymbol deps scopeid sym
              checkConsistent res
          resolveScopeTempExp TempExp { tempExp = ex } =
            do
              res <- resolveScopeExpTail history deps scopeid tempref ex
              checkConsistent res

          logError :: (MonadIO m, MonadMessages Message m) =>
                      TempRefError
                   -> ExceptT TempRefError m (ResolvedScope TempRef)
          logError err =
            do
              liftIO $! IOArray.writeArray restab tempref (Left err)
              liftIO $! IOBitArray.writeArray mask tempref True
              throwError err

          tempexp = renametab Array.! tempref
        in do
          -- Check if we have a valid result
          maskbit <- liftIO $! IOBitArray.readArray mask tempref
          if maskbit
            -- If we do, then return it
            then lookupScopeTempRef scopeid tempref
            else do
              out <- runExceptT $! resolveScopeTempExp tempexp
              liftIO $! IOArray.writeArray restab tempref out
              liftIO $! IOBitArray.writeArray mask tempref True
              return out

    resolveScopeTempRef :: (MonadIO m, MonadMessages Message m) =>
                           ScopeID
                        -> TempRef
                        -> m TempRefResult
    resolveScopeTempRef = resolveScopeTempRefTail HashSet.empty HashMap.empty

    lookupScopeTempRef :: (MonadIO m, MonadMessages Message m) =>
                          ScopeID
                       -> TempRef
                       -> m TempRefResult
    lookupScopeTempRef scopeid tempref =
      let
        TempScope { tempScopeResTab = restab } = tempscopes Array.! scopeid
      in do
        liftIO $! IOArray.readArray restab tempref

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
              TempScope { tempScopeInherits = inherits } =
                tempscopes Array.! currscope

              -- Foldable function to visit superscopes
              foldfun True _ = return True
              foldfun False (tempref, _) =
                do
                  res <- lookupScopeTempRef currscope tempref
                  case res of
                    Left _ -> return False
                    Right ResolvedScope { resolvedScopeID = superscope } ->
                      searchInherit excludes superscope
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
          Nothing -> liftIO $! IOBitArray.newArray arrbounds False
        searchInherit excludes src

    -- | Full resolution on a symbol.
    resolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                     ScopeID
                  -> Visibility
                  -> Bool
                  -- ^ Whether or not to search enclosing scopes as well.
                  -> Symbol
                  -- ^ The symbol to resolve.
                  -> ExceptT SymbolError m (Resolution TempRef)
    resolveSymbol scopeid expectvis withenclosing sym =
      let
        TempScope { tempScopeEnclosing = enclosing,
                    tempScopeTruths = truths,
                    tempScopeBuilders = builders,
                    tempScopeNames = names,
                    tempScopeSymbolTempRefs = symtemprefs } =
          tempscopes Array.! scopeid
        -- | Try to look up a direct (local) definition in a scope by its
        -- symbol.
        resolveDirect :: (MonadIO m, MonadMessages Message m) =>
                         ExceptT SymbolError m (Resolution TempRef)
        resolveDirect =
          let
            checkVisibility :: (Monad m) =>
                               Maybe (ResolvedScope TempRef)
                            -- ^ The resolved scope ID
                            -> Visibility
                            -- ^ The actual visibility level
                            -> ExceptT SymbolError m (Resolution TempRef)
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
                do
                  -- Attempt to resolve the scope expression.
                  ent <- lookupScopeTempRef scopeid content
                  case ent of
                    Left _ -> undef
                    Right res -> checkVisibility (Just res) actualvis
              Nothing -> undef

            truthLookup = case HashMap.lookup sym truths of
              Just Truth { truthVisibility = actualvis } ->
                checkVisibility Nothing actualvis
              Nothing -> undef
          in
            oneValid [defsLookup, builderLookup, truthLookup]

        -- | Attempt to resolve the symbol from imports.
        resolveFromImports :: (MonadIO m, MonadMessages Message m) =>
                              ExceptT SymbolError m (Resolution TempRef)
        resolveFromImports =
          let
            resolveFromImport :: (MonadIO m, MonadMessages Message m) =>
                                 (Import (), TempImport)
                              -> ExceptT SymbolError m (Resolution TempRef)
            resolveFromImport (Import { importNames = importnames },
                               TempImport { tempImportData = importscope,
                                            tempImportDeps = deps })
                -- Check if the symbol is one of the ones in the
                -- import list, or if the list is empty (in which case
                -- we import everything)
              | HashSet.null importnames || HashSet.member sym importnames =
                let
                  addPathElem impscope newdeps
                              Direct { directResolvedScope = resscope } =
                    let
                      -- For direct resolutions, synthesize a new
                      -- indirect resolution.
                      imported = Imported { importedScope = impscope }
                      nonlocal = NonLocal { nonLocalSym = sym,
                                            nonLocalScope = impscope,
                                            nonLocalSrc = imported,
                                            nonLocalTail = [],
                                            nonLocalDeps = newdeps,
                                            nonLocalResolvedScope = resscope }
                    in
                      return Indirect { indirectSrc = nonlocal }
                  -- For indirect resolutions, add a path element and
                  -- union the dependencies.
                  addPathElem impscope newdeps
                              ind @ Indirect {
                                      indirectSrc =
                                        nl @ NonLocal { nonLocalTail = pathtail,
                                                        nonLocalSrc = src,
                                                        nonLocalDeps = olddeps }
                                    } =
                    let
                      imported = Imported { importedScope = impscope }
                      newnonlocal = nl { nonLocalSrc = imported,
                                         nonLocalTail = src : pathtail,
                                         nonLocalDeps = olddeps <> newdeps }
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

            reconcileFiltered :: (MonadIO m, MonadMessages Message m) =>
                                 [Resolution TempRef]
                              -> ExceptT SymbolError m (Resolution TempRef)
            reconcileFiltered [] = undef
            reconcileFiltered valids =
              let
                foldfun accum Direct {} =
                  do
                    internalError "Shouldn't see direct resolution" []
                    return accum
                foldfun accum Indirect {
                                indirectSrc = nl @ NonLocal {
                                                     nonLocalScope = nlscope,
                                                     nonLocalSym = nlsym
                                                   }
                              } =
                  let
                    ref = Ref { refScopeID = nlscope, refSymbol = nlsym }
                  in do
                    when (HashMap.member ref accum)
                         (internalError "Inconsistent dependencies" [])
                    return (HashMap.insert ref nl accum)
              in do
                -- Combine all valids by the definition they
                -- actually reference
                validtab <- foldM foldfun HashMap.empty valids
                case HashMap.toList validtab of
                  [] ->
                    do
                      internalError "Shouldn't see empty list" []
                      undef
                  -- One unique object imported: this is what we want.
                  [(_, src)] -> return Indirect { indirectSrc = src }
                  -- If we have more than one, it's an import collision
                  _ -> collision validtab

            reconcileImports :: (MonadIO m, MonadMessages Message m) =>
                                [Either SymbolError (Resolution TempRef)]
                             -> ExceptT SymbolError m (Resolution TempRef)
            -- We found nothing
            reconcileImports [] = undef
            -- Easy case: we found only one, no reconciliation required
            reconcileImports [Left out] = throwError out
            reconcileImports [Right out] = return out
            reconcileImports resolutions =
              reconcileFiltered (rights resolutions)

            mapfun (_, Left _, _) = return (Left Undefined)
            mapfun (imp, Right timp, _) =
              runExceptT (resolveFromImport (imp, timp))

            filterfun (Left Undefined) = False
            filterfun _ = True
          in do
            TempScopeLinks { tempScopeResolvedImports = imports } <-
              liftIO $! IOArray.readArray scopelinks scopeid
            resolved <- mapM mapfun imports
            reconcileImports (filter filterfun resolved)

        resolveFromInherits :: (MonadIO m, MonadMessages Message m) =>
                               ExceptT SymbolError m (Resolution TempRef)
        resolveFromInherits =
          let
            resolveFromInherit :: (MonadIO m, MonadMessages Message m) =>
                                  TempInherit
                               -> ExceptT SymbolError m (Resolution TempRef)
            resolveFromInherit TempInherit { tempInheritScope = inheritscope,
                                             tempInheritDeps = deps } =
              let
                  addPathElem Direct { directResolvedScope = resscope } =
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
                  addPathElem ind @ Indirect {
                                      indirectSrc =
                                        nl @ NonLocal { nonLocalTail = pathtail,
                                                        nonLocalSrc = src,
                                                        nonLocalDeps = olddeps }
                                    } =
                    let
                      inherited = Inherited { inheritedScope = inheritscope }
                      newnonlocal = nl { nonLocalSrc = inherited,
                                         nonLocalTail = src : pathtail,
                                         nonLocalDeps = olddeps <> deps }
                    in
                      return ind { indirectSrc = newnonlocal }
              in do
                -- Do resolution from that scope at protected
                -- visibility, ignoring enclosing scopes.
                res <- resolveSymbol inheritscope Protected False sym
                -- Add the inherit path element.
                addPathElem res

            reconcileFiltered :: (MonadIO m, MonadMessages Message m) =>
                                 [(Resolution TempRef)]
                              -> ExceptT SymbolError m (Resolution TempRef)
            reconcileFiltered [] = undef
            reconcileFiltered resolutions =
              let
                -- Filter out all the resolutions that are overridden
                -- by other resolutions.
                filterOverrides accum [] = return accum
                filterOverrides accum
                                (Indirect {
                                   indirectSrc =
                                     nl @ NonLocal {
                                            nonLocalSrc =
                                              Inherited {
                                                inheritedScope = currscope
                                              }
                                          }
                                 } : rest) =
                  let
                    skipscope = Just scopeid

                    checkOverride Indirect {
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
                              iscycle <- checkInherit skipscope currscope scope2
                              -- Only drop the current if it's
                              -- overridden AND we don't have a cyclic
                              -- inheritence.
                              return (not iscycle)
                          else
                            -- If we're not overridden, keep the current.
                            return False

                    -- Invalid resolution we haven't reported yet, but
                    -- we will.
                    checkOverride _ = return False
                  in do
                    -- Check if the current definition is overridden
                    -- by any of the rest.
                    checks <- mapM checkOverride rest
                    if or checks
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
                      internalError "Should not see empty non-override set" []
                      undef
                  -- Exactly one resolution.  This is what we want.
                  [(_, out)] -> return Indirect { indirectSrc = out }
                  -- Ambiguous resolution
                  _ -> ambiguous nonoverrides

            -- | Take a raw set of inherited resolutions and eliminate
            -- all overridden resolutions.  If this reduces the set to
            -- a singleton, that is the result; otherwise, it's an
            -- ambiguous resolution.
            reconcileInherits :: (MonadIO m, MonadMessages Message m) =>
                                 [Either SymbolError (Resolution TempRef)]
                              -> ExceptT SymbolError m (Resolution TempRef)
            -- Easy case: we found nothing
            reconcileInherits [] = undef
            -- Easy case; we found exactly one, no reconciliation required
            reconcileInherits [Left err] = throwError err
            reconcileInherits [Right out] = return out
            reconcileInherits resolutions =
              reconcileFiltered (rights resolutions)

            mapfun (Left _, _) = return (Left Undefined)
            mapfun (Right inherit, _) =
              runExceptT (resolveFromInherit inherit)

            filterfun (Left Undefined) = False
            filterfun _ = True
          in do
            TempScopeLinks { tempScopeResolvedInherits = inherits } <-
              liftIO $! IOArray.readArray scopelinks scopeid
            -- First resolve all inherits
            resolved <- mapM mapfun inherits
            -- Now reconcile all the results down to one
            reconcileInherits (filter filterfun resolved)

        -- | Attempt to resolve enclosing definitions, if we are
        -- allowed to look there.
        resolveFromEnclosing :: (MonadIO m, MonadMessages Message m) =>
                                ExceptT SymbolError m (Resolution TempRef)
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
                            indirectSrc =
                              nl @ NonLocal { nonLocalTail = pathtail,
                                              nonLocalSrc = src }
                          } ->
                      let
                        newsrc = Enclosing { enclosingScope = encscopeid,
                                             enclosingDepth = 1 }
                        newnonlocal = nl { nonLocalSrc = newsrc,
                                           nonLocalTail = src : pathtail }
                      in
                      return ind { indirectSrc = newnonlocal }
              -- If there is no enclosing scope, report undefined
              Nothing -> undef
            -- Otherwise, report undefined.
          | otherwise = undef
      in
        -- Search for results in precedence order
        firstValid [resolveDirect, resolveFromImports,
                    resolveFromInherits, resolveFromEnclosing]

    -- | Attempt to resolve all symbols in a scope, if it's in the
    -- workset.
    resolveScopeSymbols :: (MonadIO m, MonadMessages Message m) =>
                           IOBitArray ScopeID
                        -> ScopeID
                        -> m (ScopeID, HashMap Symbol SymbolResult)
    resolveScopeSymbols changedset scopeid  =
      let
        tryResolveSymbol :: (MonadIO m, MonadMessages Message m) =>
                            HashMap Symbol SymbolResult
                         -> Symbol
                         -> m (Symbol, SymbolResult)
        tryResolveSymbol resolutions sym =
          -- Note: this will only see symbols that are not directly defined
          case HashMap.lookup sym resolutions of
            Just oldres ->
              let
                mark = liftIO $! IOBitArray.writeArray changedset scopeid True
              in do
                -- Catch any errors
                res <- runExceptT $! resolveSymbol scopeid Hidden True sym
                -- Mark this scope if we change the resolution
                unless (oldres == res) mark
                return (sym, res)
            Nothing ->
              do
                internalError "Entry missing from resolution map" []
                return (sym, Left Undefined)
      in do
        resolutions <- liftIO $! IOArray.readArray resolvetabs scopeid
        -- Resolve all symbols using the previous scope
        newres <- mapM (tryResolveSymbol resolutions) (HashMap.keys resolutions)
        return (scopeid, HashMap.fromList newres)

    resolveScopeStaticExps :: (MonadIO m, MonadMessages Message m) =>
                              ScopeID
                           -> m ()
    resolveScopeStaticExps scopeid =
      let
        TempScope { tempScopeRenameTab = renametab,
                    tempScopeResTab = restab } = tempscopes Array.! scopeid
      in
        mapM_ (resolveScopeTempRef scopeid) (Array.indices renametab)

    resolveScopeLinks :: (MonadIO m, MonadMessages Message m) =>
                         ScopeID
                      -> m ()
    resolveScopeLinks scopeid =
      let
        TempScope { tempScopeImports = imports,
                    tempScopeInherits = inherits } = tempscopes Array.! scopeid

        getInherit :: (MonadIO m, MonadMessages Message m) =>
                      (TempRef, Position)
                   -> m (Either TempRefError TempInherit, Position)
        getInherit (tempref, pos) =
          do
            ent <- lookupScopeTempRef scopeid tempref
            case ent of
              Left err -> return (Left err, pos)
              Right ResolvedScope { resolvedScopeID = resscope,
                                    resolvedScopeDeps = resdeps } ->
                return (Right TempInherit { tempInheritScope = resscope,
                                            tempInheritDeps = resdeps }, pos)

        getImport :: (MonadIO m, MonadMessages Message m) =>
                      Import (TempRef, Position)
                   -> m (Import (), Either TempRefError TempImport, Position)
        getImport i @ Import { importExp = (tempref, pos) } =
          do
            ent <- lookupScopeTempRef scopeid tempref
            case ent of
              Left err -> return (i { importExp = () }, Left err, pos)
              Right ResolvedScope { resolvedScopeID = resscope,
                                    resolvedScopeDeps = resdeps } ->
                return (i { importExp = () },
                        Right TempImport { tempImportData = resscope,
                                           tempImportDeps = resdeps }, pos)
      in do
        -- Use the new resolutions to resolve all imports
        newimports <- mapM getImport imports
        -- Use the new resoultions to resolve all inherits
        newinherits <- mapM getInherit inherits
        liftIO $! IOArray.writeArray scopelinks scopeid
                                     TempScopeLinks {
                                       tempScopeResolvedImports = newimports,
                                       tempScopeResolvedInherits = newinherits
                                     }

    -- | Check all scopes to see if they need to be marked in the new workset.
    markScopes :: (MonadIO m, MonadMessages Message m) =>
                  BitArray ScopeID
               -> m ()
    markScopes changedset =
      let
        -- | Check all scopes on which this one depends, mark it if
        -- any of them have changed.
        markScope :: (MonadIO m, MonadMessages Message m) =>
                     ScopeID
                  -> m ()
        markScope scopeid =
          let
            TempScope { tempScopeEnclosing = enclosing } =
              tempscopes Array.! scopeid

            -- | Mark the current scope if 'idx' is marked in the changedset
            mark idx =
              let
                markTrue =
                  liftIO $! IOBitArray.writeArray nextworkset scopeid True
              in
                when (changedset BitArray.! idx) markTrue

          in do
            TempScopeLinks { tempScopeResolvedInherits = inherits,
                             tempScopeResolvedImports = imports } <-
              liftIO $! IOArray.readArray scopelinks scopeid
            -- Mark the enclosing scope
            mapM_ mark enclosing
            -- Mark the inherited scopes
            mapM_ (mapM_ (mark . tempInheritScope) . fst) inherits
            -- Mark the imported scopes
            mapM_ (mapM_ (mark . tempImportData) . (\(_, b, _) -> b)) imports
      in
        mapM_ markScope (BitArray.indices changedset)

    resolveRound :: (MonadIO m, MonadMessages Message m) => m ()
    resolveRound =
      let
        trueIdxs bitarr = map fst (filter snd (BitArray.assocs bitarr))

        idxs = trueIdxs workset

        updateResolved (idx, newrestab) =
          liftIO $! IOArray.writeArray resolvetabs idx newrestab
      in do
        -- XXX Refactor this part; it's probably wrong

        changedset <- liftIO (IOBitArray.newArray arrbounds True)
        -- Attempt to resolve all the external symbols in scopes that
        -- were in the work-set
        newrestabs <- mapM (resolveScopeSymbols changedset) idxs
        -- Update the newly-resolved scopes all at once
        mapM_ updateResolved newrestabs
        -- Use the newly resolved symbols to resolve static expressions
        newrestabs <- mapM resolveScopeStaticExps idxs
        -- Resolve the scope links using the newly updated resolutions
        mapM_ resolveScopeLinks idxs
        -- Propagate all changes to the dependencies of the changed scopes
        frozenchangedset <- liftIO $! IOBitArray.freeze nextworkset
        markScopes frozenchangedset

    recurse =
      do
        -- Make the next workset
        newworkset <- liftIO $! IOBitArray.freeze nextworkset
        -- Recurse with the new tempscopes and workset array
        resolveScopes resolvetabs scopelinks tempscopes newworkset nextworkset
  in do
    -- Clear the next workset array
    liftIO $! IOBitArray.fill nextworkset False
    -- Run one round
    resolveRound
    -- Check to see if we're done
    done <- liftIO $! IOBitArray.and nextworkset
    unless done recurse

-- | Resolve all references in a 'Surface' syntax structure and link
-- all scopes together.
resolve :: (MonadMessages Message m, MonadSymbols m,
            MonadPositions m, MonadIO m) =>
           Surface (Scope (Exp Seq Symbol))
        -- ^ The syntax structure to resolve.
        -> m (Surface (Resolved (Exp Seq Ref)))
        -- ^ The resolved syntax structure.
resolve surface @ Surface { surfaceScopes = scopes } =
  let
    arrbounds = Array.bounds scopes

    -- Create the initial state for resolution
    initialState :: (MonadMessages Message m, MonadIO m) =>
                    m (Array ScopeID TempScope,
                       IOArray ScopeID (HashMap Symbol SymbolResult),
                       IOArray ScopeID TempScopeLinks)
    initialState =
      let
        mapfun :: (MonadMessages Message m, MonadIO m) =>
                  IOArray ScopeID (HashMap Symbol SymbolResult)
               -> (ScopeID, Scope (Exp Seq Symbol))
               -> m (ScopeID, TempScope)
        mapfun resolvetabs (scopeid, scope @ Scope { scopeEnclosing = enclosing,
                                                     scopeBuilders = builders,
                                                     scopeTruths = truths,
                                                     scopeNames = names }) =
          let
            -- Gather up all the symbols referenced in the scope
            syms = foldl (foldl (flip HashSet.insert)) HashSet.empty scope
            ents = zip (HashSet.toList syms) (repeat (Left Undefined))
          in do
            tempscope <- renameScope scope
            liftIO $! IOArray.writeArray resolvetabs scopeid
                                         (HashMap.fromList ents)
            return (scopeid, tempscope)

        emptylinks = TempScopeLinks { tempScopeResolvedImports = [],
                                      tempScopeResolvedInherits = [] }
      in do
        -- Run through the scopes, extract the referenced symbols,
        -- link up the encloded scopes.
        resolvetabs <- liftIO $! IOArray.newArray_ arrbounds
        tempscopes <- mapM (mapfun resolvetabs) (Array.assocs scopes)
        scopelinks <- liftIO $! IOArray.newArray arrbounds emptylinks
        return (Array.array arrbounds tempscopes, resolvetabs, scopelinks)

    -- The initial working set visits everything
    initworkset = (BitArray.true arrbounds)
  in do
    -- Set up the initial state
    nextworkset <- liftIO (IOBitArray.newArray arrbounds True)
    (tempscopes, resolvetabs, scopelinks) <- initialState
    -- Run resolution
    resolveScopes resolvetabs scopelinks tempscopes initworkset nextworkset
    frozenresolvetabs <- liftIO $! IOArray.unsafeFreeze resolvetabs
    frozenscopelinks <- liftIO $! IOArray.unsafeFreeze scopelinks
    -- Finalize the results and report errors
    resolvedscopes <- finishScopes tempscopes frozenresolvetabs frozenscopelinks
    return surface { surfaceScopes = resolvedscopes }
