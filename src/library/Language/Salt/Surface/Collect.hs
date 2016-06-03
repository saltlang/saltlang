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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Language.Salt.Surface.Collect(
       collectComponents,
       collectFiles,
       componentFileName
       ) where

import Control.Monad
import Control.Monad.Genpos
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols
import Data.Array hiding (accum, elems)
import Data.Array.IO(IOArray)
import Data.Foldable hiding (elem)
import Data.HashMap.Strict(HashMap)
import Data.HashTable.IO(BasicHashTable)
import Data.Maybe
import Data.Position.Filename
import Data.PositionElement
import Data.Symbol
import Language.Salt.Message
import Language.Salt.Surface.Common
import Prelude hiding (elem, exp, init)
import System.FilePath
import System.IO.Error

import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as Unsafe
import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashTable.IO as HashTable
import qualified Language.Salt.Surface.AST as AST
import qualified Language.Salt.Surface.Syntax as Syntax

type Table = BasicHashTable [Symbol] (Maybe Syntax.Component)

data TempScope expty =
  TempScope {
    tempScopeDefID :: !Syntax.DefID,
    tempScopeBuilders :: !(HashMap Symbol [Syntax.Builder expty]),
    tempScopeTruths :: !(HashMap Symbol [Syntax.Truth expty]),
    tempScopeSyntax :: !(HashMap Symbol [Syntax.Syntax expty]),
    tempScopeProofs :: ![Syntax.Proof expty],
    tempScopeImports :: ![Syntax.Import expty],
    tempScopeNames :: !(IOArray Visibility (HashMap Symbol [Syntax.DefID])),
    tempScopeDefs :: ![(Syntax.DefID, Syntax.Def expty)]
  }

data CollectState expty =
  CollectState {
    collectNextScope :: !ScopeID,
    collectScopes :: ![TempScope expty],
    collectFinishedScopes :: ![(ScopeID, Syntax.Scope expty)]
  }

type CollectT m = StateT (CollectState (Syntax.Exp Symbol)) (ReaderT Table m)

emptyState :: CollectState expty
emptyState = CollectState { collectNextScope = toEnum 0, collectScopes = [],
                            collectFinishedScopes = [] }

scopeStackUnderflow :: Strict.ByteString
scopeStackUnderflow = Strict.fromString "Scope stack underflow"

-- | Begin a new scope.
beginScope :: MonadIO m => CollectT m ()
beginScope =
  do
    namesarr <- liftIO $! IOArray.newArray (Hidden, Public) HashMap.empty
    cstate @ CollectState { collectScopes = scopes } <- get
    put cstate { collectScopes = TempScope { tempScopeBuilders = HashMap.empty,
                                             tempScopeTruths = HashMap.empty,
                                             tempScopeSyntax = HashMap.empty,
                                             tempScopeDefs = [],
                                             tempScopeProofs = [],
                                             tempScopeImports = [],
                                             tempScopeNames = namesarr,
                                             tempScopeDefID = toEnum 0 } :
                                 scopes }

-- | Check whether a component exists
componentExists :: MonadIO m => [Symbol] -> CollectT m Bool
componentExists path =
  do
    tab <- ask
    res <- liftIO $! HashTable.lookup tab path
    return $! isJust res

-- | Add a component to the register
addComponent :: MonadIO m => [Symbol] -> Syntax.Component -> CollectT m ()
addComponent path component =
  do
    tab <- ask
    liftIO $! HashTable.insert tab path $! Just component

-- | Add a Builder definition to the current scope.
addBuilder :: MonadMessages Message m =>
              Symbol -> Syntax.Builder (Syntax.Exp Symbol) ->
              CollectT m ()
addBuilder sym builder @ Syntax.Builder { Syntax.builderPos = pos } =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeBuilders = builders } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newbuilders = HashMap.insertWith (++) sym [builder] builders
          newscope = first { tempScopeBuilders = newbuilders }
          newstate = cstate { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a Truth definition to the current scope.
addTruth :: MonadMessages Message m =>
            Symbol -> Syntax.Truth (Syntax.Exp Symbol) -> CollectT m ()
addTruth sym truth @ Syntax.Truth { Syntax.truthPos = pos } =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeTruths = truths } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newtruths = HashMap.insertWith (++) sym [truth] truths
          newscope = first { tempScopeTruths = newtruths }
          newstate = cstate { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a Proof definition to the current scope.
addProof :: MonadMessages Message m =>
            Syntax.Proof (Syntax.Exp Symbol) -> CollectT m ()
addProof proof @ Syntax.Proof { Syntax.proofPos = pos } =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of proofs
      first @ TempScope { tempScopeProofs = proofs } : rest ->
        put cstate { collectScopes = first { tempScopeProofs = proof :
                                                               proofs } :
                                     rest }
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add an import to the current scope.
addImport :: MonadMessages Message m =>
             Syntax.Import (Syntax.Exp Symbol) -> CollectT m ()
addImport import' @ Syntax.Import { Syntax.importPos = pos } =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of proofs
      first @ TempScope { tempScopeImports = imports } : rest ->
        put cstate { collectScopes = first { tempScopeImports = import' :
                                                                imports } :
                                     rest }
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a syntax definition to the current scope.
addSyntax :: MonadMessages Message m =>
             Symbol -> Syntax.Syntax (Syntax.Exp Symbol) -> CollectT m ()
addSyntax sym syntax @ Syntax.Syntax { Syntax.syntaxPos = pos } =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeSyntax = syntaxes } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newsyntax = HashMap.insertWith (++) sym [syntax] syntaxes
          newscope = first { tempScopeSyntax = newsyntax }
          newstate = cstate { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a definition into a scope, merging it in with what's already there.
addDef :: MonadMessages Message m =>
          Syntax.Def (Syntax.Exp Symbol) ->
          CollectT m Syntax.DefID
addDef def @ Syntax.Def {} =
  do
    cstate @ CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of defs for this visibility
      first @ TempScope { tempScopeDefID = defid,
                          tempScopeDefs = defs } : rest ->
        let
          newscope = first { tempScopeDefID = succ defid,
                             tempScopeDefs = (defid, def) : defs }
          newstate = cstate { collectScopes = newscope : rest }
        in do
          put newstate
          return defid
      -- A stack underflow is an internal error
      _ -> error "Scope stack underflow"

-- | Add a definition into a scope, merging it in with what's already there.
addNames :: (MonadMessages Message m, MonadIO m) =>
            Visibility -> [Symbol] -> Syntax.DefID -> Position -> CollectT m ()
addNames vis syms defid pos =
  do
    CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of defs for this visibility
      TempScope { tempScopeNames = names } : _ ->
        let
          foldfun accum sym = HashMap.insertWith (++) sym [defid] accum
        in do
          curr <- liftIO $! IOArray.readArray names vis
          liftIO $! IOArray.writeArray names vis (foldl foldfun curr syms)
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Finish the current scope and return its ID
finishScope :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
               CollectT m ScopeID
finishScope =
  let
    collapseBuilders _ (_, []) = error "Empty builder list!"
    collapseBuilders accum (sym, [single]) =
      return $! HashMap.insert sym single accum
    collapseBuilders accum (sym, (builders @ (first : _))) =
      do
        duplicateBuilder sym (map position builders)
        return $! HashMap.insert sym first accum

    collapseTruths _ (_, []) = error "Empty truth list!"
    collapseTruths accum (sym, [single]) =
      return $! HashMap.insert sym single accum
    collapseTruths accum (sym, (truths @ (first : _))) =
      do
        duplicateTruth sym (map position truths)
        return $! HashMap.insert sym first accum

    collapseSyntax _ (_, []) = error "Empty syntax list!"
    collapseSyntax accum (sym, [single]) =
      return $! HashMap.insert sym single accum
    collapseSyntax accum (sym, (syntax @ (first : _))) =
      do
        duplicateSyntax sym (map position syntax)
        return $! HashMap.insert sym first accum
  in do
    CollectState { collectNextScope = scopeid, collectScopes = scopes,
                   collectFinishedScopes = finished } <- get
    case scopes of
      [] -> error "Scope stack underflow!"
      TempScope { tempScopeBuilders = builders, tempScopeTruths = truths,
                  tempScopeProofs = proofs, tempScopeSyntax = syntax,
                  tempScopeImports = imports, tempScopeNames = names,
                  tempScopeDefs = defs, tempScopeDefID = defid } :
        rest ->
        let
          defarr = array (toEnum 0, pred defid) defs
        in do
          namesarr <- liftIO $! Unsafe.unsafeFreeze names
          collapsedbuilders <- foldM collapseBuilders HashMap.empty
                                     (HashMap.toList builders)
          collapsedtruths <- foldM collapseTruths HashMap.empty
                                   (HashMap.toList truths)
          collapsedsyntax <- foldM collapseSyntax HashMap.empty
                                   (HashMap.toList syntax)
          -- Update the state
          put CollectState {
                collectFinishedScopes =
                   (scopeid,
                    Syntax.Scope { Syntax.scopeBuilders = collapsedbuilders,
                                   Syntax.scopeTruths = collapsedtruths,
                                   Syntax.scopeSyntax = collapsedsyntax,
                                   Syntax.scopeProofs = proofs,
                                   Syntax.scopeImports = imports,
                                   Syntax.scopeDefs = defarr,
                                   Syntax.scopeNames = namesarr }) : finished,
                collectScopes = rest, collectNextScope = succ scopeid
              }
          return scopeid

collectPattern :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  AST.Pattern ->
                  CollectT m (Syntax.Pattern (Syntax.Exp Symbol), [Symbol])
collectPattern pat =
  let
    -- | Collect a pattern, return the name to which the pattern is bound
    -- at the top level.
    collectNamedPattern :: (MonadMessages Message m,
                            MonadSymbols m, MonadIO m) =>
                           HashMap Symbol [Position] -> AST.Pattern ->
                           CollectT m (Maybe Symbol,
                                       Syntax.Pattern (Syntax.Exp Symbol),
                                       HashMap Symbol [Position])
    -- For an as-pattern, bind the field with that name to its own
    -- name, and further deconstruct the pattern.
    collectNamedPattern binds AST.As { AST.asName = sym, AST.asPat = inner,
                                       AST.asPos = pos } =
      do
        (collectedPat, newbinds) <- collectPattern' binds inner
        return (Just sym, Syntax.As { Syntax.asName = sym,
                                      Syntax.asPat = collectedPat,
                                      Syntax.asPos = pos },
                HashMap.insertWith (++) sym [pos] newbinds)
    -- For a name pattern, we bind the field with that name to its own name.
    collectNamedPattern binds AST.Name { AST.nameSym = sym,
                                         AST.namePos = pos } =
      return (Just sym, Syntax.Name { Syntax.nameSym = sym,
                                      Syntax.namePos = pos },
              HashMap.insertWith (++) sym [pos] binds)
    -- For a typed pattern, proceed to the inner pattern and take its name.
    collectNamedPattern binds AST.Typed { AST.typedPat = inner,
                                          AST.typedType = ty,
                                          AST.typedPos = pos } =
      do
        (msym, collectedPat, newbinds) <- collectNamedPattern binds inner
        collectedType <- collectExp ty
        return (msym, Syntax.Typed { Syntax.typedPat = collectedPat,
                                     Syntax.typedType = collectedType,
                                     Syntax.typedPos = pos }, newbinds)
    -- All other patterns can't be given names.
    collectNamedPattern binds pat' =
      do
        (collectedPat, newbinds) <- collectPattern' binds pat'
        return (Nothing, collectedPat, newbinds)

    -- | Collect an entry
    collectEntry :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                    (HashMap Symbol [Syntax.Entry (Syntax.Exp Symbol)],
                     HashMap Symbol [Position]) ->
                    AST.Entry ->
                    CollectT m (HashMap Symbol
                                        [Syntax.Entry (Syntax.Exp Symbol)],
                                HashMap Symbol [Position])
    -- For a named entry, use the name as the index and the pattern as
    -- the pattern.
    collectEntry (accum, binds) AST.Named { AST.namedSym = sym,
                                            AST.namedVal = inner,
                                            AST.namedPos = pos } =
      do
        (collectedPat, newbinds) <- collectPattern' binds inner
        return (HashMap.insertWith (++) sym [Syntax.Entry {
                                               Syntax.entryPat = collectedPat,
                                               Syntax.entryPos = pos
                                             }] accum,
                HashMap.insertWith (++) sym [pos] newbinds)
    -- For an unnamed entry, we try to extract the name from the pattern itself.
    collectEntry (accum, binds) AST.Unnamed { AST.unnamedPat = pat' } =
      let
        pos = position pat
      in do
        named <- collectNamedPattern binds pat'
        case named of
          (Just sym, collectedPat, newbinds) ->
            return (HashMap.insertWith (++) sym
                                       [Syntax.Entry {
                                          Syntax.entryPat = collectedPat,
                                          Syntax.entryPos = pos
                                        }] accum,
                    HashMap.insertWith (++) sym [pos] newbinds)
          (Nothing, _, newbinds) ->
            do
              namelessField (position pat)
              return (accum, newbinds)

    collectPattern' :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                       HashMap Symbol [Position] -> AST.Pattern ->
                       CollectT m (Syntax.Pattern (Syntax.Exp Symbol),
                                   HashMap Symbol [Position])
    -- For split, collect all fields into a HashMap.
    collectPattern' binds AST.Split { AST.splitFields = fields,
                                      AST.splitStrict = strict,
                                      AST.splitPos = pos } =
      let
        collapseField accum (_, []) =
          do
            internalError "Empty field list!" pos
            return accum
        collapseField accum (sym, [single]) =
          return $! HashMap.insert sym single accum
        collapseField accum (sym, (syntax @ (first : _))) =
          do
            duplicateField sym (map position syntax)
            return $! HashMap.insert sym first accum
      in do
        (collectedFields, newbinds) <-
          foldM collectEntry (HashMap.empty, binds) fields
        collapsedFields <- foldM collapseField HashMap.empty
                                 (HashMap.toList collectedFields)
        return (Syntax.Split { Syntax.splitStrict = strict,
                               Syntax.splitFields = collapsedFields,
                               Syntax.splitPos = pos }, newbinds)

    -- The rest of these are entirely straightforward
    collectPattern' binds AST.Option { AST.optionPats = pats,
                                       AST.optionPos = pos } =
      let
        checkMismatch :: (MonadMessages Message m, MonadSymbols m) =>
                         [HashMap Symbol [Position]] ->
                         m (HashMap Symbol [Position])
        checkMismatch bindlist =
          let
            commonbinds = foldr1 (HashMap.intersectionWith (++)) bindlist

            appfun :: (MonadMessages Message m, MonadSymbols m) =>
                      HashMap Symbol [Position] -> m ()
            appfun binds' =
              let
                extras = HashMap.toList (HashMap.difference binds' commonbinds)

                report (sym, poslist) = patternBindMismatch sym poslist
              in
                mapM_ report extras
          in do
            mapM_ appfun bindlist
            return commonbinds

        foldfun binds' (optlist, bindlist) pat' =
          do
            (collectedPat, newbinds) <- collectPattern' binds' pat'
            return (collectedPat : optlist, newbinds : bindlist)
      in do
        (collected, bindlist) <- foldM (foldfun binds) ([], []) pats
        newbinds <- checkMismatch bindlist
        return (Syntax.Option { Syntax.optionPats = collected,
                                Syntax.optionPos = pos }, newbinds)
    collectPattern' binds AST.Deconstruct { AST.deconstructName = sym,
                                            AST.deconstructPat = inner,
                                            AST.deconstructPos = pos } =
      do
        (collectedPat, newbinds) <- collectPattern' binds inner
        return (Syntax.Deconstruct { Syntax.deconstructName = sym,
                                     Syntax.deconstructPat = collectedPat,
                                     Syntax.deconstructPos = pos }, newbinds)
    collectPattern' binds AST.Typed { AST.typedPat = inner, AST.typedType = ty,
                                      AST.typedPos = pos } =
      do
        (collectedPat, newbinds) <- collectPattern' binds inner
        collectedType <- collectExp ty
        return (Syntax.Typed { Syntax.typedPat = collectedPat,
                               Syntax.typedType = collectedType,
                               Syntax.typedPos = pos }, newbinds)
    collectPattern' binds AST.As { AST.asName = sym, AST.asPat = inner,
                                   AST.asPos = pos } =
      do
        (collectedPat, newbinds) <- collectPattern' binds inner
        return (Syntax.As { Syntax.asName = sym, Syntax.asPat = collectedPat,
                            Syntax.asPos = pos },
                HashMap.insertWith (++) sym [pos] newbinds)
    collectPattern' binds AST.Name { AST.nameSym = sym, AST.namePos = pos } =
      return (Syntax.Name { Syntax.nameSym = sym, Syntax.namePos = pos },
              HashMap.insertWith (++) sym [pos] binds)
    collectPattern' binds AST.Exact { AST.exactLit = l } =
      return (Syntax.Exact { Syntax.exactLit = l }, binds)
  in do
    (collectedPat, binds) <- collectPattern' HashMap.empty pat
    return (collectedPat, HashMap.keys binds)

-- | Fold function to collect compound expression components.
collectCompound :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                   [Syntax.Compound (Syntax.Exp Symbol)] -> AST.Compound ->
                   CollectT m [Syntax.Compound (Syntax.Exp Symbol)]
-- For the rest, collect the tree and add it to the statement list.
collectCompound accum
                AST.Element { AST.elemVal = AST.Def { AST.defPattern = pat,
                                                      AST.defInit = init,
                                                      AST.defPos = pos } } =
  do
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    (collectedPat, binds) <- collectPattern pat
    defid <- addDef Syntax.Def { Syntax.defPattern = collectedPat,
                                 Syntax.defInit = collectedInit,
                                 Syntax.defPos = pos }
    addNames Public binds defid pos
    return $! Syntax.Init { Syntax.initId = defid } : accum
collectCompound accum AST.Element {
                        AST.elemVal = elem @ AST.Builder {
                                               AST.builderName = sym
                                             }
                      } =
  do
    collectElement Public elem
    return $! Syntax.Decl { Syntax.declSym = sym } : accum
collectCompound accum AST.Element {
                        AST.elemVal = elem @ AST.Fun { AST.funName = sym }
                      } =
  do
    collectElement Public elem
    return $! Syntax.Decl { Syntax.declSym = sym } : accum
-- All other elements just get added to the scope
collectCompound accum AST.Element { AST.elemVal = elem } =
  do
    collectElement Public elem
    return accum
-- Expressions get added to the list
collectCompound accum AST.Exp { AST.expVal = exp } =
  do
    collectedExp <- collectExp exp
    return $! Syntax.Exp { Syntax.expVal = collectedExp } : accum

-- | Collect an expression.
collectExp :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
              AST.Exp -> CollectT m (Syntax.Exp Symbol)
-- For a compound statement, construct a sub-scope
collectExp AST.Compound { AST.compoundBody = body, AST.compoundPos = pos } =
  do
    beginScope
    collectedBody <- foldM collectCompound [] body
    scopeid <- finishScope
    return Syntax.Compound { Syntax.compoundScope = scopeid,
                             Syntax.compoundBody = reverse collectedBody,
                             Syntax.compoundPos = pos }
-- For record values, gather up all the bindings into a HashMap.
collectExp AST.Record { AST.recordType = False, AST.recordFields = fields,
                        AST.recordPos = pos } =
  let
    collapseField :: (MonadMessages Message m, MonadSymbols m) =>
                     (HashMap FieldName (Syntax.Exp Symbol)) ->
                     (FieldName, [Syntax.Exp Symbol]) ->
                     CollectT m (HashMap FieldName (Syntax.Exp Symbol))
    collapseField accum (_, []) =
      do
        internalError (Strict.fromString "Empty field list") pos
        return accum
    collapseField accum (fname, [single]) =
      return $! HashMap.insert fname single accum
    collapseField accum (fname @ FieldName { fieldSym = sym },
                         fields' @ (first : _)) =
      do
        duplicateField sym (map position fields')
        return $! HashMap.insert fname first accum

    -- Fold function.  Only build a HashMap.
    collectValueField :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                         HashMap FieldName [Syntax.Exp Symbol] -> AST.Field ->
                         CollectT m (HashMap FieldName [Syntax.Exp Symbol])
    collectValueField accum AST.Field { AST.fieldName = fname,
                                        AST.fieldVal = val } =
      do
        collectedVal <- collectExp val
        return $! HashMap.insertWith (++) fname [collectedVal] accum
  in do
    collectedFields <- foldM collectValueField HashMap.empty fields
    collapsedFields <- foldM collapseField HashMap.empty
                             (HashMap.toList collectedFields)
    return Syntax.Record { Syntax.recordFields = collapsedFields,
                           Syntax.recordPos = pos }
-- For record types, gather up all the names into a Fields structure.
collectExp AST.Record { AST.recordType = True, AST.recordFields = fields,
                        AST.recordPos = pos } =
  do
    collectedFields <- collectFields fields pos
    return Syntax.RecordType { Syntax.recordTypeFields = collectedFields,
                               Syntax.recordTypePos = pos }
-- For 'Seq's, walk down the right spine and gather up all 'Seq's into a list.
collectExp AST.Seq { AST.seqExps = exps, AST.seqPos = pos } =
  do
    collectedExps <- mapM collectExp exps
    return Syntax.Seq { Syntax.seqExps = collectedExps, Syntax.seqPos = pos }
-- The remainder of these are straightforward
collectExp AST.Abs { AST.absKind = kind, AST.absCases = cases,
                     AST.absPos = pos } =
  do
    collectedCases <- mapM collectCase cases
    return Syntax.Abs { Syntax.absKind = kind,
                        Syntax.absCases = collectedCases,
                        Syntax.absPos = pos }
collectExp AST.Match { AST.matchVal = val, AST.matchCases = cases,
                       AST.matchPos = pos } =
  do
    collectedVal <- collectExp val
    collectedCases <- mapM collectCase cases
    return Syntax.Match { Syntax.matchVal = collectedVal,
                          Syntax.matchCases = collectedCases,
                          Syntax.matchPos = pos }
collectExp AST.Ascribe { AST.ascribeVal = val, AST.ascribeType = ty,
                         AST.ascribePos = pos } =
  do
    collectedVal <- collectExp val
    collectedType <- collectExp ty
    return Syntax.Ascribe { Syntax.ascribeVal = collectedVal,
                            Syntax.ascribeType = collectedType,
                            Syntax.ascribePos = pos }
collectExp AST.Tuple { AST.tupleFields = fields, AST.tuplePos = pos } =
  do
    collectedFields <- mapM collectExp fields
    return Syntax.Tuple { Syntax.tupleFields = collectedFields,
                          Syntax.tuplePos = pos }
collectExp AST.Project { AST.projectVal = val, AST.projectFields = fields,
                         AST.projectPos = pos } =
  do
    collectedVal <- collectExp val
    return Syntax.Project { Syntax.projectVal = collectedVal,
                            Syntax.projectFields = fields,
                            Syntax.projectPos = pos }
collectExp AST.Sym { AST.symName = sym, AST.symPos = pos } =
  return Syntax.Sym { Syntax.symRef = sym, Syntax.symPos = pos }
collectExp AST.With { AST.withVal = val, AST.withArgs = args,
                      AST.withPos = pos } =
  do
    collectedVal <- collectExp val
    collectedArgs <- collectExp args
    return Syntax.With { Syntax.withVal = collectedVal,
                         Syntax.withArgs = collectedArgs,
                         Syntax.withPos = pos }
collectExp AST.Where { AST.whereVal = val, AST.whereProp = prop,
                       AST.wherePos = pos } =
  do
    collectedVal <- collectExp val
    collectedProp <- collectExp prop
    return Syntax.Where { Syntax.whereVal = collectedVal,
                          Syntax.whereProp = collectedProp,
                          Syntax.wherePos = pos }
collectExp AST.Anon { AST.anonKind = kind, AST.anonSuperTypes = supers,
                      AST.anonParams = params, AST.anonContent = content,
                      AST.anonPos = pos } =
  do
    collectedSupers <- mapM collectExp supers
    collectedParams <- collectFields params pos
    collectedContent <- collectScope content
    return Syntax.Anon { Syntax.anonKind = kind,
                         Syntax.anonParams = collectedParams,
                         Syntax.anonSuperTypes = collectedSupers,
                         Syntax.anonContent = collectedContent,
                         Syntax.anonPos = pos }
collectExp (AST.Literal lit) = return (Syntax.Literal lit)

-- | Collect a list of AST fields into a Syntax fields structure (a HashMap
-- and an ordering), and report duplicates.
collectFields :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                 [AST.Field] -> Position ->
                 CollectT m (Syntax.Fields (Syntax.Exp Symbol))
collectFields fields fieldspos =
  let
    collapseField :: (MonadMessages Message m, MonadSymbols m) =>
                     (HashMap FieldName (Syntax.Field (Syntax.Exp Symbol))) ->
                     (FieldName, [Syntax.Field (Syntax.Exp Symbol)]) ->
                     CollectT m (HashMap FieldName
                                         (Syntax.Field (Syntax.Exp Symbol)))
    collapseField accum (_, []) =
      do
        internalError (Strict.fromString "Empty field list") fieldspos
        return accum
    collapseField accum (fname, [single]) =
      return $! HashMap.insert fname single accum
    collapseField accum (fname @ FieldName { fieldSym = sym },
                         fields' @ (first : _)) =
      do
        duplicateField sym (map position fields')
        return $! HashMap.insert fname first accum

    -- | Fold function.  Builds both a list and a HashMap.
    collectField :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                    (HashMap FieldName [Syntax.Field (Syntax.Exp Symbol)],
                     [FieldName]) ->
                    AST.Field ->
                    CollectT m (HashMap FieldName
                                        [Syntax.Field (Syntax.Exp Symbol)],
                                [FieldName])
    collectField (tab, fields') AST.Field { AST.fieldName = fname,
                                            AST.fieldVal = val,
                                            AST.fieldPos = pos } =
      do
        collectedVal <- collectExp val
        return (HashMap.insertWith (++) fname [Syntax.Field {
                                                 Syntax.fieldVal = collectedVal,
                                                 Syntax.fieldPos = pos
                                               }] tab,
                fname : fields')
  in do
    -- Get a HashMap as well as a list of field names.
    (tab, fieldlist) <- foldM collectField (HashMap.empty, []) fields
    collapsed <- foldM collapseField HashMap.empty (HashMap.toList tab)
    return Syntax.Fields {
             -- The list of field names becomes an array.
             Syntax.fieldsOrder = listArray (1, fromIntegral (length fieldlist))
                                            fieldlist,
             -- The HashMap becomes the bindings.
             Syntax.fieldsBindings = collapsed
           }

-- | Collect a case.  This is straightforward.
collectCase :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
               AST.Case -> CollectT m (Syntax.Case (Syntax.Exp Symbol))
collectCase AST.Case { AST.casePat = pat, AST.caseBody = body,
                       AST.casePos = pos } =
  do
    (collectedPat, _) <- collectPattern pat
    collectedBody <- collectExp body
    return Syntax.Case { Syntax.casePat = collectedPat,
                         Syntax.caseBody = collectedBody,
                         Syntax.casePos = pos }


-- | Collect a definition.
collectElement :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  Visibility
               -- ^ The visibility at which everything is defined.
               -> AST.Element
               -- ^ The element to collect
               -> CollectT m ()
-- For builder definitions, turn the definition into a 'Def', whose
-- initializer is an anonymous builder.
collectElement vis AST.Builder { AST.builderName = sym,
                                 AST.builderKind = kind,
                                 AST.builderParams = params,
                                 AST.builderSuperTypes = supers,
                                 AST.builderContent = content,
                                 AST.builderPos = pos } =
  do
    collectedSupers <- mapM collectExp supers
    collectedParams <- collectFields params pos
    case content of
        AST.Body body ->
          do
            collectedBody <- collectScope body
            addBuilder sym Syntax.Builder {
                             Syntax.builderKind = kind,
                             Syntax.builderVisibility = vis,
                             Syntax.builderParams = collectedParams,
                             Syntax.builderSuperTypes = collectedSupers,
                             Syntax.builderContent =
                               Syntax.Anon {
                                 Syntax.anonKind = kind,
                                 Syntax.anonParams = collectedParams,
                                 Syntax.anonSuperTypes = collectedSupers,
                                 Syntax.anonContent = collectedBody,
                                 Syntax.anonPos = pos },
                             Syntax.builderPos = pos
                           }
        AST.Value value ->
          do
            collectedValue <- collectExp value
            addBuilder sym Syntax.Builder {
                             Syntax.builderKind = kind,
                             Syntax.builderVisibility = vis,
                             Syntax.builderParams = collectedParams,
                             Syntax.builderSuperTypes = collectedSupers,
                             Syntax.builderContent = collectedValue,
                             Syntax.builderPos = pos
                           }
-- For Defs and Imports, collect the components and add them to the list of
-- elements.
collectElement vis AST.Def { AST.defPattern = pat, AST.defInit = init,
                             AST.defPos = pos } =
  do
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    (collectedPat, binds) <- collectPattern pat
    defid <- addDef Syntax.Def { Syntax.defPattern = collectedPat,
                                 Syntax.defInit = collectedInit,
                                 Syntax.defPos = pos }
    addNames vis binds defid pos
collectElement vis AST.Import { AST.importExp = exp,
                                AST.importPos = pos } =
  do
    collectedExp <- collectExp exp
    addImport Syntax.Import { Syntax.importExp = collectedExp,
                              Syntax.importVisibility = vis,
                              Syntax.importPos = pos }
-- For Funs, we do the same kind of transformation we did with
-- Builders: create a 'Def' whose initializer is an anonymous function.
collectElement _ AST.Fun { AST.funName = sym, AST.funCases = cases,
                             AST.funPos = pos } =
  do
    collectedCases <- mapM collectCase cases
    _ <- addDef Syntax.Def {
                  Syntax.defPattern = Syntax.Name { Syntax.nameSym = sym,
                                                    Syntax.namePos = pos },
                  Syntax.defInit = Just Syntax.Abs { Syntax.absKind = Lambda,
                                                     Syntax.absCases =
                                                       collectedCases,
                                                     Syntax.absPos = pos },
                  Syntax.defPos = pos
                }
    return ()
-- Truth definitions are a straghtforward translation.
collectElement vis AST.Truth { AST.truthName = sym,
                               AST.truthKind = kind,
                               AST.truthProof = Nothing,
                               AST.truthContent = content,
                               AST.truthPos = pos } =
  do
    collectedContent <- collectExp content
    addTruth sym Syntax.Truth { Syntax.truthKind = kind,
                                Syntax.truthVisibility = vis,
                                Syntax.truthContent = collectedContent,
                                Syntax.truthProof = Nothing,
                                Syntax.truthPos = pos }
collectElement vis AST.Truth { AST.truthName = sym,
                               AST.truthKind = kind,
                               AST.truthProof = Just proof,
                               AST.truthContent = content,
                               AST.truthPos = pos } =
  do
    collectedContent <- collectExp content
    collectedProof <- collectExp proof
    addTruth sym Syntax.Truth { Syntax.truthKind = kind,
                                Syntax.truthVisibility = vis,
                                Syntax.truthContent = collectedContent,
                                Syntax.truthProof = Just collectedProof,
                                Syntax.truthPos = pos }
collectElement _ AST.Syntax { AST.syntaxSym = sym, AST.syntaxFixity = fixity,
                              AST.syntaxPrecs = precs, AST.syntaxPos = pos } =
  let
    collectPrecs (ordering, exp) =
      do
        collectedExp <- collectExp exp
        return (ordering, collectedExp)
  in do
    collectedPrecs <- mapM collectPrecs precs
    addSyntax sym Syntax.Syntax { Syntax.syntaxFixity = fixity,
                                  Syntax.syntaxPrecs = collectedPrecs,
                                  Syntax.syntaxPos = pos }
-- Proofs and imports get added to directives, and are processed in a
-- later phase.
collectElement _ AST.Proof { AST.proofName = pname, AST.proofBody = body,
                             AST.proofPos = pos } =
  do
    collectedName <- collectExp pname
    collectedBody <- collectExp body
    addProof Syntax.Proof { Syntax.proofName = collectedName,
                            Syntax.proofBody = collectedBody,
                            Syntax.proofPos = pos }

-- | Collect a scope.  This gathers up all the defintions and truths into
-- HashMaps, and saves the imports and proofs.  Syntax directives then get
-- parsed and used to update definitions.
collectScope :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                AST.Scope -> CollectT m ScopeID
collectScope groups =
  let
    -- | Collect a group.  This rolls all elements of the group into the
    -- temporary scope, with the group's visibility.
    collectGroup :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                    AST.Group
                 -- ^ The group
                 -> CollectT m ()
    collectGroup AST.Group { AST.groupVisibility = vis,
                             AST.groupElements = elems } =
      mapM_ (collectElement vis) elems
  in do
    beginScope
    mapM_ collectGroup groups
    finishScope

-- | The Collect phase of the compiler.  Collect consumes an AST and
-- produces a Surface structure.
--
-- The primary responsibility of this phase is to construct scopes,
-- which contain all definitions in an indexed map, and further
-- grouped by visibility.  At the end of collection, all scopes are
-- represented as tables, containing the definitions they _directly_
-- define.  Derived scopes are _not_ constructed by this phase.
--
-- Collect also does some minor restructuring of the tree, including
-- the following:
-- * 'Seq's are transformed into a single constructor which contains a
--   list of 'Exp's
-- * Record patterns and literals have their fields stored in a 'HashMap'.
-- * Syntax directives are parsed and stored as data in the scope.
-- * Scopes are split into definitions, truths, and directives (proofs
--   and imports).
--
-- Collect can issue several messages:
-- * Duplicate record fields.
-- * Uninitialized definition without a top-level name (warning).
-- * Multiple truth definitions with the same names.
-- * Malformed syntax directives.
-- * Undefined symbols in syntax directives.
collectAST :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
              Position
           -- ^ Position corresponding to this entire file.
           -> Maybe [Symbol]
           -- ^ The name of a definition that is expected to appear at the
           -- top level.
           -> AST.AST
           -- ^ The 'AST' to collect.
           -> CollectT m ()
collectAST filepos expected AST.AST { AST.astComponent = component,
                                      AST.astScope = scope } =
  let
    -- Grab the component name out of a Maybe Component
    actual =
      do
        val <- component
        return $! AST.componentName val
  in case (expected, actual) of
    (Just expected', _) ->
      -- We have an expected component name.  Get the expected
      -- definition from that, and assert that the actual component
      -- name is the same.
      let
        defname = last expected'
      in do
        unless (expected == actual) (badComponentName expected' actual filepos)
        beginScope
        mapM_ (collectElement Public) scope
        scopeid <- finishScope
        addComponent expected' Syntax.Component {
                                 Syntax.compScope = scopeid,
                                 Syntax.compExpected = Just defname
                               }
    (Nothing, Just actual') ->
      -- We have no expected component name, but do have an actual
      -- one.  Get the expected definition name from the actual one.
      let
        defname = last actual'
      in do
        beginScope
        mapM_ (collectElement Public) scope
        scopeid <- finishScope
        addComponent actual' Syntax.Component {
                               Syntax.compScope = scopeid,
                               Syntax.compExpected = Just defname
                             }
    (Nothing, Nothing) ->
      -- We don't have anything.  There is no expected definition
      -- name, and everything at the top level is public.
      do
        beginScope
        mapM_ (collectElement Public) scope
        _ <- finishScope
        return ()

-- | Get the file name for a component.
componentFileName :: MonadSymbols m =>
                     [Symbol]
                  -- ^ The component name to convert.
                  -> m Strict.ByteString
                  -- ^ The file name for this component.
componentFileName cname =
  let
    pathSepBStr = Strict.fromString [pathSeparator]
    saltExt = Strict.fromString $! extSeparator : "salt"
  in do
    bstrs <- mapM name cname
    return $! Strict.concat [Strict.intercalate pathSepBStr bstrs, saltExt]

-- | Load a file containing a component.
loadComponent :: (MonadIO m, MonadLoader Strict.ByteString Lazy.ByteString m,
                  MonadMessages Message m, MonadGenpos m, MonadSymbols m) =>
                 [Symbol]
              -- ^ The name of the component to load.
              -> Position
              -- ^ The position at which the reference to this
              -- component occurred.
              -> m (Maybe (Filename, Lazy.ByteString))
loadComponent cname pos =
  do
    fstr <- componentFileName cname
    -- XXX Replace this with some sort of progress messages framework
    liftIO (Strict.putStr (Strict.concat ["Loaded ", fstr, "\n"]))
    -- Call the loader to get the file contents.
    loaded <- load fstr
    case loaded of
    -- If an error occurs while loading, report it.
      Left err ->
        let
          errstr = Strict.fromString $! ioeGetErrorString err
        in do
          if isDoesNotExistError err
            then cannotFindComponent cname pos
            else cannotAccessComponent cname fstr errstr pos
          return Nothing
      Right (fname, content) -> return (Just (fname, content))

collectComponent :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                     MonadMessages Message m, MonadGenpos m,
                     MonadSymbols m, MonadIO m) =>
                    (Filename -> Lazy.ByteString -> m (Maybe AST.AST))
                 -- ^ The parsing function to use.
                 -> Position
                 -- ^ The position at which the reference to this
                 -- component occurred.
                 -> [Symbol]
                 -- ^ The name of the component to collect.
                 -> CollectT m ()
collectComponent parseFunc pos cname =
  let
    addEmpty =
      do
        tab <- ask
        liftIO $! HashTable.insert tab cname Nothing

    collectUse AST.Use { AST.useName = uname, AST.usePos = upos } =
      collectComponent parseFunc upos uname

    loadAndCollect =
      do
        -- Call the loader to get the file contents.
        loaded <- loadComponent cname pos
        case loaded of
          Nothing -> addEmpty
          Just (fname, content) ->
            -- Otherwise, continue
            do
              res <- lift $! lift $! parseFunc fname content
              case res of
                Just ast @ AST.AST { AST.astUses = uses } ->
                  let
                    fpos = File { fileName = fname }
                  in do
                    collectAST fpos (Just cname) ast
                    mapM_ collectUse uses
                Nothing -> addEmpty
  in do
    done <- componentExists cname
    unless done loadAndCollect
    return ()

collectEndPos :: Position
collectEndPos =
  Synthetic { synthDesc = Strict.fromString "End of collect phase" }

collectComponents :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                      MonadMessages Message m, MonadGenpos m,
                      MonadSymbols m, MonadIO m) =>
                     (Filename -> Lazy.ByteString -> m (Maybe AST.AST))
                  -- ^ The parsing function to use.
                  -> [(Position, [Symbol])]
                  -- ^ The names of the components to collect.
                  -> m (Syntax.Surface (Syntax.Exp Symbol))
collectComponents parsefunc files =
  let
    collectOne (pos, fname) = collectComponent parsefunc pos fname

    foldfun comps (_, Nothing) = return comps
    foldfun comps (_, Just comp) = return $! comp : comps
  in do
    tab <- liftIO HashTable.new
    ((), CollectState { collectNextScope = scopeid,
                        collectFinishedScopes = scopes,
                        collectScopes = stack }) <-
      runReaderT (runStateT (mapM_ collectOne files) emptyState) tab
    -- Check that the scope stack is empty
    case stack of
      [] -> return ()
      _ -> internalError "Non-empty scope stack at end of collect" collectEndPos
    comps <- liftIO $! HashTable.foldM foldfun [] tab
    return $! Syntax.Surface {
                Syntax.surfScopes = array (toEnum 0, pred scopeid) scopes,
                Syntax.surfComponents = comps
              }

-- | Load a file containing a component
loadFile :: (MonadIO m, MonadLoader Strict.ByteString Lazy.ByteString m,
             MonadMessages Message m, MonadGenpos m) =>
            Strict.ByteString
         -- ^ The name of the file.
         -> Position
         -- ^ The position at which the reference to this
         -- file occurred.
         -> m (Maybe (Filename, Lazy.ByteString))
loadFile fstr pos =
  do
    -- Call the loader to get the file contents.
    loaded <- load fstr
    -- XXX Replace this with some sort of progress messages framework
    liftIO (Strict.putStr (Strict.concat ["Loaded ", fstr, "\n"]))
    case loaded of
    -- If an error occurs while loading, report it.
      Left err ->
        let
          errstr = Strict.fromString $! ioeGetErrorString err
        in do
          if isDoesNotExistError err
            then cannotFindFile fstr pos
            else cannotAccessFile fstr errstr pos
          return Nothing
      Right (fname, content) -> return (Just (fname, content))

collectFile :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                  MonadMessages Message m, MonadGenpos m,
                  MonadSymbols m, MonadIO m) =>
                 (Filename -> Lazy.ByteString -> m (Maybe AST.AST))
              -- ^ The parsing function to use.
              -> Position
              -- ^ The position at which the reference to this
              -- component occurred.
              -> Strict.ByteString
              -- ^ The name of the file to collect.
              -> CollectT m ()
collectFile parseFunc pos fstr =
  let
    collectUse AST.Use { AST.useName = uname, AST.usePos = upos } =
      collectComponent parseFunc upos uname
  in do
    -- Call the loader to get the file contents.
    loaded <- loadFile fstr pos
    case loaded of
      Nothing -> return ()
      Just (fname, content) ->
        -- Otherwise, continue
        do
          res <- lift $! lift $! parseFunc fname content
          case res of
            Just ast @ AST.AST { AST.astUses = uses } ->
              let
                fpos = File { fileName = fname }
              in do
                collectAST fpos Nothing ast
                mapM_ collectUse uses
            Nothing -> return ()

collectFiles :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                 MonadMessages Message m, MonadGenpos m,
                 MonadSymbols m, MonadIO m) =>
                (Filename -> Lazy.ByteString -> m (Maybe AST.AST))
             -- ^ The parsing function to use.
             -> [(Position, Strict.ByteString)]
             -- ^ The names of the files to collect.
             -> m (Syntax.Surface (Syntax.Exp Symbol))
collectFiles parsefunc files =
  let
    collectOne (pos, fname) = collectFile parsefunc pos fname

    foldfun comps (_, Nothing) = return comps
    foldfun comps (_, Just comp) = return $! comp : comps
  in do
    tab <- liftIO HashTable.new
    ((), CollectState { collectNextScope = scopeid,
                        collectFinishedScopes = scopes,
                        collectScopes = stack }) <-
      runReaderT (runStateT (mapM_ collectOne files) emptyState) tab
    -- Check that the scope stack is empty
    case stack of
      [] -> return ()
      _ -> internalError "Non-empty scope stack at end of collect" collectEndPos
    comps <- liftIO $! HashTable.foldM foldfun [] tab
    return $! Syntax.Surface {
                Syntax.surfScopes = array (toEnum 0, pred scopeid) scopes,
                Syntax.surfComponents = comps
              }
