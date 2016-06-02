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
       collectComponent,
       collectFile,
       componentFileName,
       loadComponent,
       loadFile
       ) where

import Control.Monad
import Control.Monad.Genpos
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Array hiding (accum, elems)
import Data.Array.IO(IOArray)
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
    tempScopeBuilders :: !(HashMap Symbol [Syntax.Builder expty]),
    tempScopeTruths :: !(HashMap Symbol [Syntax.Truth expty]),
    tempScopeSyntax :: !(HashMap Symbol [Syntax.Syntax expty]),
    tempScopeProofs :: ![Syntax.Proof expty],
    tempScopeImports :: ![Syntax.Import expty],
    tempScopeElems :: !(IOArray Visibility [expty])
  }

data CollectState expty =
  CollectState {
    collectNextScope :: !ScopeID,
    collectScopes :: ![TempScope expty],
    collectFinishedScopes :: ![(ScopeID, Syntax.Scope expty)]
  }

type CollectT m = StateT CollectState (ReaderT Table m)

scopeStackUnderflow :: Strict.ByteString
scopeStackUnderflow = Strict.toString "Scope stack underflow"

-- | Begin a new scope.
beginScope :: Monad m => CollectT m ScopeID
beginScope =
  do
    elemsarr <- IOArray.newArray (Hidden, Public) []
    state @ CollectState { collectScopes = scopes } <- get
    put state { collectScopes = TempScope { tempScopeBuilders = HashMap.empty,
                                            tempScopeTruths = HashMap.empty,
                                            tempScopeProofs = [],
                                            tempScopeImports = [],
                                            tempScopeSyntax = [],
                                            tempScopeElems = elemsarr } :
                                scopes }

-- | Check whether a component exists
componentExists :: Monad m => [Symbol] -> CollectT m Bool
componentExists path =
  do
    tab <- ask
    res <- HashTable.lookup tab path
    return $! isJust res

-- | Add a component to the register
addComponent :: Monad m => [Symbol] -> Syntax.Component -> CollectT m ()
addComponent path component =
  do
    tab <- ask
    HashTable.insert tab $! Just component

-- | Add a Builder definition to the current scope.
addBuilder :: Monad m => Symbol -> Syntax.Builder expty -> CollectT m ()
addBuilder sym builder @ Syntax.Builder { Syntax.builderPos = pos } =
  do
    state @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeBuilders = builders } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newbuilders = HashMap.insertWith (++) sym [builder] builders
          newscope = first { tempScopeBuilders = newbuilders }
          newstate = state { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a Truth definition to the current scope.
addTruth :: Monad m => Symbol -> Syntax.Truth expty -> CollectT m ()
addTruth sym truth @ Syntax.Truth { Syntax.truthPos = pos } =
  do
    state @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeTruths = truths } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newtruths = HashMap.insertWith (++) sym [truth] truths
          newscope = first { tempScopeTruths = newtruths }
          newstate = state { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a Proof definition to the current scope.
addProof :: Monad m => Syntax.Proof expty -> CollectT m ()
addProof sym proof @ Syntax.Proof { Syntax.proofPos = pos } =
  do
    state @ CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of proofs
      first @ TempScope { tempScopeProofs = proofs } : rest ->
        put state { collectScopes = first { tempScopeProofs = proof } : rest }
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a syntax definition to the current scope.
addSyntax :: Monad m => Symbol -> Syntax.Syntax expty -> m ()
addSyntax sym syntax @ Syntax.Syntax { Syntax.syntaxPos = pos } =
  do
    state @ CollectState { collectScopes = scopes } <- get
    case scopes of
      first @ TempScope { tempScopeSyntax = syntaxes } : rest ->
        let
          -- Concatenate the new entry with the existing ones
          newsyntax = HashMap.insertWith (++) sym [syntax] syntaxes
          newscope = first { tempScopeTruths = newsyntax }
          newstate = state { collectScopes = newscope : rest }
        in
          put newstate
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Add a definition into a scope, merging it in with what's already there.
addDef :: Monad m => Visibility -> Syntax.Def expty -> CollectT m ()
addDef vis elem @ Syntax.Def { Syntax.defPos = pos } =
  do
    state @ CollectState { collectScopes = scopes } <- get
    case scopes of
      -- Append to the list of defs for this visibility
      first @ TempScope { tempScopeElems = elems } : rest ->
        do
          curr <- IOArray.readArray elems vis
          IOArray.writeArray elems vis (elem : curr)
      -- A stack underflow is an internal error
      _ -> internalError scopeStackUnderflow pos

-- | Finish the current scope and return its ID
finishScope :: MonadMessages Message m =>
               CollectT m ScopeID
finishScope =
  let
    collapseBuilders _ [] = error "Empty builder list!"
    collapseBuilders _ [single] = return single
    collapseBuilders sym (builders @ first : _) =
      do
        duplicateBuilder sym (map position builders)
        return first

    collapseTruths _ [] = error "Empty truth list!"
    collapseTruths _ [single] = return single
    collapseTruths sym (truths @ first : _) =
      do
        duplicateTruth sym (map position truths)
        return first

    collapseSyntax _ [] = error "Empty syntax list!"
    collapseSyntax _ [single] = return single
    collapseSyntax sym (syntax @ first : _) =
      do
        duplicateSyntax sym (map position syntax)
        return first
  in do
    CollectState { collectNextScope = scopeid, collectScopes = scopes,
                   collectFinishedScopes = finished } <- get
    case scopes of
      [] -> error "Scope stack underflow!"
      TempScope { tempScopeBuilders = builders, tempScopeTruths = truths,
                  tempScopeProofs = proofs, tempScopeSyntax = syntax,
                  tempScopeImports = imports, tempScopeElems = tempelems } :
        rest ->
        do
          elemsarr <- Unsafe.unsafeFreeze tempelems
          collapsedbuilders <- HashMap.mapWithKey collapseBuilders builders
          collapsedtruths <- HashMap.mapWithKey collapseTruths truths
          -- Update the state
          put CollectState {
                collectFinishedScopes =
                   (scopeid,
                    Syntax.Scope { Syntax.scopeBuilders = collapsedbuilders,
                                   Syntax.scopeTruths = collapsedtruths,
                                   Syntax.scopeProofs = proofs,
                                   Syntax.scopeSyntax = syntax,
                                   Syntax.scopeImports = imports,
                                   Syntax.scopeElems = elemsarr }) : finished,
                collectScopes = rest, collectNextScope = succ scopeid
              }
          return scopeid

-- | Collect a pattern, return the name to which the pattern is bound
-- at the top level.
collectNamedPattern :: (MonadMessages Message m, MonadSymbols m) =>
                       AST.Pattern ->
                       CollectT m (Maybe Symbol,
                                   Syntax.Pattern (Syntax.Exp Symbol))
-- For an as-pattern, bind the field with that name to its own
-- name, and further deconstruct the pattern.
collectNamedPattern AST.As { AST.asName = sym, AST.asPat = pat,
                             AST.asPos = pos } =
  do
    collectedPat <- collectPattern pat
    return (Just sym, Syntax.As { Syntax.asName = sym,
                                  Syntax.asPat = collectedPat,
                                  Syntax.asPos = pos })
-- For a name pattern, we bind the field with that name to its own name.
collectNamedPattern AST.Name { AST.nameSym = sym, AST.namePos = pos } =
  return (Just sym, Syntax.Name { Syntax.nameSym = sym,
                                  Syntax.namePos = pos })
-- For a typed pattern, proceed to the inner pattern and take its name.
collectNamedPattern AST.Typed { AST.typedPat = pat, AST.typedType = ty,
                                AST.typedPos = pos } =
  do
    (msym, collectedPat) <- collectNamedPattern pat
    collectedType <- collectExp ty
    return (msym, Syntax.Typed { Syntax.typedPat = collectedPat,
                                 Syntax.typedType = collectedType,
                                 Syntax.typedPos = pos })
-- All other patterns can't be given names.
collectNamedPattern pat =
  do
    collectedPat <- collectPattern pat
    return (Nothing, collectedPat)

-- | Collect an entry
collectEntry :: (MonadMessages Message m, MonadSymbols m) =>
                HashMap Symbol (Syntax.Entry (Syntax.Exp Symbol)) ->
                AST.Entry ->
                CollectT m (HashMap Symbol (Syntax.Entry (Syntax.Exp Symbol)))
-- For a named entry, use the name as the index and the pattern as
-- the pattern.
collectEntry accum AST.Named { AST.namedSym = sym, AST.namedVal = pat,
                               AST.namedPos = pos } =
  do
    collectedPat <- collectPattern pat
    if HashMap.member sym accum
      then do
        duplicateField sym pos
        return accum
      else return $! HashMap.insert sym Syntax.Entry {
                                          Syntax.entryPat = collectedPat,
                                          Syntax.entryPos = pos
                                        } accum
-- For an unnamed entry, we try to extract the name from the pattern itself.
collectEntry accum AST.Unnamed { AST.unnamedPat = pat } =
  let
    pos = position pat
  in do
    named <- collectNamedPattern pat
    case named of
      (Just sym, collectedPat)
        | HashMap.member sym accum ->
          do
            duplicateField sym pos
            return accum
        | otherwise ->
            return $! HashMap.insert sym Syntax.Entry {
                                           Syntax.entryPat = collectedPat,
                                           Syntax.entryPos = pos
                                         } accum
      (Nothing, _) ->
        do
          namelessField (position pat)
          return accum

collectPattern :: (MonadMessages Message m, MonadSymbols m) =>
                  AST.Pattern -> CollectT m (Syntax.Pattern (Syntax.Exp Symbol))
-- For split, collect all fields into a HashMap.
collectPattern AST.Split { AST.splitFields = fields, AST.splitStrict = strict,
                           AST.splitPos = pos } =
  do
    collectedFields <- foldM collectEntry HashMap.empty fields
    return Syntax.Split { Syntax.splitStrict = strict,
                          Syntax.splitFields = collectedFields,
                          Syntax.splitPos = pos }
-- The rest of these are entirely straightforward
collectPattern AST.Option { AST.optionPats = pats, AST.optionPos = pos } =
  do
    collectedPats <- mapM collectPattern pats
    return Syntax.Option { Syntax.optionPats = collectedPats,
                           Syntax.optionPos = pos }
collectPattern AST.Deconstruct { AST.deconstructName = sym,
                                 AST.deconstructPat = pat,
                                 AST.deconstructPos = pos } =
  do
    collectedPat <- collectPattern pat
    return Syntax.Deconstruct { Syntax.deconstructName = sym,
                                Syntax.deconstructPat = collectedPat,
                                Syntax.deconstructPos = pos }
collectPattern AST.Typed { AST.typedPat = pat, AST.typedType = ty,
                           AST.typedPos = pos } =
  do
    collectedPat <- collectPattern pat
    collectedType <- collectExp ty
    return Syntax.Typed { Syntax.typedPat = collectedPat,
                          Syntax.typedType = collectedType,
                          Syntax.typedPos = pos }
collectPattern AST.As { AST.asName = sym, AST.asPat = pat, AST.asPos = pos } =
  do
    collectedPat <- collectPattern pat
    return Syntax.As { Syntax.asName = sym, Syntax.asPat = collectedPat,
                       Syntax.asPos = pos }
collectPattern AST.Name { AST.nameSym = sym, AST.namePos = pos } =
  return Syntax.Name { Syntax.nameSym = sym, Syntax.namePos = pos }
collectPattern AST.Exact { AST.exactLit = l } =
  return Syntax.Exact { Syntax.exactLit = l }

-- | Fold function to collect compound expression components.
collectCompound :: (MonadMessages Message m, MonadSymbols m) =>
                   [Syntax.Compound (Syntax.Exp Symbol)] -> AST.Compound ->
                   CollectT m [Syntax.Compound (Syntax.Exp Symbol)]
-- For the rest, collect the tree and add it to the statement list.
collectCompound scope @ TempDynamicScope { tdCompounds = compounds }
                AST.Element { AST.elemVal = AST.Def { AST.defPattern = pat,
                                                      AST.defInit = init,
                                                      AST.defPos = pos } } =
  do
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    collectedPat <- collectPattern pat
    return scope {
             tdCompounds = Syntax.Element {
                 Syntax.elemVal = Syntax.Def { Syntax.defPattern = collectedPat,
                                               Syntax.defInit = collectedInit,
                                               Syntax.defPos = pos }
               } : compounds
           }
collectCompound accum AST.Element {
                        AST.elemVal = elem @ AST.Builder {
                                               AST.builderName = sym
                                             }
                      } =
  do
    collectElement elem
    return $! Syntax.Decl { Syntax.declSym = sym } : accum
collectCompound accum AST.Element {
                        AST.elemVal = elem @ AST.Fun { AST.funName = sym }
                      } =
  do
    collectElement elem
    return $! Syntax.Decl { Syntax.declSym = sym } : accum
-- All other elements just get added to the scope
collectCompound accum AST.Element { AST.elemVal = elem } =
  do
    collectElement elem
    return accum
-- Expressions get added to the list
collectCompound accum AST.Exp { AST.expVal = exp } =
  do
    collectedExp <- collectExp exp
    return $! Syntax.Exp { Syntax.expVal = collectedExp } : accum

-- | Collect an expression.
collectExp :: (MonadMessages Message m, MonadSymbols m) =>
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
    -- Fold function.  Only build a HashMap.
    collectValueField :: (MonadMessages Message m, MonadSymbols m) =>
                         HashMap FieldName (Syntax.Exp Symbol) -> AST.Field ->
                         CollectT m (HashMap FieldName (Syntax.Exp Symbol))
    collectValueField accum AST.Field { AST.fieldName = fname,
                                        AST.fieldVal = val,
                                        AST.fieldPos = pos' } =
      do
        collectedVal <- collectExp val
        if HashMap.member fname accum
          then do
            duplicateField (fieldSym fname) pos'
            return accum
          else return $! HashMap.insert fname collectedVal accum
  in do
    collectedFields <- foldM collectValueField HashMap.empty fields
    return Syntax.Record { Syntax.recordFields = collectedFields,
                           Syntax.recordPos = pos }
-- For record types, gather up all the names into a Fields structure.
collectExp AST.Record { AST.recordType = True, AST.recordFields = fields,
                        AST.recordPos = pos } =
  do
    collectedFields <- collectFields fields
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
    collectedParams <- collectFields params
    collectedContent <- collectScope content
    return Syntax.Anon { Syntax.anonKind = kind,
                         Syntax.anonParams = collectedParams,
                         Syntax.anonSuperTypes = collectedSupers,
                         Syntax.anonContent = collectedContent,
                         Syntax.anonPos = pos }
collectExp (AST.Literal lit) = return (Syntax.Literal lit)

-- | Collect a list of AST fields into a Syntax fields structure (a HashMap
-- and an ordering), and report duplicates.
collectFields :: (MonadMessages Message m, MonadSymbols m) =>
                 [AST.Field] -> CollectT m (Syntax.Fields (Syntax.Exp Symbol))
collectFields fields =
  let
    -- | Fold function.  Builds both a list and a HashMap.
    collectField :: (MonadMessages Message m, MonadSymbols m) =>
                    (HashMap FieldName (Syntax.Field (Syntax.Exp Symbol)),
                     [FieldName]) ->
                    AST.Field ->
                    CollectT m (HashMap FieldName (Syntax.Field (Syntax.Exp Symbol)),
                                [FieldName])
    collectField (tab, fields') AST.Field { AST.fieldName = fname,
                                            AST.fieldVal = val,
                                            AST.fieldPos = pos } =
      do
        collectedVal <- collectExp val
        if HashMap.member fname tab
          then do
            duplicateField (fieldSym fname) pos
            return (tab, fields')
          else let
            field = Syntax.Field { Syntax.fieldVal = collectedVal,
                                   Syntax.fieldPos = pos }
          in
            return (HashMap.insert fname field tab, fname : fields')
  in do
    -- Get a HashMap as well as a list of field names.
    (tab, fieldlist) <- foldM collectField (HashMap.empty, []) fields
    return Syntax.Fields {
             -- The list of field names becomes an array.
             Syntax.fieldsOrder = listArray (1, fromIntegral (length fieldlist))
                                            fieldlist,
             -- The HashMap becomes the bindings.
             Syntax.fieldsBindings = tab
           }

-- | Collect a case.  This is straightforward.
collectCase :: (MonadMessages Message m, MonadSymbols m) =>
               AST.Case -> CollectT m (Syntax.Case (Syntax.Exp Symbol))
collectCase AST.Case { AST.casePat = pat, AST.caseBody = body,
                       AST.casePos = pos } =
  do
    collectedPat <- collectPattern pat
    collectedBody <- collectExp body
    return Syntax.Case { Syntax.casePat = collectedPat,
                         Syntax.caseBody = collectedBody,
                         Syntax.casePos = pos }


-- | Collect a definition.
collectElement :: (MonadMessages Message m, MonadSymbols m) =>
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
    collectedParams <- collectFields params
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
collectElement vis accum AST.Def { AST.defPattern = pat, AST.defInit = init,
                                   AST.defPos = pos } =
  do
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    collectedPat <- collectPattern pat
    return $! addDef vis Syntax.Def { Syntax.defPattern = collectedPat,
                                      Syntax.defInit = collectedInit,
                                      Syntax.defPos = pos } accum
collectElement vis accum AST.Import { AST.importExp = exp,
                                      AST.importPos = pos } =
  do
    collectedExp <- collectExp exp
    return $! addDef vis Syntax.Import { Syntax.importExp = collectedExp,
                                         Syntax.importPos = pos } accum
-- For Funs, we do the same kind of transformation we did with
-- Builders: create a 'Def' whose initializer is an anonymous function.
collectElement vis accum
               AST.Fun { AST.funName = sym, AST.funCases = cases,
                         AST.funPos = pos } =
  do
    collectedCases <- mapM collectCase cases
    return $! addDef vis Syntax.Def {
                           Syntax.defPattern =
                              Syntax.Name { Syntax.nameSym = sym,
                                            Syntax.namePos = pos },
                           Syntax.defInit =
                              Just Syntax.Abs {
                                     Syntax.absKind = Lambda,
                                     Syntax.absCases = collectedCases,
                                     Syntax.absPos = pos
                                   },
                           Syntax.defPos = pos
                         } accum
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
collectScope :: (MonadMessages Message m, MonadSymbols m) =>
                AST.Scope -> CollectT m ScopeID
collectScope groups =
  let
    -- | Collect a group.  This rolls all elements of the group into the
    -- temporary scope, with the group's visibility.
    collectGroup :: (MonadMessages Message m, MonadSymbols m) =>
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
collectAST :: (MonadMessages Message m, MonadSymbols m) =>
              Position
           -- ^ Position corresponding to this entire file.
           -> Maybe [Symbol]
           -- ^ The name of a definition that is expected to appear at the
           -- top level.
           -> AST.AST
           -- ^ The 'AST' to collect.
           -> m Syntax.Component
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
        return Syntax.Component { Syntax.compScope = scopeid,
                                  Syntax.compExpected = Just defname }
    (Nothing, Just actual') ->
      -- We have no expected component name, but do have an actual
      -- one.  Get the expected definition name from the actual one.
      let
        defname = last actual'
      in do
        beginScope
        mapM_ (collectElement Public) scope
        scopeid <- finishScope
        return Syntax.Component { Syntax.compScope = scopeid,
                                  Syntax.compExpected = Just defname }
    (Nothing, Nothing) ->
      -- We don't have anything.  There is no expected definition
      -- name, and everything at the top level is public.
      do
        beginScope
        mapM_ (collectElement Public) scope
        scopeid <- finishScope
        return Syntax.Component { Syntax.compScope = scopeid,
                                  Syntax.compExpected = Nothing }

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
                 -> m ()
collectComponent parseFunc pos cname =
  let
    addEmpty =
      do
        tab <- ask
        HashTable.insert tab cname Nothing

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
              res <- parseFunc fname content
              case res of
                Just ast @ AST.AST { AST.astUses = uses } ->
                  let
                    fpos = File { fileName = fname }
                  in do
                    component <- collectAST fpos (Just cname) ast
                    addComponent cname component
                    mapM_ collectUse uses
                Nothing -> addEmpty
  in do
    done <- componentExists cname
    unless done loadAndCollect
    return ()

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
            -> m (Maybe (Syntax.Component (Syntax.Exp Symbol)))
collectFile parseFunc pos fstr =
  let
    collectUse AST.Use { AST.useName = uname, AST.usePos = upos } =
      collectComponent parseFunc upos uname
  in do
    -- Call the loader to get the file contents.
    loaded <- loadFile fstr pos
    case loaded of
      Nothing -> return Nothing
      Just (fname, content) ->
        -- Otherwise, continue
        do
          res <- parseFunc fname content
          case res of
            Just ast @ AST.AST { AST.astUses = uses } ->
              let
                fpos = File { fileName = fname }
              in do
                component <- collectAST fpos Nothing ast
                mapM_ collectUse uses
                return (Just component)
            Nothing -> return Nothing
