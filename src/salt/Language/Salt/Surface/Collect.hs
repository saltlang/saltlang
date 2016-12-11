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
{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

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
import Control.Monad.ScopeBuilder
import Control.Monad.Symbols
import Data.Array hiding (accum, elems)
import Data.Array.IO(IOArray)
import Data.Foldable hiding (elem)
import Data.HashMap.Strict(HashMap)
import Data.HashTable.IO(BasicHashTable)
import Data.Maybe
import Data.Position.Filename
import Data.PositionElement
import Data.ScopeID
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

-- | A temporary scope.  We build this up and then convert it into a
-- real scope.
data TempSaltScope expty =
  TempSaltScope {
    -- | The scope's ID
    tempScopeDefID :: !Syntax.DefID,
    -- | Builder definitions.  We keep all definitions to generate
    -- nicer error messages.
    tempScopeBuilders :: !(HashMap Symbol [Syntax.Builder expty]),
    -- | Truth definitions.  We keep all definitions to generate
    -- nicer error messages.
    tempScopeTruths :: !(HashMap Symbol [Syntax.Truth expty]),
    -- | Syntax directives.  We keep all directives to generate
    -- nicer error messages.
    tempScopeSyntax :: !(HashMap Symbol [Syntax.Syntax expty]),
    -- | Proofs.  There is no difference between the format here and
    -- in a real scope.
    tempScopeProofs :: ![Syntax.Proof expty],
    -- | Imports.  There is no difference between the format here and
    -- in a real scope.
    tempScopeImports :: ![Syntax.Import expty],
    -- | Names.  We write to this array, then freeze the array when
    -- completing the scope.
    tempScopeNames :: !(IOArray Visibility (HashMap Symbol [Syntax.DefID])),
    -- | Definitions.  We turn this into an array when completing the scope.
    tempScopeDefs :: ![(Syntax.DefID, Syntax.Def expty)],
    tempScopeID :: !ScopeID,
    tempScopeEnclosing :: !(Maybe ScopeID),
    tempScopePos :: !Position
  }

instance (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
         TempScope (TempSaltScope expty) (Syntax.Scope expty) m where
  createSubscope pos scopeid [] =
    do
      namesarr <- liftIO $! IOArray.newArray (Hidden, Public) HashMap.empty
      return TempSaltScope {
               tempScopeDefID = toEnum 0, tempScopeBuilders = HashMap.empty,
               tempScopeTruths = HashMap.empty, tempScopeSyntax = HashMap.empty,
               tempScopeDefs = [], tempScopeProofs = [], tempScopePos = pos,
               tempScopeImports = [], tempScopeNames = namesarr,
               tempScopeID = scopeid, tempScopeEnclosing = Nothing
             }
  createSubscope pos scopeid (TempSaltScope { tempScopeID = enclosing } : _) =
    do
      namesarr <- liftIO $! IOArray.newArray (Hidden, Public) HashMap.empty
      return TempSaltScope {
               tempScopeDefID = toEnum 0, tempScopeBuilders = HashMap.empty,
               tempScopeTruths = HashMap.empty, tempScopeSyntax = HashMap.empty,
               tempScopeDefs = [], tempScopeProofs = [], tempScopePos = pos,
               tempScopeImports = [], tempScopeNames = namesarr,
               tempScopeID = scopeid, tempScopeEnclosing = Just enclosing
             }

  finalizeScope TempSaltScope {
                  tempScopeBuilders = builders, tempScopeTruths = truths,
                  tempScopeProofs = proofs, tempScopeSyntax = syntax,
                  tempScopeImports = imports, tempScopeNames = names,
                  tempScopeDefs = defs, tempScopeDefID = defid,
                  tempScopeID = scopeid, tempScopeEnclosing = enclosing,
                  tempScopePos = pos
                } =
    let
      -- These functions turn lists of definitions into single
      -- definitions.  If the lists are length 1, then we're good.
      -- Otherwise, we have a namespace collision.
      collapseBuilders accum (_, []) =
        do
          internalError "Empty builder list" [pos]
          return accum
      collapseBuilders accum (sym, [single]) =
        return $! HashMap.insert sym single accum
      collapseBuilders accum (sym, builderlist @ (first : _)) =
        do
          duplicateBuilder sym (map position builderlist)
          return $! HashMap.insert sym first accum

      collapseTruths accum (_, []) =
        do
          internalError "Empty truth list" [pos]
          return accum
      collapseTruths accum (sym, [single]) =
        return $! HashMap.insert sym single accum
      collapseTruths accum (sym, truthlist @ (first : _)) =
        do
          duplicateTruth sym (map position truthlist)
          return $! HashMap.insert sym first accum

      collapseSyntax accum (_, []) =
        do
          internalError "Empty syntax list" [pos]
          return accum
      collapseSyntax accum (sym, [single]) =
        return $! HashMap.insert sym single accum
      collapseSyntax accum (sym, syntaxlist @ (first : _)) =
        do
          duplicateSyntax sym (map position syntaxlist)
          return $! HashMap.insert sym first accum

      defarr = array (toEnum 0, pred defid) defs
    in do
      -- Freeze the names array
      namesarr <- liftIO $! Unsafe.unsafeFreeze names
      -- Collapse all element types that report errors on
      -- namespace collisions, and report those errors.
      collapsedbuilders <- foldM collapseBuilders HashMap.empty
                                 (HashMap.toList builders)
      collapsedtruths <- foldM collapseTruths HashMap.empty
                               (HashMap.toList truths)
      collapsedsyntax <- foldM collapseSyntax HashMap.empty
                               (HashMap.toList syntax)
      return (scopeid, Syntax.Scope { Syntax.scopeBuilders = collapsedbuilders,
                                      Syntax.scopeTruths = collapsedtruths,
                                      Syntax.scopeSyntax = collapsedsyntax,
                                      Syntax.scopeProofs = proofs,
                                      Syntax.scopeImports = imports,
                                      Syntax.scopeDefs = defarr,
                                      Syntax.scopeNames = namesarr,
                                      Syntax.scopeEnclosing = enclosing })

type CollectT m =
  ReaderT Table (ScopeBuilderT (TempSaltScope (Syntax.Exp Syntax.Seq Symbol))
                               (Syntax.Scope (Syntax.Exp Syntax.Seq Symbol)) m)

-- | Check whether a component exists.
componentExists :: MonadIO m =>
                   [Symbol]
                -- ^ Component name to check.
                -> CollectT m Bool
                -- ^ Whether the component exists.
componentExists path =
  do
    tab <- ask
    res <- liftIO $! HashTable.lookup tab path
    return $! isJust res

-- | Add a component.
addComponent :: MonadIO m =>
                [Symbol]
             -- ^ The component name.
             -> Syntax.Component
             -- ^ The component.
             -> CollectT m ()
addComponent path component =
  do
    tab <- ask
    liftIO $! HashTable.insert tab path (Just component)

-- | Add a 'Builder' definition to the current scope.
addBuilder :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
              Symbol
           -- ^ The name of the 'Builder'.
           -> Syntax.Builder (Syntax.Exp Syntax.Seq Symbol)
           -- ^ The 'Builder' definition.
           -> CollectT m ()
addBuilder sym builder =
  let
    addBuilder' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
                   TempSaltScope (Syntax.Exp Syntax.Seq Symbol)
    addBuilder' scope @ TempSaltScope { tempScopeBuilders = builders } =
      let
        newbuilders = HashMap.insertWith (++) sym [builder] builders
      in
        scope { tempScopeBuilders = newbuilders }
  in
    updateScope addBuilder'

-- | Add a 'Truth' definition to the current scope.
addTruth :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
            Symbol
         -- ^ The name of the 'Truth'.
         -> Syntax.Truth (Syntax.Exp Syntax.Seq Symbol)
         -- ^ The 'Truth' definition.
         -> CollectT m ()
addTruth sym truth =
  let
    addTruth' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
                 TempSaltScope (Syntax.Exp Syntax.Seq Symbol)
    addTruth' scope @ TempSaltScope { tempScopeTruths = truths } =
      let
        newtruths = HashMap.insertWith (++) sym [truth] truths
      in
        scope { tempScopeTruths = newtruths }
  in
    updateScope addTruth'

-- | Add a 'Proof' to the current scope.
addProof :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
            Syntax.Proof (Syntax.Exp Syntax.Seq Symbol)
         -- ^ The 'Proof' definition.
         -> CollectT m ()
addProof proof =
  let
    addProof' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
                   TempSaltScope (Syntax.Exp Syntax.Seq Symbol)
    addProof' scope @ TempSaltScope { tempScopeProofs = proofs } =
      scope { tempScopeProofs = proof : proofs }
  in
    updateScope addProof'

-- | Add an import to the current scope.
addImport :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
             Syntax.Import (Syntax.Exp Syntax.Seq Symbol)
          -- ^ The 'Import' structure.
          -> CollectT m ()
addImport import' =
  let
    addImport' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
                  TempSaltScope (Syntax.Exp Syntax.Seq Symbol)
    addImport' scope @ TempSaltScope { tempScopeImports = imports } =
      scope { tempScopeImports = import' : imports }
  in
    updateScope addImport'

-- | Add a syntax directive to the current scope.
addSyntax :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
             Symbol
          -- ^ The 'Symbol' to which the syntax directive applies.
          -> Syntax.Syntax (Syntax.Exp Syntax.Seq Symbol)
          -- ^ The syntax directive.
          -> CollectT m ()
addSyntax sym syntax =
  let
    addSyntax' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
                 TempSaltScope (Syntax.Exp Syntax.Seq Symbol)
    addSyntax' scope @ TempSaltScope { tempScopeSyntax = syntaxes } =
      let
        newsyntaxes = HashMap.insertWith (++) sym [syntax] syntaxes
      in
        scope { tempScopeSyntax = newsyntaxes }
  in
    updateScope addSyntax'

-- | Add a definition into a scope, get a 'DefID' for it.
addDef :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
          Syntax.Def (Syntax.Exp Syntax.Seq Symbol)
       -- ^ The definition to add.
       -> CollectT m Syntax.DefID
       -- ^ The 'DefID' for the definition that was added.
addDef def @ Syntax.Def {} =
  let
    addDef' :: TempSaltScope (Syntax.Exp Syntax.Seq Symbol) ->
               (Syntax.DefID, TempSaltScope (Syntax.Exp Syntax.Seq Symbol))
    addDef' scope @ TempSaltScope { tempScopeDefID = defid,
                                    tempScopeDefs = defs } =
      (defid, scope { tempScopeDefs = (defid, def) : defs,
                      tempScopeDefID = succ defid })
  in
    alterScope addDef'

-- | Add names that refer to a given 'DefID'
addNames ::  (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
            Visibility
         -- ^ The visibility of the names.
         -> [Symbol]
         -- ^ The names to add.
         -> Syntax.DefID
         -- ^ The 'DefID' to which the names refer.
         -> CollectT m ()
addNames vis syms defid =
  let
    foldfun accum sym = HashMap.insertWith (++) sym [defid] accum

    addNames' :: (MonadSymbols m, MonadMessages Message m, MonadIO m) =>
                 TempSaltScope (Syntax.Exp Syntax.Seq Symbol) -> m ()
    addNames' TempSaltScope { tempScopeNames = names } =
      do
        curr <- liftIO $! IOArray.readArray names vis
        liftIO $! IOArray.writeArray names vis (foldl foldfun curr syms)
  in do
    currscope <- getScope
    addNames' currscope

-- | Collect a pattern.  Also collect all the names that it binds.
collectPattern :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  AST.Pattern
               -- ^ The pattern to collect.
               -> CollectT m (Syntax.Pattern (Syntax.Exp Syntax.Seq Symbol),
                              [Symbol])
               -- ^ Collected pattern and names that it binds.
collectPattern pat =
  let
    -- | Foldable version of collectPattern', also returns the name to
    -- which the top-level pattern is bound.
    collectNamedPattern :: (MonadMessages Message m,
                            MonadSymbols m, MonadIO m) =>
                           HashMap Symbol [Position]
                        -- ^ Set of bound names.
                        -> AST.Pattern
                        -- ^ The pattern to collect.
                        -> CollectT m (Maybe Symbol,
                                       Syntax.Pattern (Syntax.Exp Syntax.Seq
                                                                  Symbol),
                                       HashMap Symbol [Position])
                        -- ^ The top-level binding, the collected
                        -- pattern, and the bound names.  For an
                        -- as-pattern, bind the field with that name
                        -- to its own name, and further deconstruct
                        -- the pattern.
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

    -- | Collect an entry.  This is a foldable function.  Note that we
    -- keep all pattern for a given name for error reporting purposes.
    collectEntry :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                    (HashMap Symbol [Syntax.Entry (Syntax.Exp Syntax.Seq
                                                              Symbol)],
                     HashMap Symbol [Position])
                 -- ^ Incoming name-pattern bindings and set of bound names.
                 -> AST.Entry
                 -- ^ The entry to collect.
                 -> CollectT m (HashMap Symbol
                                        [Syntax.Entry (Syntax.Exp Syntax.Seq
                                                                  Symbol)],
                                HashMap Symbol [Position])
                 -- ^ The name-pattern bindings and set of bound names.
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
        -- See if the pattern has a top-level binding name.
        named <- collectNamedPattern binds pat'
        case named of
          -- If it does, use that as the name for the entry.
          (Just sym, collectedPat, newbinds) ->
            return (HashMap.insertWith (++) sym
                                       [Syntax.Entry {
                                          Syntax.entryPat = collectedPat,
                                          Syntax.entryPos = pos
                                        }] accum,
                    HashMap.insertWith (++) sym [pos] newbinds)
          -- Otherwise, it's an error
          (Nothing, _, newbinds) ->
            do
              namelessField (position pat)
              return (accum, newbinds)

    -- | Internal foldable form of collectPattern.  This returns bound
    -- names as a 'HashMap' to all positions, which aids in error
    -- reporting.
    collectPattern' :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                       HashMap Symbol [Position]
                    -- ^ The incoming set of bound names.
                    -> AST.Pattern
                    -- ^ The pattern to collect.
                    -> CollectT m (Syntax.Pattern (Syntax.Exp Syntax.Seq
                                                              Symbol),
                                   HashMap Symbol [Position])
                    -- ^ The collected pattern and bound names.
    -- For split, collect all fields, then report duplicates.
    collectPattern' binds AST.Split { AST.splitFields = fields,
                                      AST.splitStrict = strict,
                                      AST.splitPos = pos } =
      let
        collapseField accum (_, []) =
          do
            internalError "Empty field list" [pos]
            return accum
        collapseField accum (sym, [single]) =
          return $! HashMap.insert sym single accum
        collapseField accum (sym, (syntax @ (first : _))) =
          do
            duplicateField sym (map position syntax)
            return $! HashMap.insert sym first accum
      in do
        -- First collect all field patterns
        (collectedFields, newbinds) <-
          foldM collectEntry (HashMap.empty, binds) fields
        -- Collapse all field patterns and report duplicates
        collapsedFields <- foldM collapseField HashMap.empty
                                 (HashMap.toList collectedFields)
        return (Syntax.Split { Syntax.splitStrict = strict,
                               Syntax.splitFields = collapsedFields,
                               Syntax.splitPos = pos }, newbinds)
    -- For option, collect all the options, then check for mismatches
    collectPattern' binds AST.Option { AST.optionPats = pats,
                                       AST.optionPos = pos } =
      let
        -- | Check for any extra bound names in any patterns, return the
        -- common name bindings.
        checkMismatch :: (MonadMessages Message m, MonadSymbols m) =>
                         [HashMap Symbol [Position]]
                      -- ^ The bound names for each option.
                      -> m (HashMap Symbol [Position])
                      -- ^ The common bound names.
        checkMismatch bindlist =
          let
            -- | The common names among all bindings
            commonbinds = foldr1 (HashMap.intersectionWith (++)) bindlist

            -- | Report any extra name bindings over 'commonbinds'.
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
        -- First collect all options
        (collected, bindlist) <- foldM (foldfun binds) ([], []) pats
        -- Check for any mismatched bindings
        newbinds <- checkMismatch bindlist
        return (Syntax.Option { Syntax.optionPats = collected,
                                Syntax.optionPos = pos }, newbinds)
    -- As and Name add bindings, so make sure to add them to the bound name set
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
    -- The rest of these are entirely straightforward
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
    collectPattern' binds AST.Exact { AST.exactLit = l } =
      return (Syntax.Exact { Syntax.exactLit = l }, binds)
  in do
    (collectedPat, binds) <- collectPattern' HashMap.empty pat
    return (collectedPat, HashMap.keys binds)

-- | Foldable function to collect compound expression components.
collectCompound :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                   [Syntax.Compound (Syntax.Exp Syntax.Seq Symbol)]
                -- ^ The incoming compound content.
                -> AST.Compound
                -- ^ The compound content to collect.
                -> CollectT m [Syntax.Compound (Syntax.Exp Syntax.Seq Symbol)]
                -- ^ The compound content.
-- For definitions, add the definition to the scope, and add an
-- initializer mark to the content.
collectCompound accum AST.Element {
                        AST.elemVal = AST.Def { AST.defPattern = pat,
                                                AST.defInit = init,
                                                AST.defPos = pos }
                      } =
  do
    -- Collect the initializer and pattern
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    (collectedPat, binds) <- collectPattern pat
    -- Add the definition to the scope
    defid <- addDef Syntax.Def { Syntax.defPattern = collectedPat,
                                 Syntax.defInit = collectedInit,
                                 Syntax.defPos = pos }
    addNames Public binds defid
    -- Indicate that the definition is initialized here
    return $! Syntax.Init { Syntax.initId = defid } : accum
-- Builders have initializers, so add the marker
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
-- All other elements (truths, proofs, etc) just get added to the
-- scope; they don't have a runtime presence
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
              AST.Exp
           -- ^ Expression to collect.
           -> CollectT m (Syntax.Exp Syntax.Seq Symbol)
           -- ^ Collected expression.
-- For a compound statement, construct a sub-scope
collectExp AST.Compound { AST.compoundBody = body, AST.compoundPos = pos } =
  do
    (collectedBody, scopeid) <- makeScope pos (foldM collectCompound [] body)
    return Syntax.Compound { Syntax.compoundScope = scopeid,
                             Syntax.compoundBody = reverse collectedBody,
                             Syntax.compoundPos = pos }
-- For record values, gather up all the bindings into a HashMap.
collectExp AST.Record { AST.recordType = False, AST.recordFields = fields,
                        AST.recordPos = pos } =
  let
    collapseField :: (MonadMessages Message m, MonadSymbols m) =>
                     HashMap FieldName (Syntax.Exp Syntax.Seq Symbol) ->
                     (FieldName, [Syntax.Exp Syntax.Seq Symbol]) ->
                     CollectT m (HashMap FieldName (Syntax.Exp Syntax.Seq
                                                               Symbol))
    collapseField accum (_, []) =
      do
        internalError (Strict.fromString "Empty field list") [pos]
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
                         HashMap FieldName [Syntax.Exp Syntax.Seq Symbol] ->
                         AST.Field ->
                         CollectT m (HashMap FieldName [Syntax.Exp Syntax.Seq
                                                                   Symbol])
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
-- For sequences and tuples, we should never see empty lists, and we
-- turn singleton lists into standalone expressions.
collectExp AST.Seq { AST.seqExps = [], AST.seqPos = pos } =
  do
    internalError "Should not see empty list in AST.Seq" [pos]
    return Syntax.Bad { Syntax.badPos = pos }
collectExp AST.Seq { AST.seqExps = [ single ] } = collectExp single
collectExp AST.Seq { AST.seqExps = exps, AST.seqPos = pos } =
  do
    collectedExps <- mapM collectExp exps
    return Syntax.Call { Syntax.callInfo =
                           Syntax.Seq { Syntax.seqExps = collectedExps,
                                        Syntax.seqPos = pos } }
collectExp AST.Tuple { AST.tupleFields = [], AST.tuplePos = pos } =
  do
    internalError "Should not see empty list in AST.Tuple" [pos]
    return Syntax.Bad { Syntax.badPos = pos }
collectExp AST.Tuple { AST.tupleFields = [ single ] } = collectExp single
collectExp AST.Tuple { AST.tupleFields = fields, AST.tuplePos = pos } =
  do
    collectedFields <- mapM collectExp fields
    return Syntax.Tuple { Syntax.tupleFields = collectedFields,
                          Syntax.tuplePos = pos }
-- The remainder of these are straightforward
collectExp AST.Sym { AST.symName = sym, AST.symPos = pos } =
  return Syntax.Sym { Syntax.symRef = sym, Syntax.symPos = pos }
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
collectExp AST.Project { AST.projectVal = val, AST.projectFields = fields,
                         AST.projectPos = pos } =
  do
    collectedVal <- collectExp val
    return Syntax.Project { Syntax.projectVal = collectedVal,
                            Syntax.projectFields = fields,
                            Syntax.projectPos = pos }
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
    collectedContent <- collectScope pos content
    return Syntax.Anon { Syntax.anonKind = kind,
                         Syntax.anonParams = collectedParams,
                         Syntax.anonSuperTypes = collectedSupers,
                         Syntax.anonContent = collectedContent,
                         Syntax.anonPos = pos }
collectExp (AST.Literal lit) = return (Syntax.Literal lit)

-- | Collect a list of AST fields into a Syntax fields structure (a HashMap
-- and an ordering), and report duplicates.
collectFields :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                 [AST.Field]
              -> Position
              -- ^ The position of the whole fields list.
              -> CollectT m (Syntax.Fields (Syntax.Exp Syntax.Seq Symbol))
              -- ^ The collected fields.
collectFields fields fieldspos =
  let
    -- This is another foldable function that turns lists of fields
    -- into single fields, reporting duplicate field errors whenever
    -- the lists have more than one element.
    collapseField accum (_, []) =
      do
        internalError (Strict.fromString "Empty field list") [fieldspos]
        return accum
    collapseField accum (fname, [single]) =
      return $! HashMap.insert fname single accum
    collapseField accum (fname @ FieldName { fieldSym = sym },
                         fields' @ (first : _)) =
      do
        duplicateField sym (map position fields')
        return $! HashMap.insert fname first accum

    -- | Foldable function.  Builds both a list and a HashMap.
    collectField :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                    (HashMap FieldName [Syntax.Field (Syntax.Exp Syntax.Seq
                                                                 Symbol)],
                     [FieldName]) ->
                    AST.Field ->
                    CollectT m (HashMap FieldName
                                        [Syntax.Field (Syntax.Exp Syntax.Seq
                                                              Symbol)],
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
    -- Gather up all the fields, as well as their order.
    (tab, fieldlist) <- foldM collectField (HashMap.empty, []) fields
    -- Collapse and report duplicates.
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
               AST.Case
            -- ^ Case to collect.
            -> CollectT m (Syntax.Case (Syntax.Exp Syntax.Seq Symbol))
            -- ^ The collected case.
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
            collectedBody <- collectScope pos body
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
    addNames vis binds defid
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
  do
    collectedPrecs <- mapM (mapM collectExp) precs
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
                Position
             -> AST.Scope
             -> CollectT m ScopeID
collectScope pos groups =
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
    (_, scopeid) <- makeScope pos (mapM_ collectGroup groups)
    return scopeid

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
        (_, scopeid) <- makeScope filepos (mapM_ (collectElement Public) scope)
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
        (_, scopeid) <- makeScope filepos (mapM_ (collectElement Public) scope)
        addComponent actual' Syntax.Component {
                               Syntax.compScope = scopeid,
                               Syntax.compExpected = Just defname
                             }
    (Nothing, Nothing) ->
      -- We don't have anything.  There is no expected definition
      -- name, and everything at the top level is public.
      do
        _ <- makeScope filepos (mapM_ (collectElement Public) scope)
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

collectComponents :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                      MonadMessages Message m, MonadGenpos m,
                      MonadSymbols m, MonadIO m) =>
                     (Filename -> Lazy.ByteString -> m (Maybe AST.AST))
                  -- ^ The parsing function to use.
                  -> [(Position, [Symbol])]
                  -- ^ The names of the components to collect.
                  -> m (Syntax.Surface (Syntax.Exp Syntax.Seq Symbol))
collectComponents parsefunc files =
  let
    collectOne (pos, fname) = collectComponent parsefunc pos fname

    foldfun comps (_, Nothing) = return comps
    foldfun comps (_, Just comp) = return $! comp : comps
  in do
    tab <- liftIO HashTable.new
    ((), scopes) <- runScopeBuilderT (runReaderT (mapM_ collectOne files) tab)
    -- Check that the scope stack is empty
    comps <- liftIO $! HashTable.foldM foldfun [] tab
    return $! Syntax.Surface { Syntax.surfaceScopes = scopes,
                               Syntax.surfaceComponents = comps }

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
             -> m (Syntax.Surface (Syntax.Exp Syntax.Seq Symbol))
collectFiles parsefunc files =
  let
    collectOne (pos, fname) = collectFile parsefunc pos fname

    foldfun comps (_, Nothing) = return comps
    foldfun comps (_, Just comp) = return $! comp : comps
  in do
    tab <- liftIO HashTable.new
    ((), scopes) <- runScopeBuilderT (runReaderT (mapM_ collectOne files) tab)
    -- Check that the scope stack is empty
    comps <- liftIO $! HashTable.foldM foldfun [] tab
    return $! Syntax.Surface { Syntax.surfaceScopes = scopes,
                               Syntax.surfaceComponents = comps }
