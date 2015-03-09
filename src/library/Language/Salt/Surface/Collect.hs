-- Copyright (c) 2014 Eric McCorkle.
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301 USA
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
import Control.Monad.Collect
import Control.Monad.Genpos
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.Array hiding (accum, elems)
import Data.HashMap.Strict(HashMap)
import Data.Maybe
import Data.Position
import Data.Symbol
import Language.Salt.Message
import Language.Salt.Surface.Common
import Prelude hiding (elem, exp, init)
import System.FilePath
import System.IO.Error

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Salt.Surface.AST as AST
import qualified Language.Salt.Surface.Syntax as Syntax

type Elems = ([Syntax.Element], [Syntax.Element],
              [Syntax.Element], [Syntax.Element])

type BuilderScope = HashMap Symbol Syntax.Builder

type SyntaxScope = HashMap Symbol Syntax.Syntax

type TruthScope = HashMap Symbol Syntax.Truth

type TempScope = (BuilderScope, SyntaxScope, TruthScope, [Syntax.Proof], Elems)

emptyTempScope :: TempScope
emptyTempScope = (HashMap.empty, HashMap.empty, HashMap.empty,
                  [], ([], [], [], []))

makeScope :: TempScope -> Syntax.Scope
makeScope (builders, syntax, truths, proofs,
           (hiddens, privates, protecteds, publics)) =
  let
    elems = listArray (Hidden, Public) [hiddens, privates, protecteds, publics]
  in
    Syntax.Scope { Syntax.scopeBuilders = builders,
                   Syntax.scopeSyntax = syntax,
                   Syntax.scopeTruths = truths,
                   Syntax.scopeElems = elems,
                   Syntax.scopeProofs = proofs }

emptyScope :: Syntax.Scope
emptyScope = makeScope emptyTempScope

-- | Add a definition into a scope, merging it in with what's already there.
addDef :: Visibility -> Syntax.Element -> TempScope -> TempScope
addDef Hidden elem (builders, syntax, truths, proofs,
                    (hiddens, privates, protecteds, publics)) =
  (builders, syntax, truths, proofs,
   (elem : hiddens, privates, protecteds, publics))
addDef Private elem (builders, syntax, truths, proofs,
                     (hiddens, privates, protecteds, publics)) =
  (builders, syntax, truths, proofs,
   (hiddens, elem : privates, protecteds, publics))
addDef Protected elem (builders, syntax, truths, proofs,
                       (hiddens, privates, protecteds, publics)) =
  (builders, syntax, truths, proofs,
   (hiddens, privates, elem : protecteds, publics))
addDef Public elem (builders, syntax, truths, proofs,
                    (hiddens, privates, protecteds, publics)) =
  (builders, syntax, truths, proofs,
   (hiddens, privates, protecteds, elem : publics))

-- | Collect a pattern, return the name to which the pattern is bound
-- at the top level.
collectNamedPattern :: (MonadMessages Message m, MonadSymbols m) =>
                       AST.Pattern -> m (Maybe Symbol, Syntax.Pattern)
-- For an as-pattern, bind the field with that name to its own
-- name, and further deconstruct the pattern.
collectNamedPattern AST.As { AST.asName = sym, AST.asPat = pat,
                             AST.asPos = pos } =
  do
    collectedPat <- collectPattern pat
    return (Just sym, Syntax.As { Syntax.asName = sym,
                                  Syntax.asPat = collectedPat,
                                  Syntax.asPos = pos
                                })
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
                HashMap Symbol Syntax.Entry ->
                AST.Entry -> m (HashMap Symbol Syntax.Entry)
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
collectEntry accum (AST.Unnamed pat) =
  let
    pos = AST.patternPosition pat
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
          namelessField (AST.patternPosition pat)
          return accum

collectPattern :: (MonadMessages Message m, MonadSymbols m) =>
                  AST.Pattern -> m Syntax.Pattern
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
collectPattern (AST.Exact l) = return (Syntax.Exact l)

-- | Collect a bunch of 'Seq's by walking down the spine and converting
-- it into a list.
collectSeq :: (MonadMessages Message m, MonadSymbols m) =>
              AST.Exp -> m [Syntax.Exp]
collectSeq =
  let
    collectSeq' :: (MonadMessages Message m, MonadSymbols m) =>
                   [Syntax.Exp] -> AST.Exp -> m [Syntax.Exp]
    collectSeq' accum AST.Seq { AST.seqFirst = first,
                                AST.seqSecond = second @ AST.Seq {} } =
      do
        collectedFirst <- collectExp first
        collectSeq' (collectedFirst : accum) second
    collectSeq' accum AST.Seq { AST.seqFirst = first,
                                AST.seqSecond = second } =
      do
        collectedFirst <- collectExp first
        collectedSecond <- collectExp second
        return $! reverse (collectedSecond : collectedFirst : accum)
    collectSeq' accum e =
      do
        collectedExp <- collectExp e
        return $! reverse (collectedExp : accum)
  in
    collectSeq' []

-- | Collect a syntax directive.  This implements a simple recursive
-- descent parser to parse the syntax directives.
--
-- Syntax directives are represented as expressions, in order to avoid
-- the need to define things like @left@, @right@, @<@ and such as
-- keywords, thus complicating the grammar.  (This also allows syntax
-- directives to be expanded later on).
--
-- This function does the work of parsing the directives, converting
-- them into annotations on definitions in the scope.
--
-- The format for syntax directives (including the syntax keyword is:
--
-- > syntax id (postfix | infix (left | right | nonassoc) |
-- >            prec (< | > | ==) id (. id)* )+
collectSyntax :: (MonadMessages Message m, MonadSymbols m) =>
                 SyntaxScope -> AST.Exp -> m SyntaxScope
collectSyntax syntax AST.Seq { AST.seqFirst = AST.Sym { AST.symName = sym},
                               AST.seqSecond = body } =
  let
    -- | Add a precedence relationship to the current scope.
    addPrec :: (MonadMessages Message m, MonadSymbols m) =>
               HashMap Symbol Syntax.Syntax -> Ordering -> AST.Exp ->
               m (HashMap Symbol Syntax.Syntax)
    addPrec syntax' ord exp =
      -- Look for the symbol in the scope.
      case HashMap.lookup sym syntax' of
        -- If we find the definition, update it.
        Just s @ Syntax.Syntax { Syntax.syntaxPrecs = precs } ->
          do
            -- Convert the expression from AST to Syntax.  We'll
            -- check its form later.
            collectedExp <- collectExp exp
            -- Add the precedence relation to the symbol definition.
            return $! HashMap.insert sym s { Syntax.syntaxPrecs =
                                               (ord, collectedExp) : precs }
                                     syntax'
              -- We already reported undefined symbols.
        Nothing -> return syntax'

    -- | Add a fixity to the current scope.  Report an error if
    -- the symbol already has a fixity other than prefix.
    addFixity :: (MonadMessages Message m, MonadSymbols m) =>
                 HashMap Symbol Syntax.Syntax ->
                 Syntax.Fixity -> Position ->
                 m (HashMap Symbol Syntax.Syntax)
    addFixity syntax' fixity pos =
      -- Look for the symbol in the scope.
      case HashMap.lookup sym syntax' of
        -- If we find the definition, and its fixity is prefix, update it.
        Just s @ Syntax.Syntax { Syntax.syntaxFixity = Syntax.Prefix } ->
          let
            newsyntax = s { Syntax.syntaxFixity = fixity }
          in
            return $! HashMap.insert sym newsyntax syntax'
        -- Otherwise report multiple fixities for this symbol.
        Just _ ->
          do
            multipleFixity pos
            return syntax'
        -- We already reported undefined symbols.
        Nothing -> return syntax'

    -- | Precedence identifier.
    --
    -- > syntax id prec (< | > | ==) id (. id)
    -- >                             ^
    precName :: (MonadMessages Message m, MonadSymbols m) =>
                Ordering -> HashMap Symbol Syntax.Syntax -> AST.Exp ->
                m (HashMap Symbol Syntax.Syntax)
    -- If the top-level expression is a Seq, then there's another
    -- directive after this one.
    precName ord syntax' AST.Seq { AST.seqFirst = exp,
                                   AST.seqSecond = rest } =
      do
        -- Add the precedence relation
        newdefs <- addPrec syntax' ord exp
        -- Parse the next directive
        startDirective newdefs rest
    -- Otherwise, this is the last directive, so add the precedence
    -- relation.
    precName ord syntax' exp = addPrec syntax' ord exp

    -- | Precedence relation.
    --
    -- > syntax id prec (< | > | ==) id (. id)*
    -- >                ^
    precRelation :: (MonadMessages Message m, MonadSymbols m) =>
                    HashMap Symbol Syntax.Syntax -> AST.Exp ->
                    m (HashMap Symbol Syntax.Syntax)
    -- If the top-level expression is a Seq, then the first
    -- expression must be a Sym, which is the relationship.  We
    -- expect to see a Field or Sym next.
    precRelation syntax'
                 AST.Seq { AST.seqFirst = AST.Sym { AST.symName = kindsym,
                                                    AST.symPos = pos },
                           AST.seqSecond = rest } =
      do
        kind <- name kindsym
        -- Get the precedence relationship from the symbol's name.
        case kind of
          "<" -> precName LT syntax' rest
          ">" -> precName GT syntax' rest
          "==" -> precName EQ syntax' rest
          -- Not a valid precedence relation.
          _ ->
            do
              badSyntaxPrec pos
              return syntax'
    -- The first expression must be a Sym, or else it's a parse error.
    precRelation syntax' AST.Seq { AST.seqFirst = kind } =
      do
        badSyntaxPrec (AST.expPosition kind)
        return syntax'
    -- The top-level expression must be a Seq, or else it's a parse error.
    precRelation syntax' exp =
      do
        badSyntax (AST.expPosition exp)
        return syntax'

    -- | Infix associativity
    --
    -- > syntax id infix (left | right | nonassoc) directive*
    -- >                 ^
    infixAssoc :: (MonadMessages Message m, MonadSymbols m) =>
                  Position -> HashMap Symbol Syntax.Syntax -> AST.Exp ->
                  m (HashMap Symbol Syntax.Syntax)
    infixAssoc fixitypos syntax'
               AST.Seq { AST.seqFirst = AST.Sym { AST.symName = kindsym,
                                                  AST.symPos = pos },
                         AST.seqSecond = rest } =
      do
        kind <- name kindsym
        -- Get the associativity from the symbol's name, then add
        -- the infix fixity to the scope, then parse another
        -- directive with the rest of the expression.
        case kind of
          "left" ->
            do
              newdefs <- addFixity syntax' (Syntax.Infix Syntax.Left)
                                   fixitypos
              startDirective newdefs rest
          "right" ->
            do
              newdefs <- addFixity syntax' (Syntax.Infix Syntax.Right)
                                   fixitypos
              startDirective newdefs rest
          "nonassoc" ->
            do
              newdefs <- addFixity syntax' (Syntax.Infix Syntax.NonAssoc)
                                   fixitypos
              startDirective newdefs rest
          -- Not a valid fixity.
          _ ->
            do
              badSyntaxAssoc pos
              return syntax'
    -- If the first expression isn't a Sym, it's a parse error.
    infixAssoc _ syntax' AST.Seq { AST.seqFirst = kind } =
      do
        badSyntaxAssoc (AST.expPosition kind)
        return syntax'
    -- If the top-level expression is a Sym, it must be the
    -- associativity, and this is the last directive.
    infixAssoc fixitypos syntax' AST.Sym { AST.symName = kindsym,
                                           AST.symPos = pos } =
      do
        kind <- name kindsym
        -- Get the associativity from the symbol's name, then add
        -- the infix fixity to the scope.
        case kind of
          "left" -> addFixity syntax' (Syntax.Infix Syntax.Left) fixitypos
          "right" -> addFixity syntax' (Syntax.Infix Syntax.Right) fixitypos
          "nonassoc" ->
            addFixity syntax' (Syntax.Infix Syntax.NonAssoc) fixitypos
          -- Not a valid fixity.
          _ ->
            do
              badSyntaxAssoc pos
              return syntax'
    -- If the top-level expression isn't a Seq or a Sym, then it's
    -- a parse error.
    infixAssoc _ syntax' exp =
      do
        badSyntax (AST.expPosition exp)
        return syntax'

    -- | Start of one directive.
    --
    -- > syntax id (postfix | infix (left | right | nonassoc) |
    -- >            prec (< | > | ==) id (. id)* )+
    -- >           ^
    startDirective :: (MonadMessages Message m, MonadSymbols m) =>
                      HashMap Symbol Syntax.Syntax -> AST.Exp ->
                      m (HashMap Symbol Syntax.Syntax)
    -- The first expression is a Sym (the kind), and there's more.
    startDirective syntax'
                   AST.Seq { AST.seqFirst = AST.Sym { AST.symName = kindsym,
                                                      AST.symPos = pos },
                             AST.seqSecond = rest } =
      do
        kind <- name kindsym
        -- Look at the kind to figure out what to do next.
        case kind of
          -- Postfix has no more info, so add it to the scope and
          -- try to parse another directive.
          "postfix" ->
            do
              newdefs <- addFixity syntax' Syntax.Postfix pos
              startDirective newdefs rest
          -- Infix expects to see an associativity next.
          "infix" -> infixAssoc pos syntax' rest
          -- Precedence relations expect more content.
          "prec" -> precRelation syntax' rest
          -- This wasn't a known syntax directive
          _ ->
            do
              badSyntaxKind pos
              return syntax'
    -- If the first expression isn't a Sym, it's a parse error.
    startDirective syntax' AST.Seq { AST.seqFirst = kind } =
      do
        badSyntaxKind (AST.expPosition kind)
        return syntax'
    -- It's ok for the top-level to be a Sym, as long as it's "postfix".
    startDirective syntax' AST.Sym { AST.symName = kindsym,
                                     AST.symPos = pos } =
      do
        kind <- name kindsym
        case kind of
          "postfix" -> addFixity syntax' Syntax.Postfix pos
          -- Infixes expect an associativity
          "infix" ->
            do
              badSyntax pos
              return syntax'
          -- Precedence relations expect more
          "prec" ->
            do
              badSyntax pos
              return syntax'
          -- This wasn't a known syntax directive
          _ ->
            do
              badSyntaxKind pos
              return syntax'
    -- If the top-level expression isn't a Seq or a Sym, then it's
    -- a parse error.
    startDirective syntax' exp =
      do
        badSyntax (AST.expPosition exp)
        return syntax'
  in
    startDirective syntax body
-- If the first expression isn't a Sym, then it's a parse error.
collectSyntax accum AST.Seq { AST.seqFirst = kind } =
  do
    badSyntaxName (AST.expPosition kind)
    return accum
-- If the top-level expression isn't a Seq, then it's a parse error.
collectSyntax accum exp =
  do
    badSyntax (AST.expPosition exp)
    return accum

type TempDynamicScope = (SyntaxScope, [Syntax.Proof], [Syntax.Compound])

emptyTempDynamicScope :: TempDynamicScope
emptyTempDynamicScope = (HashMap.empty, [], [])

collectCompound :: (MonadMessages Message m, MonadSymbols m) =>
                   TempDynamicScope -> AST.Compound -> m TempDynamicScope
-- Add proofs to the list of proofs for the entire dynamic scope.
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Proof { AST.proofName = sym,
                                         AST.proofBody = body,
                                         AST.proofPos = pos }) =
  do
    collectedName <- collectExp sym
    collectedBody <- collectExp body
    return (syntax, Syntax.Proof { Syntax.proofName = collectedName,
                                   Syntax.proofBody = collectedBody,
                                   Syntax.proofPos = pos } : proofs, compounds)
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Syntax { AST.syntaxExp = exp }) =
  do
    newsyntax <- collectSyntax syntax exp
    return (newsyntax, proofs, compounds)
-- Turn builder definitions into local builder definitions.
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Builder { AST.builderName = sym,
                                           AST.builderKind = kind,
                                           AST.builderParams = params,
                                           AST.builderSuperTypes = supers,
                                           AST.builderContent = content,
                                           AST.builderPos = pos }) =
  do
    collectedSupers <- mapM collectExp supers
    collectedParams <- collectFields params
    case content of
      AST.Body body ->
        do
          collectedBody <- collectScope body
          return (syntax, proofs,
                  Syntax.Local {
                    Syntax.localName = sym,
                    Syntax.localBuilder =
                      Syntax.Builder {
                        Syntax.builderKind = kind,
                        Syntax.builderVisibility = Public,
                        Syntax.builderParams = collectedParams,
                        Syntax.builderSuperTypes = collectedSupers,
                        Syntax.builderContent =
                          Syntax.Anon { Syntax.anonKind = kind,
                                        Syntax.anonParams = collectedParams,
                                        Syntax.anonSuperTypes = collectedSupers,
                                        Syntax.anonContent = collectedBody,
                                        Syntax.anonPos = pos },
                        Syntax.builderPos = pos
                      }
                  } : compounds)
      AST.Value value ->
        do
          collectedValue <- collectExp value
          return (syntax, proofs,
                  Syntax.Local {
                    Syntax.localName = sym,
                    Syntax.localBuilder =
                      Syntax.Builder {
                        Syntax.builderKind = kind,
                        Syntax.builderVisibility = Public,
                        Syntax.builderParams = collectedParams,
                        Syntax.builderSuperTypes = collectedSupers,
                        Syntax.builderContent = collectedValue,
                        Syntax.builderPos = pos
                      }
                  } : compounds)
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Truth { AST.truthName = sym,
                                         AST.truthKind = kind,
                                         AST.truthContent = content,
                                         AST.truthPos = pos }) =
  do
    collectedContent <- collectExp content
    return (syntax, proofs,
            Syntax.Dynamic {
              Syntax.dynamicName = sym,
              Syntax.dynamicTruth =
                Syntax.Truth { Syntax.truthKind = kind,
                               Syntax.truthVisibility = Public,
                               Syntax.truthContent = collectedContent,
                               Syntax.truthPos = pos }
            } : compounds)
-- For the rest, collect the tree and add it to the statement list.
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Def { AST.defPattern = pat, AST.defInit = init,
                                       AST.defPos = pos }) =
  do
    collectedInit <- case init of
      Just exp -> liftM Just (collectExp exp)
      Nothing -> return Nothing
    collectedPat <- collectPattern pat
    return (syntax, proofs,
            Syntax.Element Syntax.Def { Syntax.defPattern = collectedPat,
                                        Syntax.defInit = collectedInit,
                                        Syntax.defPos = pos } : compounds)
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Import { AST.importExp = exp,
                                          AST.importPos = pos }) =
  do
    collectedExp <- collectExp exp
    return (syntax, proofs,
            Syntax.Element Syntax.Import { Syntax.importExp = collectedExp,
                                           Syntax.importPos = pos } : compounds)
collectCompound (syntax, proofs, compounds)
                (AST.Element AST.Fun { AST.funName = sym, AST.funCases = cases,
                                       AST.funPos = pos }) =
  do
    collectedCases <- mapM collectCase cases
    return (syntax, proofs,
            Syntax.Element Syntax.Def {
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
                           } : compounds)
collectCompound (syntax, proofs, compounds) (AST.Exp exp) =
  do
    collectedExp <- collectExp exp
    return (syntax, proofs, Syntax.Exp collectedExp : compounds)

-- | Collect an expression.
collectExp :: (MonadMessages Message m, MonadSymbols m) =>
              AST.Exp -> m Syntax.Exp
-- For a compound statement, construct a dynamic scope from all the statements.
collectExp AST.Compound { AST.compoundBody = body, AST.compoundPos = pos } =
  do
    (syntax, proofs, stms) <- foldM collectCompound emptyTempDynamicScope body
    return Syntax.Compound { Syntax.compoundSyntax = syntax,
                             Syntax.compoundProofs = proofs,
                             Syntax.compoundBody = reverse stms,
                             Syntax.compoundPos = pos }
-- For record values, gather up all the bindings into a HashMap.
collectExp AST.Record { AST.recordType = False, AST.recordFields = fields,
                        AST.recordPos = pos } =
  let
    -- Fold function.  Only build a HashMap.
    collectValueField :: (MonadMessages Message m, MonadSymbols m) =>
                         HashMap FieldName Syntax.Exp -> AST.Field ->
                         m (HashMap FieldName Syntax.Exp)
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
collectExp e @ AST.Seq { AST.seqPos = pos } =
  do
    collectedSeq <- collectSeq e
    return Syntax.Seq { Syntax.seqExps = collectedSeq, Syntax.seqPos = pos }
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
  return Syntax.Sym { Syntax.symName = sym, Syntax.symPos = pos }
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
                 [AST.Field] -> m Syntax.Fields
collectFields fields =
  let
    -- | Fold function.  Builds both a list and a HashMap.
    collectField :: (MonadMessages Message m, MonadSymbols m) =>
                    (HashMap FieldName Syntax.Field, [FieldName]) ->
                    AST.Field ->
                    m (HashMap FieldName Syntax.Field, [FieldName])
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
               AST.Case -> m Syntax.Case
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
               -> TempScope
               -- ^ The scope into which to accumulate definitions
               -> AST.Element
               -- ^ The element to collect
               -> m TempScope
-- For builder definitions, turn the definition into a 'Def', whose
-- initializer is an anonymous builder.
collectElement vis accum @ (builders, syntax, truths, proofs, elems)
               AST.Builder { AST.builderName = sym,
                             AST.builderKind = kind,
                             AST.builderParams = params,
                             AST.builderSuperTypes = supers,
                             AST.builderContent = content,
                             AST.builderPos = pos } =
  do
    collectedSupers <- mapM collectExp supers
    collectedParams <- collectFields params
    if HashMap.member sym builders
      then do
        duplicateBuilder sym pos
        return accum
      else case content of
        AST.Body body ->
          do
            collectedBody <- collectScope body
            return (HashMap.insert sym
                      Syntax.Builder {
                        Syntax.builderKind = kind,
                        Syntax.builderVisibility = vis,
                        Syntax.builderParams = collectedParams,
                        Syntax.builderSuperTypes = collectedSupers,
                        Syntax.builderContent =
                          Syntax.Anon { Syntax.anonKind = kind,
                                        Syntax.anonParams = collectedParams,
                                        Syntax.anonSuperTypes = collectedSupers,
                                        Syntax.anonContent = collectedBody,
                                        Syntax.anonPos = pos },
                        Syntax.builderPos = pos
                      } builders, syntax, truths, proofs, elems)
        AST.Value value ->
          do
            collectedValue <- collectExp value
            return (HashMap.insert sym
                      Syntax.Builder {
                        Syntax.builderKind = kind,
                        Syntax.builderVisibility = vis,
                        Syntax.builderParams = collectedParams,
                        Syntax.builderSuperTypes = collectedSupers,
                        Syntax.builderContent = collectedValue,
                        Syntax.builderPos = pos
                      } builders, syntax, truths, proofs, elems)
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
collectElement vis accum @ (builders, syntax, truths, proofs, elems)
               AST.Truth { AST.truthName = sym,
                           AST.truthKind = kind,
                           AST.truthContent = content,
                           AST.truthPos = pos } =
  do
    collectedContent <- collectExp content
    if HashMap.member sym truths
      then do
        duplicateTruth sym pos
        return accum
      else
        let
          truth = Syntax.Truth { Syntax.truthKind = kind,
                                 Syntax.truthVisibility = vis,
                                 Syntax.truthContent = collectedContent,
                                 Syntax.truthPos = pos }
          newtruths = HashMap.insert sym truth truths
        in
          return (builders, syntax, newtruths, proofs, elems)
collectElement _ (builders, syntax, truths, proofs, elems)
               AST.Syntax { AST.syntaxExp = exp } =
  do
    newsyntax <- collectSyntax syntax exp
    return (builders, newsyntax, truths, proofs, elems)
-- Proofs and imports get added to directives, and are processed in a
-- later phase.
collectElement _ (builders, syntax, truths, proofs, elems)
               AST.Proof { AST.proofName = sym, AST.proofBody = body,
                           AST.proofPos = pos } =
  do
    collectedName <- collectExp sym
    collectedBody <- collectExp body
    return (builders, syntax, truths,
            Syntax.Proof { Syntax.proofName = collectedName,
                           Syntax.proofBody = collectedBody,
                           Syntax.proofPos = pos } : proofs, elems)

-- | Collect a scope.  This gathers up all the defintions and truths into
-- HashMaps, and saves the imports and proofs.  Syntax directives then get
-- parsed and used to update definitions.
collectScope :: (MonadMessages Message m, MonadSymbols m) =>
                AST.Scope -> m Syntax.Scope
collectScope groups =
  let
    -- | Collect a group.  This rolls all elements of the group into the
    -- temporary scope, with the group's visibility.
    collectGroup :: (MonadMessages Message m, MonadSymbols m) =>
                    TempScope
                 -- ^ The 'Scope' into which to accumulate.
                 -> AST.Group
                 -- ^ The group
                 -> m TempScope
    collectGroup accum AST.Group { AST.groupVisibility = vis,
                                   AST.groupElements = elems } =
      foldM (collectElement vis) accum elems
  in do
    tmpscope <- foldM collectGroup emptyTempScope groups
    return $! makeScope tmpscope

-- | The Collect phase of the compiler.  Collect consumes an AST and
-- produces a Syntax structure.
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
-- * Failure of a file to contain an appropriately-named module definition.
-- * Duplicate record fields.
-- * Uninitialized definition without a top-level name (warning).
-- * Multiple truth definitions with the same names.
-- * Malformed syntax directives.
-- * Undefined symbols in syntax directives.
collectAST :: (MonadMessages Message m, MonadSymbols m) =>
              Position
           -- ^ Position corresponding to this entire file.
           -> Maybe [Symbol]
           -- ^ The name of a module that is expected to appear at the
           -- top level.
           -> AST.AST
           -- ^ The 'AST' to collect.
           -> m Syntax.Scope
collectAST filepos expected AST.AST { AST.astComponent = component,
                                      AST.astScope = scope } =
  let
    -- Grab the component name out of a Maybe Component
    actual =
      do
        val <- component
        return $! AST.componentName val

    -- We expect to see a module with the same name as the file (minus
    -- the .salt extension).  Track whether or not we've seen it.
    collectWithExpected :: (MonadMessages Message m, MonadSymbols m) =>
                           Symbol -> m Syntax.Scope
    collectWithExpected defname =
      let
        collect' (accum, _) elem @ AST.Builder { AST.builderName = modname }
          -- If we see the def we're looking for, save it as public.
          | modname == defname =
            do
              collected <- collectElement Public accum elem
              return (collected, True)
        collect' (accum, seen) elem =
          -- Otherwise, make it hidden
          do
            collected <- collectElement Hidden accum elem
            return (collected, seen)
      in do
        (tmpscope, seen) <- foldM collect' (emptyTempScope, False) scope
        -- Report an error if there was no top-level module definition
        -- with the name of the file.
        unless seen (noTopLevelDef defname filepos)
        return $! makeScope tmpscope

  in case (expected, actual) of
    (Just expected', _) ->
      -- We have an expected component name.  Get the expected
      -- definition from that, and assert that the actual component
      -- name is the same.
      let
        defname = last expected'
      in do
        unless (expected == actual) (badComponentName expected' actual filepos)
        collectWithExpected defname
    (Nothing, Just actual') ->
      -- We have no expected component name, but do have an actual
      -- one.  Get the expected definition name from the actual one.
      let
        defname = last actual'
      in
        collectWithExpected defname
    (Nothing, Nothing) ->
      -- We don't have anything.  There is no expected definition
      -- name, and everything at the top level is public.
      do
        accum <- foldM (collectElement Public) emptyTempScope scope
        return $! makeScope accum

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
              -> m (Strict.ByteString, Maybe Lazy.ByteString)
loadComponent cname pos =
  do
    fname <- componentFileName cname
    -- XXX Replace this with some sort of progress messages framework
    liftIO (Strict.putStr (Strict.concat ["Loaded ", fname, "\n"]))
    -- Call the loader to get the file contents.
    loaded <- load fname
    case loaded of
    -- If an error occurs while loading, report it.
      Left err ->
        let
          errstr = Strict.fromString $! ioeGetErrorString err
        in do
          if isDoesNotExistError err
            then cannotFindComponent cname pos
            else cannotAccessComponent cname fname errstr pos
          return (fname, Nothing)
      Right content -> return (fname, Just content)

collectComponent :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                     MonadMessages Message m, MonadGenpos m,
                     MonadCollect m, MonadSymbols m, MonadIO m) =>
                    (Strict.ByteString -> Lazy.ByteString -> m (Maybe AST.AST))
                 -- ^ The parsing function to use.
                 -> Position
                 -- ^ The position at which the reference to this
                 -- component occurred.
                 -> [Symbol]
                 -- ^ The name of the component to collect.
                 -> m ()
collectComponent parseFunc pos cname =
  let
    collectUse AST.Use { AST.useName = uname, AST.usePos = upos } =
      collectComponent parseFunc upos uname

    loadAndCollect =
      do
        -- Call the loader to get the file contents.
        (fname, loaded) <- loadComponent cname pos
        fpos <- file fname
        case loaded of
          Nothing -> addComponent cname emptyScope
          Just content ->
            -- Otherwise, continue
            do
              res <- parseFunc fname content
              case res of
                Just ast @ AST.AST { AST.astUses = uses } ->
                  do
                    scope <- collectAST fpos (Just cname) ast
                    addComponent cname scope
                    mapM_ collectUse uses
                Nothing -> addComponent cname emptyScope
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
         -> m (Maybe Lazy.ByteString)
loadFile fname pos =
  do
    -- Call the loader to get the file contents.
    loaded <- load fname
    -- XXX Replace this with some sort of progress messages framework
    liftIO (Strict.putStr (Strict.concat ["Loaded ", fname, "\n"]))
    case loaded of
    -- If an error occurs while loading, report it.
      Left err ->
        let
          errstr = Strict.fromString $! ioeGetErrorString err
        in do
          if isDoesNotExistError err
            then cannotFindFile fname pos
            else cannotAccessFile fname errstr pos
          return Nothing
      Right content -> return (Just content)

collectFile :: (MonadLoader Strict.ByteString Lazy.ByteString m,
                MonadMessages Message m, MonadGenpos m,
                MonadCollect m, MonadSymbols m, MonadIO m) =>
               (Strict.ByteString -> Lazy.ByteString -> m (Maybe AST.AST))
            -- ^ The parsing function to use.
            -> Position
            -- ^ The position at which the reference to this
            -- component occurred.
            -> Strict.ByteString
            -- ^ The name of the file to collect.
            -> m (Maybe Syntax.Scope)
collectFile parseFunc pos fname =
  let
    collectUse AST.Use { AST.useName = uname, AST.usePos = upos } =
      collectComponent parseFunc upos uname
  in do
    fpos <- file fname
    -- Call the loader to get the file contents.
    loaded <- loadFile fname pos
    case loaded of
      Nothing -> return Nothing
      Just content ->
        -- Otherwise, continue
        do
          res <- parseFunc fname content
          case res of
            Just ast @ AST.AST { AST.astUses = uses } ->
              do
                scope <- collectAST fpos Nothing ast
                mapM_ collectUse uses
                return (Just scope)
            Nothing -> return Nothing
