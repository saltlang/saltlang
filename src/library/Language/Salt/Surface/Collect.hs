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
{-# LANGUAGE OverloadedStrings #-}

module Language.Salt.Surface.Collect(
       collect
       ) where

import Control.Monad
import Control.Monad.Symbols
import Data.Array hiding (accum, elems)
import Data.HashMap.Strict(HashMap)
import Data.Map(Map)
--import Data.Monoid
import Data.Position
import Data.Symbol
import Language.Salt.Frontend
import Language.Salt.Message
import Language.Salt.Surface.Common
import Prelude hiding (elem, exp)

--import qualified Data.ByteString as Strict
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Language.Salt.Surface.AST as AST
import qualified Language.Salt.Surface.Syntax as Syntax

type DefScope = HashMap Symbol (Map Visibility [Syntax.Element])

type TruthScope = HashMap Symbol Syntax.Truth

type TempScope = (DefScope, TruthScope, [Syntax.Directive], [AST.Exp])

emptyTempScope :: TempScope
emptyTempScope = (HashMap.empty, HashMap.empty, [], [])

makeScope :: TempScope -> Frontend Syntax.Scope
makeScope (entries, truths, directives, syntaxes) =
  let
    makeScopeEntry :: Map Visibility [Syntax.Element] -> Syntax.Defs
    makeScopeEntry entry =
      let
        (lo, _) = Map.findMin entry
        (hi, _) = Map.findMax entry

        syntax = Syntax.Syntax { Syntax.syntaxFixity = Syntax.Prefix,
                                 Syntax.syntaxPrecs = [] }
      in
        Syntax.Defs { Syntax.defs = listArray (lo, hi) (Map.elems entry),
                      Syntax.defsSyntax = syntax }

    defscope = HashMap.map makeScopeEntry entries
  in do
    defs <- foldM collectSyntax defscope syntaxes
    return Syntax.Scope { Syntax.scopeDefs = defs,
                          Syntax.scopeTruths = truths,
                          Syntax.scopeDirectives = directives }

-- | Add a definition into a scope, merging it in with what's already there.
addDef :: Symbol -> Visibility -> Syntax.Element -> TempScope -> TempScope
addDef sym vis elem (defs, truths, directives, syntax) =
  (HashMap.insertWith (Map.unionWith (++)) sym (Map.singleton vis [elem]) defs,
   truths, directives, syntax)

-- | Collect a pattern, return the name to which the pattern is bound
-- at the top level.
collectNamedPattern :: AST.Pattern -> Frontend (Maybe Symbol, Syntax.Pattern)
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
collectEntry :: HashMap Symbol Syntax.Entry ->
                AST.Entry -> Frontend (HashMap Symbol Syntax.Entry)
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

collectPattern :: AST.Pattern -> Frontend Syntax.Pattern
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
collectSeq :: AST.Exp -> Frontend [Syntax.Exp]
collectSeq =
  let
    collectSeq' :: [Syntax.Exp] -> AST.Exp -> Frontend [Syntax.Exp]
    collectSeq' accum AST.Seq { AST.seqFirst = first,
                                AST.seqSecond = second @ AST.Seq {} } =
      do
        collectedFirst <- collectExp first
        collectSeq' (collectedFirst : accum) second
    collectSeq' accum e =
      do
        collectedExp <- collectExp e
        return $! reverse (collectedExp : accum)
  in
    collectSeq' []

-- | Collect an expression.
collectExp :: AST.Exp -> Frontend Syntax.Exp
-- For record values, gather up all the bindings into a HashMap.
collectExp AST.Record { AST.recordType = False, AST.recordFields = fields,
                        AST.recordPos = pos } =
  let
    -- Fold function.  Only build a HashMap.
    collectValueField :: HashMap FieldName Syntax.Exp -> AST.Field ->
                         Frontend (HashMap FieldName Syntax.Exp)
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
                            Syntax.projectName = fields,
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
    return Syntax.Builder { Syntax.builderKind = kind,
                            Syntax.builderParams = collectedParams,
                            Syntax.builderSuperTypes = collectedSupers,
                            Syntax.builderContent = collectedContent,
                            Syntax.builderPos = pos }
collectExp (AST.Literal lit) = return (Syntax.Literal lit)

-- | Collect a list of AST fields into a Syntax fields structure (a HashMap
-- and an ordering), and report duplicates.
collectFields :: [AST.Field] -> Frontend Syntax.Fields
collectFields fields =
  let
    -- | Fold function.  Builds both a list and a HashMap.
    collectField :: (HashMap FieldName Syntax.Field, [FieldName]) ->
                    AST.Field ->
                    Frontend (HashMap FieldName Syntax.Field, [FieldName])
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
collectCase :: AST.Case -> Frontend Syntax.Case
collectCase AST.Case { AST.casePat = pat, AST.caseBody = body,
                       AST.casePos = pos } =
  do
    collectedPat <- collectPattern pat
    collectedBody <- collectExp body
    return Syntax.Case { Syntax.casePat = collectedPat,
                         Syntax.caseBody = collectedBody,
                         Syntax.casePos = pos }

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
collectSyntax :: HashMap Symbol Syntax.Defs
              -- ^ The definition scope
              -> AST.Exp
              -- ^ The syntax directive expression
              -> Frontend (HashMap Symbol Syntax.Defs)
collectSyntax =
  let
    -- | Start of the whole syntax statement.
    --
    -- > syntax id (postfix | infix (left | right | nonassoc) |
    -- >        ^   prec (< | > | ==) id (. id)* )+
    start :: HashMap Symbol Syntax.Defs -> AST.Exp ->
             Frontend (HashMap Symbol Syntax.Defs)
    start defs AST.Seq { AST.seqFirst = AST.Sym { AST.symName = sym,
                                                  AST.symPos = sympos},
                         AST.seqSecond = body } =
      let
        -- | Add a precedence relationship to the current scope.
        addPrec :: HashMap Symbol Syntax.Defs -> Ordering -> AST.Exp ->
                   Frontend (HashMap Symbol Syntax.Defs)
        addPrec defs' ord exp =
          -- Look for the symbol in the scope.
          case HashMap.lookup sym defs' of
            -- If we find the definition, update it.
            Just d @ Syntax.Defs { Syntax.defsSyntax =
                                      s @ Syntax.Syntax {
                                        Syntax.syntaxPrecs = precs
                                      } } ->
              do
                -- Convert the expression from AST to Syntax.  We'll
                -- check its form later.
                collectedExp <- collectExp exp
                -- Add the precedence relation to the symbol definition.
                return $! HashMap.insert sym
                  d { Syntax.defsSyntax =
                         s { Syntax.syntaxPrecs =
                                (ord, collectedExp) : precs } } defs'
            -- We already reported undefined symbols.
            Nothing -> return defs'

        -- | Add a fixity to the current scope.  Report an error if
        -- the symbol already has a fixity other than prefix.
        addFixity :: HashMap Symbol Syntax.Defs -> Syntax.Fixity -> Position ->
                     Frontend (HashMap Symbol Syntax.Defs)
        addFixity defs' fixity pos =
          -- Look for the symbol in the scope.
          case HashMap.lookup sym defs' of
            -- If we find the definition, and its fixity is prefix, update it.
            Just d @ Syntax.Defs { Syntax.defsSyntax =
                                      s @ Syntax.Syntax {
                                        Syntax.syntaxFixity = Syntax.Prefix
                                      } } ->
              let
                newsyntax = s { Syntax.syntaxFixity = fixity }
              in
                return $! HashMap.insert sym
                                         d { Syntax.defsSyntax = newsyntax }
                                         defs'
            -- Otherwise report multiple fixities for this symbol.
            Just _ ->
              do
                multipleFixity pos
                return defs'
            -- We already reported undefined symbols.
            Nothing -> return defs'

        -- | Precedence identifier.
        --
        -- > syntax id prec (< | > | ==) id (. id)*
        -- >                             ^
        precName :: Ordering -> HashMap Symbol Syntax.Defs -> AST.Exp ->
                    Frontend (HashMap Symbol Syntax.Defs)
        -- If the top-level expression is a Seq, then there's another
        -- directive after this one.
        precName ord defs' AST.Seq { AST.seqFirst = exp,
                                     AST.seqSecond = rest } =
          do
            -- Add the precedence relation
            newdefs <- addPrec defs' ord exp
            -- Parse the next directive
            startDirective newdefs rest
        -- Otherwise, this is the last directive, so add the precedence relation.
        precName ord defs' exp = addPrec defs' ord exp

        -- | Precedence relation.
        --
        -- > syntax id prec (< | > | ==) id (. id)*
        -- >                ^
        precRelation :: HashMap Symbol Syntax.Defs -> AST.Exp ->
                        Frontend (HashMap Symbol Syntax.Defs)
        -- If the top-level expression is a Seq, then the first
        -- expression must be a Sym, which is the relationship.  We
        -- expect to see a Field or Sym next.
        precRelation defs' AST.Seq { AST.seqFirst =
                                        AST.Sym { AST.symName = kindsym,
                                                  AST.symPos = pos },
                                     AST.seqSecond = rest } =
          do
            kind <- name kindsym
            -- Get the precedence relationship from the symbol's name.
            case kind of
              "<" -> precName LT defs' rest
              ">" -> precName GT defs' rest
              "==" -> precName EQ defs' rest
              -- Not a valid precedence relation.
              _ ->
                do
                  badSyntaxPrec pos
                  return defs'
        -- The first expression must be a Sym, or else it's a parse error.
        precRelation defs' AST.Seq { AST.seqFirst = kind } =
          do
            badSyntaxPrec (AST.expPosition kind)
            return defs'
        -- The top-level expression must be a Seq, or else it's a parse error.
        precRelation defs' exp =
          do
            badSyntax (AST.expPosition exp)
            return defs'

        -- | Infix associativity
        --
        -- > syntax id infix (left | right | nonassoc) directive*
        -- >                 ^
        infixAssoc :: Position -> HashMap Symbol Syntax.Defs -> AST.Exp ->
                      Frontend (HashMap Symbol Syntax.Defs)
        infixAssoc fixitypos defs'
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
                  newdefs <- addFixity defs' (Syntax.Infix Syntax.Left)
                                       fixitypos
                  startDirective newdefs rest
              "right" ->
                do
                  newdefs <- addFixity defs' (Syntax.Infix Syntax.Right)
                                       fixitypos
                  startDirective newdefs rest
              "nonassoc" ->
                do
                  newdefs <- addFixity defs' (Syntax.Infix Syntax.NonAssoc)
                                       fixitypos
                  startDirective newdefs rest
              -- Not a valid fixity.
              _ ->
                do
                  badSyntaxAssoc pos
                  return defs'
        -- If the first expression isn't a Sym, it's a parse error.
        infixAssoc _ defs' AST.Seq { AST.seqFirst = kind } =
          do
            badSyntaxAssoc (AST.expPosition kind)
            return defs'
        -- If the top-level expression is a Sym, it must be the
        -- associativity, and this is the last directive.
        infixAssoc fixitypos defs' AST.Sym { AST.symName = kindsym,
                                             AST.symPos = pos } =
          do
            kind <- name kindsym
            -- Get the associativity from the symbol's name, then add
            -- the infix fixity to the scope.
            case kind of
              "left" -> addFixity defs' (Syntax.Infix Syntax.Left) fixitypos
              "right" -> addFixity defs' (Syntax.Infix Syntax.Right) fixitypos
              "nonassoc" ->
                addFixity defs' (Syntax.Infix Syntax.NonAssoc) fixitypos
              -- Not a valid fixity.
              _ ->
                do
                  badSyntaxAssoc pos
                  return defs'
        -- If the top-level expression isn't a Seq or a Sym, then it's
        -- a parse error.
        infixAssoc _ defs' exp =
          do
            badSyntax (AST.expPosition exp)
            return defs'

        -- | Start of one directive.
        --
        -- > syntax id (postfix | infix (left | right | nonassoc) |
        -- >            prec (< | > | ==) id (. id)* )+
        -- >           ^
        startDirective :: HashMap Symbol Syntax.Defs -> AST.Exp ->
                          Frontend (HashMap Symbol Syntax.Defs)
        -- The first expression is a Sym (the kind), and there's more.
        startDirective defs' AST.Seq { AST.seqFirst =
                                          AST.Sym { AST.symName = kindsym,
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
                  newdefs <- addFixity defs' Syntax.Postfix pos
                  startDirective newdefs rest
              -- Infix expects to see an associativity next.
              "infix" -> infixAssoc pos defs' rest
              -- Precedence relations expect more content.
              "prec" -> precRelation defs' rest
              -- This wasn't a known syntax directive
              _ ->
                do
                  badSyntaxKind pos
                  return defs'
        -- If the first expression isn't a Sym, it's a parse error.
        startDirective defs' AST.Seq { AST.seqFirst = kind } =
          do
            badSyntaxKind (AST.expPosition kind)
            return defs'
        -- It's ok for the top-level to be a Sym, as long as it's "postfix".
        startDirective defs' AST.Sym { AST.symName = kindsym,
                                       AST.symPos = pos } =
          do
            kind <- name kindsym
            case kind of
              "postfix" -> addFixity defs' Syntax.Postfix pos
              -- Infixes expect an associativity
              "infix" ->
                do
                  badSyntax pos
                  return defs'
              -- Precedence relations expect more
              "prec" ->
                do
                  badSyntax pos
                  return defs'
              -- This wasn't a known syntax directive
              _ ->
                do
                  badSyntaxKind pos
                  return defs'
        -- If the top-level expression isn't a Seq or a Sym, then it's
        -- a parse error.
        startDirective defs' exp =
          do
            badSyntax (AST.expPosition exp)
            return defs'
      in do
        -- Check that the symbol is actually defined
        unless (HashMap.member sym defs) (undefSymbol sym sympos)
        -- Parse one directive.
        startDirective defs body
    -- If the first expression isn't a Sym, then it's a parse error.
    start defs AST.Seq { AST.seqFirst = kind } =
      do
        badSyntaxName (AST.expPosition kind)
        return defs
    -- If the top-level expression isn't a Seq, then it's a parse error.
    start defs exp =
      do
        badSyntax (AST.expPosition exp)
        return defs
  in
    start

-- | Collect a definition.
collectElement :: Visibility
               -- ^ The visibility at which everything is defined.
               -> TempScope
               -- ^ The scope into which to accumulate definitions
               -> AST.Element
               -- ^ The element to collect
               -> Frontend TempScope
-- For builder definitions, turn the definition into a 'Def', whose
-- initializer is an anonymous builder.
collectElement vis accum AST.Builder { AST.builderName = sym,
                                       AST.builderKind = kind,
                                       AST.builderParams = params,
                                       AST.builderSuperTypes = supers,
                                       AST.builderContent = AST.Body body,
                                       AST.builderPos = pos } =
  do
    collectedSupers <- mapM collectExp supers
    collectedParams <- collectFields params
    collectedBody <- collectScope body
    return $! addDef sym vis
                     Syntax.Def {
                       Syntax.defInit =
                          Just Syntax.Builder {
                                 Syntax.builderKind = kind,
                                 Syntax.builderParams = collectedParams,
                                 Syntax.builderSuperTypes = collectedSupers,
                                 Syntax.builderContent = collectedBody,
                                 Syntax.builderPos = pos
                               },
                       Syntax.defPos = pos
                     } accum
-- Defs are the most involved element to collect.  We need to walk
-- down the pattern, creating definitions for every Name we find.  The
-- content of these definitions is a projection of the earlier name
-- bindings.
--
-- We also generate a warning for an uninitialized def without a
-- top-level name binding.

-- For Funs, we do the same kind of transformation we did with
-- Builders: create a 'Def' whose initializer is an anonymous function.
collectElement vis accum AST.Fun { AST.funName = sym, AST.funCases = cases,
                                   AST.funPos = pos } =
  do
    collectedCases <- mapM collectCase cases
    return $! addDef sym vis
                     Syntax.Def {
                       Syntax.defInit =
                          Just Syntax.Abs {
                                 Syntax.absKind = Lambda,
                                 Syntax.absCases = collectedCases,
                                 Syntax.absPos = pos
                               },
                       Syntax.defPos = pos
                     } accum
-- Truth definitions are a straghtforward translation.
collectElement vis accum @ (defs, truths, directives, syntax)
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
        in
          return (defs, HashMap.insert sym truth truths, directives, syntax)
-- Syntax directives get stashed in a list and processed at the end of the
-- scope.
collectElement _ (defs, truths, directives, syntax)
               AST.Syntax { AST.syntaxExp = exp } =
  return (defs, truths, directives, exp : syntax)
-- Proofs and imports get added to directives, and are processed in a
-- later phase.
collectElement _ (defs, truths, directives, syntax)
               AST.Proof { AST.proofName = sym, AST.proofBody = body,
                           AST.proofPos = pos } =
  do
    collectedName <- collectExp sym
    collectedBody <- collectExp body
    return (defs, truths,
            Syntax.Proof { Syntax.proofName = collectedName,
                           Syntax.proofBody = collectedBody,
                           Syntax.proofPos = pos } : directives, syntax)
collectElement _ (defs, truths, directives, syntax)
               AST.Import { AST.importExp = exp, AST.importPos = pos } =
  do
    collectedExp <- collectExp exp
    return (defs, truths,
            Syntax.Import { Syntax.importExp = collectedExp,
                            Syntax.importPos = pos } : directives, syntax)

-- | Collect a scope.  This gathers up all the defintions and truths into
-- HashMaps, and saves the imports and proofs.  Syntax directives then get
-- parsed and used to update definitions.
collectScope :: AST.Scope -> Frontend Syntax.Scope
collectScope groups =
  let
    -- | Collect a group.  This rolls all elements of the group into the
    -- temporary scope, with the group's visibility.
    collectGroup :: TempScope
                 -- ^ The 'Scope' into which to accumulate.
                 -> AST.Group
                 -- ^ The group
                 -> Frontend TempScope
    collectGroup accum AST.Group { AST.groupVisibility = vis,
                                   AST.groupElements = elems } =
      foldM (collectElement vis) accum elems
  in do
    tmpscope <- foldM collectGroup emptyTempScope groups
    makeScope tmpscope

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
collect :: Position
        -- ^ Position corresponding to this entire file.
        -> Symbol
        -- ^ The name of a module that is expected to appear at the top level.
        -> AST.AST
        -- ^ The 'AST' to collect.
        -> Frontend Syntax.Scope
collect filepos expected AST.AST { AST.astScope = scope } =
  let
    -- We expect to see a module with the same name as the file (minus
    -- the .salt extension).  Track whether or not we've seen it.
    collect' :: (TempScope, Bool) -> AST.Element ->
                Frontend (TempScope, Bool)
    collect' (accum, _) elem @ AST.Builder { AST.builderKind = Module,
                                             AST.builderName = modname }
      | modname == expected =
        do
          collected <- collectElement Public accum elem
          return (collected, True)
    collect' (accum, seen) elem =
      do
        collected <- collectElement Hidden accum elem
        return (collected, seen)
  in do
    (tmpscope, seen) <- foldM collect' (emptyTempScope, False) scope
    -- Report an error if there was no top-level module definition
    -- with the name of the file.
    unless seen (noTopLevelDef expected filepos)
    makeScope tmpscope
