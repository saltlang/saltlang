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

module Language.Salt.Surface.Collect(
       collect
       ) where

import Control.Monad
import Data.Array hiding (accum, elems)
import Data.HashMap.Strict(HashMap)
import Data.Map(Map)
--import Data.Monoid
import Data.Position
import Data.Symbol
import Language.Salt.Frontend
import Language.Salt.Message
import Language.Salt.Surface.Common
import Prelude hiding (elem)

--import qualified Data.ByteString as Strict
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Language.Salt.Surface.AST as AST
import qualified Language.Salt.Surface.Syntax as Syntax

type TempScope = HashMap Symbol (Map Visibility [Syntax.Element])

makeScope :: TempScope -> Syntax.Scope
makeScope =
  let
    makeScopeEntry :: Map Visibility [Syntax.Element] -> Syntax.Defs
    makeScopeEntry entry =
      let
        (lo, _) = Map.findMin entry
        (hi, _) = Map.findMax entry
      in
        Syntax.Defs { Syntax.defs = listArray (lo, hi) (Map.elems entry) }
  in
    Syntax.Scope . HashMap.map makeScopeEntry

addEntry :: Symbol -> Visibility -> Syntax.Element -> TempScope -> TempScope
addEntry sym vis elem = HashMap.insertWith (Map.unionWith (++)) sym
                                           (Map.singleton vis [elem])


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
-- For split, collect all fields into a map
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

-- | Collect a bunch of 'Seq's
collectSeq :: AST.Exp -> Frontend [Syntax.Exp]
collectSeq AST.Seq { AST.seqFirst = first,
                     AST.seqSecond = second @ AST.Seq {} } =
  do
    collectedFirst <- collectExp first
    collectedRest <- collectSeq second
    return (collectedFirst : collectedRest)
collectSeq e =
  do
    collectedExp <- collectExp e
    return [collectedExp]

collectValueField :: HashMap Symbol Syntax.Exp -> AST.Field ->
                     Frontend (HashMap Symbol Syntax.Exp)
collectValueField accum AST.Field { AST.fieldName = sym, AST.fieldVal = val,
                                    AST.fieldPos = pos } =
  do
    collectedVal <- collectExp val
    if HashMap.member sym accum
      then do
        duplicateField sym pos
        return accum
      else return $! HashMap.insert sym collectedVal accum

-- | Collect an expression.
collectExp :: AST.Exp -> Frontend Syntax.Exp
collectExp AST.Record { AST.recordType = False, AST.recordFields = fields,
                        AST.recordPos = pos } =
  do
    collectedFields <- foldM collectValueField HashMap.empty fields
    return Syntax.Record { Syntax.recordFields = collectedFields,
                           Syntax.recordPos = pos }
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
collectExp AST.Project { AST.projectVal = val, AST.projectName = sym,
                         AST.projectPos = pos } =
  do
    collectedVal <- collectExp val
    return Syntax.Project { Syntax.projectVal = collectedVal,
                            Syntax.projectName = sym,
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

collectField :: (HashMap Symbol Syntax.Field, [Symbol]) -> AST.Field ->
                Frontend (HashMap Symbol Syntax.Field, [Symbol])
collectField (tab, fields) AST.Field { AST.fieldName = sym, AST.fieldVal = val,
                                       AST.fieldPos = pos } =
  do
    collectedVal <- collectExp val
    if HashMap.member sym tab
      then do
        duplicateField sym pos
        return (tab, fields)
      else let
          field = Syntax.Field { Syntax.fieldVal = collectedVal,
                                 Syntax.fieldPos = pos }
        in
          return (HashMap.insert sym field tab, sym : fields)

collectFields :: [AST.Field] -> Frontend Syntax.Fields
collectFields fields =
  do
    (tab, fieldlist) <- foldM collectField (HashMap.empty, []) fields
    return Syntax.Fields {
             Syntax.fieldsOrder = listArray (1, fromIntegral (length fieldlist))
                                            fieldlist,
             Syntax.fieldsBindings = tab
           }

-- | Collect a case.
collectCase :: AST.Case -> Frontend Syntax.Case
collectCase AST.Case { AST.casePat = pat, AST.caseBody = body,
                       AST.casePos = pos } =
  do
    collectedPat <- collectPattern pat
    collectedBody <- collectExp body
    return Syntax.Case { Syntax.casePat = collectedPat,
                         Syntax.caseBody = collectedBody,
                         Syntax.casePos = pos }

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
    return $! addEntry sym vis
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
    return $! addEntry sym vis
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
collectElement vis accum AST.Truth { AST.truthName = sym, AST.truthKind = kind,
                                     AST.truthContent = content,
                                     AST.truthPos = pos } =
  do
    collectedContent <- collectExp content
    return $! addEntry sym vis Syntax.Truth {
                                 Syntax.truthKind = kind,
                                 Syntax.truthContent = collectedContent,
                                 Syntax.truthPos = pos
                               } accum

collectScope :: AST.Scope -> Frontend Syntax.Scope
collectScope groups =
  let
    -- | Collect a group
    collectGroup :: TempScope
                 -- ^ The 'Scope' into which to accumulate.
                 -> AST.Group
                 -- ^ The group
                 -> Frontend TempScope
    collectGroup accum AST.Group { AST.groupVisibility = vis,
                                   AST.groupElements = elems } =
      foldM (collectElement vis) accum elems
  in do
    tmpscope <- foldM collectGroup HashMap.empty groups
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
--
-- Collect can issue several messages:
-- * Failure of a file to contain an appropriately-named module definition.
-- * Duplicate record fields.
-- * Uninitialized definition without a top-level name (warning).
collect :: Position
        -- ^ Position corresponding to this entire file.
        -> Symbol
        -- ^ The name of a module that is expected to appear at the top level.
        -> AST.AST
        -- ^ The 'AST' to collect.
        -> Frontend Syntax.Scope
collect filepos expected ast =
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
    (tmpscope, seen) <- foldM collect' (HashMap.empty, False) ast
    -- Report an error if there was no top-level module definition
    -- with the name of the file.
    unless seen (noTopLevelDef expected filepos)
    return $! makeScope tmpscope
