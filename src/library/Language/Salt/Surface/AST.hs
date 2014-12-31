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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- | The Abstract Syntax Tree, as yielded by a parser.  This is
-- rendered into Absyn by Collect, which gathers all entries
-- into a table.
--
-- Note that this structure isn't meant to be processed in any truly
-- meaningful way.
module Language.Salt.Surface.AST(
       BuilderKind(..),
       TruthKind(..),
       Visibility(..),
       AST(..),
       Use(..),
       Group(..),
       Content(..),
       Scope,
       Element(..),
       Compound(..),
       Pattern(..),
       Exp(..),
       Field(..),
       Entry(..),
       Case(..),
       elementPosition,
       compoundPosition,
       patternPosition,
       expPosition,
       casePosition,
       -- * Graphviz Renderer
       astDot
       ) where

import Control.Monad
import Control.Monad.Positions
import Control.Monad.State
import Control.Monad.Symbols
import Data.Hashable
import Data.List hiding (init)
import Data.Position
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common
import Prelude hiding (sequence, init, exp)
import Text.FormatM
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict

-- | Type of an AST.
data AST =
  AST {
    -- | The headers.
    astUses :: ![Use],
    -- | The top-level scope.
    astScope :: ![Element]
  }
  deriving (Ord, Eq)

-- | Use directives.  These import content from another file.
data Use =
  -- | A use header.
  Use {
    -- | The qualified name in the use directive.
    useName :: ![Symbol],
    -- | The position in source from which this arises.
    usePos :: !Position
  }

-- | Represents a group of definitions with a given visibility.
--
-- Corresponds to the syntax
-- > (visibility): def1 def2 ...
data Group =
  Group {
    -- | The visibility of the definitions.
    groupVisibility :: !Visibility,
    -- | The definitions in the group.
    groupElements :: ![Element],
    -- | The position in source from which this arises.
    groupPos :: !Position
  }

-- | Content of a definition.  Used wherever we can see a definition
-- body, or else an expression.
data Content =
    -- | An actual definition body.
    Body !Scope
    -- | An expression.
  | Value !Exp

-- | Type of a scope.
type Scope = [Group]

-- | Scope elements.  These represent declarations, imports, and truth
-- statements inside a scope.  Note: some of these can be built from
-- others.
data Element =
    -- | Scope entities.  This is a common structure for a number of
    -- entity declarations that all have the same structure: modules,
    -- signatures, and classes.
    Builder {
      -- | The name of the builder.
      builderName :: !Symbol,
      -- | The type of entity the builder represents.
      builderKind :: !BuilderKind,
      -- | The parameters of the builder entity.
      builderParams :: ![Field],
      -- | The declared supertypes for this builder entity.
      builderSuperTypes :: ![Exp],
      -- | The entities declared by the builder.
      builderContent :: !Content,
      -- | The position in source from which this arises.
      builderPos :: !Position
    }
    -- | Value definitions.  These are declarations coupled with
    -- values.  These include function declarations.
  | Def {
      -- | The pattern for the definition
      defPattern :: !Pattern,
      -- | The value's initializer.
      defInit :: !(Maybe Exp),
      -- | The position in source from which this arises.
      defPos :: !Position
    }
    -- | A function definition.
  | Fun {
      -- | The name of the function.
      funName :: !Symbol,
      -- | The cases, in order.
      funCases :: ![Case],
      -- | The position in source from which this arises.
      funPos :: !Position
    }
    -- | Truths.  These are similar to declarations, except that they
    -- affect the proof environment.  These include theorems and
    -- invariants.
  | Truth {
      -- | The class of the truth.
      truthKind :: !TruthKind,
      -- | The name of the truth.
      truthName :: !Symbol,
      -- | The truth proposition.
      truthContent :: !Exp,
      -- | The position in source from which this arises.
      truthPos :: !Position
    }
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
  | Proof {
      -- | The name of the theorem being proven.
      proofName :: !Exp,
      -- | The body of the proof.
      proofBody :: !Exp,
      -- | The position in source from which this arises.
      proofPos :: !Position
    }
  | Import {
      -- | The value to import.
      importExp :: !Exp,
      -- | The position in source from which this arises.
      importPos :: !Position
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound =
    -- | An ordinary expression.
    Exp !Exp
    -- | A definition.
  | Element !Element

-- | A pattern, for pattern match expressions.
data Pattern =
    Option {
      -- | The option patterns
      optionPats :: ![Pattern],
      -- | The position in source from which this arises.
      optionPos :: !Position
    }
    -- | A deconstructor pattern.  Mirrors a call expression.
  | Deconstruct {
      -- | The name of the constructor.
      deconstructName :: !Symbol,
      -- | The arguments to the constructor.
      deconstructPat :: !Pattern,
      -- | The position in source from which this arises.
      deconstructPos :: !Position
    }
    -- | A projection.  Mirrors a record expression.
  | Split {
      -- | The fields being projected.
      splitFields :: ![Entry],
      -- | Whether or not the binding is strict (ie. it omits some fields)
      splitStrict :: !Bool,
      -- | The position in source from which this arises.
      splitPos :: !Position
    }
    -- | A typed pattern.  Fixes the type of a pattern.
  | Typed {
      -- | The pattern whose type is being fixed.
      typedPat :: !Pattern,
      -- | The type to which the pattern is fixed.
      typedType :: !Exp,
      -- | The position in source from which this arises.
      typedPos :: !Position
    }
    -- | An as pattern.  Captures part of a pattern as a name, but
    -- continues to match.
    --
    -- Corresponds to the syntax
    -- > name as (pattern)
  | As {
      -- | The name to bind.
      asName :: !Symbol,
      -- | The sub-pattern to match.
      asPat :: !Pattern,
      -- | The position in source from which this arises.
      asPos :: !Position
    }
    -- | A name binding.  Binds the given name to the contents.
  | Name {
      -- | The name to bind.
      nameSym :: !Symbol,
      -- | The position in source from which this arises.
      namePos :: !Position
    }
  | Exact !Literal

-- | Expressions.  These represent computed values of any type.
data Exp =
    -- | An expression that may contain declarations as well as
    -- expressions.
    Compound {
      -- | The body of the compound expression.
      compoundBody :: ![Compound],
      -- | The position in source from which this arises.
      compoundPos :: !Position
    }
    -- | An abstraction.  Generally represents anything with
    -- parameters and cases.  Specifically, represents lambdas,
    -- foralls, and exists.
  | Abs {
      -- | The kind of the abstraction.
      absKind :: !AbstractionKind,
      -- | The cases for the abstraction.
      absCases :: ![Case],
      -- | The position in source from which this arises.
      absPos :: !Position
    }
    -- | Match statement.  Given a set of (possibly typed) patterns,
    -- find the meet of all types above the argument's type in the
    -- cases, then apply the first pattern for the meet type that
    -- matches the argument.
  | Match {
      -- | The argument to the match statement.
      matchVal :: !Exp,
      -- | The cases, in order.
      matchCases :: ![Case],
      -- | The position in source from which this arises.
      matchPos :: !Position
    }
    -- | Ascribe expression.  Fixes the type of a given expression.
  | Ascribe {
      -- | The expression whose type is being set.
      ascribeVal :: !Exp,
      -- | The type.
      ascribeType :: !Exp,
      -- | The position in source from which this arises.
      ascribePos :: !Position
    }
    -- | A sequence expressions.  The entire sequence represents a
    -- function call, possibly with inorder symbols.  This is
    -- re-parsed once the inorder symbols are known.
  | Seq {
      -- | The first expression.
      seqFirst :: !Exp,
      -- | The second expression.
      seqSecond :: !Exp,
      -- | The position in source from which this arises.
      seqPos :: !Position
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | Whether this represents a record type or a record value.
      recordType :: !Bool,
      -- | The fields in this record expression.
      recordFields :: ![Field],
      -- | The position in source from which this arises.
      recordPos :: !Position
    }
    -- | A tuple literal.
  | Tuple {
      -- | The fields in this tuple expression.
      tupleFields :: ![Exp],
      -- | The position in source from which this arises.
      tuplePos :: !Position
    }
    -- | A field expression.  Accesses the given field in a record.
    -- Note that for non-static functions, this implies an extra
    -- argument.
  | Project {
      -- | The inner expression
      projectVal :: !Exp,
      -- | The name(s) of the field(s) being accessed.
      projectFields :: ![FieldName],
      -- | The position in source from which this arises.
      projectPos :: !Position
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symName :: !Symbol,
      -- | The position in source from which this arises.
      symPos :: !Position
    }
    -- | A with expression.  Represents currying.
  | With {
      -- | The value to which some arguments are being applied.
      withVal :: !Exp,
      -- | The argument(s) to apply
      withArgs :: !Exp,
      -- | The position in source from which this arises.
      withPos :: !Position
    }
    -- | Where expression.  Constructs refinement types.
  | Where {
      -- | The value to which some arguments are being applied.
      whereVal :: !Exp,
      -- | The argument(s) to apply
      whereProp :: !Exp,
      -- | The position in source from which this arises.
      wherePos :: !Position
    }
    -- | Anonymous builder.
  | Anon {
      -- | The type of entity the builder represents.
      anonKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      anonSuperTypes :: ![Exp],
      -- | The parameters of the builder entity.
      anonParams :: ![Field],
      -- | The entities declared by the builder.
      anonContent :: !Scope,
      -- | The position in source from which this arises.
      anonPos :: !Position
    }
    -- | Number literal.
  | Literal !Literal

-- | An entry in a record field or call argument list.
data Entry =
    -- | A named field.  This sets a specific field or argument to a
    -- certain value.
    Named {
      -- | The name of the field or argument being set.
      namedSym :: !Symbol,
      -- | The value to which the field or argument is set.
      namedVal :: !Pattern,
      -- | The position in source from which this arises.
      namedPos :: !Position
     }
    -- | Unnamed field.  This is just an expression.
  | Unnamed !Pattern

-- | A field.
data Field =
  Field {
    -- | The name being bound.
    fieldName :: !FieldName,
    -- | The value assigned to the bound name.
    fieldVal :: !Exp,
    -- | The position in source from which this arises.
    fieldPos :: !Position
  }

-- | A case in a match statement or a function definition.
data Case =
  Case {
    -- | The pattern to match for this case.
    casePat :: !Pattern,
    -- | The expression to execute for this case.
    caseBody :: !Exp,
    -- | The position in source from which this arises.
    casePos :: !Position
  }

elementPosition :: Element -> Position
elementPosition Builder { builderPos = pos } = pos
elementPosition Def { defPos = pos } = pos
elementPosition Fun { funPos = pos } = pos
elementPosition Truth { truthPos = pos } = pos
elementPosition Proof { proofPos = pos } = pos
elementPosition Import { importPos = pos } = pos

patternPosition :: Pattern -> Position
patternPosition Option { optionPos = pos } = pos
patternPosition Deconstruct { deconstructPos = pos } = pos
patternPosition Split { splitPos = pos } = pos
patternPosition Typed { typedPos = pos } = pos
patternPosition As { asPos = pos } = pos
patternPosition Name { namePos = pos } = pos
patternPosition (Exact l) = literalPosition l

compoundPosition :: Compound -> Position
compoundPosition (Exp e) = expPosition e
compoundPosition (Element e) = elementPosition e

expPosition :: Exp -> Position
expPosition Compound { compoundPos = pos } = pos
expPosition Abs { absPos = pos } = pos
expPosition Match { matchPos = pos } = pos
expPosition Ascribe { ascribePos = pos } = pos
expPosition Seq { seqPos = pos } = pos
expPosition Record { recordPos = pos } = pos
expPosition Tuple { tuplePos = pos } = pos
expPosition Project { projectPos = pos } = pos
expPosition Sym { symPos = pos } = pos
expPosition With { withPos = pos } = pos
expPosition Where { wherePos = pos } = pos
expPosition Anon { anonPos = pos } = pos
expPosition (Literal l) = literalPosition l

casePosition :: Case -> Position
casePosition Case { casePos = pos } = pos

instance Eq Use where
  Use { useName = name1 } == Use { useName = name2 } = name1 == name2

instance Eq Group where
  Group { groupVisibility = vis1, groupElements = elems1 } ==
    Group { groupVisibility = vis2, groupElements = elems2 } =
      vis1 == vis2 && elems1 == elems2

instance Eq Content where
  Body b1 == Body b2 = b1 == b2
  Value v1 == Value v2 = v1 == v2
  _ == _ = False

instance Eq Element where
  Builder { builderName = name1, builderKind = cls1, builderParams = params1,
          builderSuperTypes = supers1, builderContent = body1 } ==
    Builder { builderName = name2, builderKind = cls2, builderParams = params2,
            builderSuperTypes = supers2, builderContent = body2 } =
    name1 == name2 && cls1 == cls2 && params1 == params2 &&
    supers1 == supers2 && body1 == body2
  Def { defPattern = pat1, defInit = init1 } ==
    Def { defPattern = pat2, defInit = init2 } =
      pat1 == pat2 && init1 == init2
  Fun { funName = name1, funCases = cases1 } ==
    Fun { funName = name2, funCases = cases2 } =
      name1 == name2 && cases1 == cases2
  Truth { truthName = name1, truthKind = kind1, truthContent = prop1 } ==
    Truth { truthName = name2, truthKind = kind2, truthContent = prop2 } =
    name1 == name2 && kind1 == kind2 && prop1 == prop2
  Proof { proofName = name1, proofBody = body1 } ==
    Proof { proofName = name2, proofBody = body2 } =
      name1 == name2 && body1 == body2
  Import { importExp = exp1 } == Import { importExp = exp2 } = exp1 == exp2
  _ == _ = False

instance Eq Compound where
  Exp e1 == Exp e2 = e1 == e2
  Element e1 == Element e2 = e1 == e2
  _ == _ = False

instance Eq Pattern where
  Option { optionPats = pats1 } == Option { optionPats = pats2 } =
    pats1 == pats2
  Deconstruct { deconstructName = name1, deconstructPat = pat1 } ==
    Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Split { splitFields = fields1, splitStrict = strict1 } ==
    Split { splitFields = fields2, splitStrict = strict2 } =
      fields1 == fields2 && strict1 == strict2
  Typed { typedPat = pat1, typedType = ty1 } ==
    Typed { typedPat = pat2, typedType = ty2 } =
      pat1 == pat2 && ty1 == ty2
  As { asName = name1, asPat = pat1 } == As { asName = name2, asPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Name { nameSym = name1 } == Name { nameSym = name2 } = name1 == name2
  Exact e1 == Exact e2 = e1 == e2
  _ == _ = False

instance Eq Exp where
  Compound { compoundBody = body1 } == Compound { compoundBody = body2 } =
    body1 == body2
  Abs { absKind = kind1, absCases = cases1 } ==
    Abs { absKind = kind2, absCases = cases2 } =
      kind1 == kind2 && cases1 == cases2
  Match { matchVal = val1, matchCases = cases1 } ==
    Match { matchVal = val2, matchCases = cases2 } =
    val1 == val2 && cases1 == cases2
  Ascribe { ascribeVal = val1, ascribeType = ty1 } ==
    Ascribe { ascribeVal = val2, ascribeType = ty2 } =
    val1 == val2 && ty1 == ty2
  Seq { seqFirst = first1, seqSecond = second1 } ==
    Seq { seqFirst = first2, seqSecond = second2 } =
      first1 == first2 && second1 == second2
  Record { recordFields = fields1, recordType = ty1 } ==
    Record { recordFields = fields2, recordType = ty2 } =
      ty1 == ty2 && fields1 == fields2
  Tuple { tupleFields = fields1 } == Tuple { tupleFields = fields2 } =
    fields1 == fields2
  Project { projectVal = val1, projectFields = name1 } ==
    Project { projectVal = val2, projectFields = name2 } =
      name1 == name2 && val1 == val2
  Sym { symName = name1 } == Sym { symName = name2 } = name1 == name2
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
  Where { whereVal = val1, whereProp = prop1 } ==
    Where { whereVal = val2, whereProp = prop2 } =
      val1 == val2 && prop1 == prop2
  Anon { anonKind = cls1, anonParams = params1,
         anonSuperTypes = supers1, anonContent = body1 } ==
    Anon { anonKind = cls2, anonParams = params2,
           anonSuperTypes = supers2, anonContent = body2 } =
      cls1 == cls2 && params1 == params2 &&
      supers1 == supers2 && body1 == body2
  Literal lit1 == Literal lit2 = lit1 == lit2
  _ == _ = False

instance Eq Entry where
  Named { namedSym = name1, namedVal = val1 } ==
    Named { namedSym = name2, namedVal = val2 } =
    name1 == name2 && val1 == val2
  Unnamed e1 == Unnamed e2 = e1 == e2
  _ == _ = False

instance Eq Field where
  Field { fieldName = name1, fieldVal = val1 } ==
    Field { fieldName = name2, fieldVal = val2 } =
    name1 == name2 && val1 == val2

instance Eq Case where
  Case { caseBody = body1, casePat = pat1 } ==
    Case { caseBody = body2, casePat = pat2 } =
      body1 == body2 && pat1 == pat2

instance Ord Use where
  compare Use { useName = name1 } Use { useName = name2 } =
    compare name1 name2

instance Ord Group where
  compare Group { groupVisibility = vis1, groupElements = elems1 }
          Group { groupVisibility = vis2, groupElements = elems2 } =
      case compare vis1 vis2 of
        EQ -> compare elems1 elems2
        out -> out

instance Ord Content where
  compare (Body b1) (Body b2) = compare b1 b2
  compare (Body _) _ = GT
  compare _ (Body _) = LT
  compare (Value v1) (Value v2) = compare v1 v2

instance Ord Element where
  compare Builder { builderName = name1, builderKind = cls1,
                    builderParams = params1, builderSuperTypes = supers1,
                    builderContent = body1 }
          Builder { builderName = name2, builderKind = cls2,
                    builderParams = params2, builderSuperTypes = supers2,
                    builderContent = body2 } =
    case compare name1 name2 of
      EQ -> case compare cls1 cls2 of
        EQ -> case compare params1 params2 of
          EQ -> case compare supers1 supers2 of
            EQ -> compare body1 body2
            out -> out
          out -> out
        out -> out
      out -> out
  compare Builder {} _ = GT
  compare _ Builder {} = LT
  compare Def { defPattern = pat1, defInit = init1 }
          Def { defPattern = pat2, defInit = init2 } =
    case compare pat1 pat2 of
      EQ -> compare init1 init2
      out -> out
  compare Def {} _ = GT
  compare _ Def {} = LT
  compare Fun { funName = name1, funCases = cases1 }
          Fun { funName = name2, funCases = cases2 } =
    case compare name1 name2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Fun {} _ = GT
  compare _ Fun {} = LT
  compare Truth { truthName = name1, truthKind = kind1, truthContent = prop1 }
          Truth { truthName = name2, truthKind = kind2, truthContent = prop2 } =
    case compare name1 name2 of
      EQ -> case compare kind1 kind2 of
        EQ -> compare prop1 prop2
        out -> out
      out -> out
  compare Truth {} _ = GT
  compare _ Truth {} = LT
  compare Proof { proofName = name1, proofBody = body1 }
          Proof { proofName = name2, proofBody = body2 } =
    case compare name1 name2 of
      EQ -> compare body1 body2
      out -> out
  compare Proof {} _ = GT
  compare _ Proof {} = LT
  compare Import { importExp = exp1 } Import { importExp = exp2 } =
    compare exp1 exp2

instance Ord Compound where
  compare (Exp e1) (Exp e2) = compare e1 e2
  compare (Exp _) _ = GT
  compare _ (Exp _) = LT
  compare (Element e1) (Element e2) = compare e1 e2

instance Ord Pattern where
  compare Option { optionPats = pats1 } Option { optionPats = pats2 } =
    compare pats1 pats2
  compare Option {} _ = GT
  compare _ Option {} = LT
  compare Deconstruct { deconstructName = name1, deconstructPat = pat1 }
          Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare Deconstruct {} _ = GT
  compare _ Deconstruct {} = LT
  compare Split { splitFields = fields1, splitStrict = strict1 }
          Split { splitFields = fields2, splitStrict = strict2 } =
    case compare strict1 strict2 of
      EQ -> compare fields1 fields2
      out -> out
  compare Split {} _ = GT
  compare _ Split {} = LT
  compare Typed { typedPat = pat1, typedType = ty1 }
          Typed { typedPat = pat2, typedType = ty2 } =
    case compare pat1 pat2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Typed {} _ = GT
  compare _ Typed {} = LT
  compare As { asName = name1, asPat = pat1 }
          As { asName = name2, asPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare As {} _ = GT
  compare _ As {} = LT
  compare Name { nameSym = name1 } Name { nameSym = name2 } =
    compare name1 name2
  compare Name {} _ = GT
  compare _ Name {} = LT
  compare (Exact e1) (Exact e2) = compare e1 e2

instance Ord Exp where
  compare Compound { compoundBody = body1 } Compound { compoundBody = body2 } =
    compare body1 body2
  compare Compound {} _ = GT
  compare _ Compound {} = LT
  compare Abs { absKind = kind1, absCases = cases1 }
          Abs { absKind = kind2, absCases = cases2 } =
    case compare kind1 kind2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Abs {} _ = GT
  compare _ Abs {} = LT
  compare Match { matchVal = val1, matchCases = cases1 }
          Match { matchVal = val2, matchCases = cases2 } =
    case compare val1 val2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Match {} _ = GT
  compare _ Match {} = LT
  compare Ascribe { ascribeVal = val1, ascribeType = ty1 }
          Ascribe { ascribeVal = val2, ascribeType = ty2 } =
    case compare val1 val2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Ascribe {} _ = GT
  compare _ Ascribe {} = LT
  compare Seq { seqFirst = first1, seqSecond = second1 }
          Seq { seqFirst = first2, seqSecond = second2 } =
    case compare first1 first2 of
      EQ -> compare second1 second2
      out -> out
  compare Seq {} _ = GT
  compare _ Seq {} = LT
  compare Record { recordFields = fields1, recordType = ty1 }
          Record { recordFields = fields2, recordType = ty2 } =
    case compare ty1 ty2 of
      EQ -> compare fields1 fields2
      out -> out
  compare Record {} _ = GT
  compare _ Record {} = LT
  compare Tuple { tupleFields = fields1 } Tuple { tupleFields = fields2 } =
    compare fields1 fields2
  compare Tuple {} _ = GT
  compare _ Tuple {} = LT
  compare Project { projectVal = val1, projectFields = name1 }
          Project { projectVal = val2, projectFields = name2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare Project {} _ = GT
  compare _ Project {} = LT
  compare Sym { symName = name1 } Sym { symName = name2 } = compare name1 name2
  compare Sym {} _ = GT
  compare _ Sym {} = LT
  compare With { withVal = val1, withArgs = args1 }
          With { withVal = val2, withArgs = args2 } =
    case compare val1 val2 of
      EQ -> compare args1 args2
      out -> out
  compare With {} _ = GT
  compare _ With {} = LT
  compare Where { whereVal = val1, whereProp = prop1 }
          Where { whereVal = val2, whereProp = prop2 } =
    case compare val1 val2 of
      EQ -> compare prop1 prop2
      out -> out
  compare Where {} _ = GT
  compare _ Where {} = LT
  compare Anon { anonKind = cls1, anonParams = params1,
                 anonSuperTypes = supers1, anonContent = body1 }
          Anon { anonKind = cls2, anonParams = params2,
                 anonSuperTypes = supers2, anonContent = body2 } =
    case compare cls1 cls2 of
      EQ -> case compare params1 params2 of
        EQ -> case compare supers1 supers2 of
          EQ -> compare body1 body2
          out -> out
        out -> out
      out -> out
  compare Anon {} _ = GT
  compare _ Anon {} = LT
  compare (Literal lit1) (Literal lit2) = compare lit1 lit2

instance Ord Entry where
  compare Named { namedSym = name1, namedVal = val1 }
          Named { namedSym = name2, namedVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare Named {} _ = GT
  compare _ Named {} = LT
  compare (Unnamed e1) (Unnamed e2) = compare e1 e2

instance Ord Field where
  compare Field { fieldName = name1, fieldVal = val1 }
          Field { fieldName = name2, fieldVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out

instance Ord Case where
  compare Case { caseBody = body1, casePat = pat1 }
          Case { caseBody = body2, casePat = pat2 } =
      case compare pat1 pat2 of
        EQ -> compare body1 body2
        out -> out

instance Hashable AST where
  hashWithSalt s AST { astUses = uses, astScope = scope } =
    s `hashWithSalt` uses `hashWithSalt` scope

instance Hashable Use where
  hashWithSalt s Use { useName = uname } = s `hashWithSalt` uname

instance Hashable Group where
  hashWithSalt s Group { groupVisibility = vis, groupElements = elems } =
    s `hashWithSalt` vis `hashWithSalt` elems

instance Hashable Content where
  hashWithSalt s (Body b) = s `hashWithSalt` (1 :: Int) `hashWithSalt` b
  hashWithSalt s (Value v) = s `hashWithSalt` (2 :: Int) `hashWithSalt` v

instance Hashable Element where
  hashWithSalt s Builder { builderName = sym, builderKind = cls,
                           builderParams = params, builderSuperTypes = supers,
                           builderContent = body } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym `hashWithSalt`
    cls `hashWithSalt` params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s Def { defPattern = pat, defInit = init } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` pat `hashWithSalt` init
  hashWithSalt s Fun { funName = sym, funCases = cases } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` sym `hashWithSalt` cases
  hashWithSalt s Truth { truthName = sym, truthKind = kind,
                         truthContent = prop } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym `hashWithSalt`
    kind `hashWithSalt` prop
  hashWithSalt s Proof { proofName = sym, proofBody = body } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` sym `hashWithSalt` body
  hashWithSalt s Import { importExp = exp } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` exp

instance Hashable Compound where
  hashWithSalt s (Element e) = s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s (Exp e) = s `hashWithSalt` (2 :: Int) `hashWithSalt` e

instance Hashable Pattern where
  hashWithSalt s Option { optionPats = pats } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` pats
  hashWithSalt s Deconstruct { deconstructName = sym, deconstructPat = pat } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Split { splitFields = fields, splitStrict = strict } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` fields `hashWithSalt` strict
  hashWithSalt s Typed { typedPat = pat, typedType = ty } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` pat `hashWithSalt` ty
  hashWithSalt s As { asName = sym, asPat = pat } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Name { nameSym = sym } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` sym
  hashWithSalt s (Exact e) = s `hashWithSalt` (7 :: Int) `hashWithSalt` e

instance Hashable  Exp where
  hashWithSalt s Compound { compoundBody = body } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` body
  hashWithSalt s Abs { absKind = kind, absCases = cases } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` kind `hashWithSalt` cases
  hashWithSalt s Match { matchVal = val, matchCases = cases } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val `hashWithSalt` cases
  hashWithSalt s Ascribe { ascribeVal = val, ascribeType = ty } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` val `hashWithSalt` ty
  hashWithSalt s Seq { seqFirst = first, seqSecond = second } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` first `hashWithSalt` second
  hashWithSalt s Record { recordFields = fields, recordType = ty } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` ty `hashWithSalt` fields
  hashWithSalt s Tuple { tupleFields = fields } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` fields
  hashWithSalt s Project { projectVal = val, projectFields = sym } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` sym `hashWithSalt` val
  hashWithSalt s Sym { symName = sym } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` sym
  hashWithSalt s With { withVal = val, withArgs = args } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt` val `hashWithSalt` args
  hashWithSalt s Where { whereVal = val, whereProp = prop } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` val `hashWithSalt` prop
  hashWithSalt s Anon { anonKind = cls, anonParams = params,
                        anonSuperTypes = supers, anonContent = body } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt` cls `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s (Literal lit) =
    s `hashWithSalt` (13 :: Int) `hashWithSalt` lit

instance Hashable Field where
  hashWithSalt s Field { fieldName = sym, fieldVal = val } =
    s `hashWithSalt` sym `hashWithSalt` val

instance Hashable Case where
  hashWithSalt s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt` pat `hashWithSalt` body

instance Hashable Entry where
  hashWithSalt s Named { namedSym = sym, namedVal = val } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym `hashWithSalt` val
  hashWithSalt s (Unnamed e) = s `hashWithSalt` (2 :: Int) `hashWithSalt` e

astDot :: MonadSymbols m => AST -> m Doc
astDot AST { astUses = uses, astScope = scope } =
  let
    astedge (_, nodename) = string "\"ast\":scope" <>
                            string "-> " <> dquoted (string nodename)
    astnode = string ("\"ast\" [ label = \"AST | <uses> uses | " ++
                      "<scope> scope\" shape = \"record\" ]")
  in do
    (usecontents, _) <- runStateT (mapM useDot uses) 0
    (contents, _) <- runStateT (mapM elementDot scope) 0
    return (string "digraph g " <>
            braces (line <>
                    vcat (map fst usecontents) <$>
                    vcat (map fst contents) <$>
                    astnode <$>
                    vcat (map astedge usecontents) <$>
                    vcat (map astedge contents) <> line))

useDot :: MonadSymbols m => Use -> StateT Word m (Doc, String)
useDot Use { useName = uname } =
  do
    unames <- mapM formatM uname
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Use | " <>
                               punctuate dot unames) <$>
                      string "shape = \"record\"") <> char ';', nodeid)

groupDot :: MonadSymbols m => Group -> StateT Word m (Doc, String)
groupDot Group { groupVisibility = vis, groupElements = elems } =
  let
    elemEdge nodeid (_, elemname) =
      dquoted (string nodeid) <> string ":elements" <>
      string " -> " <> dquoted (string elemname)
  in do
    nodeid <- getNodeID
    bodyContents <-mapM elementDot elems
    return (vcat (map fst bodyContents) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Group | " <>
                               string (show vis) <>
                               string " | <elements> elements") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (elemEdge nodeid) bodyContents), nodeid)

builderBodyDot :: MonadSymbols m => Content -> StateT Word m (Doc, String)
builderBodyDot (Body elems) =
  let
    groupEdge nodeid (_, groupname) =
      dquoted (string nodeid) <> string ":groups" <>
      string " -> " <> dquoted (string groupname)
  in do
    nodeid <- getNodeID
    bodyContents <-mapM groupDot elems
    return (vcat (map fst bodyContents) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Body | " <>
                               string "<groups> groups") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (groupEdge nodeid) bodyContents), nodeid)
builderBodyDot (Value elems) =
  do
    nodeid <- getNodeID
    (valuenode, valuename) <-expDot elems
    return (valuenode <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Value | " <>
                               string "<value> value\"") <$>
                      string "shape = \"record\"") <>
            dquoted (string nodeid <> string ":value") <>
            string " -> " <> string valuename, nodeid)

elementDot :: MonadSymbols m => Element -> StateT Word m (Doc, String)
elementDot Builder { builderName = sym, builderKind = cls,
                     builderParams = params, builderSuperTypes = supers,
                     builderContent = body } =
  let
    paramEdge nodeid (_, paramname) =
      dquoted (string nodeid) <> string ":params" <>
      string " -> " <> dquoted (string paramname)

    supersEdge nodeid (_, supername) =
      dquoted (string nodeid) <> string ":supers" <>
      string " -> " <> dquoted (string supername)
  in do
    nodeid <- getNodeID
    namestr <- name sym
    paramContents <- mapM fieldDot params
    supersContents <- mapM expDot supers
    (bodynode, bodyname) <- builderBodyDot body
    return (vcat (map fst paramContents) <$> vcat (map fst supersContents) <$>
            bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Builder | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | " <> string (show cls) <>
                               string (" | <params> params" ++
                                       " | <supers> supers" ++
                                       " | <body> body")) <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (paramEdge nodeid) paramContents) <$>
            vcat (map (supersEdge nodeid) supersContents) <$>
            dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)
elementDot Def { defPattern = pat, defInit = Just init } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (initnode, initname) <- expDot init
    return (patnode <$> initnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Def | " <>
                               string ("<pat> pattern" ++
                                       " | <init> init")) <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
            string " -> " <> dquoted (string patname) <$>
            dquoted (string nodeid) <> string ":init" <>
            string " -> " <> dquoted (string initname), nodeid)
elementDot Def { defPattern = pat, defInit = Nothing } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Def | " <>
                               string "<pat> pattern | <init> init") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
            string " -> " <> dquoted (string patname), nodeid)
elementDot Fun { funName = sym, funCases = cases } =
  let
    caseEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":cases" <>
      string " -> " <> dquoted (string casename)
  in do
    nodeid <- getNodeID
    namestr <- name sym
    caseContents <- mapM caseDot cases
    return (vcat (map fst caseContents) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Fun | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <cases> cases") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (caseEdge nodeid) caseContents), nodeid)
elementDot Truth { truthName = sym, truthKind = kind, truthContent = prop } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (bodynode, bodyname) <- expDot prop
    return (bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Truth | " <>
                               string (show kind) <> string " | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <body> body") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)
elementDot Proof { proofName = pname, proofBody = body } =
  do
    nodeid <- getNodeID
    (namenode, namename) <- expDot pname
    (bodynode, bodyname) <- expDot body
    return (namenode <$> bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Proof | <name> name | <body> body") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname) <$>
            dquoted (string nodeid) <> string ":name" <>
            string " -> " <> dquoted (string namename), nodeid)
elementDot Import { importExp = exp } =
  do
    nodeid <- getNodeID
    (namenode, namename) <- expDot exp
    return (namenode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Import | <exp> exp") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":exp" <>
            string " -> " <> dquoted (string namename), nodeid)

compoundDot :: MonadSymbols m => Compound -> StateT Word m (Doc, String)
compoundDot (Exp e) = expDot e
compoundDot (Element e) = elementDot e

patternDot :: MonadSymbols m => Pattern -> StateT Word m (Doc, String)
patternDot Option { optionPats = pats } =
  let
    patEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":pats" <>
      string " -> " <> dquoted (string patname)
  in do
    patContents <- mapM patternDot pats
    nodeid <- getNodeID
    return (vcat (map fst patContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Options | " <>
                              string "<pats> patterns") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (patEdge nodeid) patContents), nodeid)
patternDot Deconstruct { deconstructName = sym, deconstructPat = pat } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Deconstruct | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<pat> pattern") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname), nodeid)
patternDot Split { splitFields = fields, splitStrict = True } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM entryDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Split | strict | " <>
                               string "<fields> fields") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
patternDot Split { splitFields = fields, splitStrict = False } =
  let
    fieldEdge nodeid (_, fieldname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string fieldname)
  in do
    fieldContents <- mapM entryDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Split | non-strict | " <>
                              string "<body> fields") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
patternDot Typed { typedPat = pat, typedType = ty } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (tynode, tyname) <- expDot ty
    return (patnode <$> tynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Typed | " <>
                               string " <type> type | " <>
                               string "<pat> pat") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname) <$>
          dquoted (string nodeid) <> string ":type" <>
          string " -> " <> dquoted (string tyname), nodeid)
patternDot As { asName = sym, asPat = pat } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "As | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<pat> pat") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname), nodeid)
patternDot Name { nameSym = sym } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Name | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"") <$>
                      string "shape = \"record\"") <> char ';', nodeid)
patternDot (Exact e) =
  do
    nodeid <- getNodeID
    (bodynode, bodyname) <- literalDot e
    return (bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Exact | " <>
                               string "<body>") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":body" <>
          string " -> " <> dquoted (string bodyname), nodeid)

expDot :: MonadSymbols m => Exp -> StateT Word m (Doc, String)
expDot Compound { compoundBody = body } =
  let
    compoundEdge nodeid (_, compoundname) =
      dquoted (string nodeid) <> string ":body" <>
      string " -> " <> dquoted (string compoundname)
  in do
    compoundContents <- mapM compoundDot body
    nodeid <- getNodeID
    return (vcat (map fst compoundContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Compound | " <>
                              string "<body> body") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (compoundEdge nodeid)
                                   compoundContents), nodeid)
expDot Abs { absKind = kind, absCases = cases } =
  let
    caseEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":cases" <>
      string " -> " <> dquoted (string casename)
  in do
    caseContents <- mapM caseDot cases
    nodeid <- getNodeID
    return (vcat (map fst caseContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Abs | " <>
                               string " | " <> format kind <>
                               string "<cases> cases") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (caseEdge nodeid) caseContents), nodeid)
expDot Match { matchVal = val, matchCases = cases } =
  let
    caseEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":cases" <>
      string " -> " <> dquoted (string casename)
  in do
    caseContents <- mapM caseDot cases
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    return (valnode <$> vcat (map fst caseContents) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Match | " <>
                               string " <val> value | " <>
                               string "<cases> cases") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val" <>
            string " -> " <> dquoted (string valname) <$>
            vcat (map (caseEdge nodeid) caseContents), nodeid)
expDot Ascribe { ascribeVal = val, ascribeType = ty } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (tynode, tyname) <- expDot ty
    return (valnode <$> tynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Ascribe | " <>
                               string " <type> type | " <>
                               string "<val> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <$>
          dquoted (string nodeid) <> string ":type" <>
          string " -> " <> dquoted (string tyname), nodeid)
expDot Seq { seqFirst = val1, seqSecond = val2 } =
  do
    nodeid <- getNodeID
    (val1node, val1name) <- expDot val1
    (val2node, val2name) <- expDot val2
    return (val1node <$> val2node <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Ascribe | " <>
                               string " <val1> value | " <>
                               string "<val2> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val1" <>
          string " -> " <> dquoted (string val1name) <$>
          dquoted (string nodeid) <> string ":val2" <>
          string " -> " <> dquoted (string val2name), nodeid)
expDot Record { recordType = True, recordFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM fieldDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Record | type | " <>
                               string "<fields> fields") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Record { recordType = False, recordFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM fieldDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Record | value | " <>
                               string "<fields> fields") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Tuple { tupleFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM expDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Tuple | " <>
                               string "<fields> fields") <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Project { projectVal = val, projectFields = fields } =
  let
    commabstr = Strict.fromString ", "
  in do
    names <- mapM (name . fieldSym) fields
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    return (valnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Project | " <>
                               string "\\\"" <>
                               bytestring (Strict.intercalate commabstr names) <>
                               string "\\\"" <>
                               string "<val> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname), nodeid)
expDot Sym { symName = sym } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Sym | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"") <$>
                      string "shape = \"record\"") <> char ';', nodeid)
expDot With { withVal = val, withArgs = args } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (argsnode, argsname) <- expDot args
    return (valnode <$> argsnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "With | " <>
                               string " <val> value | " <>
                               string "<args> args") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <$>
          dquoted (string nodeid) <> string ":args" <>
          string " -> " <> dquoted (string argsname), nodeid)
expDot Where { whereVal = val, whereProp = prop } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (propnode, propname) <- expDot prop
    return (valnode <$> propnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Where | " <>
                               string " <val> value | " <>
                               string "<prop> prop") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <$>
          dquoted (string nodeid) <> string ":prop" <>
          string " -> " <> dquoted (string propname), nodeid)
expDot Anon { anonKind = cls, anonParams = params,
              anonContent = body, anonSuperTypes = supers } =
  let
    paramEdge nodeid (_, paramname) =
      dquoted (string nodeid) <> string ":params" <>
      string " -> " <> dquoted (string paramname)

    supersEdge nodeid (_, supername) =
      dquoted (string nodeid) <> string ":supers" <>
      string " -> " <> dquoted (string supername)

    groupEdge nodeid (_, groupname) =
      dquoted (string nodeid) <> string ":groups" <>
      string " -> " <> dquoted (string groupname)
  in do
    nodeid <- getNodeID
    paramContents <- mapM fieldDot params
    supersContents <- mapM expDot supers
    bodyContents <- mapM groupDot body
    return (vcat (map fst paramContents) <$> vcat (map fst supersContents) <$>
            vcat (map fst bodyContents) <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Anon | " <>
                               string " | " <> string (show cls) <>
                               string (" | <params> params" ++
                                       " | <supers> supers" ++
                                       " | <body> body")) <$>
                      string "shape = \"record\"") <>
            char ';' <$> vcat (map (paramEdge nodeid) paramContents) <$>
            vcat (map (supersEdge nodeid) supersContents) <$>
            vcat (map (groupEdge nodeid) bodyContents), nodeid)
expDot (Literal l) =
  do
    nodeid <- getNodeID
    (bodynode, bodyname) <- literalDot l
    return (bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Literal | " <>
                               string "<body>") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":body" <>
          string " -> " <> dquoted (string bodyname), nodeid)

entryDot :: MonadSymbols m => Entry -> StateT Word m (Doc, String)
entryDot Named { namedSym = sym, namedVal = val } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (expnode, expname) <- patternDot val
    return (expnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Named | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<value> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":value" <>
            string " -> " <> dquoted (string expname), nodeid)
entryDot (Unnamed e) =
  do
    nodeid <- getNodeID
    (expnode, expname) <- patternDot e
    return (expnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Unnamed | " <>
                               string "<value> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":value" <>
          string " -> " <> dquoted (string expname), nodeid)

fieldDot :: MonadSymbols m => Field -> StateT Word m (Doc, String)
fieldDot Field { fieldName = FieldName { fieldSym = sym },
                 fieldVal = val } =
  do
    nodeid <- getNodeID
    namestr <- name sym
    (valnode, valname) <- expDot val
    return (valnode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Field | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <value> value") <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":value" <>
            string " -> " <> dquoted (string valname), nodeid)

caseDot :: MonadSymbols m => Case -> StateT Word m (Doc, String)
caseDot Case { casePat = pat, caseBody = body } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (bodynode, bodyname) <- expDot body
    return (patnode <$> bodynode <$> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Case | " <>
                               string (" | <pat> pattern" ++
                                       " | <body> body")) <$>
                      string "shape = \"record\"") <>
            char ';' <$> dquoted (string nodeid) <> string ":pat" <>
            string " -> " <> dquoted (string patname) <$>
            dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)

instance (MonadPositions m, MonadSymbols m) => FormatM m AST where
  formatM AST { astUses = uses, astScope = scope } =
    do
      usesdoc <- liftM listDoc (mapM formatM uses)
      scopedoc <- liftM listDoc (mapM formatM scope)
      return (constructorDoc (string "AST")
                             [(string "uses", usesdoc),
                              (string "scope", scopedoc)])


instance (MonadPositions m, MonadSymbols m) => FormatM m Use where
  formatM Use { useName = uname, usePos = pos } =
    do
      posdoc <- formatM pos
      unames <- mapM formatM uname
      return (constructorDoc (string "Use")
                             [(string "pos", posdoc),
                              (string "name", punctuate dot unames)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Group where
  formatM Group { groupElements = elems, groupPos = pos,
                  groupVisibility = vis} =
    do
      posdoc <- formatM pos
      elemdocs <- mapM formatM elems
      return (constructorDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "visibility", format vis),
                              (string "elems", listDoc elemdocs)])

instance (MonadPositions m, MonadSymbols m) => FormatM m [Group] where
  formatM = liftM listDoc . mapM formatM

instance (MonadPositions m, MonadSymbols m) => FormatM m Content where
  formatM (Body b) =
    do
      valdoc <- formatM b
      return (constructorDoc (string "Body")
                             [(string "value", valdoc)])
  formatM (Value v) =
    do
      valdoc <- formatM v
      return (constructorDoc (string "Value")
                             [(string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Element where
  formatM Builder { builderName = sym, builderKind = cls,
                    builderSuperTypes = supers, builderParams = params,
                    builderContent = body, builderPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- mapM formatM params
      bodydoc <- formatM body
      return (constructorDoc (string "Builder")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", listDoc paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])
  formatM Def { defPattern = pat, defInit = Just init, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      initdoc <- formatM init
      return (constructorDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "init", initdoc)])
  formatM Def { defPattern = pat, defInit = Nothing, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])
  formatM Fun { funName = sym, funCases = cases, funPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (constructorDoc (string "Fun")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "cases", listDoc casedocs)])
  formatM Truth { truthName = sym, truthContent = content,
                  truthKind = kind, truthPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      contentdoc <- formatM content
      return (constructorDoc (string "Truth")
                             [(string "name", namedoc),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "content", contentdoc)])
  formatM Proof { proofName = sym, proofBody = body, proofPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      bodydoc <- formatM body
      return (constructorDoc (string "Proof")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "body", bodydoc)])
  formatM Import { importExp = exp, importPos = pos } =
    do
      expdoc <- formatM exp
      posdoc <- formatM pos
      return (constructorDoc (string "Import")
                             [(string "exp", expdoc),
                              (string "pos", posdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Compound where
  formatM (Exp e) = formatM e
  formatM (Element e) = formatM e

instance (MonadPositions m, MonadSymbols m) => FormatM m Pattern where
  formatM Option { optionPats = pats, optionPos = pos } =
    do
      posdoc <- formatM pos
      patsdoc <- mapM formatM pats
      return (constructorDoc (string "Options")
                             [(string "pos", posdoc),
                              (string "patterns", listDoc patsdoc)])
  formatM Deconstruct { deconstructName = sym, deconstructPat = pat,
                        deconstructPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Deconstruct")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Split { splitFields = fields, splitStrict = True, splitPos = pos } =
    do
      fieldsdoc <- mapM formatM fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "true"),
                              (string "fields", listDoc fieldsdoc)])
  formatM Split { splitFields = fields, splitStrict = False, splitPos = pos } =
    do
      fieldsdoc <- mapM formatM fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "false"),
                              (string "fields", listDoc fieldsdoc)])
  formatM Typed { typedPat = pat, typedType = ty, typedPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      tydoc <- formatM ty
      return (constructorDoc (string "Typed")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "type", tydoc)])
  formatM As { asName = sym, asPat = pat, asPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "As")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Name { nameSym = sym, namePos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (constructorDoc (string "Name")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM (Exact e) = formatM e

instance (MonadPositions m, MonadSymbols m) => FormatM m Exp where
  formatM Compound { compoundBody = body, compoundPos = pos } =
    do
      posdoc <- formatM pos
      bodydoc <- mapM formatM body
      return (constructorDoc (string "Compound")
                             [(string "pos", posdoc),
                              (string "body", listDoc bodydoc)])
  formatM Abs { absKind = kind, absCases = cases, absPos = pos } =
    do
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (constructorDoc (string "Abs")
                             [(string "pos", posdoc),
                              (string "kind", format kind),
                              (string "cases", listDoc casedocs)])
  formatM Match { matchVal = val, matchCases = cases, matchPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      casedocs <- mapM formatM cases
      return (constructorDoc (string "Match")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "cases", listDoc casedocs)])
  formatM Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      tydoc <- formatM ty
      return (constructorDoc (string "Ascribe")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "type", tydoc)])
  formatM Seq { seqFirst = val1, seqSecond = val2, seqPos = pos } =
    do
      posdoc <- formatM pos
      val1doc <- formatM val1
      val2doc <- formatM val2
      return (constructorDoc (string "Seq")
                             [(string "pos", posdoc),
                              (string "first", val1doc),
                              (string "second", val2doc)])
  formatM Record { recordType = True, recordFields = fields, recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (constructorDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "type", string "true"),
                              (string "fields", listDoc fielddocs)])
  formatM Record { recordType = False, recordFields = fields,
                   recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (constructorDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "type", string "false"),
                              (string "fields", listDoc fielddocs)])
  formatM Tuple { tupleFields = fields, tuplePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (constructorDoc (string "Tuple")
                             [(string "pos", posdoc),
                              (string "fields", listDoc fielddocs)])
  formatM Project { projectVal = val, projectFields = fields,
                    projectPos = pos } =
    do
      fielddocs <- mapM formatM fields
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Project")
                             [(string "fields", listDoc fielddocs),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM Sym { symName = sym, symPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (constructorDoc (string "Sym")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM With { withVal = val, withArgs = args, withPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      argdocs <- formatM args
      return (constructorDoc (string "With")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "arg", argdocs)])
  formatM Where { whereVal = val, whereProp = prop, wherePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      propdoc <- formatM prop
      return (constructorDoc (string "Where")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "prop", propdoc)])
  formatM Anon { anonKind = cls, anonParams = params, anonContent = body,
                 anonSuperTypes = supers, anonPos = pos } =

    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- mapM formatM params
      bodydoc <- mapM formatM body
      return (constructorDoc (string "Anon")
                             [(string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", listDoc paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", listDoc bodydoc)])
  formatM (Literal l) = formatM l

instance (MonadPositions m, MonadSymbols m) => FormatM m Entry where
  formatM Named { namedSym = sym, namedVal = val, namedPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Named")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM (Unnamed e) =
    do
      valdoc <- formatM e
      return (constructorDoc (string "Unnamed")
                             [(string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Field where
  formatM Field { fieldName = sym, fieldVal = val, fieldPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Field")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Case where
  formatM Case { casePat = pat, caseBody = body, casePos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      bodydoc <- formatM body
      return (constructorDoc (string "Case")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "body", bodydoc)])

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] AST where
  xpickle =
    xpWrap (\(uses, scope) -> AST { astUses = uses, astScope = scope },
            \AST { astUses = uses, astScope = scope } -> (uses, scope))
           (xpElemNodes (gxFromString "AST")
                        (xpPair (xpElemNodes (gxFromString "uses") xpickle)
                                (xpElemNodes (gxFromString "scope") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Use where
  xpickle = xpWrap (\(pos, uname) -> Use { useName = uname, usePos = pos },
                    \Use { useName = uname, usePos = pos } -> (pos, uname))
                   (xpElem (gxFromString "Use") xpickle
                           (xpElemNodes (gxFromString "name") xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Group where
  xpickle = xpWrap (\((vis, pos), elems) -> Group { groupVisibility = vis,
                                                    groupElements = elems,
                                                    groupPos = pos },
                    \Group { groupVisibility = vis, groupElements = elems,
                             groupPos = pos } -> ((vis, pos), elems))
                   (xpElem (gxFromString "Group")
                           (xpPair xpickle xpickle)
                           (xpElemNodes (gxFromString "elements") xpickle))

bodyPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Content
bodyPickler =
  let
    revfunc (Body b) = b
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Body, revfunc) (xpElemNodes (gxFromString "Body") xpickle)

valuePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Content
valuePickler =
  let
    revfunc (Value v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Value, revfunc) (xpElemNodes (gxFromString "Value") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Content where
  xpickle =
    let
      picker (Body _) = 0
      picker (Value _) = 1
    in
      xpAlt picker [bodyPickler, valuePickler]

builderPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Element
builderPickler =
  let
    revfunc Builder { builderName = sym, builderKind = kind,
                      builderParams = params, builderSuperTypes = supers,
                      builderContent = body, builderPos = pos } =
      ((sym, kind, pos), (params, supers, body))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, kind, pos), (params, supers, body)) ->
             Builder { builderName = sym, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos }, revfunc)
           (xpElem (gxFromString "Builder")
                   (xpTriple xpickle xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)))

defPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Element
defPickler =
  let
    revfunc Def { defPattern = pat, defInit = init, defPos = pos } =
      (pos, (pat, init))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (pat, init)) -> Def { defPattern = pat, defInit = init,
                                         defPos = pos }, revfunc)
           (xpElem (gxFromString "Def") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpOption (xpElemNodes (gxFromString "init")
                                                  xpickle))))

funPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Element
funPickler =
  let
    revfunc Fun { funName = sym, funCases = cases, funPos = pos } =
      ((sym, pos), cases)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, pos), cases) -> Fun { funName = sym, funCases = cases,
                                        funPos = pos }, revfunc)
           (xpElem (gxFromString "Fun") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "cases") xpickle))

truthPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
truthPickler =
  let
    revfunc Truth { truthName = sym, truthKind = kind,
                    truthContent = body, truthPos = pos } =
      ((sym, kind, pos), body)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, kind, pos), body) ->
             Truth { truthName = sym, truthContent = body,
                     truthKind = kind, truthPos = pos }, revfunc)
           (xpElem (gxFromString "Truth") (xpTriple xpickle xpickle xpickle)
                   (xpElemNodes (gxFromString "type") xpickle))

proofPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
proofPickler =
  let
    revfunc Proof { proofName = pname, proofBody = body, proofPos = pos } =
      (pos, (pname, body))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (pname, body)) -> Proof { proofName = pname,
                                             proofBody = body,
                                             proofPos = pos }, revfunc)
           (xpElem (gxFromString "Proof") xpickle
                   (xpPair (xpElemNodes (gxFromString "name") xpickle)
                           (xpElemNodes (gxFromString "type") xpickle)))

importPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
importPickler =
  let
    revfunc Import { importExp = exp, importPos = pos } = (pos, exp)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, exp) -> Import { importExp = exp, importPos = pos }, revfunc)
           (xpElem (gxFromString "Import") xpickle
                   (xpElemNodes (gxFromString "name") xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Element where
  xpickle =
    let
      picker Builder {} = 0
      picker Def {} = 1
      picker Fun {} = 2
      picker Truth {} = 3
      picker Proof {} = 4
      picker Import {} = 5
    in
      xpAlt picker [builderPickler, defPickler, funPickler,
                    truthPickler, proofPickler, importPickler]

expPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Compound
expPickler =
  let
    revfunc (Exp e) = e
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Exp, revfunc) (xpElemNodes (gxFromString "Exp") xpickle)

elementPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Compound
elementPickler =
  let
    revfunc (Element e) = e
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Element, revfunc) (xpElemNodes (gxFromString "Element") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Compound where
  xpickle =
    let
      picker (Exp _) = 0
      picker (Element _) = 1
    in
      xpAlt picker [expPickler, elementPickler]

optionPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Pattern
optionPickler =
  let
    revfunc Option { optionPats = pats, optionPos = pos } = (pos, pats)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, pats) -> Option { optionPats = pats,
                                     optionPos = pos }, revfunc)
           (xpElem (gxFromString "Option") xpickle
                   (xpElemNodes (gxFromString "patterns") xpickle))

deconstructPickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text) =>
                      PU [NodeG [] tag text] Pattern
deconstructPickler =
  let
    revfunc Deconstruct { deconstructName = sym, deconstructPat = pat,
                          deconstructPos = pos } =
      ((sym, pos), pat)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, pos), pat) -> Deconstruct { deconstructName = sym,
                                                 deconstructPat = pat,
                                                 deconstructPos = pos },
            revfunc)
           (xpElem (gxFromString "Deconstruct") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "pattern") xpickle))

splitPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Pattern
splitPickler =
  let
    revfunc Split { splitStrict = strict, splitFields = fields,
                    splitPos = pos } =
      ((strict, pos), fields)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((strict, pos), fields) -> Split { splitStrict = strict,
                                                splitFields = fields,
                                                splitPos = pos }, revfunc)
           (xpElem (gxFromString "Split")
                   (xpPair (xpAttr (gxFromString "strict") xpPrim) xpickle)
                   (xpElemNodes (gxFromString "fields") xpickle))

typedPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Pattern
typedPickler =
  let
    revfunc Typed { typedPat = pat, typedType = ty, typedPos = pos } =
      (pos, (pat, ty))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (pat, ty)) -> Typed { typedPat = pat, typedType = ty,
                                         typedPos = pos }, revfunc)
           (xpElem (gxFromString "Typed") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "type") xpickle)))

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
asPickler =
  let
    revfunc As { asName = sym, asPat = pat, asPos = pos } = ((sym, pos), pat)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, pos), pat) -> As { asName = sym, asPat = pat,
                                        asPos = pos }, revfunc)
           (xpElem (gxFromString "As") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "pattern") xpickle))

namePickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
namePickler =
  let
    revfunc Name { nameSym = sym, namePos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Name { nameSym = sym, namePos = pos }, revfunc)
           (xpElemAttrs (gxFromString "Name") (xpPair xpickle xpickle))

exactPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Pattern
exactPickler =
  let
    revfunc (Exact v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Exact, revfunc) (xpElemNodes (gxFromString "Exact") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Pattern where
  xpickle =
    let
      picker Option {} = 0
      picker Deconstruct {} = 1
      picker Split {} = 2
      picker Typed {} = 3
      picker As {} = 4
      picker Name {} = 5
      picker (Exact _) = 6
    in
      xpAlt picker [optionPickler, deconstructPickler, splitPickler,
                    typedPickler, asPickler, namePickler, exactPickler]

compoundPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Exp
compoundPickler =
  let
    revfunc Compound { compoundBody = body, compoundPos = pos } = (pos, body)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, body) -> Compound { compoundBody = body,
                                       compoundPos = pos }, revfunc)
           (xpElem (gxFromString "Compound") xpickle
                   (xpElemNodes (gxFromString "body") xpickle))

absPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Exp
absPickler =
  let
    revfunc Abs { absKind = kind, absCases = cases, absPos = pos } =
      ((kind, pos), cases)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((kind, pos), cases) -> Abs { absKind = kind, absCases = cases,
                                           absPos = pos }, revfunc)
           (xpElem (gxFromString "Abs") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "fields") xpickle))

matchPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
matchPickler =
  let
    revfunc Match { matchVal = val, matchCases = cases, matchPos = pos } =
      (pos, (val, cases))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (val, cases)) -> Match { matchVal = val, matchCases = cases,
                                            matchPos = pos }, revfunc)
           (xpElem (gxFromString "Match") xpickle
                   (xpPair (xpElemNodes (gxFromString "value") xpickle)
                           (xpElemNodes (gxFromString "cases") xpickle)))

ascribePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
ascribePickler =
  let
    revfunc Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
      (pos, (val, ty))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (val, ty)) -> Ascribe { ascribeVal = val,
                                           ascribeType = ty,
                                           ascribePos = pos }, revfunc)
           (xpElem (gxFromString "Ascribe") xpickle
                   (xpPair (xpElemNodes (gxFromString "value") xpickle)
                           (xpElemNodes (gxFromString "type") xpickle)))

seqPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
seqPickler =
  let
    revfunc Seq { seqFirst = first, seqSecond = second, seqPos = pos } =
      (pos, (first, second))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (first, second)) -> Seq { seqFirst = first,
                                             seqSecond = second,
                                             seqPos = pos }, revfunc)
           (xpElem (gxFromString "Seq") xpickle (xpPair xpickle xpickle))

recordPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
recordPickler =
  let
    revfunc Record { recordType = istype, recordFields = fields,
                     recordPos = pos } = ((istype, pos), fields)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((istype, pos), fields) -> Record { recordType = istype,
                                                 recordFields = fields,
                                                 recordPos = pos }, revfunc)
           (xpElem (gxFromString "Record")
                   (xpPair (xpAttr (gxFromString "is-type") xpPrim) xpickle)
                   (xpElemNodes (gxFromString "fields") xpickle))

tuplePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
tuplePickler =
  let
    revfunc Tuple { tupleFields = fields, tuplePos = pos } = (pos, fields)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, fields) -> Tuple { tupleFields = fields,
                                      tuplePos = pos }, revfunc)
           (xpElem (gxFromString "Record") xpickle
                   (xpElemNodes (gxFromString "fields") xpickle))

projectPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
projectPickler =
  let
    revfunc Project { projectFields = fields, projectVal = val,
                      projectPos = pos } = (pos, (fields, val))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (fields, val)) -> Project { projectFields = fields,
                                                 projectVal = val,
                                                 projectPos = pos }, revfunc)
           (xpElem (gxFromString "Project") xpickle
                   (xpPair (xpElemNodes (gxFromString "value") xpickle)
                           (xpElemNodes (gxFromString "fields") xpickle)))

symPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Exp
symPickler =
  let
    revfunc Sym { symName = sym, symPos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Sym { symName = sym, symPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "Sym") (xpPair xpickle xpickle))

withPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
withPickler =
  let
    revfunc With { withVal = val, withArgs = args, withPos = pos } =
      (pos, (val, args))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (val, args)) -> With { withVal = val, withArgs = args,
                                          withPos = pos }, revfunc)
           (xpElem (gxFromString "With") xpickle
                   (xpPair (xpElemNodes (gxFromString "value") xpickle)
                           (xpElemNodes (gxFromString "args") xpickle)))

wherePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
wherePickler =
  let
    revfunc Where { whereVal = val, whereProp = prop, wherePos = pos } =
      (pos, (val, prop))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (val, prop)) -> Where { whereVal = val, whereProp = prop,
                                           wherePos = pos }, revfunc)
           (xpElem (gxFromString "Where") xpickle
                   (xpPair (xpElemNodes (gxFromString "value") xpickle)
                           (xpElemNodes (gxFromString "prop") xpickle)))

anonPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
anonPickler =
  let
    revfunc Anon { anonKind = kind, anonParams = params, anonContent = body,
                   anonSuperTypes = supers, anonPos = pos } =
      ((kind, pos), (params, supers, body))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((kind, pos), (params, supers, body)) ->
             Anon { anonKind = kind, anonParams = params, anonContent = body,
                    anonSuperTypes = supers, anonPos = pos }, revfunc)
           (xpElem (gxFromString "Anon")
                   (xpPair xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)))

literalPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
literalPickler =
  let
    revfunc (Literal v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Literal, revfunc) (xpElemNodes (gxFromString "Literal") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Exp where
  xpickle =
    let
      picker Compound {} = 0
      picker Abs {} = 1
      picker Match {} = 2
      picker Ascribe {} = 3
      picker Seq {} = 4
      picker Record {} = 5
      picker Tuple {} = 6
      picker Project {} = 7
      picker Sym {} = 8
      picker With {} = 9
      picker Where {} = 10
      picker Anon {} = 11
      picker (Literal _) = 12
    in
      xpAlt picker [compoundPickler, absPickler, matchPickler, ascribePickler,
                    seqPickler, recordPickler, tuplePickler, projectPickler,
                    symPickler, withPickler, wherePickler,
                    anonPickler, literalPickler]

namedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Entry
namedPickler =
  let
    revfunc Named { namedSym = sym, namedVal = val,
                    namedPos = pos } = ((sym, pos), val)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, pos), val) -> Named { namedSym = sym, namedVal = val,
                                          namedPos = pos }, revfunc)
           (xpElem (gxFromString "Named") (xpPair xpickle xpickle) xpickle)

unnamedPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Entry
unnamedPickler =
  let
    revfunc (Unnamed v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Unnamed, revfunc)
           (xpElemNodes (gxFromString "Unnamed") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Entry where
  xpickle =
    let
      picker Named {} = 0
      picker (Unnamed _) = 1
    in
      xpAlt picker [namedPickler, unnamedPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Field where
  xpickle = xpWrap (\((sym, pos), val) -> Field { fieldName = sym,
                                                    fieldVal = val,
                                                    fieldPos = pos },
                    \Field { fieldName = sym, fieldVal = val,
                             fieldPos = pos } -> ((sym, pos), val))
                   (xpElem (gxFromString "Field")
                           (xpPair xpickle xpickle)
                           xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Case where
  xpickle = xpWrap (\(pos, (pat, exp)) -> Case { casePat = pat,
                                                 caseBody = exp,
                                                 casePos = pos },
                    \Case { casePat = pat, caseBody = body, casePos = pos } ->
                      (pos, (pat, body)))
                   (xpElem (gxFromString "Case") xpickle
                           (xpPair (xpElemNodes (gxFromString "pattern")
                                                xpickle)
                                   (xpElemNodes (gxFromString "body")
                                                xpickle)))
