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
-- rendered into surface syntax by Collect, which gathers all entries
-- into a table.
--
-- Note that this structure isn't meant to be processed in any truly
-- meaningful way.
module Language.Salt.Surface.AST(
       BuilderKind(..),
       TruthKind(..),
       Visibility(..),
       Group(..),
       Content(..),
       Scope,
       Element(..),
       Compound(..),
       Pattern(..),
       Literal(..),
       Exp(..),
       Field(..),
       Entry(..),
       Case(..),
       elementPosition
       ) where

import Data.ByteString(ByteString)
import Data.Hashable
--import Data.Monoid hiding ((<>))
import Data.Position
import Data.Ratio
import Data.Symbol
import Language.Salt.Surface.Common
import Prelude hiding (sequence, init, exp)
--import Text.Format hiding ((<$>))
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

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
data Content con =
    -- | An actual definition body.
    Body !con
    -- | An expression.
  | Value !Exp

-- | Type of a scope.
type Scope = [Element]

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
      -- | The declared supertypes for this builder entity.
      builderSuperTypes :: ![Exp],
      -- | The parameters of the builder entity.
      builderParams :: ![Field],
      -- | The entities declared by the builder.
      builderContent :: !(Content Scope),
      -- | The position in source from which this arises.
      builderPos :: !Position
    }
    -- | Declaration.  Declares the type of a symbol, but does not
    -- give an initializer.
  | Decl {
      -- | Name of the value being declared.
      declName :: !Symbol,
      -- | The type of the value being declared.
      declType :: !Exp,
      -- | The position in source from which this arises.
      declPos :: !Position
    }
    -- | Value definitions.  These are declarations coupled with
    -- values.  These include function declarations.
  | Def {
      -- | The pattern for the definition
      defPattern :: !Pattern,
      -- | The value's initializer.
      defInit :: !Exp,
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
      truthContent :: !(Content Exp),
      -- | The position in source from which this arises.
      truthPos :: !Position
    }
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
  | Proof {
      -- | The name of the theorem being proven.
      proofName :: !Symbol,
      -- | The body of the proof.
      proofBody :: !Exp,
      -- | The position in source from which this arises.
      proofPos :: !Position
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

-- | A literal value.
data Literal =
    -- | A number literal.
    Num {
      -- | The number value.
      numVal :: !Rational,
      -- | The position in source from which this arises.
      numPos :: !Position
    }
    -- | A string literal.
  | Str {
      -- | The string value.
      strVal :: !ByteString,
      -- | The position in source from which this arises.
      strPos :: !Position
    }
    -- | A Character literal.
  | Char {
      -- | The character value.
      charVal :: !Char,
      -- | The position in source from which this arises.
      charPos :: !Position
    }
    -- | A unit value.
  | Unit {
      -- | The position in source from which this arises.
      unitPos :: !Position
    }

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
      -- | The name of the field being accessed.
      projectName :: !Symbol,
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
    fieldName :: !Symbol,
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
elementPosition Decl { declPos = pos } = pos
elementPosition Def { defPos = pos } = pos
elementPosition Fun { funPos = pos } = pos
elementPosition Truth { truthPos = pos } = pos
elementPosition Proof { proofPos = pos } = pos

instance Eq Group where
  Group { groupVisibility = vis1, groupElements = elems1 } ==
    Group { groupVisibility = vis2, groupElements = elems2 } =
      vis1 == vis2 && elems1 == elems2

instance Eq con => Eq (Content con) where
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
  Decl { declName = name1, declType = ty1 } ==
    Decl { declName = name2, declType = ty2 } =
      name1 == name2 && ty1 == ty2
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

instance Eq Literal where
  Num { numVal = num1 } == Num { numVal = num2 } = num1 == num2
  Str { strVal = str1 } == Str { strVal = str2 } = str1 == str2
  Char { charVal = chr1 } == Char { charVal = chr2 } = chr1 == chr2
  Unit {} == Unit {} = True
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
  Project { projectVal = val1, projectName = name1 } ==
    Project { projectVal = val2, projectName = name2 } =
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


instance Ord Group where
  compare Group { groupVisibility = vis1, groupElements = elems1 }
          Group { groupVisibility = vis2, groupElements = elems2 } =
      case compare vis1 vis2 of
        EQ -> compare elems1 elems2
        out -> out

instance Ord con => Ord (Content con) where
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
  compare Decl { declName = name1, declType = ty1 }
          Decl { declName = name2, declType = ty2 } =
    case compare name1 name2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Decl {} _ = GT
  compare _ Decl {} = LT
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

instance Ord Literal where
  compare Num { numVal = num1 } Num { numVal = num2 } = compare num1 num2
  compare Num {} _ = GT
  compare _ Num {} = LT
  compare Str { strVal = str1 } Str { strVal = str2 } = compare str1 str2
  compare Str {} _ = GT
  compare _ Str {} = LT
  compare Char { charVal = chr1 } Char { charVal = chr2 } = compare chr1 chr2
  compare Char {} _ = GT
  compare _ Char {} = LT
  compare Unit {} Unit {} = EQ

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
  compare Project { projectVal = val1, projectName = name1 }
          Project { projectVal = val2, projectName = name2 } =
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

instance Hashable Group where
  hashWithSalt s Group { groupVisibility = vis1, groupElements = elems1 } =
    s `hashWithSalt` vis1 `hashWithSalt` elems1

instance Hashable con => Hashable (Content con) where
  hashWithSalt s (Body b) = s `hashWithSalt` (1 :: Int) `hashWithSalt` b
  hashWithSalt s (Value v) = s `hashWithSalt` (2 :: Int) `hashWithSalt` v

instance Hashable Element where
  hashWithSalt s Builder { builderName = name, builderKind = cls,
                           builderParams = params, builderSuperTypes = supers,
                           builderContent = body } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name `hashWithSalt`
    cls `hashWithSalt` params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s Decl { declName = name, declType = ty } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` name `hashWithSalt` ty
  hashWithSalt s Def { defPattern = pat, defInit = init } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` pat `hashWithSalt` init
  hashWithSalt s Fun { funName = name, funCases = cases } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` name `hashWithSalt` cases
  hashWithSalt s Truth { truthName = name, truthKind = kind,
                         truthContent = prop } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` name `hashWithSalt`
    kind `hashWithSalt` prop
  hashWithSalt s Proof { proofName = name, proofBody = body } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` name `hashWithSalt` body

instance Hashable Compound where
  hashWithSalt s (Element e) = s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s (Exp e) = s `hashWithSalt` (2 :: Int) `hashWithSalt` e

instance Hashable Pattern where
  hashWithSalt s Option { optionPats = pats } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` pats
  hashWithSalt s Deconstruct { deconstructName = name, deconstructPat = pat } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` name `hashWithSalt` pat
  hashWithSalt s Split { splitFields = fields, splitStrict = strict } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` fields `hashWithSalt` strict
  hashWithSalt s Typed { typedPat = pat, typedType = ty } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` pat `hashWithSalt` ty
  hashWithSalt s As { asName = name, asPat = pat } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` name `hashWithSalt` pat
  hashWithSalt s Name { nameSym = name } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` name
  hashWithSalt s (Exact e) = s `hashWithSalt` (7 :: Int) `hashWithSalt` e

instance Hashable Literal where
  hashWithSalt s Num { numVal = num } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` num
  hashWithSalt s Str { strVal = str } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` str
  hashWithSalt s Char { charVal = chr } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` chr
  hashWithSalt s Unit {} = s `hashWithSalt` (4 :: Int)

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
  hashWithSalt s Project { projectVal = val, projectName = name } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` name `hashWithSalt` val
  hashWithSalt s Sym { symName = name } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` name
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
  hashWithSalt s Field { fieldName = name, fieldVal = val } =
    s `hashWithSalt` name `hashWithSalt` val

instance Hashable Case where
  hashWithSalt s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt` pat `hashWithSalt` body

instance Hashable Entry where
  hashWithSalt s Named { namedSym = name, namedVal = val } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name `hashWithSalt` val
  hashWithSalt s (Unnamed e) = s `hashWithSalt` (2 :: Int) `hashWithSalt` e

{-
instance Format sym => Format (Element sym) where
  format Builder { builderName = name, builderKind = cls,
                 builderSuperTypes = supers, builderParams = params,
                 builderBody = body } =
    let
      header = show cls <+> name
      withparams = case params of
        [] -> header
        params' -> parenList header params'
      withsupers = case supers of
        [] -> withparams
        supers' -> withparams <+> colon <+>
          (nest 2 (sep (punctuate comma supers')))
    in
      braceBlock withsupers body
  format Def { defName = name, defMutable = True,
               defType = Just ty, defInit = val } =
    name <+> colon <+> nest 2 (sep [format "mutable", format ty,
                                    equals, format val])
  format Def { defName = name, defMutable = False,
               defType = Just ty, defInit = val } =
    name <+> colon <+> nest 2 (sep [format ty, equals, format val])
  format Def { defName = name, defMutable = True,
               defType = Nothing, defInit = val } =
    name <+> colon <+> nest 2 (sep [format "mutable", equals, format val])
  format Def { defName = name, defMutable = False,
               defType = Nothing, defInit = val } =
    name <+> equals <+> nest 2 val
  format Truth { truthKind = cls, truthName = name, truthProp = prop } =
    hang (format cls <+> name <+> equals) 2 prop
  format Alias { aliasClass = Import, aliasName = Just name, aliasSrc = src } =
    "import" <+> src <+> "as" <+> name <> semi
  format Alias { aliasClass = Import, aliasName = Nothing, aliasSrc = src } =
    "import" <+> src <> semi
  format Alias { aliasClass = Export, aliasName = Just name, aliasSrc = src } =
    "export" <+> src <+> "as" <+> name <> semi
  format Alias { aliasClass = Export, aliasName = Nothing, aliasSrc = src } =
    "export" <+> src <> semi
  format Alias { aliasClass = Open, aliasSrc = src } =
    "import" <+> src <+> ".*;"

instance Format sym => Format (Compound sym) where
  format (Decl d) = format d
  format (Exp e) = format e

instance Format sym => Format (Exp sym) where
  format Compound { compoundBody = body } =
    block 2 (lbrace) (sep body) rbrace
  format Func { funcCases = cases } = (sep (punctuate (format "|") cases))
  format Match { matchVal = val, matchCases = cases } =
    hang (format "match" <+> val) 2 (sep (punctuate (format "|") cases))
  format Ascribe { ascribeVal = val, ascribeType = ty } =
    hang (val <+> colon) 2 ty
  format Seq { seqVals = vals } = nest 2 (sep vals)
  format Record { recFields = fields } =
    lparen <> (nest 2 (sep (punctuate comma fields))) <> rparen
  format Project { projectVal = val, projectName = name } = val <> "." <> name
  format Sym { symName = name } = format name

instance Format sym => Format (Pattern sym) where
  format Project { projectFields = fields, projectStrict = True } =
    lparen <> (nest 2 (sep (punctuate comma fields))) <> rparen
  format Project { projectFields = fields, projectStrict = False } =
    let
      withdots = (map format fields) ++ [format "..."]
    in
    lparen <> (nest 2 (sep (punctuate comma withdots))) <> rparen
  format Construct { constructName = name, constructArgs = args,
                     constructStrict = True } = parenList name args
  format Construct { constructName = name, constructArgs = args,
                     constructStrict = False } =
    parenList name ((map format args) ++ [format "..."])
  format Typed { typedPat = pat, typedType = ty } =
    hang (pat <+> colon) 2 ty
  format As { asName = name, asPat = pat } =
    format pat <+> format "as" <+> format name
  format Name { nameSym = name } = format name

instance Format sym => Format (Field sym) where
  format Field { fieldName = name, fieldType = ty } =
    hang (name <+> colon) 2 ty

instance Format sym => Format (Case sym) where
  format Case { casePat = pat, caseBody = body } =
    hang (pat <+> equals) 2 body

instance (Format sym, Format (con sym)) => Format (Entry con sym) where
  format Named { namedName = name, namedVal = val } =
    hang val 2 (format "as" <+> name)
  format (Unnamed e) = format e

instance Format sym => Show (Element sym) where
  show = show . format

instance Format sym => Show (Exp sym) where
  show = show . format

-}

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Group where
  xpickle = xpWrap (\((vis, pos), elems) -> Group { groupVisibility = vis,
                                                    groupElements = elems,
                                                    groupPos = pos },
                    \Group { groupVisibility = vis, groupElements = elems,
                             groupPos = pos } -> ((vis, pos), elems))
                   (xpElem (gxFromString "group")
                           (xpPair xpickle xpickle)
                           (xpElemNodes (gxFromString "elements") xpickle))

bodyPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] con) =>
               PU [NodeG [] tag text] (Content con)
bodyPickler =
  let
    revfunc (Body b) = b
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Body, revfunc) (xpElemNodes (gxFromString "Body") xpickle)

valuePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] (Content con)
valuePickler =
  let
    revfunc (Value v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Value, revfunc) (xpElemNodes (gxFromString "Value") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] con) =>
         XmlPickler [NodeG [] tag text] (Content con) where
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
    revfunc Builder { builderName = name, builderKind = kind,
                      builderParams = params, builderSuperTypes = supers,
                      builderContent = body, builderPos = pos } =
      ((name, kind, pos), (params, supers, body))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, kind, pos), (params, supers, body)) ->
             Builder { builderName = name, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos }, revfunc)
           (xpElem (gxFromString "Builder")
                   (xpTriple xpickle xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)))

declPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Element
declPickler =
  let
    revfunc Decl { declName = name, declType = ty, declPos = pos } =
      ((name, pos), ty)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), ty) -> Decl { declName = name, declType = ty,
                                         declPos = pos }, revfunc)
           (xpElem (gxFromString "Decl") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "type") xpickle))

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
                           (xpElemNodes (gxFromString "init") xpickle)))

funPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Element
funPickler =
  let
    revfunc Fun { funName = name, funCases = cases, funPos = pos } =
      ((name, pos), cases)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), cases) -> Fun { funName = name, funCases = cases,
                                        funPos = pos }, revfunc)
           (xpElem (gxFromString "Fun") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "cases") xpickle))

truthPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
truthPickler =
  let
    revfunc Truth { truthName = name, truthKind = kind,
                    truthContent = body, truthPos = pos } =
      ((name, kind, pos), body)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, kind, pos), body) ->
             Truth { truthName = name, truthContent = body,
                     truthKind = kind, truthPos = pos }, revfunc)
           (xpElem (gxFromString "Truth") (xpTriple xpickle xpickle xpickle)
                   (xpElemNodes (gxFromString "type") xpickle))

proofPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
proofPickler =
  let
    revfunc Proof { proofName = name, proofBody = body, proofPos = pos } =
      ((name, pos), body)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), body) -> Proof { proofName = name, proofBody = body,
                                            proofPos = pos }, revfunc)
           (xpElem (gxFromString "Proof") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "type") xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Element where
  xpickle =
    let
      picker Builder {} = 0
      picker Decl {} = 1
      picker Def {} = 2
      picker Fun {} = 3
      picker Truth {} = 4
      picker Proof {} = 5
    in
      xpAlt picker [builderPickler, declPickler, defPickler,
                    funPickler, truthPickler, proofPickler]

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
    revfunc Deconstruct { deconstructName = name, deconstructPat = pat,
                          deconstructPos = pos } =
      ((name, pos), pat)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), pat) -> Deconstruct { deconstructName = name,
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
                           (xpElemNodes (gxFromString "init") xpickle)))

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
asPickler =
  let
    revfunc As { asName = name, asPat = pat, asPos = pos } = ((name, pos), pat)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), pat) -> As { asName = name, asPat = pat,
                                        asPos = pos }, revfunc)
           (xpElem (gxFromString "As") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "pattern") xpickle))

namePickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
namePickler =
  let
    revfunc Name { nameSym = name, namePos = pos } = (name, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(name, pos) -> Name { nameSym = name, namePos = pos }, revfunc)
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

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
numPickler =
  let
    revfunc Num { numVal = num, numPos = pos } =
      (numerator num, denominator num, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(numer, denom, pos) -> Num { numVal = numer % denom,
                                          numPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "Num")
                        (xpTriple (xpAttr (gxFromString "numerator") xpPrim)
                                  (xpAttr (gxFromString "denominator") xpPrim)
                                  xpickle))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
strPickler =
  let
    revfunc Str { strVal = str, strPos = pos } = (pos, gxFromByteString str)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, str) -> Str { strVal = gxToByteString str,
                                 strPos = pos }, revfunc)
           (xpElem (gxFromString "Str") xpickle (xpContent xpText0))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
charPickler =
  let
    revfunc Char { charVal = chr, charPos = pos } = (chr, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(chr, pos) -> Char { charVal = chr, charPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "char")
                        (xpPair (xpAttr (gxFromString "value") xpPrim) xpickle))

unitPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
unitPickler = xpWrap (Unit, unitPos) (xpElemAttrs (gxFromString "Unit") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Literal where
  xpickle =
    let
      picker Num {} = 0
      picker Str {} = 1
      picker Char {} = 2
      picker Unit {} = 3
    in
      xpAlt picker [numPickler, strPickler, charPickler, unitPickler]

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
    revfunc Project { projectName = name, projectVal = val,
                      projectPos = pos } = ((name, pos), val)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((name, pos), val) -> Project { projectName = name,
                                             projectVal = val,
                                             projectPos = pos }, revfunc)
           (xpElem (gxFromString "Project") (xpPair xpickle xpickle)
                   (xpElemNodes (gxFromString "value") xpickle))

symPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Exp
symPickler =
  let
    revfunc Sym { symName = name, symPos = pos } = (name, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(name, pos) -> Sym { symName = name, symPos = pos }, revfunc)
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
  xpickle = xpWrap (\((name, pos), val) -> Field { fieldName = name,
                                                    fieldVal = val,
                                                    fieldPos = pos },
                    \Field { fieldName = name, fieldVal = val,
                             fieldPos = pos } -> ((name, pos), val))
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
