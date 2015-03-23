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

-- | Abstract Syntax structure.  This represents the surface language
-- in a more abstract form than is found in the AST structure.
module Language.Salt.Surface.Syntax(
       Component(..),
       Assoc(..),
       Fixity(..),
       Syntax(..),
       Truth(..),
       Proof(..),
       Scope(..),
       Builder(..),
       Element(..),
       Compound(..),
       Pattern(..),
       Literal(..),
       Exp(..),
       Entry(..),
       Fields(..),
       Field(..),
       Case(..)
       ) where

import Control.Monad.Positions
import Control.Monad.State
import Control.Monad.Symbols
import Data.Array
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.Position
import Data.List(sort)
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common
import Prelude hiding (init, exp, Either(..))
import Text.FormatM
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.HashMap.Strict as HashMap

data Assoc = Left | Right | NonAssoc
  deriving (Ord, Eq, Enum, Show)

data Fixity = Prefix | Infix !Assoc | Postfix
  deriving (Ord, Eq, Show)

-- | A component.  Essentially, a scope that may or may not have an
-- expected definition.
data Component =
  Component {
    -- | The expected definition.
    compExpected :: !(Maybe Symbol),
    -- | The top-level scope for this component.
    compScope :: !Scope
  }

-- | A static scope.  Elements are split up by kind, into builder definitions,
-- syntax directives, truths, proofs, and regular definitions.
data Scope =
  Scope {
    -- | All the builders defined in this scope.
    scopeBuilders :: !(HashMap Symbol Builder),
    -- | The syntax directives for this scope.
    scopeSyntax :: !(HashMap Symbol Syntax),
    -- | The truth environment for this scope.  This contains all
    -- theorems, axioms, and invariants.
    scopeTruths :: !(HashMap Symbol Truth),
    -- | All concrete definitions for this scope.
    scopeElems :: !(Array Visibility [Element]),
    -- | Proofs given in this scope.
    scopeProofs :: ![Proof]
  }
  deriving Eq

-- | Syntax information for a symbol.
data Syntax =
  Syntax {
    -- | Fixity (and associativity).
    syntaxFixity :: !Fixity,
    -- | Precedence relations.
    syntaxPrecs :: ![(Ordering, Exp)]
  }
  deriving (Ord, Eq)

-- | Truths.  These are similar to declarations, except that they
-- affect the proof environment.  These include theorems and
-- invariants.
data Truth =
  Truth {
    -- | The class of the truth.
    truthKind :: !TruthKind,
    -- | The visibility of the truth.
    truthVisibility :: !Visibility,
    -- | The truth proposition.
    truthContent :: !Exp,
    -- | The position in source from which this arises.
    truthPos :: !Position
  }

-- | A builder definition.
data Builder =
  Builder {
    -- | The type of entity the builder represents.
    builderKind :: !BuilderKind,
    -- | The visibility of this definition.
    builderVisibility :: !Visibility,
    -- | The parameters of the builder entity.
    builderParams :: !Fields,
    -- | The declared supertypes for this builder entity.
    builderSuperTypes :: ![Exp],
    -- | The entities declared by the builder.
    builderContent :: !Exp,
    -- | The position in source from which this arises.
    builderPos :: !Position
  }

-- | Directives.  These are static commands, like imports or proofs,
-- which influence a scope, but do not directly define anything.
data Proof =
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
    Proof {
      -- | The name of the theorem being proven.
      proofName :: !Exp,
      -- | The body of the proof.
      proofBody :: !Exp,
      -- | The position in source from which this arises.
      proofPos :: !Position
    }

-- | Scope elements.  These represent declarations and imports
-- statements inside a scope.
data Element =
    -- | Value definitions.  These are declarations coupled with
    -- values.  These include function declarations.
    Def {
      -- | The binding pattern.
      defPattern :: !Pattern,
      -- | The value's initializer.
      defInit :: !(Maybe Exp),
      -- | The position in source from which this arises.
      defPos :: !Position
    }
    -- | An import directive.
  | Import {
      -- | The name(s) to import.
      importExp :: !Exp,
      -- | The position in source from which this arises.
      importPos :: !Position
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound =
    -- | An ordinary expression.
    Exp !Exp
    -- | A regular definition.
  | Element !Element
    -- | A dynamic truth definition.
  | Dynamic {
      -- | Name of the truth definition.
      dynamicName :: !Symbol,
      -- | The dynamic truth definition.
      dynamicTruth :: !Truth
    }
    -- | A local builder definition.
  | Local {
      -- | The name of the builder.
      localName :: !Symbol,
      -- | The local builder definition.
      localBuilder :: !Builder
    }

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
      splitFields :: !(HashMap Symbol Entry),
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
    -- expressions.  This structure defines a dynamic scope.  Syntax
    -- directives and proofs are moved out-of-line, while truths,
    -- builders, and regular definitions remain in-line.
    Compound {
      -- | Syntax directives that occurred in the compound expression.
      compoundSyntax :: !(HashMap Symbol Syntax),
      -- | Proofs given in the compound expression.
      compoundProofs :: ![Proof],
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
      seqExps :: ![Exp],
      -- | The position in source from which this arises.
      seqPos :: !Position
    }
  | RecordType {
      recordTypeFields :: !Fields,
      recordTypePos :: !Position
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | The fields in this record expression.
      recordFields :: !(HashMap FieldName Exp),
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
    -- | Builder literal.
  | Anon {
      -- | The type of entity the builder represents.
      anonKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      anonSuperTypes :: ![Exp],
      -- | The parameters of the builder entity.
      anonParams :: !Fields,
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
  Entry {
    -- | The value to which the field or argument is set.
    entryPat :: !Pattern,
    -- | The position in source from which this arises.
    entryPos :: !Position
   }

-- | Representation of fields.  This contains information for
-- interpreting record as well as tuple values.
data Fields =
  Fields {
    -- | Named bindings.
    fieldsBindings :: !(HashMap FieldName Field),
    -- | A mapping from field positions to names.
    fieldsOrder :: !(Array Word FieldName)
  }

-- | A field.
data Field =
  Field {
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

instance Eq Component where
  Component { compExpected = expected1, compScope = scope1 } ==
    Component { compExpected = expected2, compScope = scope2 } =
      expected1 == expected2 && scope1 == scope2

instance Eq Truth where
  Truth { truthKind = kind1, truthVisibility = vis1,
          truthContent = content1 } ==
    Truth { truthKind = kind2, truthVisibility = vis2,
            truthContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && content1 == content2

instance Eq Builder where
  Builder { builderKind = kind1, builderVisibility = vis1,
            builderParams = params1, builderSuperTypes = supers1,
            builderContent = content1 } ==
    Builder { builderKind = kind2, builderVisibility = vis2,
              builderParams = params2, builderSuperTypes = supers2,
              builderContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && params1 == params2 &&
      supers1 == supers2 && content1 == content2

instance Eq Proof where
  Proof { proofName = name1, proofBody = body1 } ==
    Proof { proofName = name2, proofBody = body2 } =
      name1 == name2 && body1 == body2

instance Eq Element where
  Def { defPattern = pat1, defInit = init1 } ==
    Def { defPattern = pat2, defInit = init2 } =
      pat1 == pat2 && init1 == init2
  Import { importExp = exp1 } == Import { importExp = exp2 } = exp1 == exp2
  _ == _ = False

instance Eq Compound where
  Exp exp1 == Exp exp2 = exp1 == exp2
  Element elem1 == Element elem2 = elem1 == elem2
  Dynamic { dynamicName = name1, dynamicTruth = truth1 } ==
    Dynamic { dynamicName = name2, dynamicTruth = truth2 } =
      name1 == name2 && truth1 == truth2
  Local { localName = name1, localBuilder = builder1 } ==
    Local { localName = name2, localBuilder = builder2 } =
      name1 == name2 && builder1 == builder2
  _ == _ = False

instance Eq Pattern where
  Option { optionPats = pats1 } == Option { optionPats = pats2 } =
    pats1 == pats2
  Deconstruct { deconstructName = name1, deconstructPat = pat1 } ==
    Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Split { splitFields = fields1, splitStrict = strict1 } ==
    Split { splitFields = fields2, splitStrict = strict2 } =
      strict1 == strict2 && fields1 == fields2
  Typed { typedPat = pat1, typedType = ty1 } ==
    Typed { typedPat = pat2, typedType = ty2 } =
      pat1 == pat2 && ty1 == ty2
  As { asName = name1, asPat = pat1 } ==
    As { asName = name2, asPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Name { nameSym = name1 } == Name { nameSym = name2 } = name1 == name2
  Exact lit1 == Exact lit2 = lit1 == lit2
  _ == _ = False

instance Eq Exp where
  Compound { compoundSyntax = syntax1, compoundProofs = proofs1,
             compoundBody = body1 } ==
    Compound { compoundSyntax = syntax2, compoundProofs = proofs2,
               compoundBody = body2 } =
      syntax1 == syntax2 && proofs1 == proofs2 && body1 == body2
  Abs { absKind = kind1, absCases = cases1 } ==
    Abs { absKind = kind2, absCases = cases2 } =
      kind1 == kind2 && cases1 == cases2
  Match { matchVal = val1, matchCases = cases1 } ==
    Match { matchVal = val2, matchCases = cases2 } =
      val1 == val2 && cases1 == cases2
  Ascribe { ascribeVal = val1, ascribeType = ty1 } ==
    Ascribe { ascribeVal = val2, ascribeType = ty2 } =
      val1 == val2 && ty1 == ty2
  Seq { seqExps = exps1 } == Seq { seqExps = exps2 } = exps1 == exps2
  RecordType { recordTypeFields = fields1 } ==
    RecordType { recordTypeFields = fields2 } =
      fields1 == fields2
  Record { recordFields = fields1 } == Record { recordFields = fields2 } =
    fields1 == fields2
  Tuple { tupleFields = fields1 } == Tuple { tupleFields = fields2 } =
    fields1 == fields2
  Project { projectVal = val1, projectFields = names1 } ==
    Project { projectVal = val2, projectFields = names2 } =
      names1 == names2 && val1 == val2
  Sym { symName = sym1 } == Sym { symName = sym2 } = sym1 == sym2
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
  Where { whereVal = val1, whereProp = prop1 } ==
    Where { whereVal = val2, whereProp = prop2 } =
      val1 == val2 && prop1 == prop2
    -- | Builder literal.
  Anon { anonKind = kind1, anonSuperTypes = supers1,
         anonParams = fields1, anonContent = content1 } ==
    Anon { anonKind = kind2, anonSuperTypes = supers2,
           anonParams = fields2, anonContent = content2 } =
      kind1 == kind2 && supers1 == supers2 &&
      fields1 == fields2 && content1 == content2
  Literal lit1 == Literal lit2 = lit1 == lit2
  _ == _ = False

instance Eq Entry where
  Entry { entryPat = pat1 } == Entry { entryPat = pat2 } = pat1 == pat2

instance Eq Fields where
  Fields { fieldsBindings = bindings1, fieldsOrder = order1 } ==
    Fields { fieldsBindings = bindings2, fieldsOrder = order2 } =
      bindings1 == bindings2 && order1 == order2

instance Eq Field where
  Field { fieldVal = val1 } == Field { fieldVal = val2 } = val1 == val2

instance Eq Case where
  Case { casePat = pat1, caseBody = body1 } ==
    Case { casePat = pat2, caseBody = body2 } =
      pat1 == pat2 && body1 == body2

instance Ord Component where
  compare Component { compExpected = expected1, compScope = scope1 }
          Component { compExpected = expected2, compScope = scope2 } =
    case compare expected1 expected2 of
      EQ -> compare scope1 scope2
      out -> out

instance Ord Truth where
  compare Truth { truthKind = kind1, truthVisibility = vis1,
                  truthContent = content1 }
          Truth { truthKind = kind2, truthVisibility = vis2,
                  truthContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare vis1 vis2 of
        EQ -> compare content1 content2
        out -> out
      out -> out

instance Ord Scope where
  compare Scope { scopeBuilders = builders1, scopeSyntax = syntax1,
                  scopeTruths = truths1, scopeElems = defs1,
                  scopeProofs = proofs1 }
          Scope { scopeBuilders = builders2, scopeSyntax = syntax2,
                  scopeTruths = truths2, scopeElems = defs2,
                  scopeProofs = proofs2 } =
    let
      builderlist1 = sort (HashMap.toList builders1)
      builderlist2 = sort (HashMap.toList builders2)
      truthlist1 = sort (HashMap.toList truths1)
      truthlist2 = sort (HashMap.toList truths2)
      syntaxlist1 = sort (HashMap.toList syntax1)
      syntaxlist2 = sort (HashMap.toList syntax2)
    in
      case compare builderlist1 builderlist2 of
        EQ -> case compare syntaxlist1 syntaxlist2 of
          EQ -> case compare truthlist1 truthlist2 of
            EQ -> case compare defs1 defs2 of
              EQ -> compare proofs1 proofs2
              out -> out
            out -> out
          out -> out
        out -> out

instance Ord Builder where
  compare Builder { builderKind = kind1, builderVisibility = vis1,
                    builderParams = params1, builderSuperTypes = supers1,
                    builderContent = content1 }
          Builder { builderKind = kind2, builderVisibility = vis2,
                    builderParams = params2, builderSuperTypes = supers2,
                    builderContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare vis1 vis2 of
        EQ -> case compare params1 params2 of
          EQ -> case compare supers1 supers2 of
            EQ -> compare content1 content2
            out -> out
          out -> out
        out -> out
      out -> out

instance Ord Proof where
  compare Proof { proofName = name1, proofBody = body1 }
          Proof { proofName = name2, proofBody = body2 } =
    case compare name1 name2 of
      EQ -> compare body1 body2
      out -> out

instance Ord Element where
  compare Def { defPattern = pat1, defInit = init1 }
          Def { defPattern = pat2, defInit = init2 } =
    case compare pat1 pat2 of
      EQ -> compare init1 init2
      out -> out
  compare Def {} _ = LT
  compare _ Def {} = GT
  compare Import { importExp = exp1 } Import { importExp = exp2 } =
    compare exp1 exp2

instance Ord Compound where
  compare (Exp exp1) (Exp exp2) = compare exp1 exp2
  compare (Exp _) _ = LT
  compare _ (Exp _) = GT
  compare (Element elem1) (Element elem2) = compare elem1 elem2
  compare (Element _) _ = LT
  compare _ (Element _) = GT
  compare Dynamic { dynamicName = name1, dynamicTruth = truth1 }
          Dynamic { dynamicName = name2, dynamicTruth = truth2 } =
    case compare name1 name2 of
      EQ -> compare truth1 truth2
      out -> out
  compare Dynamic {} _ = LT
  compare _ Dynamic {} = GT
  compare Local { localName = name1, localBuilder = builder1 }
          Local { localName = name2, localBuilder = builder2 } =
    case compare name1 name2 of
      EQ -> compare builder1 builder2
      out -> out

instance Ord Pattern where
  compare Option { optionPats = pats1 } Option { optionPats = pats2 } =
    compare pats1 pats2
  compare Option {} _ = LT
  compare _ Option {} = GT
  compare Deconstruct { deconstructName = name1, deconstructPat = pat1 }
          Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare Deconstruct {} _ = LT
  compare _ Deconstruct {} = GT
  compare Split { splitFields = fields1, splitStrict = strict1 }
          Split { splitFields = fields2, splitStrict = strict2 } =
    let
      fieldlist1 = sort (HashMap.toList fields1)
      fieldlist2 = sort (HashMap.toList fields2)
    in
      case compare strict1 strict2 of
        EQ -> compare fieldlist1 fieldlist2
        out -> out
  compare Split {} _ = LT
  compare _ Split {} = GT
  compare Typed { typedPat = pat1, typedType = ty1 }
          Typed { typedPat = pat2, typedType = ty2 } =
    case compare pat1 pat2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Typed {} _ = LT
  compare _ Typed {} = GT
  compare As { asName = name1, asPat = pat1 }
          As { asName = name2, asPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare As {} _ = LT
  compare _ As {} = GT
  compare Name { nameSym = name1 } Name { nameSym = name2 } =
    compare name1 name2
  compare Name {} _ = LT
  compare _ Name {} = GT
  compare (Exact lit1) (Exact lit2) = compare lit1 lit2

instance Ord Exp where
  compare Compound { compoundSyntax = syntax1, compoundProofs = proofs1,
                     compoundBody = body1 }
          Compound { compoundSyntax = syntax2, compoundProofs = proofs2,
                     compoundBody = body2 } =
    let
      syntaxlist1 = sort (HashMap.toList syntax1)
      syntaxlist2 = sort (HashMap.toList syntax2)
    in case compare syntaxlist1 syntaxlist2 of
      EQ -> case compare proofs1 proofs2 of
        EQ -> compare body1 body2
        out -> out
      out -> out
  compare Compound {} _ = LT
  compare _ Compound {} = GT
  compare Abs { absKind = kind1, absCases = cases1 }
          Abs { absKind = kind2, absCases = cases2 } =
    case compare kind1 kind2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Abs {} _ = LT
  compare _ Abs {} = GT
  compare Match { matchVal = val1, matchCases = cases1 }
          Match { matchVal = val2, matchCases = cases2 } =
    case compare val1 val2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Match {} _ = LT
  compare _ Match {} = GT
  compare Ascribe { ascribeVal = val1, ascribeType = ty1 }
          Ascribe { ascribeVal = val2, ascribeType = ty2 } =
    case compare val1 val2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Ascribe {} _ = LT
  compare _ Ascribe {} = GT
  compare Seq { seqExps = exps1 } Seq { seqExps = exps2 } = compare exps1 exps2
  compare Seq {} _ = LT
  compare _ Seq {} = GT
  compare RecordType { recordTypeFields = fields1 }
          RecordType { recordTypeFields = fields2 } =
    compare fields1 fields2
  compare RecordType {} _ = LT
  compare _ RecordType {} = GT
  compare Record { recordFields = fields1 } Record { recordFields = fields2 } =
    let
      fieldlist1 = sort (HashMap.toList fields1)
      fieldlist2 = sort (HashMap.toList fields2)
    in
      compare fieldlist1 fieldlist2
  compare Record {} _ = LT
  compare _ Record {} = GT
  compare Tuple { tupleFields = fields1 } Tuple { tupleFields = fields2 } =
    compare fields1 fields2
  compare Tuple {} _ = LT
  compare _ Tuple {} = GT
  compare Project { projectVal = val1, projectFields = names1 }
          Project { projectVal = val2, projectFields = names2 } =
    case compare names1 names2 of
      EQ -> compare val1 val2
      out -> out
  compare Project {} _ = LT
  compare _ Project {} = GT
  compare Sym { symName = sym1 } Sym { symName = sym2 } = compare sym1 sym2
  compare Sym {} _ = LT
  compare _ Sym {} = GT
  compare With { withVal = val1, withArgs = args1 }
          With { withVal = val2, withArgs = args2 } =
    case compare val1 val2 of
      EQ -> compare args1 args2
      out -> out
  compare With {} _ = LT
  compare _ With {} = GT
  compare Where { whereVal = val1, whereProp = prop1 }
          Where { whereVal = val2, whereProp = prop2 } =
    case compare val1 val2 of
      EQ -> compare prop1 prop2
      out -> out
  compare Where {} _ = LT
  compare _ Where {} = GT
  compare Anon { anonKind = kind1, anonSuperTypes = supers1,
                 anonParams = fields1, anonContent = content1 }
          Anon { anonKind = kind2, anonSuperTypes = supers2,
                 anonParams = fields2, anonContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare supers1 supers2 of
        EQ -> case compare fields1 fields2 of
          EQ -> compare content1 content2
          out -> out
        out -> out
      out -> out
  compare Anon {} _ = LT
  compare _ Anon {} = GT
  compare (Literal lit1) (Literal lit2) = compare lit1 lit2

instance Ord Entry where
  compare Entry { entryPat = pat1 } Entry { entryPat = pat2 } =
    compare pat1 pat2

instance Ord Fields where
  compare Fields { fieldsBindings = bindings1, fieldsOrder = order1 }
          Fields { fieldsBindings = bindings2, fieldsOrder = order2 } =
    let
      binds1 = map (\sym -> (sym, HashMap.lookup sym bindings1)) (elems order1)
      binds2 = map (\sym -> (sym, HashMap.lookup sym bindings2)) (elems order2)
    in
      compare binds1 binds2

instance Ord Field where
  compare Field { fieldVal = val1 } Field { fieldVal = val2 } =
    compare val1 val2

instance Ord Case where
  compare Case { casePat = pat1, caseBody = body1 }
          Case { casePat = pat2, caseBody = body2 } =
    case compare pat1 pat2 of
      EQ -> compare body1 body2
      out -> out

instance Hashable Component where
  hashWithSalt s Component { compExpected = expected, compScope = scope } =
    s `hashWithSalt` expected `hashWithSalt` scope

instance Hashable Assoc where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Fixity where
  hashWithSalt s Prefix = hashWithSalt s (0 :: Int)
  hashWithSalt s (Infix assoc) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` assoc
  hashWithSalt s Postfix = hashWithSalt s (2 :: Int)

instance Hashable Syntax where
  hashWithSalt s Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
    s `hashWithSalt` fixity `hashWithSalt` precs

instance Hashable Truth where
  hashWithSalt s Truth { truthKind = kind, truthVisibility = vis,
                         truthContent = content } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt` content

instance Hashable Scope where
  hashWithSalt s Scope { scopeBuilders = builders, scopeSyntax = syntax,
                         scopeTruths = truths, scopeElems = defs,
                         scopeProofs = proofs } =
    let
      builderlist = sort (HashMap.toList builders)
      truthlist = sort (HashMap.toList truths)
      syntaxlist = sort (HashMap.toList syntax)
    in
      s `hashWithSalt` builderlist `hashWithSalt` syntaxlist `hashWithSalt`
      truthlist `hashWithSalt` elems defs `hashWithSalt` proofs

instance Hashable Builder where
  hashWithSalt s Builder { builderKind = kind, builderVisibility = vis,
                           builderParams = params, builderSuperTypes = supers,
                           builderContent = content } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` content

instance Hashable Proof where
  hashWithSalt s Proof { proofName = sym, proofBody = body } =
    s `hashWithSalt` sym `hashWithSalt` body

instance Hashable Element where
  hashWithSalt s Def { defPattern = pat, defInit = init } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` pat `hashWithSalt` init
  hashWithSalt s Import { importExp = exp } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` exp

instance Hashable Compound where
  hashWithSalt s (Exp exp) = s `hashWithSalt` (0 :: Int) `hashWithSalt` exp
  hashWithSalt s (Element e) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s Dynamic { dynamicName = sym, dynamicTruth = truth } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` sym `hashWithSalt` truth
  hashWithSalt s Local { localName = sym, localBuilder = builder } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` sym `hashWithSalt` builder

instance Hashable Pattern where
  hashWithSalt s Option { optionPats = pats } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` pats
  hashWithSalt s Deconstruct { deconstructName = sym, deconstructPat = pat } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Split { splitFields = fields, splitStrict = strict } =
    let
      fieldlist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (2 :: Int) `hashWithSalt` fieldlist `hashWithSalt` strict
  hashWithSalt s Typed { typedPat = pat, typedType = ty } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` pat `hashWithSalt` ty
  hashWithSalt s As { asName = sym, asPat = pat } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Name { nameSym = sym } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym
  hashWithSalt s (Exact lit) = s `hashWithSalt` (6 :: Int) `hashWithSalt` lit

instance Hashable Exp where
  hashWithSalt s Compound { compoundSyntax = syntax, compoundProofs = proofs,
                            compoundBody = body } =
    let
      syntaxlist = sort (HashMap.toList syntax)
    in
      s `hashWithSalt` (1 :: Int) `hashWithSalt` syntaxlist `hashWithSalt`
      proofs `hashWithSalt` body
  hashWithSalt s Abs { absKind = kind, absCases = cases } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` kind `hashWithSalt` cases
  hashWithSalt s Match { matchVal = val, matchCases = cases } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val `hashWithSalt` cases
  hashWithSalt s Ascribe { ascribeVal = val, ascribeType = ty } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` val `hashWithSalt` ty
  hashWithSalt s Seq { seqExps = exps } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` exps
  hashWithSalt s Record { recordFields = fields } =
    let
      fieldlist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (6 :: Int) `hashWithSalt` fieldlist
  hashWithSalt s RecordType { recordTypeFields = fields } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` fields
  hashWithSalt s Tuple { tupleFields = fields } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` fields
  hashWithSalt s Project { projectVal = val, projectFields = sym } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` sym `hashWithSalt` val
  hashWithSalt s Sym { symName = sym } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt` sym
  hashWithSalt s With { withVal = val, withArgs = args } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` val `hashWithSalt` args
  hashWithSalt s Where { whereVal = val, whereProp = prop } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt` val `hashWithSalt` prop
  hashWithSalt s Anon { anonKind = cls, anonParams = params,
                        anonSuperTypes = supers, anonContent = body } =
    s `hashWithSalt` (13 :: Int) `hashWithSalt` cls `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s (Literal lit) =
    s `hashWithSalt` (14 :: Int) `hashWithSalt` lit

instance Hashable Entry where
  hashWithSalt s Entry { entryPat = pat } = s `hashWithSalt` pat

instance Hashable Fields where
  hashWithSalt s Fields { fieldsBindings = bindings, fieldsOrder = order } =
    let
      binds = map (\sym -> (sym, HashMap.lookup sym bindings)) (elems order)
    in
      s `hashWithSalt` binds

instance Hashable Field where
  hashWithSalt s = hashWithSalt s . fieldVal

instance Hashable Case where
  hashWithSalt s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt` pat `hashWithSalt` body
{-
precDot :: MonadSymbols m => (Ordering, Exp) -> StateT Word m (Doc, String)
precDot (ord, exp) =
  let
    orddoc = case ord of
      LT -> string "<"
      EQ -> string "=="
      GT -> string ">"
  in do
    nodeid <- getNodeID
    (expnode, expname) <- expDot exp
    return (expnode <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Prec | " <>
                               orddoc <> string " | " <>
                               string "<exp> exp\"") <$>
                      string "shape = \"record\"") <>
            dquoted (string nodeid <> string ":exp") <>
            string " -> " <> string expname, nodeid)

syntaxDot :: MonadSymbols m => Syntax -> StateT Word m (Doc, String)
syntaxDot Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
  let
    elemEdge nodeid (_, elemname) =
      dquoted (string nodeid) <> string ":precs" <>
      string " -> " <> dquoted (string elemname)
  in do
    nodeid <- getNodeID
    precdocs <- mapM precDot precs
    return (vcat (map fst precdocs) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Syntax | " <>
                               format fixity <> string " | " <>
                               string "<precs> precs\"") <$>
                      string "shape = \"record\"") <>
            dquoted (string nodeid <> string ":value") <>
            char ';' <$> vcat (map (elemEdge nodeid) precdocs), nodeid)
-}
instance Format Assoc where format = string . show
instance Format Fixity where format = string . show

instance (MonadSymbols m, MonadPositions m) => FormatM m (Ordering, Exp) where
  formatM (ord, exp) =
    let
      orddoc = case ord of
        LT -> string "<"
        EQ -> string "=="
        GT -> string ">"
    in do
      expdoc <- formatM exp
      return $! constructorDoc (string "Prec") [(string "ord", orddoc),
                                                (string "name", expdoc)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Syntax where
  formatM Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
    do
      precdocs <- mapM formatM precs
      return $! constructorDoc (string "Syntax")
                               [(string "fixity", format fixity),
                                (string "scope", listDoc precdocs)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Truth where
  formatM Truth { truthVisibility = vis, truthContent = content,
                  truthKind = kind, truthPos = pos } =
    do
      posdoc <- formatM pos
      contentdoc <- formatM content
      return (constructorDoc (string "Truth")
                             [(string "visibility", format vis),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "content", contentdoc)])

formatMap :: (MonadSymbols m, MonadPositions m, FormatM m key, FormatM m val) =>
             HashMap key val -> m Doc
formatMap hashmap =
  let
    formatEntry (sym, ent) =
      do
        symdoc <- formatM sym
        entdoc <- formatM ent
        return $! tupleDoc [symdoc, entdoc]
  in do
    entrydocs <- mapM formatEntry (HashMap.toList hashmap)
    return $! listDoc entrydocs

formatElems :: (MonadSymbols m, MonadPositions m) =>
               Array Visibility [Element] -> m Doc
formatElems arr =
  do
    hiddendocs <- mapM formatM (arr ! Hidden)
    privatedocs <- mapM formatM (arr ! Private)
    protecteddocs <- mapM formatM (arr ! Protected)
    publicdocs <- mapM formatM (arr ! Public)
    return $! constructorDoc (string "Elems")
                             [(string "hidden", listDoc hiddendocs),
                              (string "private", listDoc privatedocs),
                              (string "protected", listDoc protecteddocs),
                              (string "public", listDoc publicdocs)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Component where
  formatM Component { compExpected = Just expected, compScope = scope } =
    do
      expecteddoc <- formatM expected
      scopedoc <- formatM scope
      return $ constructorDoc (string "Component")
                              [(string "expected", expecteddoc),
                               (string "scope", scopedoc)]
  formatM Component { compExpected = Nothing, compScope = scope } =
    do
      scopedoc <- formatM scope
      return $ constructorDoc (string "Component")
                              [(string "scope", scopedoc)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Scope where
  formatM Scope { scopeBuilders = builders, scopeSyntax = syntax,
                  scopeTruths = truths, scopeElems = defs,
                  scopeProofs = proofs } =
    do
      buildersdoc <- formatMap builders
      syntaxdoc <- formatMap syntax
      truthsdoc <- formatMap truths
      elemsdoc <- formatElems defs
      proofsdoc <- mapM formatM proofs
      return $! constructorDoc (string "Scope")
                               [(string "builders", buildersdoc),
                                (string "syntax", syntaxdoc),
                                (string "truths", truthsdoc),
                                (string "elems", elemsdoc),
                                (string "proofs", listDoc proofsdoc)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Builder where
  formatM Builder { builderVisibility = vis, builderKind = cls,
                    builderSuperTypes = supers, builderParams = params,
                    builderContent = body, builderPos = pos } =
    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- formatM params
      bodydoc <- formatM body
      return (constructorDoc (string "Builder")
                             [(string "visibility", format vis),
                              (string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m) => FormatM m Proof where
  formatM Proof { proofName = exp, proofBody = body, proofPos = pos } =
    do
      namedoc <- formatM exp
      posdoc <- formatM pos
      bodydoc <- formatM body
      return (constructorDoc (string "Proof")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m) => FormatM m Element where
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
  formatM Dynamic { dynamicName = sym, dynamicTruth = truth } =
    do
      symdoc <- formatM sym
      truthdoc <- formatM truth
      return (constructorDoc (string "Dynamic")
                             [(string "sym", symdoc),
                              (string "truth", truthdoc)])
  formatM Local { localName = sym, localBuilder = builder } =
    do
      symdoc <- formatM sym
      builderdoc <- formatM builder
      return (constructorDoc (string "Local")
                             [(string "sym", symdoc),
                              (string "builder", builderdoc)])

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
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "true"),
                              (string "fields", fieldsdoc)])
  formatM Split { splitFields = fields, splitStrict = False, splitPos = pos } =
    do
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "false"),
                              (string "fields", fieldsdoc)])
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
  formatM Compound { compoundSyntax = syntax, compoundProofs = proofs,
                     compoundBody = body, compoundPos = pos } =
    do
      syntaxdoc <- formatMap syntax
      proofsdoc <- mapM formatM proofs
      posdoc <- formatM pos
      bodydoc <- mapM formatM body
      return (constructorDoc (string "Compound")
                             [(string "pos", posdoc),
                              (string "syntax", syntaxdoc),
                              (string "proofs", listDoc proofsdoc),
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
  formatM Seq { seqExps = exps, seqPos = pos } =
    do
      posdoc <- formatM pos
      expdocs <- mapM formatM exps
      return (constructorDoc (string "Seq")
                             [(string "pos", posdoc),
                              (string "exps", listDoc expdocs)])
  formatM Record { recordFields = fields, recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatMap fields
      return (constructorDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
  formatM RecordType { recordTypeFields = fields,
                       recordTypePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatM fields
      return (constructorDoc (string "RecordType")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
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
      paramdocs <- formatM params
      bodydoc <- formatM body
      return (constructorDoc (string "Anon")
                             [(string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])
  formatM (Literal l) = formatM l

instance (MonadPositions m, MonadSymbols m) => FormatM m Entry where
  formatM Entry { entryPat = pat, entryPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Entry")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Fields where
  formatM Fields { fieldsBindings = bindings, fieldsOrder = order } =
    do
      bindingsdoc <- formatMap bindings
      orderdoc <- mapM formatM (elems order)
      return (constructorDoc (string "Fields")
                             [(string "bindings", bindingsdoc),
                              (string "value", listDoc orderdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Field where
  formatM Field { fieldVal = val, fieldPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Field")
                             [(string "pos", posdoc),
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
         XmlPickler (Attributes tag text) Assoc where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Left, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "Left")),
                   xpWrap (const Right, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "Right")),
                   xpWrap (const NonAssoc, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "NonAssoc"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Fixity where
  xpickle =
    let
      picker Prefix = 0
      picker (Infix _) = 1
      picker Postfix = 2

      unpackInfix (Infix a) = ((), Just a)
      unpackInfix _ = error "Can't unpack"

      packInfix ((), Just a) = Infix a
      packInfix _ = error "Need associativity for infix"
    in
      xpAlt picker [xpWrap (const Prefix, const ((), Nothing))
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Prefix"))
                                   (xpOption xpZero)),
                    xpWrap (packInfix, unpackInfix)
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Infix"))
                                   (xpOption xpickle)),
                    xpWrap (const Postfix, const ((), Nothing))
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Postfix"))
                                   (xpOption xpZero))]

precPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] (Ordering, Exp)
precPickler = xpElem (gxFromString "prec")
                     (xpAttr (gxFromString "order") xpPrim) xpickle

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Syntax where
  xpickle =
    xpWrap (\(fixity, precs) -> Syntax { syntaxFixity = fixity,
                                         syntaxPrecs = precs },
            \Syntax { syntaxFixity = fixity, syntaxPrecs = precs } ->
            (fixity, precs))
           (xpElem (gxFromString "Syntax") xpickle (xpList precPickler))

mapPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler (Attributes tag text) key,
               XmlPickler [NodeG [] tag text] val,
               Hashable key, Eq key) =>
              PU [NodeG [] tag text] (HashMap key val)
mapPickler = xpWrap (HashMap.fromList, HashMap.toList)
                    (xpElemNodes (gxFromString "Map")
                                 (xpList (xpElem (gxFromString "entry")
                                                 xpickle xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Truth where
  xpickle =
    xpWrap (\((vis, kind, pos), body) ->
             Truth { truthVisibility = vis, truthContent = body,
                     truthKind = kind, truthPos = pos },
            \Truth { truthVisibility = vis, truthKind = kind,
                    truthContent = body, truthPos = pos } ->
              ((vis, kind, pos), body))
           (xpElem (gxFromString "Truth") (xpTriple xpickle xpickle xpickle)
                   (xpElemNodes (gxFromString "type") xpickle))

defsPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] (Array Visibility [Element])
defsPickler =
  xpWrap (\(hiddens, privates, protecteds, publics) ->
           listArray (Hidden, Public) [hiddens, privates, protecteds, publics],
          \arr -> (arr ! Hidden, arr ! Private, arr ! Protected, arr ! Public))
         (xpElemNodes (gxFromString "defs")
                      (xp4Tuple (xpElemNodes (gxFromString "hidden")
                                             (xpList xpickle))
                                (xpElemNodes (gxFromString "private")
                                             (xpList xpickle))
                                (xpElemNodes (gxFromString "protected")
                                             (xpList xpickle))
                                (xpElemNodes (gxFromString "public")
                                             (xpList xpickle))))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Component where
  xpickle =
    xpWrap (\(expected, scope) -> Component { compExpected = expected,
                                              compScope = scope },
            \Component { compExpected = expected,
                         compScope = scope } -> (expected, scope))
           (xpElem (gxFromString "Component") (xpOption xpickle) xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Scope where
  xpickle =
    xpWrap (\(builders, syntax, truths, defs, proofs) ->
             Scope { scopeBuilders = builders, scopeSyntax = syntax,
                     scopeTruths = truths, scopeElems = defs,
                     scopeProofs = proofs },
            \Scope { scopeBuilders = builders, scopeSyntax = syntax,
                     scopeTruths = truths, scopeElems = defs,
                     scopeProofs = proofs } ->
            (builders, syntax, truths, defs, proofs))
           (xpElemNodes (gxFromString "Scope")
                        (xp5Tuple mapPickler mapPickler mapPickler
                                  defsPickler (xpList xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Builder where
  xpickle =
    xpWrap (\((vis, kind, pos), (params, supers, body)) ->
             Builder { builderVisibility = vis, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos },
            \Builder { builderVisibility = vis, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos } ->
            ((vis, kind, pos), (params, supers, body)))
           (xpElem (gxFromString "Builder")
                   (xpTriple xpickle xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Proof where
  xpickle = xpWrap (\(pos, (pname, body)) -> Proof { proofName = pname,
                                                     proofBody = body,
                                                     proofPos = pos },
                    \Proof { proofName = pname, proofBody = body,
                             proofPos = pos } -> (pos, (pname, body)))
                   (xpElem (gxFromString "Proof") xpickle
                           (xpPair (xpElemNodes (gxFromString "name") xpickle)
                                   (xpElemNodes (gxFromString "type") xpickle)))

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
      picker Def {} = 0
      picker Import {} = 1
    in
      xpAlt picker [ defPickler, importPickler ]

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

dynamicPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Compound
dynamicPickler =
  let
    revfunc (Dynamic { dynamicName = sym, dynamicTruth = truth }) = (sym, truth)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, truth) -> Dynamic { dynamicName = sym,
                                       dynamicTruth = truth }, revfunc)
           (xpElem (gxFromString "Dynamic") xpickle xpickle)

localPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Compound
localPickler =
  let
    revfunc (Local { localName = sym, localBuilder = builder }) = (sym, builder)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, builder) -> Local { localName = sym,
                                       localBuilder = builder }, revfunc)
           (xpElem (gxFromString "Local") xpickle xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Compound where
  xpickle =
    let
      picker (Exp _) = 0
      picker (Element _) = 1
      picker Dynamic {} = 2
      picker Local {} = 3
    in
      xpAlt picker [expPickler, elementPickler, dynamicPickler, localPickler]

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
                   (xpElemNodes (gxFromString "fields") mapPickler))

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

compoundPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Exp
compoundPickler =
  let
    revfunc Compound { compoundSyntax = syntax, compoundProofs = proofs,
                       compoundBody = body, compoundPos = pos } =
      (pos, (syntax, proofs, body))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, (syntax, proofs, body)) ->
             Compound { compoundSyntax = syntax, compoundProofs = proofs,
                        compoundBody = body, compoundPos = pos }, revfunc)
           (xpElem (gxFromString "Compound") xpickle
                   (xpTriple (xpElemNodes (gxFromString "syntax")
                                          mapPickler)
                             (xpElemNodes (gxFromString "proofs")
                                          (xpList xpickle))
                             (xpElemNodes (gxFromString "body")
                                          (xpList xpickle))))

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
    revfunc Seq { seqExps = exps, seqPos = pos } = (pos, exps)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, exps) -> Seq { seqExps = exps, seqPos = pos }, revfunc)
           (xpElem (gxFromString "Seq") xpickle (xpList xpickle))

recordPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
recordPickler =
  let
    revfunc Record { recordFields = fields, recordPos = pos } = (pos, fields)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, fields) -> Record { recordFields = fields,
                                       recordPos = pos }, revfunc)
           (xpElem (gxFromString "Record") xpickle
                   (xpElemNodes (gxFromString "fields") mapPickler))

recordTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text) =>
                     PU [NodeG [] tag text] Exp
recordTypePickler =
  let
    revfunc RecordType { recordTypeFields = fields,
                         recordTypePos = pos } = (pos, fields)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, fields) -> RecordType { recordTypeFields = fields,
                                           recordTypePos = pos }, revfunc)
           (xpElem (gxFromString "Record") xpickle
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
      picker RecordType {} = 6
      picker Tuple {} = 7
      picker Project {} = 8
      picker Sym {} = 9
      picker With {} = 10
      picker Where {} = 11
      picker Anon {} = 12
      picker (Literal _) = 13
    in
      xpAlt picker [compoundPickler, absPickler, matchPickler, ascribePickler,
                    seqPickler, recordPickler, recordTypePickler, tuplePickler,
                    projectPickler, symPickler, withPickler, wherePickler,
                    anonPickler, literalPickler]

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

makeArray :: [a] -> Array Word a
makeArray l = listArray (1, fromIntegral (length l)) l

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Entry where
  xpickle = xpWrap (\(pos, pat) -> Entry { entryPat = pat, entryPos = pos },
                    \Entry { entryPat = pat, entryPos = pos } -> (pos, pat))
                   (xpElem (gxFromString "Entry") xpickle xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Fields where
  xpickle = xpWrap (\(bindings, order) ->
                     Fields { fieldsBindings = bindings,
                              fieldsOrder = makeArray order },
                    \Fields { fieldsBindings = bindings,
                              fieldsOrder = order } -> (bindings, elems order))
                   (xpElemNodes (gxFromString "Fields")
                                (xpPair mapPickler (xpList xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Field where
  xpickle = xpWrap (\(pos, val) -> Field { fieldVal = val, fieldPos = pos },
                    \Field { fieldVal = val, fieldPos = pos } -> (pos, val))
                   (xpElem (gxFromString "Field") xpickle xpickle)

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
