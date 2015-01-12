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

import Data.Array
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.Position
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common

data Assoc = Left | Right | NonAssoc
  deriving (Ord, Eq, Enum, Show)

data Fixity = Prefix | Infix !Assoc | Postfix
  deriving (Ord, Eq, Show)

-- | Syntax information for a symbol.
data Syntax =
  Syntax {
    -- | Fixity (and associativity).
    syntaxFixity :: !Fixity,
    -- | Precedence relations.
    syntaxPrecs :: ![(Ordering, Exp)]
  }
  deriving Eq

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

-- | A static scope.  Elements are split up by kind, into builder definitions,
-- syntax directives, truths, proofs, and regular definitions.
data Scope =
  Scope {
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
      projectName :: ![FieldName],
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
  Project { projectVal = val1, projectName = names1 } ==
    Project { projectVal = val2, projectName = names2 } =
      names1 == names2 && val1 == val2
  Sym { symName = sym1 } == Sym { symName = sym2 } = sym1 == sym2
    -- | A with expression.  Represents currying.
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
    -- | Where expression.  Constructs refinement types.
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

instance Hashable Assoc where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Fixity where
  hashWithSalt s Prefix = hashWithSalt s (0 :: Int)
  hashWithSalt s (Infix assoc) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` assoc
  hashWithSalt s Postfix = hashWithSalt s (2 :: Int)
