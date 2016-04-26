-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
       Component(..),
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
       -- * Graphviz Renderer
       astDot
       ) where

import Control.Monad
import Control.Monad.Positions
import Control.Monad.State
import Control.Monad.Symbols
import Data.Hashable
import Data.List hiding (init, concat)
import Data.PositionElement
import Data.Symbol
import Data.Word
import Language.Salt.Format
import Language.Salt.Surface.Common
import Prelude hiding (sequence, init, exp, concat)
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict

-- | Type of an AST.
data AST =
  AST {
    astComponent :: !(Maybe Component),
    -- | The headers.
    astUses :: ![Use],
    -- | The top-level scope.
    astScope :: ![Element]
  }
  deriving (Ord, Eq)

-- | A component statement.
data Component =
  Component {
    -- | The component name.
    componentName :: ![Symbol],
    -- | The position in source from which this arises.
    componentPos :: !Position
  }

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
    Body { bodyScope :: !Scope }
    -- | An expression.
  | Value { valueExp :: !Exp }

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
      -- | A proof (may or may not be supplied).
      truthProof :: !(Maybe Exp),
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
  | Syntax {
      -- | The value to syntax.
      syntaxExp :: !Exp,
      -- | The position in source from which this arises.
      syntaxPos :: !Position
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound =
    -- | An ordinary expression.
    Exp { expVal :: !Exp }
    -- | A definition.
  | Element { elemVal :: !Element }

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
    -- | An exact pattern.  Matches only the given literal.
  | Exact { exactLit :: !Literal }

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
      -- | The expressions in the sequence.
      seqExps :: ![Exp],
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
  | Literal { literalVal :: !Literal }

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
  | Unnamed { unnamedPat :: !Pattern }

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

instance PositionElement Component where
  position Component { componentPos = pos } = pos

instance PositionElement Use where
  position Use { usePos = pos } = pos

instance PositionElement Group where
  position Group { groupPos = pos } = pos

instance PositionElement Element where
  position Builder { builderPos = pos } = pos
  position Def { defPos = pos } = pos
  position Fun { funPos = pos } = pos
  position Truth { truthPos = pos } = pos
  position Proof { proofPos = pos } = pos
  position Import { importPos = pos } = pos
  position Syntax { syntaxPos = pos } = pos

instance PositionElement Compound where
  position Exp { expVal = e } = position e
  position Element { elemVal = e } = position e

instance PositionElement Pattern where
  position Option { optionPos = pos } = pos
  position Deconstruct { deconstructPos = pos } = pos
  position Split { splitPos = pos } = pos
  position Typed { typedPos = pos } = pos
  position As { asPos = pos } = pos
  position Name { namePos = pos } = pos
  position Exact { exactLit = l } = position l

instance PositionElement Exp where
  position Compound { compoundPos = pos } = pos
  position Abs { absPos = pos } = pos
  position Match { matchPos = pos } = pos
  position Ascribe { ascribePos = pos } = pos
  position Seq { seqPos = pos } = pos
  position Record { recordPos = pos } = pos
  position Tuple { tuplePos = pos } = pos
  position Project { projectPos = pos } = pos
  position Sym { symPos = pos } = pos
  position With { withPos = pos } = pos
  position Where { wherePos = pos } = pos
  position Anon { anonPos = pos } = pos
  position Literal { literalVal = l } = position l

instance PositionElement Entry where
  position Named { namedPos = pos } = pos
  position Unnamed { unnamedPat = pat } = position pat

instance PositionElement Field where
  position Field { fieldPos = pos } = pos

instance PositionElement Case where
  position Case { casePos = pos } = pos

instance Eq Component where
  Component { componentName = name1 } == Component { componentName = name2 } =
    name1 == name2

instance Eq Use where
  Use { useName = name1 } == Use { useName = name2 } = name1 == name2

instance Eq Group where
  Group { groupVisibility = vis1, groupElements = elems1 } ==
    Group { groupVisibility = vis2, groupElements = elems2 } =
      vis1 == vis2 && elems1 == elems2

instance Eq Content where
  Body { bodyScope = b1 } == Body { bodyScope = b2 } = b1 == b2
  Value { valueExp = v1 } == Value { valueExp = v2 } = v1 == v2
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
  Truth { truthName = name1, truthKind = kind1,
          truthContent = prop1, truthProof = proof1 } ==
    Truth { truthName = name2, truthKind = kind2,
            truthContent = prop2, truthProof = proof2 } =
    name1 == name2 && kind1 == kind2 && prop1 == prop2 && proof1 == proof2
  Proof { proofName = name1, proofBody = body1 } ==
    Proof { proofName = name2, proofBody = body2 } =
      name1 == name2 && body1 == body2
  Import { importExp = exp1 } == Import { importExp = exp2 } = exp1 == exp2
  Syntax { syntaxExp = exp1 } == Syntax { syntaxExp = exp2 } = exp1 == exp2
  _ == _ = False

instance Eq Compound where
  Exp { expVal = e1 } == Exp { expVal = e2 } = e1 == e2
  Element { elemVal = e1 } == Element { elemVal = e2 } = e1 == e2
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
  Exact { exactLit = e1 } == Exact { exactLit = e2 } = e1 == e2
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
  Seq { seqExps = exps1 } == Seq { seqExps = exps2 } = exps1 == exps2
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
  Literal { literalVal = lit1 } == Literal { literalVal = lit2 } = lit1 == lit2
  _ == _ = False

instance Eq Entry where
  Named { namedSym = name1, namedVal = val1 } ==
    Named { namedSym = name2, namedVal = val2 } =
    name1 == name2 && val1 == val2
  Unnamed { unnamedPat = e1 } == Unnamed { unnamedPat = e2 } = e1 == e2
  _ == _ = False

instance Eq Field where
  Field { fieldName = name1, fieldVal = val1 } ==
    Field { fieldName = name2, fieldVal = val2 } =
    name1 == name2 && val1 == val2

instance Eq Case where
  Case { caseBody = body1, casePat = pat1 } ==
    Case { caseBody = body2, casePat = pat2 } =
      body1 == body2 && pat1 == pat2

instance Ord Component where
  compare Component { componentName = name1 }
          Component { componentName = name2 } =
    compare name1 name2

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
  compare Body { bodyScope = b1 } Body { bodyScope = b2 } = compare b1 b2
  compare Body {} _ = GT
  compare _ Body {} = LT
  compare Value { valueExp = v1 } Value { valueExp = v2 } = compare v1 v2

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
  compare Truth { truthName = name1, truthKind = kind1,
                  truthContent = prop1, truthProof = proof1 }
          Truth { truthName = name2, truthKind = kind2,
                  truthContent = prop2, truthProof = proof2 } =
    case compare name1 name2 of
      EQ -> case compare kind1 kind2 of
        EQ -> case compare prop1 prop2 of
          EQ -> compare proof1 proof2
          out -> out
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
  compare Import {} _ = GT
  compare _ Import {} = LT
  compare Syntax { syntaxExp = exp1 } Syntax { syntaxExp = exp2 } =
    compare exp1 exp2

instance Ord Compound where
  compare Exp { expVal = e1} Exp { expVal = e2 } = compare e1 e2
  compare Exp {} _ = GT
  compare _ Exp {} = LT
  compare Element { elemVal = e1 } Element { elemVal = e2 } = compare e1 e2

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
  compare Exact { exactLit = e1 } Exact { exactLit = e2 } = compare e1 e2

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
  compare Seq { seqExps = exps1 } Seq { seqExps = exps2 } = compare exps1 exps2
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
  compare Literal { literalVal = lit1 } Literal { literalVal = lit2 } =
    compare lit1 lit2

instance Ord Entry where
  compare Named { namedSym = name1, namedVal = val1 }
          Named { namedSym = name2, namedVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare Named {} _ = GT
  compare _ Named {} = LT
  compare Unnamed { unnamedPat = e1 } Unnamed { unnamedPat = e2 } =
    compare e1 e2

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
  hashWithSalt s AST { astComponent = component, astUses = uses,
                       astScope = scope } =
    s `hashWithSalt` component `hashWithSalt` uses `hashWithSalt` scope

instance Hashable Component where
  hashWithSalt s Component { componentName = cname } = s `hashWithSalt` cname

instance Hashable Use where
  hashWithSalt s Use { useName = uname } = s `hashWithSalt` uname

instance Hashable Group where
  hashWithSalt s Group { groupVisibility = vis, groupElements = elems } =
    s `hashWithSalt` vis `hashWithSalt` elems

instance Hashable Content where
  hashWithSalt s Body { bodyScope = b } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` b
  hashWithSalt s Value { valueExp = v } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` v

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
                         truthContent = prop, truthProof = proof } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym `hashWithSalt`
    kind `hashWithSalt` prop `hashWithSalt` proof
  hashWithSalt s Proof { proofName = sym, proofBody = body } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` sym `hashWithSalt` body
  hashWithSalt s Import { importExp = exp } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` exp
  hashWithSalt s Syntax { syntaxExp = exp } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` exp

instance Hashable Compound where
  hashWithSalt s Element { elemVal = e } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s Exp { expVal = e } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` e

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
  hashWithSalt s Seq { seqExps = exps } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` exps
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
  hashWithSalt s Literal { literalVal = lit } =
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
  hashWithSalt s Unnamed { unnamedPat = p } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` p

astDot :: MonadSymbols m => AST -> m Doc
astDot AST { astComponent = Just component, astUses = uses, astScope = scope } =
  let
    astedge (_, nodename) = string "\"ast\":scope" <>
                            string "-> " <> dquoted (string nodename)
    astnode = string ("\"ast\" [ label = \"AST | <uses> uses | " ++
                      "<scope> scope\" shape = \"record\" ]")
  in do
    (componentcontents, aftercomponent) <- runStateT (componentDot component) 0
    (usecontents, afteruses) <- runStateT (mapM useDot uses) aftercomponent
    (contents, _) <- runStateT (mapM elementDot scope) afteruses
    return (string "digraph g " <>
            braces (line <>
                    fst componentcontents <!>
                    vcat (map fst usecontents) <!>
                    vcat (map fst contents) <!>
                    astnode <!>
                    astedge componentcontents <!>
                    vcat (map astedge usecontents) <!>
                    vcat (map astedge contents) <> line))
astDot AST { astComponent = Nothing, astUses = uses, astScope = scope } =
  let
    astedge (_, nodename) = string "\"ast\":scope" <>
                            string "-> " <> dquoted (string nodename)
    astnode = string ("\"ast\" [ label = \"AST | <uses> uses | " ++
                      "<scope> scope\" shape = \"record\" ]")
  in do
    (usecontents, afteruses) <- runStateT (mapM useDot uses) 0
    (contents, _) <- runStateT (mapM elementDot scope) afteruses
    return (string "digraph g " <>
            braces (line <>
                    vcat (map fst usecontents) <!>
                    vcat (map fst contents) <!>
                    astnode <!>
                    vcat (map astedge usecontents) <!>
                    vcat (map astedge contents) <> line))

componentDot :: MonadSymbols m => Component -> StateT Word m (Doc, String)
componentDot Component { componentName = cname } =
  do
    unames <- mapM formatM cname
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Component | " <>
                               concat (punctuate dot unames)) <!>
                      string "shape = \"record\"") <> char ';', nodeid)

useDot :: MonadSymbols m => Use -> StateT Word m (Doc, String)
useDot Use { useName = uname } =
  do
    unames <- mapM formatM uname
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Use | " <>
                               concat (punctuate dot unames)) <!>
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
    return (vcat (map fst bodyContents) <!>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Group | " <>
                               string (show vis) <>
                               string " | <elements> elements") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (elemEdge nodeid) bodyContents), nodeid)

builderBodyDot :: MonadSymbols m => Content -> StateT Word m (Doc, String)
builderBodyDot Body { bodyScope = elems } =
  let
    groupEdge nodeid (_, groupname) =
      dquoted (string nodeid) <> string ":groups" <>
      string " -> " <> dquoted (string groupname)
  in do
    nodeid <- getNodeID
    bodyContents <-mapM groupDot elems
    return (vcat (map fst bodyContents) <!>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Body | " <>
                               string "<groups> groups") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (groupEdge nodeid) bodyContents), nodeid)
builderBodyDot Value { valueExp = elems } =
  do
    nodeid <- getNodeID
    (valuenode, valuename) <-expDot elems
    return (valuenode <!>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Value | " <>
                               string "<value> value\"") <!>
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
    return (vcat (map fst paramContents) <!> vcat (map fst supersContents) <!>
            bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Builder | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | " <> string (show cls) <>
                               string (" | <params> params" ++
                                       " | <supers> supers" ++
                                       " | <body> body")) <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (paramEdge nodeid) paramContents) <!>
            vcat (map (supersEdge nodeid) supersContents) <!>
            dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)
elementDot Def { defPattern = pat, defInit = Just init } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (initnode, initname) <- expDot init
    return (patnode <!> initnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Def | " <>
                               string ("<pat> pattern" ++
                                       " | <init> init")) <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
            string " -> " <> dquoted (string patname) <!>
            dquoted (string nodeid) <> string ":init" <>
            string " -> " <> dquoted (string initname), nodeid)
elementDot Def { defPattern = pat, defInit = Nothing } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Def | " <>
                               string "<pat> pattern | <init> init") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
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
    return (vcat (map fst caseContents) <!>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Fun | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <cases> cases") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (caseEdge nodeid) caseContents), nodeid)
elementDot Truth { truthName = sym, truthKind = kind,
                   truthContent = prop, truthProof = Nothing } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (bodynode, bodyname) <- expDot prop
    return (bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Truth | " <>
                               string (show kind) <> string " | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <body> body") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)
elementDot Truth { truthName = sym, truthKind = kind,
                   truthContent = prop, truthProof = Just proof } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (bodynode, bodyname) <- expDot prop
    (proofnode, proofname) <- expDot proof
    return (bodynode <!> proofnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Truth | " <>
                               string (show kind) <> string " | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <body> body" <!>
                               string " | <proof> proof") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname) <!>
            dquoted (string nodeid) <> string ":proof" <>
            string " -> " <> dquoted (string proofname), nodeid)
elementDot Proof { proofName = pname, proofBody = body } =
  do
    nodeid <- getNodeID
    (namenode, namename) <- expDot pname
    (bodynode, bodyname) <- expDot body
    return (namenode <!> bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Proof | <name> name | <body> body") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname) <!>
            dquoted (string nodeid) <> string ":name" <>
            string " -> " <> dquoted (string namename), nodeid)
elementDot Import { importExp = exp } =
  do
    nodeid <- getNodeID
    (namenode, namename) <- expDot exp
    return (namenode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Import | <exp> exp") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":exp" <>
            string " -> " <> dquoted (string namename), nodeid)
elementDot Syntax { syntaxExp = exp } =
  do
    nodeid <- getNodeID
    (expnode, expname) <- expDot exp
    return (expnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Syntax | <exp> exp") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":exp" <>
            string " -> " <> dquoted (string expname), nodeid)

compoundDot :: MonadSymbols m => Compound -> StateT Word m (Doc, String)
compoundDot Exp { expVal = e } = expDot e
compoundDot Element { elemVal = e } = elementDot e

patternDot :: MonadSymbols m => Pattern -> StateT Word m (Doc, String)
patternDot Option { optionPats = pats } =
  let
    patEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":pats" <>
      string " -> " <> dquoted (string patname)
  in do
    patContents <- mapM patternDot pats
    nodeid <- getNodeID
    return (vcat (map fst patContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Options | " <>
                              string "<pats> patterns") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (patEdge nodeid) patContents), nodeid)
patternDot Deconstruct { deconstructName = sym, deconstructPat = pat } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Deconstruct | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<pat> pattern") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname), nodeid)
patternDot Split { splitFields = fields, splitStrict = True } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM entryDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Split | strict | " <>
                               string "<fields> fields") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
patternDot Split { splitFields = fields, splitStrict = False } =
  let
    fieldEdge nodeid (_, fieldname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string fieldname)
  in do
    fieldContents <- mapM entryDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Split | non-strict | " <>
                              string "<body> fields") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
patternDot Typed { typedPat = pat, typedType = ty } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (tynode, tyname) <- expDot ty
    return (patnode <!> tynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Typed | " <>
                               string " <type> type | " <>
                               string "<pat> pat") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname) <!>
          dquoted (string nodeid) <> string ":type" <>
          string " -> " <> dquoted (string tyname), nodeid)
patternDot As { asName = sym, asPat = pat } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    return (patnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "As | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<pat> pat") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
          string " -> " <> dquoted (string patname), nodeid)
patternDot Name { nameSym = sym } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Name | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"") <!>
                      string "shape = \"record\"") <> char ';', nodeid)
patternDot Exact { exactLit = e } =
  do
    nodeid <- getNodeID
    (bodynode, bodyname) <- literalDot e
    return (bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Exact | " <>
                               string "<body>") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":body" <>
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
    return (vcat (map fst compoundContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Compound | " <>
                              string "<body> body") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (compoundEdge nodeid)
                                   compoundContents), nodeid)
expDot Abs { absKind = kind, absCases = cases } =
  let
    caseEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":cases" <>
      string " -> " <> dquoted (string casename)
  in do
    caseContents <- mapM caseDot cases
    nodeid <- getNodeID
    return (vcat (map fst caseContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Abs | " <>
                               string " | " <> format kind <>
                               string "<cases> cases") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (caseEdge nodeid) caseContents), nodeid)
expDot Match { matchVal = val, matchCases = cases } =
  let
    caseEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":cases" <>
      string " -> " <> dquoted (string casename)
  in do
    caseContents <- mapM caseDot cases
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    return (valnode <!> vcat (map fst caseContents) <!>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Match | " <>
                               string " <val> value | " <>
                               string "<cases> cases") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val" <>
            string " -> " <> dquoted (string valname) <!>
            vcat (map (caseEdge nodeid) caseContents), nodeid)
expDot Ascribe { ascribeVal = val, ascribeType = ty } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (tynode, tyname) <- expDot ty
    return (valnode <!> tynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Ascribe | " <>
                               string " <type> type | " <>
                               string "<val> value") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <!>
          dquoted (string nodeid) <> string ":type" <>
          string " -> " <> dquoted (string tyname), nodeid)
expDot Seq { seqExps = exps } =
  let
    seqEdge nodeid (_, casename) =
      dquoted (string nodeid) <> string ":vals" <>
      string " -> " <> dquoted (string casename)
  in do
    nodeid <- getNodeID
    seqContents <- mapM expDot exps
    return (vcat (map fst seqContents) <!>
            brackets (string "label = " <>
                      dquoted (string "Seq | " <>
                               string "<vals> values") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val1" <>
            vcat (map (seqEdge nodeid) seqContents), nodeid)
expDot Record { recordType = True, recordFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM fieldDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Record | type | " <>
                               string "<fields> fields") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Record { recordType = False, recordFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM fieldDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Record | value | " <>
                               string "<fields> fields") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Tuple { tupleFields = fields } =
  let
    fieldEdge nodeid (_, patname) =
      dquoted (string nodeid) <> string ":fields" <>
      string " -> " <> dquoted (string patname)
  in do
    fieldContents <- mapM expDot fields
    nodeid <- getNodeID
    return (vcat (map fst fieldContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Tuple | " <>
                               string "<fields> fields") <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (fieldEdge nodeid) fieldContents), nodeid)
expDot Project { projectVal = val, projectFields = fields } =
  let
    commabstr = Strict.fromString ", "
  in do
    names <- mapM (name . fieldSym) fields
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    return (valnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Project | " <>
                               string "\\\"" <>
                               bytestring (Strict.intercalate commabstr
                                                              names) <>
                               string "\\\"" <>
                               string "<val> value") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname), nodeid)
expDot Sym { symName = sym } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Sym | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"") <!>
                      string "shape = \"record\"") <> char ';', nodeid)
expDot With { withVal = val, withArgs = args } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (argsnode, argsname) <- expDot args
    return (valnode <!> argsnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "With | " <>
                               string " <val> value | " <>
                               string "<args> args") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <!>
          dquoted (string nodeid) <> string ":args" <>
          string " -> " <> dquoted (string argsname), nodeid)
expDot Where { whereVal = val, whereProp = prop } =
  do
    nodeid <- getNodeID
    (valnode, valname) <- expDot val
    (propnode, propname) <- expDot prop
    return (valnode <!> propnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Where | " <>
                               string " <val> value | " <>
                               string "<prop> prop") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":val" <>
          string " -> " <> dquoted (string valname) <!>
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
    return (vcat (map fst paramContents) <!> vcat (map fst supersContents) <!>
            vcat (map fst bodyContents) <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Anon | " <>
                               string " | " <> string (show cls) <>
                               string (" | <params> params" ++
                                       " | <supers> supers" ++
                                       " | <body> body")) <!>
                      string "shape = \"record\"") <>
            char ';' <!> vcat (map (paramEdge nodeid) paramContents) <!>
            vcat (map (supersEdge nodeid) supersContents) <!>
            vcat (map (groupEdge nodeid) bodyContents), nodeid)
expDot Literal { literalVal = l } =
  do
    nodeid <- getNodeID
    (bodynode, bodyname) <- literalDot l
    return (bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Literal | " <>
                               string "<body>") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":body" <>
          string " -> " <> dquoted (string bodyname), nodeid)

entryDot :: MonadSymbols m => Entry -> StateT Word m (Doc, String)
entryDot Named { namedSym = sym, namedVal = val } =
  do
    namestr <- name sym
    nodeid <- getNodeID
    (expnode, expname) <- patternDot val
    return (expnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Named | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string "<value> value") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":value" <>
            string " -> " <> dquoted (string expname), nodeid)
entryDot Unnamed { unnamedPat = p } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot p
    return (patnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Unnamed | " <>
                               string "<value> value") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":value" <>
          string " -> " <> dquoted (string patname), nodeid)

fieldDot :: MonadSymbols m => Field -> StateT Word m (Doc, String)
fieldDot Field { fieldName = FieldName { fieldSym = sym },
                 fieldVal = val } =
  do
    nodeid <- getNodeID
    namestr <- name sym
    (valnode, valname) <- expDot val
    return (valnode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Field | " <>
                               string "\\\"" <> bytestring namestr <>
                               string "\\\"" <>
                               string " | <value> value") <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":value" <>
            string " -> " <> dquoted (string valname), nodeid)

caseDot :: MonadSymbols m => Case -> StateT Word m (Doc, String)
caseDot Case { casePat = pat, caseBody = body } =
  do
    nodeid <- getNodeID
    (patnode, patname) <- patternDot pat
    (bodynode, bodyname) <- expDot body
    return (patnode <!> bodynode <!> dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Case | " <>
                               string (" | <pat> pattern" ++
                                       " | <body> body")) <!>
                      string "shape = \"record\"") <>
            char ';' <!> dquoted (string nodeid) <> string ":pat" <>
            string " -> " <> dquoted (string patname) <!>
            dquoted (string nodeid) <> string ":body" <>
            string " -> " <> dquoted (string bodyname), nodeid)

instance (MonadPositions m, MonadSymbols m) => FormatM m AST where
  formatM AST { astComponent = Just component,
                astUses = uses, astScope = scope } =
    do
      componentdoc <- formatM component
      usesdoc <- liftM listDoc (mapM formatM uses)
      scopedoc <- liftM listDoc (mapM formatM scope)
      return (compoundApplyDoc (string "AST")
                             [(string "component", componentdoc),
                              (string "uses", usesdoc),
                              (string "scope", scopedoc)])
  formatM AST { astComponent = Nothing, astUses = uses, astScope = scope } =
    do
      usesdoc <- liftM listDoc (mapM formatM uses)
      scopedoc <- liftM listDoc (mapM formatM scope)
      return (compoundApplyDoc (string "AST")
                             [(string "uses", usesdoc),
                              (string "scope", scopedoc)])


instance (MonadPositions m, MonadSymbols m) => FormatM m Component where
  formatM Component { componentName = cname, componentPos = pos } =
    do
      posdoc <- formatM pos
      unames <- mapM formatM cname
      return (compoundApplyDoc (string "Component")
                             [(string "pos", posdoc),
                              (string "name", concat (punctuate dot unames))])

instance (MonadPositions m, MonadSymbols m) => FormatM m Use where
  formatM Use { useName = uname, usePos = pos } =
    do
      posdoc <- formatM pos
      unames <- mapM formatM uname
      return (compoundApplyDoc (string "Use")
                             [(string "pos", posdoc),
                              (string "name", concat (punctuate dot unames))])

instance (MonadPositions m, MonadSymbols m) => FormatM m Group where
  formatM Group { groupElements = elems, groupPos = pos,
                  groupVisibility = vis} =
    do
      posdoc <- formatM pos
      elemdocs <- mapM formatM elems
      return (compoundApplyDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "visibility", format vis),
                              (string "elems", listDoc elemdocs)])

instance (MonadPositions m, MonadSymbols m) => FormatM m [Group] where
  formatM = liftM listDoc . mapM formatM

instance (MonadPositions m, MonadSymbols m) => FormatM m Content where
  formatM Body { bodyScope = b } =
    do
      valdoc <- formatM b
      return (compoundApplyDoc (string "Body")
                             [(string "value", valdoc)])
  formatM Value { valueExp = v } =
    do
      valdoc <- formatM v
      return (compoundApplyDoc (string "Value")
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
      return (compoundApplyDoc (string "Builder")
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
      return (compoundApplyDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "init", initdoc)])
  formatM Def { defPattern = pat, defInit = Nothing, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])
  formatM Fun { funName = sym, funCases = cases, funPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (compoundApplyDoc (string "Fun")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "cases", listDoc casedocs)])
  formatM Truth { truthName = sym, truthContent = content,
                  truthKind = kind, truthProof = Nothing,
                  truthPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      contentdoc <- formatM content
      return (compoundApplyDoc (string "Truth")
                             [(string "name", namedoc),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "content", contentdoc)])
  formatM Truth { truthName = sym, truthContent = content,
                  truthKind = kind, truthProof = Just proof,
                  truthPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      contentdoc <- formatM content
      proofdoc <- formatM proof
      return (compoundApplyDoc (string "Truth")
                             [(string "name", namedoc),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "content", contentdoc),
                              (string "proof", proofdoc)])
  formatM Proof { proofName = sym, proofBody = body, proofPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      bodydoc <- formatM body
      return (compoundApplyDoc (string "Proof")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "body", bodydoc)])
  formatM Import { importExp = exp, importPos = pos } =
    do
      expdoc <- formatM exp
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Import")
                             [(string "exp", expdoc),
                              (string "pos", posdoc)])
  formatM Syntax { syntaxExp = exp, syntaxPos = pos } =
    do
      expdoc <- formatM exp
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Syntax")
                             [(string "exp", expdoc),
                              (string "pos", posdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Compound where
  formatM Exp { expVal = e } = formatM e
  formatM Element { elemVal = e } = formatM e

instance (MonadPositions m, MonadSymbols m) => FormatM m Pattern where
  formatM Option { optionPats = pats, optionPos = pos } =
    do
      posdoc <- formatM pos
      patsdoc <- mapM formatM pats
      return (compoundApplyDoc (string "Options")
                             [(string "pos", posdoc),
                              (string "patterns", listDoc patsdoc)])
  formatM Deconstruct { deconstructName = sym, deconstructPat = pat,
                        deconstructPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "Deconstruct")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Split { splitFields = fields, splitStrict = True, splitPos = pos } =
    do
      fieldsdoc <- mapM formatM fields
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "true"),
                              (string "fields", listDoc fieldsdoc)])
  formatM Split { splitFields = fields, splitStrict = False, splitPos = pos } =
    do
      fieldsdoc <- mapM formatM fields
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "false"),
                              (string "fields", listDoc fieldsdoc)])
  formatM Typed { typedPat = pat, typedType = ty, typedPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      tydoc <- formatM ty
      return (compoundApplyDoc (string "Typed")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "type", tydoc)])
  formatM As { asName = sym, asPat = pat, asPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "As")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Name { nameSym = sym, namePos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Name")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM Exact { exactLit = e } = formatM e

instance (MonadPositions m, MonadSymbols m) => FormatM m Exp where
  formatM Compound { compoundBody = body, compoundPos = pos } =
    do
      posdoc <- formatM pos
      bodydoc <- mapM formatM body
      return (compoundApplyDoc (string "Compound")
                             [(string "pos", posdoc),
                              (string "body", listDoc bodydoc)])
  formatM Abs { absKind = kind, absCases = cases, absPos = pos } =
    do
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (compoundApplyDoc (string "Abs")
                             [(string "pos", posdoc),
                              (string "kind", format kind),
                              (string "cases", listDoc casedocs)])
  formatM Match { matchVal = val, matchCases = cases, matchPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      casedocs <- mapM formatM cases
      return (compoundApplyDoc (string "Match")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "cases", listDoc casedocs)])
  formatM Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      tydoc <- formatM ty
      return (compoundApplyDoc (string "Ascribe")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "type", tydoc)])
  formatM Seq { seqExps = exps, seqPos = pos } =
    do
      posdoc <- formatM pos
      seqdocs <- mapM formatM exps
      return (compoundApplyDoc (string "Seq")
                             [(string "pos", posdoc),
                              (string "exps", listDoc seqdocs)])
  formatM Record { recordType = True, recordFields = fields, recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (compoundApplyDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "type", string "true"),
                              (string "fields", listDoc fielddocs)])
  formatM Record { recordType = False, recordFields = fields,
                   recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (compoundApplyDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "type", string "false"),
                              (string "fields", listDoc fielddocs)])
  formatM Tuple { tupleFields = fields, tuplePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (compoundApplyDoc (string "Tuple")
                             [(string "pos", posdoc),
                              (string "fields", listDoc fielddocs)])
  formatM Project { projectVal = val, projectFields = fields,
                    projectPos = pos } =
    do
      fielddocs <- mapM formatM fields
      posdoc <- formatM pos
      valdoc <- formatM val
      return (compoundApplyDoc (string "Project")
                             [(string "fields", listDoc fielddocs),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM Sym { symName = sym, symPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Sym")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM With { withVal = val, withArgs = args, withPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      argdocs <- formatM args
      return (compoundApplyDoc (string "With")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "arg", argdocs)])
  formatM Where { whereVal = val, whereProp = prop, wherePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      propdoc <- formatM prop
      return (compoundApplyDoc (string "Where")
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
      return (compoundApplyDoc (string "Anon")
                             [(string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", listDoc paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", listDoc bodydoc)])
  formatM Literal { literalVal = l } = formatM l

instance (MonadPositions m, MonadSymbols m) => FormatM m Entry where
  formatM Named { namedSym = sym, namedVal = val, namedPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      valdoc <- formatM val
      return (compoundApplyDoc (string "Named")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM Unnamed { unnamedPat = p } =
    do
      valdoc <- formatM p
      return (compoundApplyDoc (string "Unnamed")
                             [(string "pat", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Field where
  formatM Field { fieldName = sym, fieldVal = val, fieldPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      valdoc <- formatM val
      return (compoundApplyDoc (string "Field")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Case where
  formatM Case { casePat = pat, caseBody = body, casePos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      bodydoc <- formatM body
      return (compoundApplyDoc (string "Case")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "body", bodydoc)])

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] AST where
  xpickle =
    xpWrap (\(component, uses, scope) -> AST { astComponent = component,
                                               astUses = uses,
                                               astScope = scope },
            \AST { astComponent = component, astUses = uses,
                   astScope = scope } -> (component, uses, scope))
           (xpElemNodes (gxFromString "AST")
                        (xpTriple (xpOption (xpElemNodes
                                              (gxFromString "component")
                                              xpickle))
                                  (xpElemNodes (gxFromString "uses") xpickle)
                                  (xpElemNodes (gxFromString "scope") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Component where
  xpickle = xpWrap (\(cname, pos) -> Component { componentName = cname,
                                                 componentPos = pos },
                    \Component { componentName = cname,
                                 componentPos = pos } -> (cname, pos))
                   (xpElemNodes (gxFromString "Component")
                                (xpPair (xpElemNodes (gxFromString "name")
                                                     xpickle)
                                        (xpElemNodes (gxFromString "pos")
                                                     xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Use where
  xpickle = xpWrap (\(uname, pos) -> Use { useName = uname, usePos = pos },
                    \Use { useName = uname, usePos = pos } -> (uname, pos))
                   (xpElemNodes (gxFromString "Use")
                                (xpPair (xpElemNodes (gxFromString "name")
                                                     xpickle)
                                        (xpElemNodes (gxFromString "pos")
                                                     xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Group where
  xpickle = xpWrap (\(vis, (elems, pos)) -> Group { groupVisibility = vis,
                                                    groupElements = elems,
                                                    groupPos = pos },
                    \Group { groupVisibility = vis, groupElements = elems,
                             groupPos = pos } -> (vis, (elems, pos)))
                   (xpElem (gxFromString "Group") xpickle
                           (xpPair (xpElemNodes (gxFromString "elements")
                                                xpickle)
                                   (xpElemNodes (gxFromString "pos") xpickle)))

bodyPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Content
bodyPickler =
  let
    revfunc Body { bodyScope = b } = b
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Body, revfunc) (xpElemNodes (gxFromString "Body") xpickle)

valuePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Content
valuePickler =
  let
    revfunc Value { valueExp = v } = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Value, revfunc) (xpElemNodes (gxFromString "Value") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Content where
  xpickle =
    let
      picker Body {} = 0
      picker Value {} = 1
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
      ((sym, kind), (params, supers, body, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, kind), (params, supers, body, pos)) ->
             Builder { builderName = sym, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos }, revfunc)
           (xpElem (gxFromString "Builder") (xpPair xpickle xpickle)
                   (xp4Tuple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

defPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Element
defPickler =
  let
    revfunc Def { defPattern = pat, defInit = init, defPos = pos } =
      (pat, init, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pat, init, pos) -> Def { defPattern = pat, defInit = init,
                                       defPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Def")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpOption (xpElemNodes (gxFromString "init")
                                                         xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

funPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Element
funPickler =
  let
    revfunc Fun { funName = sym, funCases = cases, funPos = pos } =
      (sym, (cases, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (cases, pos)) -> Fun { funName = sym, funCases = cases,
                                        funPos = pos }, revfunc)
           (xpElem (gxFromString "Fun") xpickle
                   (xpPair (xpElemNodes (gxFromString "cases") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

truthPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
truthPickler =
  let
    revfunc Truth { truthName = sym, truthKind = kind, truthContent = body,
                    truthProof = proof, truthPos = pos } =
      ((sym, kind), (body, proof, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, kind), (body, proof, pos)) ->
             Truth { truthName = sym, truthContent = body, truthKind = kind,
                     truthProof = proof, truthPos = pos }, revfunc)
           (xpElem (gxFromString "Truth") (xpPair xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                             (xpOption (xpElemNodes (gxFromString "type")
                                                    xpickle))
                             (xpElemNodes (gxFromString "pos") xpickle)))

proofPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
proofPickler =
  let
    revfunc Proof { proofName = pname, proofBody = body, proofPos = pos } =
      (pname, body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pname, body, pos) -> Proof { proofName = pname,
                                           proofBody = body,
                                           proofPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Proof")
                        (xpTriple (xpElemNodes (gxFromString "name") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

importPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
importPickler =
  let
    revfunc Import { importExp = exp, importPos = pos } = (exp, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(exp, pos) -> Import { importExp = exp, importPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Import")
                        (xpPair (xpElemNodes (gxFromString "name") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

syntaxPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Element
syntaxPickler =
  let
    revfunc Syntax { syntaxExp = exp, syntaxPos = pos } = (exp, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(exp, pos) -> Syntax { syntaxExp = exp, syntaxPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Syntax")
                        (xpPair (xpElemNodes (gxFromString "name") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

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
      picker Syntax {} = 6
    in
      xpAlt picker [ builderPickler, defPickler, funPickler,
                     truthPickler, proofPickler, importPickler,
                     syntaxPickler ]

expPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Compound
expPickler =
  let
    revfunc Exp { expVal = e } = e
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Exp, revfunc) (xpElemNodes (gxFromString "Exp") xpickle)

elementPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Compound
elementPickler =
  let
    revfunc Element { elemVal = e } = e
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Element, revfunc) (xpElemNodes (gxFromString "Element") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Compound where
  xpickle =
    let
      picker Exp {} = 0
      picker Element {} = 1
    in
      xpAlt picker [expPickler, elementPickler]

optionPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Pattern
optionPickler =
  let
    revfunc Option { optionPats = pats, optionPos = pos } = (pats, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pats, pos) -> Option { optionPats = pats,
                                     optionPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Option")
                        (xpPair (xpElemNodes (gxFromString "patterns") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

deconstructPickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text) =>
                      PU [NodeG [] tag text] Pattern
deconstructPickler =
  let
    revfunc Deconstruct { deconstructName = sym, deconstructPat = pat,
                          deconstructPos = pos } = (sym, (pat, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (pat, pos)) -> Deconstruct { deconstructName = sym,
                                                deconstructPat = pat,
                                                deconstructPos = pos },
            revfunc)
           (xpElem (gxFromString "Deconstruct") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

splitPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Pattern
splitPickler =
  let
    revfunc Split { splitStrict = strict, splitFields = fields,
                    splitPos = pos } =
      (strict, (fields, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(strict, (fields, pos)) -> Split { splitStrict = strict,
                                                splitFields = fields,
                                                splitPos = pos }, revfunc)
           (xpElem (gxFromString "Split")
                   (xpAttr (gxFromString "strict") xpPrim)
                   (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

typedPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Pattern
typedPickler =
  let
    revfunc Typed { typedPat = pat, typedType = ty, typedPos = pos } =
      (pat, ty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pat, ty, pos) -> Typed { typedPat = pat, typedType = ty,
                                         typedPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Typed")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
asPickler =
  let
    revfunc As { asName = sym, asPat = pat, asPos = pos } = (sym, (pat, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (pat, pos)) -> As { asName = sym, asPat = pat,
                                       asPos = pos }, revfunc)
           (xpElem (gxFromString "As") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

namePickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Pattern
namePickler =
  let
    revfunc Name { nameSym = sym, namePos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Name { nameSym = sym, namePos = pos }, revfunc)
           (xpElem (gxFromString "Name") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

exactPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Pattern
exactPickler =
  let
    revfunc Exact { exactLit = v } = v
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
      picker Exact {} = 6
    in
      xpAlt picker [optionPickler, deconstructPickler, splitPickler,
                    typedPickler, asPickler, namePickler, exactPickler]

compoundPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Exp
compoundPickler =
  let
    revfunc Compound { compoundBody = body, compoundPos = pos } = (body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(body, pos) -> Compound { compoundBody = body,
                                       compoundPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Compound")
                        (xpPair (xpElemNodes (gxFromString "body") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

absPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Exp
absPickler =
  let
    revfunc Abs { absKind = kind, absCases = cases, absPos = pos } =
      (kind, (cases, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(kind, (cases, pos)) -> Abs { absKind = kind, absCases = cases,
                                           absPos = pos }, revfunc)
           (xpElem (gxFromString "Abs") xpickle
                   (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

matchPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
matchPickler =
  let
    revfunc Match { matchVal = val, matchCases = cases, matchPos = pos } =
      (val, cases, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, cases, pos) -> Match { matchVal = val, matchCases = cases,
                                          matchPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Match")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "cases") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

ascribePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
ascribePickler =
  let
    revfunc Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
      (val, ty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, ty, pos) -> Ascribe { ascribeVal = val,
                                         ascribeType = ty,
                                         ascribePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Ascribe")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

seqPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
seqPickler =
  let
    revfunc Seq { seqExps = exps, seqPos = pos } = (exps, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(exps, pos) -> Seq { seqExps = exps, seqPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Seq")
                        (xpPair (xpElemNodes (gxFromString "exps")
                                             (xpList xpickle))
                                (xpElemNodes (gxFromString "pos") xpickle)))

recordPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
recordPickler =
  let
    revfunc Record { recordType = istype, recordFields = fields,
                     recordPos = pos } = (istype, (fields, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(istype, (fields, pos)) -> Record { recordType = istype,
                                                 recordFields = fields,
                                                 recordPos = pos }, revfunc)
           (xpElem (gxFromString "Record")
                   (xpAttr (gxFromString "is-type") xpPrim)
                   (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

tuplePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
tuplePickler =
  let
    revfunc Tuple { tupleFields = fields, tuplePos = pos } = (fields, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, pos) -> Tuple { tupleFields = fields,
                                      tuplePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Record")
                        (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

projectPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
projectPickler =
  let
    revfunc Project { projectFields = fields, projectVal = val,
                      projectPos = pos } = (fields, val, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, val, pos) -> Project { projectFields = fields,
                                             projectVal = val,
                                             projectPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Project")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "fields") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

symPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Exp
symPickler =
  let
    revfunc Sym { symName = sym, symPos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Sym { symName = sym, symPos = pos }, revfunc)
           (xpElem (gxFromString "Sym") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

withPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
withPickler =
  let
    revfunc With { withVal = val, withArgs = args, withPos = pos } =
      (val, args, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, args, pos) -> With { withVal = val, withArgs = args,
                                        withPos = pos }, revfunc)
           (xpElemNodes (gxFromString "With")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "args") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

wherePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
wherePickler =
  let
    revfunc Where { whereVal = val, whereProp = prop, wherePos = pos } =
      (val, prop, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, prop, pos) -> Where { whereVal = val, whereProp = prop,
                                         wherePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Where")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "prop") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

anonPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
anonPickler =
  let
    revfunc Anon { anonKind = kind, anonParams = params, anonContent = body,
                   anonSuperTypes = supers, anonPos = pos } =
      (kind, (params, supers, body, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(kind, (params, supers, body, pos)) ->
             Anon { anonKind = kind, anonParams = params, anonContent = body,
                    anonSuperTypes = supers, anonPos = pos }, revfunc)
           (xpElem (gxFromString "Anon") xpickle
                   (xp4Tuple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

literalPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Exp
literalPickler =
  let
    revfunc Literal { literalVal = v } = v
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
      picker Literal {} = 12
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
                    namedPos = pos } = (sym, (val, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (val, pos)) -> Named { namedSym = sym, namedVal = val,
                                          namedPos = pos }, revfunc)
           (xpElem (gxFromString "Named") xpickle
                   (xpPair (xpElemNodes (gxFromString "val") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

unnamedPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Entry
unnamedPickler =
  let
    revfunc Unnamed { unnamedPat = v } = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Unnamed, revfunc)
           (xpElemNodes (gxFromString "Unnamed") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Entry where
  xpickle =
    let
      picker Named {} = 0
      picker Unnamed {} = 1
    in
      xpAlt picker [namedPickler, unnamedPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Field where
  xpickle = xpWrap (\(sym, (val, pos)) -> Field { fieldName = sym,
                                                  fieldVal = val,
                                                  fieldPos = pos },
                    \Field { fieldName = sym, fieldVal = val,
                             fieldPos = pos } -> (sym, (val, pos)))
                   (xpElem (gxFromString "Field") xpickle
                           (xpPair (xpElemNodes (gxFromString "val") xpickle)
                                   (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Case where
  xpickle = xpWrap (\(pat, exp, pos) -> Case { casePat = pat,
                                               caseBody = exp,
                                               casePos = pos },
                    \Case { casePat = pat, caseBody = body, casePos = pos } ->
                      (pat, body, pos))
                   (xpElemNodes (gxFromString "Case")
                                (xpTriple (xpElemNodes (gxFromString "pattern")
                                                       xpickle)
                                          (xpElemNodes (gxFromString "body")
                                                       xpickle)
                                          (xpElemNodes (gxFromString "pos")
                                                       xpickle)))
