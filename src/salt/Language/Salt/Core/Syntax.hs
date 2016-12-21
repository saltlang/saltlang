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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances,
             MultiParamTypeClasses, DeriveTraversable #-}

-- | The Salt core language.  Salt's surface syntax is transliterated
-- into Core, which is then type-checked and compiled.  Core is
-- generally not meant for human consumption.
module Language.Salt.Core.Syntax(
       module Language.Salt.Common,

       -- * Terms
       Intro(..),
       Elim(..),
       Element(..),
       Quantifier(..),

       -- * Cases and Patterns
       Case(..),
       Pattern(..),

       -- * Literals
       Literal(..),

       -- * Computations
       Cmd(..),
       Comp(..),

       -- * Fields
       Field(..),

       -- * Positions
       Position,

       -- * Newtypes
       RankID(..)
       ) where

import Bound
import Bound.Scope.ExtraInstances()
import Bound.Var.ExtraInstances()
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Positions
import Control.Monad.Symbols
import Data.Array hiding (accum)
import Data.Default
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.Hashable.ExtraInstances()
import Data.HashMap.Strict(HashMap)
import Data.List(sortBy)
import Data.Position.BasicPosition(BasicPosition)
import Data.Position.DWARFPosition
import Data.PositionElement
import Data.Ratio
import Data.Symbol
import Data.Traversable
import Data.Word
import Language.Salt.Common
import Language.Salt.Format
import Prelude hiding (foldl, mapM)
import Prelude.Extras(Eq1(..), Ord1(..))
import Prelude.Extras.ExtraInstances()
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)
import Text.Format hiding ((<$>))

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy.UTF8 as Lazy

type Position = DWARFPosition [Symbol] [Symbol]

newtype RankID = RankID { rankID :: Graph.Node }
  deriving (Eq, Ord, Ix)

-- | A literal value.
data Literal =
    -- | A number literal.
    Num {
      -- | The number value.
      numVal :: !Rational
    }
    -- | A string literal.
  | Str {
      -- | The string value.
      strVal :: !Strict.ByteString
    }
    -- | A Character literal.
  | Char {
      -- | The character value.
      charVal :: !Char
    }
    deriving (Ord, Eq)

-- | Quantifier types.
data Quantifier =
  -- | Universal quantifiers.
    Forall
  -- | Existential quantifiers.
  | Exists
    deriving (Ord, Eq, Enum)

-- | A pattern binding.  Represents how to deconstruct a value and
-- bind it to variables.
data Pattern bound =
    -- | A deconstruction.  Takes a type apart.  Some types have
    -- constructors; nameless record types don't (they should use the
    -- "unused" symbol).
    Deconstruct {
      -- | The type constructor.  For nameless records, use the
      -- "unused" symbol.
      deconstructConstructor :: !bound,
      -- | The fields in the record being bound.  Note: all fields are
      -- given names by transliteration.
      deconstructBinds :: HashMap FieldName (Field (Pattern bound)),
      -- | Whether or not the binding is strict (ie. it omits some names)
      deconstructStrict :: !Bool,
      -- | The position in source from which this originates.
      deconstructPos :: !Position
    }
    -- | An "as" binding.  Allows part of a pattern to be bound to a
    -- name, but further deconstructed by another pattern.  For
    -- example "x as (y, z)".
  | As {
      -- | The outer name, to which the entire datatype is bound.
      asName :: !bound,
      -- | The inner binding, which further deconstructs the binding.
      asBind :: Pattern bound,
      -- | The position in source from which this originates.
      asPos :: !Position
    }
    -- | A simple name binding.  This does the same thing as an as
    -- pattern, but does not further deconstruct the binding.
    --
    -- In the intended use case, we have a special symbol representing
    -- a wildcard (namely, the unused symbol), so we don't bother
    -- defining another constructor for wildcards.
  | Name {
      -- | The bound variable type being bound.
      nameSym :: !bound,
      -- | The position in source from which this originates.
      namePos :: !Position
    }
    -- | A constant.  Constrains the binding to the given value.
  | Exact {
      -- | The literal.
      exactLiteral :: !Literal,
      -- | The position in source from which this originates.
      exactPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | A case.  Consists of a pattern and a body wrapped in a scope.
-- Used to describe functions and computations with parameters.
data Case bound free =
  Case {
    -- | the pattern for this case.
    casePat :: Pattern bound,
    -- | The body of the case.
    caseBody :: Scope bound (Intro bound) free,
    -- | The position in source from which this originates.
    casePos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | An element.  This is either an argument in a function type or a
-- field in a record type.  The pattern introduces variables in
-- subsequent elements.
data Element bound free =
  Element {
    -- | The name of the element.
    elemName :: !FieldName,
    -- | The index of this element in a tuple.
    elemTupleIdx :: !Word,
    -- | The binding pattern of the element.
    elemPat :: Pattern bound,
    -- | The type of the element.
    elemType :: Scope bound (Intro bound) free,
    -- | The position in source from which this originates.
    elemPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | A field.
data Field valty =
  Field {
    -- | The value assigned to the bound name.
    fieldVal :: !valty,
    -- | The position in source from which this arises.
    fieldPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | Terms.  Represents pure terms in the language.  Terms are further
-- subdivided into types, propositions, and elimination and
-- introduction terms (both of which represent values).
--
-- Types and propositions do not support decidable equality, and thus
-- cannot be computed upon.
--
-- Values can be computed upon, and can appear in a pattern match.
-- Elimination terms are those terms whose type can be inferred in
-- type checking.  Introduction terms are those terms that require a
-- type to be checked against.
data Intro bound free =
    -- Types.  These do not support decidable equality.  As such, they
    -- cannot be the result of a computation, and cannot appear in
    -- patterns.

    -- | Dependent product type.  This is the type given to functions.
    FuncType {
      -- | The binding order for arguments.  This is used to determine
      -- the order in which to evaluate scopes.
      funcTypeArgs :: ![Element bound free],
      -- | The return type of the function, which can reference the
      -- value of any argument by their binding name.
      funcTypeRetTy :: Scope bound (Intro bound) free,
      -- | The position in source from which this originates.
      funcTypePos :: !Position
    }
    -- | Dependent sum type.  This is the type given to structures.
  | RecordType {
      -- | The remaining elements of the sum type.  The first element
      -- in the binding order is a degenerate scope; the remaining
      -- scopes may reference any previous elements from the binding
      -- order, but not themselves or any future scopes.
      --
      -- Note: all fields are given names by transliteration.
      recTypeBody :: ![Element bound free],
      -- | The position in source from which this originates.
      recTypePos :: !Position
    }
    -- | Refinement type.  This type represents all members of a type
    -- satisfying a given proposition.
  | RefineType {
      -- | The base type.
      refineType :: Intro bound free,
      -- | Binding patterns and propositions for values of the base
      -- type.  These express the constraints on the base type.
      refineCases :: [Case bound free],
      -- | The position in source from which this originates.
      refinePos :: !Position
    }
    -- | Computation type.  This type represents a computation, and
    -- includes both its result type and a specification of its
    -- behavior.
  | CompType {
      -- | The result type of the computation.
      compType :: Intro bound free,
      -- | Binding patterns and behavior specifications for values of
      -- the return type.  These express the constraints on the base
      -- type.
      compCases :: [Case bound free],
      -- | The position in source from which this originates.
      compTypePos :: !Position
    }
    -- | Type of types of a given rank.  The type hierarchy is
    -- statified, so @type(n)@ is an insance of @type(n+1)@.  In the
    -- type checker, we implement this by building up greater-than
    -- relationships among rank variables, and then checking for
    -- cycles.
  | Type {
      -- | The rank variable for this type.
      typeRank :: !RankID,
      -- | The position in source from which this originates.
      typePos :: !Position
    }
    -- | Type of propositions.
  | PropType {
      -- | The position in source from which this originates.
      propPos :: !Position
    }

    -- Propositions.  These do not support decidable equality.  As such,
    -- they cannot be the result of a computation, and cannot appear in
    -- a pattern.

    -- | Quantified proposition.
  | Quantified {
      -- | The kind of quantification this represents (forall/exists).
      quantKind :: !Quantifier,
      -- | The type of the quantifier variable.  If a sum type is
      -- used, this will be treated similarly to a multi-argument
      -- function.
      quantType :: Intro bound free,
      -- | A case statement which denotes a proposition.
      quantCases :: [Case bound free],
      -- | The position in source from which this originates.
      quantPos :: !Position
    }
    -- | A lambda expression.  Represents a function value.  Lambdas
    -- cannot appear in patterns, though they can be computed on.
    --
    -- Lambdas will ultimately need to be extended to support
    -- overloading, which adds an inherent multiple dispatch ability.
    -- This is obviousy highly nontrivial to implement.
  | Lambda {
      -- | The cases describing this function's behavior.
      lambdaCases :: [Case bound free],
      -- | The position in source from which this originates.
      lambdaPos :: !Position
    }

    -- | An eta expansion.  This is present for type checking only.
    -- This represents a "frozen" substitution.

    -- XXX Quite possibly this will be removed
  | Eta {
      etaTerm :: Elim bound free,
      etaType :: Intro bound free,
      -- | The position in source from which this originates.
      etaPos :: !Position
    }
    -- | A record.  Records can be named or ordered in the surface
    -- syntax.  Ordered records are transliterated into named
    -- records, with the fields "1", "2", and so on.
  | Record {
      -- | The bindings for this record.  These are introduction terms.
      recFields :: !(HashMap FieldName (Field (Intro bound free))),
      -- | The position in source from which this originates.
      recPos :: !Position
    }
    -- | A tuple.  These denote record values, but their
    -- interpretation depends on the expected type.
  | Tuple {
      -- | The fields of the tuple.
      tupleFields :: !(Array Word (Intro bound free)),
      -- | The position in source from which this originates.
      tuplePos :: !Position
    }
    -- | A collection of one or more terms, each of which is
    -- bound to a name.  Each of the members of the group may reference
    -- eachother.
  | Fix {
      -- | The self-reference symbol.
      fixSym :: !bound,
      -- | The term, parameterized by the self-reference @fixSym@.
      fixTerm :: !(Scope bound (Intro bound) free),
      -- | The position in source from which this originates.
      fixPos :: !Position
    }
    -- | A computation value.  This is essentially a "frozen"
    -- computation.
  | Comp {
      compBody :: Comp bound free,
      -- | The position in source from which this originates.
      compPos :: !Position
    }
    -- | An elimination term presented as an introduction term.
  | Elim {
      -- | The wrapped elimination term.
      elimTerm :: Elim bound free
    }
    -- | A literal value.
  | Literal {
      -- | The literal value.
      literalVal :: !Literal,
      -- | The position in source from which this originates.
      literalPos :: !Position
    }
    -- | A symbol whose meaning is understood implicitly by the
    -- compiler.  This includes intrinsic functions, as well as
    -- anything that is auto-generated during transliteration.
  | Constructor {
      -- | The name of the constructor.
      constructorSym :: !bound,
      -- | The position in source from which this originates.
      constructorPos :: !Position
    }
    -- | Placeholder for a malformed term, allowing type checking to
    -- continue in spite of errors.
  | BadIntro {
      -- | The position in source from which this originates.
      badIntroPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | Elimination Terms.  These terms generate a type in type checking.
data Elim bound free =
    -- | Call term.  Represents a call to a function.  The type of the
    -- term comes from the called function's return type.
    --
    -- As with structures, calls can be named or ordered in surface
    -- syntax.  Also similar to structures, ordered calls are
    -- transliterated into named calls with parameter names "1", "2",
    -- and so on.
    Call {
      -- | The argument to the call. Multiple arguments are
      -- implemented using either a record or a tuple.
      callArg :: Intro bound free,
      -- | The function being called.  This must be an elimination
      -- term.
      callFunc :: Elim bound free,
      -- | The position in source from which this originates.
      callPos :: !Position
    }
    -- | A typed term.  This is an introduction term with an explicit
    -- type tag, which makes it an elimination term.
  | Typed {
      -- | The introduction term being typed.
      typedTerm :: Intro bound free,
      -- | The type of the introduction term.
      typedType :: Intro bound free,
      -- | The position in source from which this originates.
      typedPos :: !Position
    }
    -- | A variable symbol.  Since we know the types of all variables,
    -- this is an elimination term.
  | Var {
      -- | The underlying symbol.
      varSym :: !free,
      -- | The position in source from which this originates.
      varPos :: !Position
    }
    -- | Placeholder for a malformed term, allowing type checking to
    -- continue in spite of errors.
  | BadElim {
      -- | The position in source from which this originates.
      badElimPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | Commands.  These represent individual statements, or combinations
-- thereof, which do not bind variables.
--
-- We don't need a lot of control flow structures.  Loops are handled
-- by the Fix structure, conditionals are handled by the pattern
-- matching inherent in Lambdas.  Widening and narrowing (ie typecase
-- and downcast) can be effectively handled by adding a multiple
-- dispatch capability).
--
-- The Core-to-LLVM compiler should therefore be smart enough to
-- figure out when it can implement a Fix as a loop, and when it can
-- inline lambdas, and when it can figure out dispatch decisions
-- statically.  This is also desirable in its own right.
data Cmd bound free =
    Value {
      -- | The term representing the value of this command
      valTerm :: Intro bound free,
      -- | The position in source from which this originates.
      valPos :: !Position
    }
    -- | Evaluate a computation value.  This allows execution of
    -- computations produced by terms.
  | Eval {
      -- | The computation value to evaluate
      evalTerm :: Intro bound free,
      -- | The position in source from which this originates.
      evalPos :: !Position
    }
    -- | Placeholder for a malformed command, allowing type checking to
    -- continue in spite of errors.
  | BadCmd {
      -- | The position in source from which this originates.
      badCmdPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | Computations. Semantically, computations are generators for
-- sequences of atomic actions, which are not guaranteed to terminate,
-- or even run without error (in the case of unverified computations.
-- In terms of programming language structures, they represent
-- higher-order stateful procedures.
--
-- Obviously, computations do not admit decidable equality.
--
-- A raw computation is something akin to a function taking void in C,
-- or a monad in Haskell. Stateful functions with arguments are
-- modeled as regular functions which produce a computation.
data Comp bound free =
    -- | A sequential composition of terms.
    Seq {
      -- | The pattern to which to bind the result of seqCmd.
      seqPat :: Pattern bound,
      -- | The type being bound.
      seqType :: Intro bound free,
      -- | The command to execute.
      seqCmd :: Cmd bound free,
      -- | The next computation to execute.
      seqNext :: Scope bound (Comp bound) free,
      -- | The position in source from which this originates.
      seqPos :: !Position
    }
    -- | Result of a computation. This is always the end of a sequence.
  | End {
      -- | The command to run to produce a result.
      endCmd :: Cmd bound free,
      -- | The position in source from which this originates.
      endPos :: !Position
    }
    -- | Placeholder for a malformed computation, allowing type checking
    -- to continue in spite of errors.
  | BadComp {
      -- | The position in source from which this originates.
      badCompPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

positionDefault :: DWARFPositionElement [Symbol] [Symbol] ty =>
                   ty
                -> BasicPosition
positionDefault e =
  let
    pos = (debugPosition e :: Position)
  in
    basicPosition pos

instance Enum RankID where
  toEnum = RankID
  fromEnum = rankID

instance DWARFPositionElement [Symbol] [Symbol] (Pattern bound) where
  debugPosition Deconstruct { deconstructPos = pos } = pos
  debugPosition As { asPos = pos } = pos
  debugPosition Name { namePos = pos } = pos
  debugPosition Exact { exactPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Case bound free) where
  debugPosition Case { casePos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Element bound free) where
  debugPosition Element { elemPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Field expty) where
  debugPosition Field { fieldPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Intro bound free) where
  debugPosition FuncType { funcTypePos = pos } = pos
  debugPosition RecordType { recTypePos = pos } = pos
  debugPosition RefineType { refinePos = pos } = pos
  debugPosition CompType { compTypePos = pos } = pos
  debugPosition Type { typePos = pos } = pos
  debugPosition PropType { propPos = pos } = pos
  debugPosition Quantified { quantPos = pos } = pos
  debugPosition Eta { etaPos = pos } = pos
  debugPosition Lambda { lambdaPos = pos } = pos
  debugPosition Record { recPos = pos } = pos
  debugPosition Tuple { tuplePos = pos } = pos
  debugPosition Fix { fixPos = pos } = pos
  debugPosition Comp { compPos = pos } = pos
  debugPosition Elim { elimTerm = term } = debugPosition term
  debugPosition Literal { literalPos = pos } = pos
  debugPosition Constructor { constructorPos = pos } = pos
  debugPosition BadIntro { badIntroPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Elim bound free) where
  debugPosition Call { callPos = pos } = pos
  debugPosition Typed { typedPos = pos } = pos
  debugPosition Var { varPos = pos } = pos
  debugPosition BadElim { badElimPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Cmd bound free) where
  debugPosition Value { valPos = pos } = pos
  debugPosition Eval { evalPos = pos } = pos
  debugPosition BadCmd { badCmdPos = pos } = pos

instance DWARFPositionElement [Symbol] [Symbol] (Comp bound free) where
  debugPosition Seq { seqPos = pos } = pos
  debugPosition End { endPos = pos } = pos
  debugPosition BadComp { badCompPos = pos } = pos

instance PositionElement (Comp bound free) where position = positionDefault
instance PositionElement (Pattern bound) where position = positionDefault
instance PositionElement (Case bound free) where position = positionDefault
instance PositionElement (Element bound free) where position = positionDefault
instance PositionElement (Field expty) where position = positionDefault
instance PositionElement (Intro bound free) where position = positionDefault
instance PositionElement (Elim bound free) where position = positionDefault
instance PositionElement (Cmd bound free) where position = positionDefault

instance Eq1 Pattern where
  Deconstruct { deconstructBinds = binds1, deconstructStrict = strict1,
                deconstructConstructor = constructor1 } ==#
    Deconstruct { deconstructBinds = binds2, deconstructStrict = strict2,
                  deconstructConstructor = constructor2 } =
      (strict1 == strict2) && (constructor1 == constructor2) &&
      (binds1 == binds2)
  As { asName = sym1, asBind = bind1 } ==#
    As { asName = sym2, asBind = bind2 } =
      (sym1 == sym2) && (bind1 ==# bind2)
  Name { nameSym = sym1 } ==# Name { nameSym = sym2 } = sym1 == sym2
  Exact { exactLiteral = lit1 } ==# Exact { exactLiteral = lit2 } = lit1 == lit2
  _ ==# _ = False

instance Eq b => Eq1 (Case b) where
  Case { casePat = pat1, caseBody = body1 } ==#
    Case { casePat = pat2, caseBody = body2 } =
    pat1 ==# pat2 && body1 ==# body2

instance Eq b => Eq1 (Element b) where
  Element { elemType = ty1, elemTupleIdx = idx1,
            elemPat = pat1, elemName = sym1 } ==#
    Element { elemType = ty2, elemTupleIdx = idx2,
              elemPat = pat2, elemName = sym2 } =
    sym1 == sym2 && idx1 == idx2 && ty1 ==# ty2 && pat1 ==# pat2

instance Eq1 Field where
  Field { fieldVal = val1 } ==# Field { fieldVal = val2 } = val1 == val2

instance Eq b => Eq1 (Intro b) where
  FuncType { funcTypeArgs = argtys1, funcTypeRetTy = retty1 } ==#
    FuncType { funcTypeArgs = argtys2, funcTypeRetTy = retty2 } =
      argtys1 ==# argtys2 && retty1 ==# retty2
  RecordType { recTypeBody = body1 } ==# RecordType { recTypeBody = body2 } =
    body1 ==# body2
  RefineType { refineType = ty1, refineCases = cases1 } ==#
    RefineType { refineType = ty2, refineCases = cases2 } =
      ty1 ==# ty2 && cases1 ==# cases2
  CompType { compType = ty1, compCases = cases1 } ==#
    CompType { compType = ty2, compCases = cases2 } =
      ty1 ==# ty2 && cases1 ==# cases2
  Type { typeRank = rank1 } ==# Type { typeRank = rank2 } = rank1 == rank2
  PropType {} ==# PropType {} = True
  Quantified { quantKind = kind1, quantType = ty1, quantCases = cases1 } ==#
    Quantified { quantKind = kind2, quantType = ty2, quantCases = cases2 } =
      kind1 == kind2 && ty1 ==# ty2 && cases1 ==# cases2
  Lambda { lambdaCases = cases1 } ==# Lambda { lambdaCases = cases2 } =
    cases1 == cases2
  Eta { etaTerm = term1, etaType = ty1 } ==#
    Eta { etaTerm = term2, etaType = ty2 } =
      term1 ==# term2 && ty1 ==# ty2
  Record { recFields = vals1 } ==# Record { recFields = vals2 } = vals1 == vals2
  Tuple { tupleFields = vals1 } ==# Tuple { tupleFields = vals2 } =
    elems vals1 ==# elems vals2
  Fix { fixSym = sym1, fixTerm = term1 } ==#
    Fix { fixSym = sym2, fixTerm = term2 } = sym1 == sym2 && term1 == term2
  Comp { compBody = body1 } ==# Comp { compBody = body2 } = body1 ==# body2
  Elim { elimTerm = term1 } ==# Elim { elimTerm = term2 } = term1 ==# term2
  Literal { literalVal = lit1 } ==# Literal { literalVal = lit2 } = lit1 == lit2
  Constructor { constructorSym = sym1 } ==#
    Constructor { constructorSym = sym2 } = sym1 == sym2
  BadIntro {} ==# BadIntro {} = True
  _ ==# _ = False

instance Eq b => Eq1 (Elim b) where
  Call { callArg = arg1, callFunc = func1 } ==#
    Call { callArg = arg2, callFunc = func2 } =
      arg1 == arg2 && func1 ==# func2
  Typed { typedTerm = term1, typedType = ty1 } ==#
    Typed { typedTerm = term2, typedType = ty2 } =
      term1 ==# term2 && ty1 ==# ty2
  Var { varSym = sym1 } ==# Var { varSym = sym2 } = sym1 == sym2
  BadElim {} ==# BadElim {} = True
  _ ==# _ = False

instance Eq b => Eq1 (Cmd b) where
  Value { valTerm = term1 } ==# Value { valTerm = term2 } = term1 ==# term2
  Eval { evalTerm = term1 } ==# Eval { evalTerm = term2 } = term1 ==# term2
  BadCmd _ ==# BadCmd _ = True
  _ ==# _ = False

instance Eq b => Eq1 (Comp b) where
  Seq { seqType = ty1, seqPat = pat1, seqCmd = cmd1, seqNext = next1 } ==#
    Seq { seqType = ty2, seqPat = pat2, seqCmd = cmd2, seqNext = next2 } =
      pat1 ==# pat2 && cmd1 ==# cmd2 && next1 ==# next2 && ty1 ==# ty2
  End { endCmd = Eval { evalTerm = Comp { compBody = c1 } } } ==# c2 = c1 ==# c2
  c1 ==# End { endCmd = Eval { evalTerm = Comp { compBody = c2 } } } = c1 ==# c2
  End { endCmd = cmd1 } ==# End { endCmd = cmd2 } = cmd1 ==# cmd2
  BadComp {} ==# BadComp {} = True
  _ ==# _ = False

instance (Eq b) => Eq (Pattern b) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Case b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Element b s) where (==) = (==#)
instance (Eq v) => Eq (Field v) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Intro b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Elim b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Cmd b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Comp b s) where (==) = (==#)

keyOrd :: Ord a => (a, b) -> (a, b) -> Ordering
keyOrd (a1, _) (a2, _) = compare a1 a2

instance Ord1 Pattern where
  compare1 Deconstruct { deconstructBinds = binds1, deconstructStrict = strict1,
                         deconstructConstructor = constructor1 }
           Deconstruct { deconstructBinds = binds2, deconstructStrict = strict2,
                         deconstructConstructor = constructor2 } =
    case compare strict1 strict2 of
      EQ -> case compare constructor1 constructor2 of
        EQ -> compare (sortBy keyOrd (HashMap.toList binds1))
                      (sortBy keyOrd (HashMap.toList binds2))
        out -> out
      out -> out
  compare1 Deconstruct {} _ = GT
  compare1 _ Deconstruct {} = LT
  compare1 As { asName = sym1, asBind = bind1 }
           As { asName = sym2, asBind = bind2 } =
    case compare sym1 sym2 of
      EQ -> compare1 bind1 bind2
      out -> out
  compare1 As {} _ = GT
  compare1 _ As {} = LT
  compare1 Name { nameSym = sym1 } Name { nameSym = sym2 } =
    compare sym1 sym2
  compare1 Name {} _ = GT
  compare1 _ Name {} = LT
  compare1 Exact { exactLiteral = val1 } Exact { exactLiteral = val2 } =
    compare val1 val2

instance Ord b => Ord1 (Case b) where
  compare1 Case { casePat = pat1, caseBody = body1 }
           Case { casePat = pat2, caseBody = body2 } =
    case compare1 pat1 pat2 of
      EQ -> compare body1 body2
      out -> out

instance Ord b => Ord1 (Element b) where
  compare1 Element { elemName = sym1, elemTupleIdx = idx1,
                     elemPat = pat1, elemType = ty1 }
           Element { elemName = sym2, elemTupleIdx = idx2,
                     elemPat = pat2, elemType = ty2 } =
    case compare sym1 sym2 of
      EQ -> case compare idx1 idx2 of
        EQ -> case compare1 ty1 ty2 of
          EQ -> compare1 pat1 pat2
          out -> out
        out -> out
      out -> out

instance Ord1 Field where
  compare1 Field { fieldVal = val1 } Field { fieldVal = val2 } =
    compare val1 val2

instance Ord b => Ord1 (Intro b) where
  compare1 FuncType { funcTypeArgs = argtys1, funcTypeRetTy = retty1 }
           FuncType { funcTypeArgs = argtys2, funcTypeRetTy = retty2 } =
    case compare1 retty1 retty2 of
      EQ -> compare1 argtys1 argtys2
      out -> out
  compare1 FuncType {} _ = GT
  compare1 _ FuncType {} = LT
  compare1 RecordType { recTypeBody = body1 }
           RecordType { recTypeBody = body2 } = compare1 body1 body2
  compare1 RecordType {} _ = GT
  compare1 _ RecordType {} = LT
  compare1 RefineType { refineType = ty1, refineCases = cases1 }
           RefineType { refineType = ty2, refineCases = cases2 } =
    case compare1 ty1 ty2 of
      EQ -> compare1 cases1 cases2
      out -> out
  compare1 RefineType {} _ = GT
  compare1 _ RefineType {} = LT
  compare1 CompType { compType = ty1, compCases = cases1 }
           CompType { compType = ty2, compCases = cases2 } =
    case compare ty1 ty2 of
      EQ -> compare1 cases1 cases2
      out -> out
  compare1 CompType {} _ = GT
  compare1 _ CompType {} = LT
  compare1 Type { typeRank = rank1 } Type { typeRank = rank2 } =
    compare rank1 rank2
  compare1 Type {} _ = LT
  compare1 _ Type {} = GT
  compare1 PropType {} PropType {} = EQ
  compare1 PropType {} _ = LT
  compare1 _ PropType {} = GT
  compare1 Quantified { quantKind = kind1, quantType = ty1,
                        quantCases = cases1 }
           Quantified { quantKind = kind2, quantType = ty2,
                        quantCases = cases2 } =
    case compare kind1 kind2 of
      EQ -> case compare1 ty1 ty2 of
        EQ -> compare1 cases1 cases2
        out -> out
      out -> out
  compare1 Quantified {} _ = GT
  compare1 _ Quantified {} = LT
  compare1 Eta { etaTerm = term1, etaType = ty1 }
           Eta { etaTerm = term2, etaType = ty2 } =
    case compare1 term1 term2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 Eta {} _ = GT
  compare1 _ Eta {} = LT
  compare1 Lambda { lambdaCases = cases1 } Lambda { lambdaCases = cases2 } =
    compare1 cases1 cases2
  compare1 Lambda {} _ = GT
  compare1 _ Lambda {} = LT
  compare1 Record { recFields = vals1 } Record { recFields = vals2 } =
    compare (sortBy keyOrd (HashMap.toList vals1))
            (sortBy keyOrd (HashMap.toList vals2))
  compare1 Record {} _ = GT
  compare1 _ Record {} = LT
  compare1 Tuple { tupleFields = vals1 } Tuple { tupleFields = vals2 } =
    compare1 (elems vals1) (elems vals2)
  compare1 Tuple {} _ = GT
  compare1 _ Tuple {} = LT
  compare1 Fix { fixSym = sym1, fixTerm = term1 }
           Fix { fixSym = sym2, fixTerm = term2 } =
    case compare sym1 sym2 of
      EQ ->  compare term1 term2
      out -> out
  compare1 Fix {} _ = GT
  compare1 _ Fix {} = LT
  compare1 Comp { compBody = body1 } Comp { compBody = body2 } =
    compare1 body1 body2
  compare1 Comp {} _ = GT
  compare1 _ Comp {} = LT
  compare1 Elim { elimTerm = term1 } Elim { elimTerm = term2 } =
    compare1 term1 term2
  compare1 Elim {} _ = GT
  compare1 _ Elim {} = LT
  compare1 Literal { literalVal = lit1 } Literal { literalVal = lit2 } =
    compare lit1 lit2
  compare1 Literal {} _ = LT
  compare1 _ Literal {} = GT
  compare1 Constructor { constructorSym = sym1 }
           Constructor { constructorSym = sym2 } =
    compare sym1 sym2
  compare1 Constructor {} _ = LT
  compare1 _ Constructor {} = GT
  compare1 BadIntro {} BadIntro {} = EQ

instance Ord b => Ord1 (Elim b) where
  compare1 Call { callArg = arg1, callFunc = func1 }
           Call { callArg = arg2, callFunc = func2 } =
    case compare1 func1 func2 of
      EQ -> compare arg1 arg2
      out -> out
  compare1 Call {} _ = GT
  compare1 _ Call {} = LT
  compare1 Typed { typedTerm = term1, typedType = ty1 }
           Typed { typedTerm = term2, typedType = ty2 } =
    case compare1 term1 term2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 Typed {} _ = GT
  compare1 _ Typed {} = LT
  compare1 Var { varSym = sym1 } Var { varSym = sym2 } = compare sym1 sym2
  compare1 Var {} _ = GT
  compare1 _ Var {} = LT
  compare1 BadElim {} BadElim {} = EQ

instance Ord b => Ord1 (Cmd b) where
  compare1 Value { valTerm = term1 } Value { valTerm = term2 } =
    compare1 term1 term2
  compare1 Value {} _ = GT
  compare1 _ Value {} = LT
  compare1 Eval { evalTerm = term1 } Eval { evalTerm = term2 } =
    compare1 term1 term2
  compare1 Eval {} _ = GT
  compare1 _ Eval {} = LT
  compare1 BadCmd {} BadCmd {} = EQ

instance Ord b => Ord1 (Comp b) where
  compare1 Seq { seqType = ty1, seqPat = pat1,
                 seqCmd = cmd1, seqNext = next1 }
           Seq { seqType = ty2, seqPat = pat2,
                 seqCmd = cmd2, seqNext = next2 } =
    case compare1 ty1 ty2 of
      EQ -> case compare1 pat1 pat2 of
        EQ -> case compare1 cmd1 cmd2 of
          EQ -> compare1 next1 next2
          out -> out
        out -> out
      out -> out
  compare1 Seq {} _ = GT
  compare1 _ Seq {} = LT
  compare1 End { endCmd = cmd1 } End { endCmd = cmd2 } = compare1 cmd1 cmd2
  compare1 End {} _ = GT
  compare1 _ End {} = LT
  compare1 BadComp {} BadComp {} = EQ

instance (Ord b) => Ord (Pattern b) where compare = compare1
instance (Ord b, Ord s) => Ord (Case b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Element b s) where compare = compare1
instance (Ord v) => Ord (Field v) where compare = compare1
instance (Ord b, Ord s) => Ord (Intro b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Elim b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Cmd b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Comp b s) where compare = compare1

instance Hashable RankID where
  hashWithSalt s = hashWithSalt s . rankID

instance Hashable Literal where
  hashWithSalt s Num { numVal = n } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n
  hashWithSalt s Str { strVal = str } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` str
  hashWithSalt s Char { charVal = c } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` c

instance (Hashable b, Ord b) => Hashable1 (Case b) where
  hashWithSalt1 s Case { casePat = pat, caseBody = body } =
    (s `hashWithSalt` pat) `hashWithSalt1` body

instance (Hashable b, Ord b) => Hashable1 (Element b) where
  hashWithSalt1 s Element { elemName = sym, elemTupleIdx = idx,
                            elemPat = pat, elemType = ty } =
    (s `hashWithSalt` sym `hashWithSalt`
     pat `hashWithSalt` idx) `hashWithSalt1` ty

instance Hashable1 Field where
  hashWithSalt1 s = hashWithSalt s . fieldVal

instance (Hashable b, Ord b) => Hashable1 (Intro b) where
  hashWithSalt1 s FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` argtys `hashWithSalt` retty
  hashWithSalt1 s RecordType { recTypeBody = body } =
    (s `hashWithSalt` (2 :: Int)) `hashWithSalt1` body
  hashWithSalt1 s RefineType { refineType = ty, refineCases = cases } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s CompType { compType = ty, compCases = cases } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Type { typeRank = rank } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` rank
  hashWithSalt1 s PropType {} = s `hashWithSalt` (6 :: Int)
  hashWithSalt1 s Quantified { quantKind = Forall, quantType = ty,
                               quantCases = cases } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Quantified { quantKind = Exists, quantType = ty,
                               quantCases = cases } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Eta { etaTerm = term, etaType = ty } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Lambda { lambdaCases = cases } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt1` cases
  hashWithSalt1 s Record { recFields = vals } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt1`
    sortBy keyOrd (HashMap.toList vals)
  hashWithSalt1 s Tuple { tupleFields = vals } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt1` elems vals
  hashWithSalt1 s Fix { fixSym = sym, fixTerm = term } =
    (s `hashWithSalt` (13 :: Int) `hashWithSalt` sym) `hashWithSalt1` term
  hashWithSalt1 s Comp { compBody = body } =
    s `hashWithSalt` (14 :: Int) `hashWithSalt1` body
  hashWithSalt1 s Elim { elimTerm = term } =
    s `hashWithSalt` (15 :: Int) `hashWithSalt1` term
  hashWithSalt1 s Literal { literalVal = term } =
    s `hashWithSalt` (16 :: Int) `hashWithSalt` term
  hashWithSalt1 s Constructor { constructorSym = sym } =
    s `hashWithSalt` (17 :: Int) `hashWithSalt` sym
  hashWithSalt1 s BadIntro {} = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable1 (Elim b) where
  hashWithSalt1 s Call { callArg = arg, callFunc = func } =
    (s `hashWithSalt` (1 :: Int) `hashWithSalt` arg) `hashWithSalt1` func
  hashWithSalt1 s Typed { typedTerm = term, typedType = ty } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Var { varSym = sym } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` sym
  hashWithSalt1 s BadElim {} = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable1 (Cmd b) where
  hashWithSalt1 s Value { valTerm = term } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1` term
  hashWithSalt1 s Eval { evalTerm = term } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` term
  hashWithSalt1 s (BadCmd _) = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable1 (Comp b) where
  hashWithSalt1 s Seq { seqCmd = cmd, seqNext = next,
                        seqType = ty, seqPat = pat } =
    (s `hashWithSalt` (1 :: Int) `hashWithSalt` pat) `hashWithSalt1`
    ty `hashWithSalt1` cmd `hashWithSalt1` next
  hashWithSalt1 s End { endCmd = cmd } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` cmd
  hashWithSalt1 s (BadComp _) = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable (Pattern b) where
  hashWithSalt s Deconstruct { deconstructConstructor = constructor,
                                deconstructBinds = binds,
                                deconstructStrict = strict } =
    (s `hashWithSalt` (1 :: Int) `hashWithSalt` constructor `hashWithSalt`
     strict) `hashWithSalt` sortBy keyOrd (HashMap.toList binds)
  hashWithSalt s As { asName = sym, asBind = bind } =
    (s `hashWithSalt` (2 :: Int) `hashWithSalt` sym) `hashWithSalt` bind
  hashWithSalt s Name { nameSym = sym } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` sym
  hashWithSalt s Exact { exactLiteral = c } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` c

instance (Hashable b, Hashable s, Ord b) => Hashable (Case b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Element b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable v) => Hashable (Field v) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Intro b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Elim b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Cmd b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Comp b s) where
  hashWithSalt = hashWithSalt1

injectpos :: Position
injectpos = Synthetic { synthDesc = Strict.fromString "Monad return" }
{-
instance MonadTrans (Pattern b) where
  lift = Exact

instance Bound (Pattern b) where
  b @ Deconstruct { deconstructBinds = binds } >>>= f =
    b { deconstructBinds = fmap (>>>= f) binds }
  b @ As { asBind = bind } >>>= f = b { asBind = bind >>>= f }
  b @ Name { nameSym = sym } >>>= _ = b { nameSym = sym }
  Exact t >>>= f = Exact (t >>= f)
-}
instance Applicative (Intro b) where
  pure = return
  (<*>) = ap

instance Applicative (Elim b) where
  pure = return
  (<*>) = ap

instance Applicative (Comp b) where
  pure = return
  (<*>) = ap

caseSubstIntro :: (a -> Intro c b) -> Case c a -> Case c b
caseSubstIntro f c @ Case { caseBody = body } = c { caseBody = body >>>= f }

elementSubstIntro :: (a -> Intro c b) -> Element c a -> Element c b
elementSubstIntro f e @ Element { elemType = ty } = e { elemType = ty >>>= f }

elimSubstIntro :: (a -> Intro c b) -> Elim c a -> Elim c b
elimSubstIntro f t @ Call { callArg = arg, callFunc = func } =
  t { callArg = arg >>= f, callFunc = elimSubstIntro f func }
elimSubstIntro f t @ Typed { typedTerm = term, typedType = ty } =
  t { typedTerm = term >>= f, typedType = ty >>= f }
elimSubstIntro _ Var {} = error "Should not see this case"
elimSubstIntro _ BadElim { badElimPos = p } = BadElim { badElimPos = p }

cmdSubstIntro :: (a -> Intro c b) -> Cmd c a -> Cmd c b
cmdSubstIntro f c @ Value { valTerm = term } = c { valTerm = term >>= f }
cmdSubstIntro f c @ Eval { evalTerm = term } = c { evalTerm = term >>= f }
cmdSubstIntro _ (BadCmd p) = BadCmd p

compSubstIntro :: (a -> Intro c b) -> Comp c a -> Comp c b
compSubstIntro f c @ Seq { seqCmd = cmd, seqNext = next, seqType = ty } =
  c { seqType = ty >>= f, seqCmd = cmdSubstIntro f cmd,
      seqNext = next >>>= compSubstIntro f . return }
compSubstIntro f c @ End { endCmd = cmd } =
  c { endCmd = cmdSubstIntro f cmd }
compSubstIntro _ (BadComp p) = BadComp p

instance Monad (Intro b) where
  return sym = Elim { elimTerm = return sym }

  t @ FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } >>= f =
    t { funcTypeArgs = fmap (elementSubstIntro f) argtys,
        funcTypeRetTy = retty >>>= f }
  t @ RecordType { recTypeBody = body } >>= f =
    t { recTypeBody = fmap (elementSubstIntro f) body }
  t @ RefineType { refineType = ty, refineCases = cases } >>= f =
    t { refineType = ty >>= f, refineCases = fmap (caseSubstIntro f) cases }
  t @ CompType { compType = ty, compCases = cases } >>= f =
    t { compType = ty >>= f, compCases = fmap (caseSubstIntro f) cases }
  Type { typeRank = rank, typePos = pos } >>= _ =
    Type { typeRank = rank, typePos = pos }
  PropType { propPos = pos } >>= _ = PropType { propPos = pos }
  t @ Quantified { quantType = ty, quantCases = cases } >>= f =
    t { quantCases = fmap (caseSubstIntro f) cases, quantType = ty >>= f }
  t @ Lambda { lambdaCases = cases } >>= f =
    t { lambdaCases = fmap (caseSubstIntro f) cases }
  t @ Record { recFields = vals } >>= f =
    t { recFields = fmap (fmap (>>= f)) vals }
  t @ Tuple { tupleFields = vals } >>= f = t { tupleFields = fmap (>>= f) vals }
  t @ Fix { fixTerm = term } >>= f = t { fixTerm = term >>>= f }
  t @ Comp { compBody = body } >>= f = t { compBody = compSubstIntro f body }
  Elim { elimTerm = Var { varSym = sym } } >>= f = f sym
  t @ Elim { elimTerm = term } >>= f = t { elimTerm = elimSubstIntro f term }
  t @ Eta { etaTerm = term, etaType = ty } >>= f =
    t { etaTerm = elimSubstIntro f term, etaType = ty >>= f }
  Literal { literalVal = lit, literalPos = p } >>= _ =
    Literal { literalVal = lit, literalPos = p }
  Constructor { constructorSym = sym, constructorPos = p } >>= _ =
    Constructor { constructorSym = sym, constructorPos = p }
  BadIntro { badIntroPos = p } >>= _ = BadIntro { badIntroPos = p }

caseSubstElim :: (a -> Elim c b) -> Case c a -> Case c b
caseSubstElim f c @ Case { caseBody = body } =
  c { caseBody = body >>>= (Elim . f) }

elementSubstElim :: (a -> Elim c b) -> Element c a -> Element c b
elementSubstElim f e @ Element { elemType = ty } =
  e { elemType = ty >>>= (Elim . f) }

cmdSubstElim :: (a -> Elim c b) -> Cmd c a -> Cmd c b
cmdSubstElim f c @ Value { valTerm = term } =
  c { valTerm = introSubstElim f term }
cmdSubstElim f c @ Eval { evalTerm = term } =
  c { evalTerm = introSubstElim f term }
cmdSubstElim _ (BadCmd p) = BadCmd p

compSubstElim :: (a -> Elim c b) -> Comp c a -> Comp c b
compSubstElim f c @ Seq { seqCmd = cmd, seqNext = next, seqType = ty } =
  c { seqType = introSubstElim f ty, seqCmd = cmdSubstElim f cmd,
      seqNext = next >>>= compSubstElim f . return }
compSubstElim f c @ End { endCmd = cmd } =
  c { endCmd = cmdSubstElim f cmd }
compSubstElim _ (BadComp p) = BadComp p

introSubstElim :: (a -> Elim c b) -> Intro c a -> Intro c b
introSubstElim f t @ FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
  t { funcTypeArgs = fmap (elementSubstElim f) argtys,
      funcTypeRetTy = retty >>>= (Elim . f) }
introSubstElim f t @ RecordType { recTypeBody = body } =
  t { recTypeBody = fmap (elementSubstElim f) body }
introSubstElim f t @ RefineType { refineType = ty, refineCases = cases } =
  t { refineType = introSubstElim f ty,
      refineCases = fmap (caseSubstElim f) cases }
introSubstElim f t @ CompType { compType = ty, compCases = cases } =
  t { compType = introSubstElim f ty, compCases = fmap (caseSubstElim f) cases }
introSubstElim _ Type { typeRank = rank, typePos = p } =
  Type { typeRank = rank, typePos = p }
introSubstElim _ PropType { propPos = p } = PropType { propPos = p }
introSubstElim f t @ Quantified { quantType = ty, quantCases = cases } =
  t { quantCases = fmap (caseSubstElim f) cases,
      quantType = introSubstElim f ty }
introSubstElim f t @ Lambda { lambdaCases = cases } =
    t { lambdaCases = fmap (caseSubstElim f) cases }
introSubstElim f t @ Record { recFields = vals } =
  t { recFields = fmap (fmap (introSubstElim f)) vals }
introSubstElim f t @ Tuple { tupleFields = vals } =
  t { tupleFields = fmap (introSubstElim f) vals }
introSubstElim f t @ Fix { fixTerm = term } = t { fixTerm = term >>>= Elim . f }
introSubstElim f t @ Comp { compBody = body } =
  t { compBody = compSubstElim f body }
introSubstElim f Elim { elimTerm = term } = Elim { elimTerm = term >>= f }
introSubstElim f t @ Eta { etaTerm = term, etaType = ty } =
  t { etaTerm = term >>= f, etaType = introSubstElim f ty }
introSubstElim _ Literal { literalVal = lit, literalPos = p } =
  Literal { literalVal = lit, literalPos = p }
introSubstElim _ Constructor { constructorSym = sym, constructorPos = p } =
  Constructor { constructorSym = sym, constructorPos = p }
introSubstElim _ BadIntro { badIntroPos = p } = BadIntro { badIntroPos = p }

instance Monad (Elim b) where
  return sym = Var { varSym = sym, varPos = injectpos }

  t @ Call { callArg = arg, callFunc = func } >>= f =
    t { callArg = introSubstElim f arg, callFunc = func >>= f }
  t @ Typed { typedTerm = term, typedType = ty } >>= f =
    t { typedTerm = introSubstElim f term, typedType = introSubstElim f ty }
  Var { varSym = sym } >>= f = f sym
  BadElim { badElimPos = p } >>= _ = BadElim { badElimPos = p }

introSubstComp :: (a -> Comp c b) -> a -> Intro c b
introSubstComp f sym =
  case f sym of
    End { endCmd = Eval { evalTerm = term } } -> term
    End { endCmd = Value { valTerm = term } } -> term
    End { endCmd = BadCmd p } -> BadIntro p
    body @ Seq { seqPos = pos } -> Comp { compBody = body, compPos = pos }
    BadComp { badCompPos = p } -> BadIntro { badIntroPos = p }

cmdSubstComp :: (a -> Comp c b) -> Cmd c a -> Cmd c b
cmdSubstComp f c @ Value { valTerm = term } =
  c { valTerm = term >>= introSubstComp f }
cmdSubstComp f c @ Eval { evalTerm = term } =
  c { evalTerm = term >>= introSubstComp f }
cmdSubstComp _ (BadCmd p) = BadCmd p

instance Monad (Comp b) where
  return sym = End { endCmd = Eval { evalTerm = return sym,
                                     evalPos = injectpos },
                     endPos = injectpos }

  c @ Seq { seqType = ty, seqCmd = cmd, seqNext = next } >>= f =
    c { seqType = ty >>= introSubstComp f, seqNext = next >>>= f,
        seqCmd = cmdSubstComp f cmd }
  c @ End { endCmd = cmd } >>= f = c { endCmd = cmdSubstComp f cmd }
  BadComp p >>= _ = BadComp p

instance Format Literal where
  format Num { numVal = num }
    | denominator num == 1 = format (numerator num)
    | otherwise = format (numerator num) <> char '/' <> format (denominator num)
  format Str { strVal = str } = dquoted (bytestring str)
  format Char { charVal = chr } = squoted (char chr)

instance Show Literal where
  show = Lazy.toString . renderOptimal 80 False . format

instance Monad m => FormatM m Literal where
  formatM = return . format

formatMBind :: (MonadPositions m, MonadSymbols m,
                FormatM m sym, FormatM m ent, Eq sym) =>
              (sym, ent) -> m (Doc, Doc)
formatMBind (fname, ent) =
  do
    fname' <- formatM fname
    ent' <- formatM ent
    return (fname', ent')

instance Format Quantifier where
  format Forall = string "forall"
  format Exists = string "exists"

instance (MonadPositions m, MonadSymbols m, FormatM m valty) =>
         FormatM m (Field valty) where
  formatM = formatM . fieldVal

instance (MonadPositions m, MonadSymbols m,
          FormatM m bound, Default bound, Eq bound) =>
         FormatM m (Pattern bound) where
  formatM Deconstruct { deconstructConstructor = sym, deconstructBinds = binds }
    | sym == def =
      do
        binddocs <- mapM formatMBind (HashMap.toList binds)
        return $! recordDoc binddocs
    | otherwise =
      do
        symdoc <- formatM sym
        binddocs <- mapM formatMBind (HashMap.toList binds)
        return $! compoundApplyDoc symdoc binddocs
  formatM As { asBind = bind, asName = sym } =
    do
      namedoc <- formatM sym
      binddoc <- formatM bind
      return (namedoc </> nest nestLevel (string "as" </> binddoc))
  formatM Name { nameSym = sym } = formatM sym
  formatM Exact { exactLiteral = e } = formatM e

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Case bound free) where
  formatM Case { casePat = pat, caseBody = body } =
    do
      patdoc <- formatM pat
      bodydoc <- formatM body
      return (patdoc </> equals </> nest nestLevel bodydoc)

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Element bound free) where
  formatM Element { elemPat = Name { nameSym = sym' },
                    elemType = ty, elemName = sym } | sym' == def =
    do
      symdoc <- formatM sym
      tydoc <- formatM ty
      case flatten tydoc of
        Just flatty ->
          return (choose [ symdoc <+> colon <+> flatty,
                           symdoc <+> colon <!> nest nestLevel tydoc ])
        Nothing -> return (symdoc <+> colon <!> nest nestLevel tydoc)
  formatM Element { elemPat = pat, elemType = ty, elemName = sym } =
    do
      symdoc <- formatM sym
      patdoc <- formatM pat
      tydoc <- formatM ty
      case flatten tydoc of
        Just flatty ->
          return (choose [ symdoc <+> colon <+> flatty <+>
                           equals <!> nest nestLevel patdoc,
                           symdoc <+> colon <!>
                           nest nestLevel (tydoc <+> equals <!>
                                           nest nestLevel patdoc) ])
        Nothing -> return (symdoc <+> colon <!>
                           nest nestLevel (tydoc <+> equals <!>
                                           nest nestLevel patdoc))

formatMCompList :: (MonadPositions m, MonadSymbols m, FormatM m bound,
                    FormatM m free, Default bound, Eq bound) =>
                  Comp bound free -> m [Doc]
formatMCompList =
  let
    formatMCompList':: (MonadPositions m, MonadSymbols m, FormatM m bound,
                        FormatM m free, Default bound, Eq bound) =>
                       [Doc] -> Comp bound free -> m [Doc]
    formatMCompList' accum Seq { seqCmd = cmd, seqPat = pat,
                                 seqType = ty, seqNext = next } =
      do
        cmddoc <- formatM cmd
        patdoc <- formatM pat
        typedoc <- formatM ty
        formatMCompList' (patdoc <+> colon </>
                          nest nestLevel (typedoc <+> equals </>
                                          nest nestLevel cmddoc) : accum)
                                         (fromScope next)
    formatMCompList' accum End { endCmd = cmd } =
      do
        cmddoc <- formatM cmd
        return $! reverse (cmddoc : accum)
    formatMCompList' accum BadComp {} =
      return $! reverse (string "<bad>" : accum)
  in
    formatMCompList' []

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Intro bound free) where
  formatM Eta {} = error "Eta is going away"
  formatM FuncType { funcTypeArgs = args, funcTypeRetTy = retty } =
    do
      argdocs <- mapM formatM args
      rettydoc <- formatM retty
      return (tupleDoc argdocs <+> string "->" </> nest nestLevel rettydoc)
  formatM RecordType { recTypeBody = body } =
    do
      bodydocs <- mapM formatM body
      return (tupleDoc bodydocs)
  formatM RefineType { refineType = ty, refineCases = cases } =
    do
      tydoc <- formatM ty
      casedocs <- mapM formatM cases
      return (tydoc </> nest nestLevel (string "\\where" <+> casesDoc casedocs))
  formatM CompType { compType = ty, compCases = cases } =
    do
      tydoc <- formatM ty
      casedocs <- mapM formatM cases
      return (tydoc </> nest nestLevel (string "\\spec" <+> casesDoc casedocs))
  formatM Type {} = return $! string "type"
  formatM PropType {} = return $! string "prop"
  formatM Quantified { quantType = ty, quantCases = cases, quantKind = kind } =
    do
      tydoc <- formatM ty
      casedocs <- mapM formatM cases
      return (format kind <+> tydoc <> dot <+> casesDoc casedocs)
  formatM Lambda { lambdaCases = cases } =
    do
      casedocs <- mapM formatM cases
      return (string "\\lambda" <+> casesDoc casedocs)
  formatM Record { recFields = fields } =
    do
      fielddocs <- mapM formatMBind (HashMap.toList fields)
      return (recordDoc fielddocs)
  formatM Tuple { tupleFields = fields } =
    do
      fielddocs <- mapM formatM (elems fields)
      return (tupleDoc fielddocs)
  formatM Fix { fixSym = sym, fixTerm = term } =
    do
      symdoc <- formatM sym
      termdoc <- formatM term
      return (string "fix " <> symdoc <+> equals <//> nest nestLevel termdoc)
  formatM Comp { compBody = body } =
    do
      bodydoc <- formatMCompList body
      return $! blockDoc bodydoc
  formatM Elim { elimTerm = term } = formatM term
  formatM Constructor { constructorSym = sym } = formatM sym
  formatM Literal { literalVal = lit } = formatM lit
  formatM BadIntro {} = return $! string "<bad>"

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Elim bound free) where
  formatM Call { callFunc = func, callArg = arg } =
    do
      argdocs <- formatM arg
      funcdoc <- formatM func
      return (funcdoc </> argdocs)
  formatM Typed { typedTerm = term, typedType = ty } =
    do
      termdoc <- formatM term
      typedoc <- formatM ty
      return (termdoc <+> colon </> nest nestLevel typedoc)
  formatM Var { varSym = sym } = formatM sym
  formatM BadElim {} = return $! string "<bad>"

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Cmd bound free) where
  formatM Value { valTerm = term } = formatM term
  formatM Eval { evalTerm = term } =
    do
      termdoc <- formatM term
      return $! string "do" </> nest nestLevel termdoc
  formatM BadCmd {} = return $! string "<bad>"

instance (MonadPositions m, MonadSymbols m, FormatM m bound,
          FormatM m free, Default bound, Eq bound) =>
         FormatM m (Comp bound free) where
  formatM comp =
    do
      stmlist <- formatMCompList comp
      return $! stmsDoc stmlist

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [(tag, text)] RankID where
  xpickle = xpWrap (RankID, rankID) (xpAttr (gxFromString "rank-var") xpickle)

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
numPickler =
  let
    revfunc Num { numVal = num } = (numerator num, denominator num)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(numer, denom) -> Num { numVal = numer % denom }, revfunc)
           (xpElemAttrs (gxFromString "Num")
                        (xpPair (xpAttr (gxFromString "numerator") xpPrim)
                                (xpAttr (gxFromString "denominator") xpPrim)))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
strPickler =
  let
    revfunc Str { strVal = str } = gxFromByteString str
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\str -> Str { strVal = gxToByteString str }, revfunc)
           (xpElemNodes (gxFromString "Str")
                        (xpElemNodes (gxFromString "value")
                                     (xpContent xpText0)))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
charPickler =
  let
    revfunc Char { charVal = chr } = chr
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\chr -> Char { charVal = chr }, revfunc)
           (xpElemAttrs (gxFromString "Char")
                        (xpAttr (gxFromString "value") xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Literal where
  xpickle =
    let
      picker Num {} = 0
      picker Str {} = 1
      picker Char {} = 2
    in
      xpAlt picker [numPickler, strPickler, charPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [(tag, text)] Quantifier where
  xpickle = xpAlt fromEnum [xpWrap (const Forall, const ())
                                   (xpAttrFixed (gxFromString "kind")
                                                (gxFromString "forall")),
                            xpWrap (const Forall, const ())
                                   (xpAttrFixed (gxFromString "kind")
                                                (gxFromString "exists"))]

mapPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler (Attributes tag text) key,
               XmlPickler [NodeG [] tag text] val,
               Hashable key, Eq key) =>
              String -> PU [NodeG [] tag text] (HashMap key val)
mapPickler entname =
  xpWrap (HashMap.fromList, HashMap.toList)
         (xpList (xpElem (gxFromString entname) xpickle xpickle))

deconstructPickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text,
                       XmlPickler [(tag, text)] bound,
                       XmlPickler [NodeG [] tag text] bound,
                       Hashable bound, Eq bound) =>
             PU [NodeG [] tag text] (Pattern bound)
deconstructPickler =
  let
    revfunc Deconstruct { deconstructStrict = strict, deconstructBinds = binds,
                          deconstructConstructor = sym, deconstructPos = pos } =
      ((sym, strict), (binds, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((sym, strict), (binds, pos)) ->
             Deconstruct { deconstructStrict = strict, deconstructBinds = binds,
                           deconstructConstructor = sym, deconstructPos = pos },
            revfunc)
           (xpElem (gxFromString "Deconstruct")
                   (xpPair xpickle (xpAttr (gxFromString "strict") xpPrim))
                   (xpPair (xpElemNodes (gxFromString "binds")
                                        (mapPickler "field"))
                           (xpElemNodes (gxFromString "pos") xpickle)))

asPickler :: (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
              XmlPickler [(tag, text)] bound,
              XmlPickler [NodeG [] tag text] bound,
              Hashable bound, Eq bound) =>
             PU [NodeG [] tag text] (Pattern bound)
asPickler =
  let
    revfunc As { asName = sym, asBind = pat, asPos = pos } = (sym, (pat, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (pat, pos)) -> As { asName = sym, asBind = pat,
                                       asPos = pos }, revfunc)
           (xpElem (gxFromString "As") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "pair") xpickle)))

namePickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Pattern bound)
namePickler =
  let
    revfunc Name { nameSym = sym, namePos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Name { nameSym = sym, namePos = pos }, revfunc)
           (xpElem (gxFromString "Name") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

constantPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [NodeG [] tag text] bound,
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Pattern bound)
constantPickler =
  let
    revfunc Exact { exactLiteral = v, exactPos = pos } = (v, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(v, pos) -> Exact { exactLiteral = v, exactPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Exact")
                        (xpPair (xpElemNodes (gxFromString "literal") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Pattern bound) where
  xpickle =
    let
      picker Deconstruct {} = 0
      picker As {} = 1
      picker Name {} = 2
      picker Exact {} = 3
    in
      xpAlt picker [ deconstructPickler, asPickler,
                     namePickler, constantPickler ]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Case bound free) where
  xpickle =
    xpWrap (\(pat, body, pos) -> Case { casePat = pat, caseBody = body,
                                        casePos = pos },
            \Case { casePat = pat, caseBody = body, casePos = pos } ->
            (pat, body, pos))
           (xpElemNodes (gxFromString "Case")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpElemNodes (gxFromString "body") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Element bound free) where
  xpickle =
    xpWrap (\((sym, idx), (pat, ty, pos)) ->
             Element { elemName = sym, elemTupleIdx = idx, elemPat = pat,
                       elemType = ty, elemPos = pos },
            \Element { elemName = sym, elemTupleIdx = idx, elemPat = pat,
                       elemType = ty, elemPos = pos } -> ((sym, idx),
                                                          (pat, ty, pos)))
           (xpElem (gxFromString "Element")
                   (xpPair xpickle
                           (xpAttr (gxFromString "tuple-index") xpPrim))
                   (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Field expty) where
  xpickle =
    xpWrap (\(val, pos) -> Field { fieldVal = val, fieldPos = pos },
            \Field { fieldVal = val, fieldPos = pos } -> (val, pos))
           (xpElemNodes (gxFromString "Field")
                        (xpPair (xpElemNodes (gxFromString "val") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

listToArr :: [a] -> Array Word a
listToArr l = listArray (0, fromIntegral (length l)) l

funcTypePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [(tag, text)] bound,
                    XmlPickler [NodeG [] tag text] bound,
                    XmlPickler [NodeG [] tag text] free,
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Intro bound free)
funcTypePickler =
  let
    revfunc FuncType { funcTypeArgs = args, funcTypeRetTy = retty,
                       funcTypePos = pos } = (args, retty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(args, retty, pos) -> FuncType { funcTypeArgs = args,
                                              funcTypeRetTy = retty,
                                              funcTypePos = pos }, revfunc)
           (xpElemNodes (gxFromString "FuncType")
                        (xpTriple (xpElemNodes (gxFromString "args") xpickle)
                                  (xpElemNodes (gxFromString "retty") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

recordTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [(tag, text)] bound,
                      XmlPickler [NodeG [] tag text] bound,
                      XmlPickler [NodeG [] tag text] free,
                      Hashable bound, Eq bound) =>
                     PU [NodeG [] tag text] (Intro bound free)
recordTypePickler =
  let
    revfunc RecordType { recTypeBody = body, recTypePos = pos } = (body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(body, pos) -> RecordType { recTypeBody = body,
                                         recTypePos = pos }, revfunc)
           (xpElemNodes (gxFromString "RecordType")
                        (xpPair (xpList (xpElemNodes (gxFromString "body")
                                                     xpickle))
                                (xpElemNodes (gxFromString "pos") xpickle)))

refineTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [(tag, text)] bound,
                      XmlPickler [NodeG [] tag text] bound,
                      XmlPickler [NodeG [] tag text] free,
                      Hashable bound, Eq bound) =>
                     PU [NodeG [] tag text] (Intro bound free)
refineTypePickler =
  let
    revfunc RefineType { refineType = ty, refineCases = cases,
                         refinePos = pos } = (ty, cases, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(ty, cases, pos) -> RefineType { refineType = ty,
                                              refineCases = cases,
                                              refinePos = pos },
            revfunc)
           (xpElemNodes (gxFromString "RefineType")
                        (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                                  (xpList (xpElemNodes (gxFromString "cases")
                                                       xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

compTypePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [(tag, text)] bound,
                    XmlPickler [NodeG [] tag text] bound,
                    XmlPickler [NodeG [] tag text] free,
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Intro bound free)
compTypePickler =
  let
    revfunc RefineType { refineType = ty, refineCases = cases,
                         refinePos = pos } = (ty, cases, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(ty, cases, pos) -> RefineType { refineType = ty,
                                              refineCases = cases,
                                              refinePos = pos },
            revfunc)
           (xpElemNodes (gxFromString "RefineType")
                        (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                                  (xpList (xpElemNodes (gxFromString "cases")
                                                       xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

typePickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Intro bound free)
typePickler =
  let
    revfunc Type { typeRank = rank, typePos = pos } = (rank, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(rank, pos) -> Type { typeRank = rank, typePos = pos }, revfunc)
           (xpElem (gxFromString "Type") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

propPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Intro bound free)
propPickler =
  let
    revfunc PropType { propPos = pos } = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (PropType, revfunc)
           (xpElemNodes (gxFromString "PropType")
                        (xpElemNodes (gxFromString "pos") xpickle))

quantifiedPickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [(tag, text)] bound,
                      XmlPickler [NodeG [] tag text] bound,
                      XmlPickler [NodeG [] tag text] free,
                      Hashable bound, Eq bound) =>
                     PU [NodeG [] tag text] (Intro bound free)
quantifiedPickler =
  let
    revfunc Quantified { quantKind = kind, quantType = ty, quantCases = cases,
                         quantPos = pos } = (kind, (ty, cases, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(kind, (ty, cases, pos)) ->
             Quantified { quantKind = kind, quantType = ty, quantCases = cases,
                          quantPos = pos },
            revfunc)
           (xpElem (gxFromString "Quantified") xpickle
                   (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                             (xpList (xpElemNodes (gxFromString "cases")
                                                  xpickle))
                             (xpElemNodes (gxFromString "pos") xpickle)))

lambdaPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [(tag, text)] bound,
                  XmlPickler [NodeG [] tag text] bound,
                  XmlPickler [NodeG [] tag text] free,
                  Hashable bound, Eq bound) =>
                 PU [NodeG [] tag text] (Intro bound free)
lambdaPickler =
  let
    revfunc Lambda { lambdaCases = cases, lambdaPos = pos } = (cases, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(cases, pos) -> Lambda { lambdaCases = cases, lambdaPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "Lambda")
                        (xpPair (xpList (xpElemNodes (gxFromString "cases")
                                                     xpickle))
                                (xpElemNodes (gxFromString "pos") xpickle)))

recordPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [(tag, text)] bound,
                  XmlPickler [NodeG [] tag text] bound,
                  XmlPickler [NodeG [] tag text] free,
                  Hashable bound, Eq bound) =>
                 PU [NodeG [] tag text] (Intro bound free)
recordPickler =
  let
    revfunc Record { recFields = vals, recPos = pos } = (vals, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(vals, pos) -> Record { recFields = vals, recPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Record")
                        (xpPair (xpElemNodes (gxFromString "fields")
                                             (mapPickler "field"))
                                (xpElemNodes (gxFromString "pos") xpickle)))

tuplePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [(tag, text)] bound,
                 XmlPickler [NodeG [] tag text] bound,
                 XmlPickler [NodeG [] tag text] free,
                 Hashable bound, Eq bound) =>
                PU [NodeG [] tag text] (Intro bound free)
tuplePickler =
  let
    revfunc Tuple { tupleFields = vals, tuplePos = pos } = (elems vals, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(vals, pos) -> Tuple { tupleFields = listToArr vals,
                                    tuplePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Record")
                        (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

fixPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [(tag, text)] bound,
               XmlPickler [NodeG [] tag text] bound,
               XmlPickler [NodeG [] tag text] free,
               Hashable bound, Eq bound) =>
              PU [NodeG [] tag text] (Intro bound free)
fixPickler =
  let
    revfunc Fix { fixSym = sym, fixTerm = term, fixPos = pos } =
      (sym, term, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, term, pos) -> Fix { fixSym = sym, fixTerm = term,
                                       fixPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Fix")
                        (xpTriple (xpElemNodes (gxFromString "sym") xpickle)
                                  (xpElemNodes (gxFromString "terms") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

compPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Intro bound free)
compPickler =
  let
    revfunc Comp { compBody = body, compPos = pos } = (body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(body, pos) -> Comp { compBody = body, compPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Comp")
                        (xpPair (xpElemNodes (gxFromString "body") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

elimPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Intro bound free)
elimPickler =
  let
    revfunc Elim { elimTerm = term } = term
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Elim, revfunc)
           (xpElemNodes (gxFromString "Elim")
                        (xpElemNodes (gxFromString "term") xpickle))

constructorPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text,
                     XmlPickler [(tag, text)] bound,
                     XmlPickler [NodeG [] tag text] bound,
                     XmlPickler [NodeG [] tag text] free,
                     Hashable bound, Eq bound) =>
                    PU [NodeG [] tag text] (Intro bound free)
constructorPickler =
  let
    revfunc Constructor { constructorSym = sym, constructorPos = pos } =
      (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Constructor { constructorSym = sym,
                                         constructorPos = pos },
            revfunc)
           (xpElem (gxFromString "Constructor") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

literalPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [(tag, text)] bound,
                   XmlPickler [NodeG [] tag text] bound,
                   XmlPickler [NodeG [] tag text] free,
                   Hashable bound, Eq bound) =>
                  PU [NodeG [] tag text] (Intro bound free)
literalPickler =
  let
    revfunc Literal { literalVal = lit, literalPos = pos } = (lit, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(lit, pos) -> Literal { literalVal = lit, literalPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "Literal")
                        (xpPair (xpElemNodes (gxFromString "literal") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

badIntroPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [NodeG [] tag text] bound,
                    XmlPickler [NodeG [] tag text] free,
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Intro bound free)
badIntroPickler =
  let
    revfunc BadIntro { badIntroPos = pos } = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (BadIntro, revfunc)
           (xpElemNodes (gxFromString "BadIntro")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Intro bound free) where
  xpickle =
    let
      picker FuncType {} = 0
      picker RecordType {} = 1
      picker RefineType {} = 2
      picker CompType {} = 3
      picker Type {} = 4
      picker PropType {} = 5
      picker Quantified {} = 6
      picker Lambda {} = 7
      picker Record {} = 8
      picker Tuple {} = 9
      picker Fix {} = 10
      picker Comp {} = 11
      picker Elim {} = 12
      picker Constructor {} = 13
      picker Literal {} = 14
      picker BadIntro {} = 15
      picker Eta {} = error "Eta not supported"
    in
      xpAlt picker [ funcTypePickler, recordTypePickler, refineTypePickler,
                     compTypePickler, typePickler, propPickler,
                     quantifiedPickler, lambdaPickler, recordPickler,
                     tuplePickler, fixPickler, compPickler, elimPickler,
                     constructorPickler, literalPickler, badIntroPickler ]


callPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Elim bound free)
callPickler =
  let
    revfunc Call { callFunc = func, callArg = arg,
                   callPos = pos } = (func, arg, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(func, arg, pos) -> Call { callFunc = func, callArg = arg,
                                         callPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "Call")
                        (xpTriple (xpElemNodes (gxFromString "func") xpickle)
                                  (xpElemNodes (gxFromString "arg") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

typedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [(tag, text)] bound,
                 XmlPickler [NodeG [] tag text] bound,
                 XmlPickler [NodeG [] tag text] free,
                 Hashable bound, Eq bound) =>
                PU [NodeG [] tag text] (Elim bound free)
typedPickler =
  let
    revfunc Typed { typedTerm = term, typedType = ty,
                    typedPos = pos } = (term, ty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(term, ty, pos) -> Typed { typedTerm = term, typedType = ty,
                                        typedPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Typed")
                        (xpTriple (xpElemNodes (gxFromString "term") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

varPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] bound,
               XmlPickler [NodeG [] tag text] free,
               Hashable bound, Eq bound) =>
              PU [NodeG [] tag text] (Elim bound free)
varPickler =
  let
    revfunc Var { varSym = sym, varPos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Var { varSym = sym, varPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Var")
                        (xpPair (xpElemNodes (gxFromString "sym") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

badElimPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] bound,
                   XmlPickler [NodeG [] tag text] free,
                   Hashable bound, Eq bound) =>
                  PU [NodeG [] tag text] (Elim bound free)
badElimPickler =
  let
    revfunc BadElim { badElimPos = pos } = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (BadElim, revfunc)
           (xpElemNodes (gxFromString "BadElim")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Elim bound free) where
  xpickle =
    let
      picker Call {} = 0
      picker Typed {} = 1
      picker Var {} = 2
      picker BadElim {} = 3
    in
      xpAlt picker [ callPickler, typedPickler, varPickler, badElimPickler ]

valuePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [(tag, text)] bound,
                 XmlPickler [NodeG [] tag text] bound,
                 XmlPickler [NodeG [] tag text] free,
                 Hashable bound, Eq bound) =>
                PU [NodeG [] tag text] (Cmd bound free)
valuePickler =
  let
    revfunc Value { valTerm = term, valPos = pos } = (term, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(term, pos) -> Value { valTerm = term, valPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Value")
                        (xpPair (xpElemNodes (gxFromString "term") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

evalPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Cmd bound free)
evalPickler =
  let
    revfunc Eval { evalTerm = term, evalPos = pos } = (term, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(term, pos) -> Eval { evalTerm = term, evalPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Eval")
                        (xpPair (xpElemNodes (gxFromString "term") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

badCmdPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] bound,
                  XmlPickler [NodeG [] tag text] free,
                  Hashable bound, Eq bound) =>
                 PU [NodeG [] tag text] (Cmd bound free)
badCmdPickler =
  let
    revfunc (BadCmd pos) = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (BadCmd, revfunc)
           (xpElemNodes (gxFromString "BadCmd")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Cmd bound free) where
  xpickle =
    let
      picker Value {} = 0
      picker Eval {} = 1
      picker BadCmd {} = 2
    in
      xpAlt picker [ valuePickler, evalPickler, badCmdPickler ]

seqPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [(tag, text)] bound,
               XmlPickler [NodeG [] tag text] bound,
               XmlPickler [NodeG [] tag text] free,
               Hashable bound, Eq bound) =>
              PU [NodeG [] tag text] (Comp bound free)
seqPickler =
  let
    revfunc Seq { seqCmd = cmd, seqPat = pat, seqType = ty,
                  seqNext = next, seqPos = pos } = (cmd, pat, ty, next, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(cmd, pat, ty, next, pos) -> Seq { seqCmd = cmd, seqPat = pat,
                                                seqType = ty, seqNext = next,
                                                seqPos = pos }, revfunc)
           (xpElemNodes (gxFromString "End")
                        (xp5Tuple (xpElemNodes (gxFromString "cmd") xpickle)
                                  (xpElemNodes (gxFromString "pat") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "next") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

endPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [(tag, text)] bound,
               XmlPickler [NodeG [] tag text] bound,
               XmlPickler [NodeG [] tag text] free,
               Hashable bound, Eq bound) =>
              PU [NodeG [] tag text] (Comp bound free)
endPickler =
  let
    revfunc End { endCmd = cmd, endPos = pos } = (cmd, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(cmd, pos) -> End { endCmd = cmd, endPos = pos }, revfunc)
           (xpElemNodes (gxFromString "End")
                        (xpPair (xpElemNodes (gxFromString "cmd") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

badCompPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] bound,
                   XmlPickler [NodeG [] tag text] free,
                   Hashable bound, Eq bound) =>
                  PU [NodeG [] tag text] (Comp bound free)
badCompPickler =
  let
    revfunc (BadComp pos) = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (BadComp, revfunc)
           (xpElemNodes (gxFromString "BadComp")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Comp bound free) where
  xpickle =
    let
      picker Seq {} = 0
      picker End {} = 1
      picker BadComp {} = 2
    in
      xpAlt picker [ seqPickler, endPickler, badCompPickler ]
