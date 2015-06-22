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
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances,
             MultiParamTypeClasses #-}

-- | The Salt core language.  Salt's surface syntax is transliterated
-- into Core, which is then type-checked and compiled.  Core is
-- generally not meant for human consumption.
module Language.Salt.Core.Syntax(
       Quantifier(..),
       Pattern(..),
       Element(..),
       Case(..),
       Term(..),
       Cmd(..),
       Comp(..)
       ) where

import Bound
import Bound.Scope.ExtraInstances()
import Bound.Var.ExtraInstances()
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Trans
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.Hashable.ExtraInstances()
import Data.HashMap.Strict(HashMap)
import Data.List(sortBy)
import Data.Monoid(mappend, mempty)
import Data.Position.DWARFPosition
import Data.Traversable
import Prelude hiding (foldl, mapM)
import Prelude.Extras(Eq1(..), Ord1(..))
import Prelude.Extras.ExtraInstances()
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)
--import Text.Format hiding ((<$>))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.UTF8 as Lazy

-- | Quantifier types.
data Quantifier =
  -- | Universal quantifiers.
    Forall
  -- | Existential quantifiers.
  | Exists
    deriving (Ord, Eq, Enum)

-- | A pattern binding.  Represents how to deconstruct a value and
-- bind it to variables.
data Pattern bound const free =
    -- | A deconstruction.  Takes a type apart.  Some types have
    -- constructors; nameless record types don't (they should use the
    -- "unused" symbol).
    Deconstruct {
      -- | The type constructor.  For nameless records, use the
      -- "unused" symbol.
      deconstructConstructor :: !bound,
      -- | The fields in the record being bound.  Note: all fields are
      -- given names by transliteration.
      deconstructBinds :: HashMap bound (Pattern bound const free),
      -- | Whether or not the binding is strict (ie. it omits some names)
      deconstructStrict :: !Bool,
      -- | The position in source from which this originates.
      deconstructPos :: !DWARFPosition
    }
    -- | An "as" binding.  Allows part of a pattern to be bound to a
    -- name, but further deconstructed by another pattern.  For
    -- example "x as (y, z)".
  | As {
      -- | The outer name, to which the entire datatype is bound.
      asName :: !bound,
      -- | The inner binding, which further deconstructs the binding.
      asBind :: Pattern bound const free,
      -- | The position in source from which this originates.
      asPos :: !DWARFPosition
    }
    -- | A simple name binding.  This does the same thing as an as
    -- pattern, but does not further deconstruct the binding.
    --
    -- In the intended use case, we have a special symbol representing
    -- a wildcard (namely, the unused symbol), so we don't bother
    -- defining another constructor for it.
  | Name {
      -- | The bound variable type being bound.
      nameSym :: !bound,
      -- | The position in source from which this originates.
      namePos :: !DWARFPosition
    }
    -- | A constant.  Constrains the binding to the given value.
  | Constant (const free)

-- | A case.  Consists of a pattern and a body wrapped in a scope.
-- Used to describe functions and computations with parameters.
data Case bound free =
  Case {
    -- | the pattern for this case.
    casePat :: Pattern bound (Term bound) free,
    -- | The body of the case.
    caseBody :: Scope bound (Term bound) free,
    -- | The position in source from which this originates.
    casePos :: !DWARFPosition
  }

-- | An element.  This is either an argument in a product type or a
-- field in a sum type.  The pattern introduces variables in
-- subsequent elements.
data Element bound free =
  Element {
    -- | The name of the element.
    elemName :: !bound,
    -- | The binding pattern of the element.
    elemPat :: Pattern bound (Term bound) free,
    -- | The type of the element.
    elemType :: Scope bound (Term bound) free,
    -- | The position in source from which this originates.
    elemPos :: !DWARFPosition
  }

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
data Term bound free =
  -- Types.  These do not support decidable equality.  As such, they
  -- cannot be the result of a computation, and cannot appear in
  -- patterns.

  -- | Dependent product type.  This is the type given to functions.
    FuncType {
      -- | The binding order for arguments.  This is used to determine
      -- the order in which to evaluate scopes.
      funcTypeArgs :: [Element bound free],
      -- | The return type of the function, which can reference the
      -- value of any argument by their binding name.
      funcTypeRetTy :: Scope bound (Term bound) free,
      -- | The position in source from which this originates.
      funcTypePos :: !DWARFPosition
    }
  -- | Dependent sum type.  This is the type given to structures.
  | RecordType {
      -- | The remaining elements of the sum type.  The first element
      -- in the binding order is a degenerate scope; the remaining
      -- scopes may reference any previous elements from the binding
      -- order, but not themselves or any future scopes.
      --
      -- Note: all fields are given names by transliteration.
      recTypeBody :: [Element bound free],
      -- | The position in source from which this originates.
      recTypePos :: !DWARFPosition
    }
  -- | Refinement type.  This type represents all members of a type
  -- satisfying a given proposition.
  | RefineType {
      -- | The base type.
      refineType :: Term bound free,
      -- | Binding patterns and propositions for values of the base
      -- type.  These express the constraints on the base type.
      refineCases :: [Case bound free],
      -- | The position in source from which this originates.
      refinePos :: !DWARFPosition
    }
  -- | Computation type.  This type represents a computation, and
  -- includes both its result type and a specification of its
  -- behavior.
  | CompType {
      -- | The result type of the computation.
      compType :: Term bound free,
      -- | Binding patterns and behavior specifications for values of
      -- the return type.  These express the constraints on the base
      -- type.
      compCases :: [Case bound free],
      -- | The position in source from which this originates.
      compTypePos :: !DWARFPosition
    }

  -- Propositions.  These do not support decidable equality.  As such,
  -- they cannot be the result of a computation, and cannot appear in
  -- a pattern.

  -- | Universal quantifier proposition.
  | Quantified {
      -- | The kind of quantification this represents (forall/exists).
      quantKind :: !Quantifier,
      -- | The type of the quantifier variable.  If a sum type is
      -- used, this will be treated similarly to a multi-argument
      -- function.
      quantType :: Term bound free,
      -- | A case statement which denotes a proposition.
      quantCases :: [Case bound free],
      -- | The position in source from which this originates.
      quantPos :: !DWARFPosition
    }

  -- Elimination Terms.  These terms generate a type in type checking.

  -- | Call term.  Represents a call to a function.  The type of the
  -- term comes from the called function's return type.
  --
  -- As with structures, calls can be named or ordered in surface
  -- syntax.  Also similar to structures, ordered calls are
  -- transliterated into named calls with parameter names "1", "2",
  -- and so on.
  | Call {
      -- | The arguments to the call.  These are introduction terms.
      callArgs :: HashMap bound (Term bound free),
      -- | The function being called.  This must be an elimination
      -- term.
      callFunc :: Term bound free,
      -- | The position in source from which this originates.
      callPos :: !DWARFPosition
    }
  -- | A typed term.  This is an introduction term with an explicit
  -- type tag, which makes it an elimination term.
  | Typed {
      -- | The introduction term being typed.
      typedTerm :: Term bound free,
      -- | The type of the introduction term.
      typedType :: Term bound free,
      -- | The position in source from which this originates.
      typedPos :: !DWARFPosition
    }
  -- | A variable symbol.  Since we know the types of all variables,
  -- this is an elimination term.
  | Var {
      -- | The underlying symbol.
      varSym :: !free,
      -- | The position in source from which this originates.
      varPos :: !DWARFPosition
    }

  -- Introduction Terms.  These terms require a type in type checking.

  -- | An eta expansion.  This is present for type checking only.
  -- This represents a "frozen" substitution.
  --
  -- XXX Quite possibly this will be removed
  | Eta {
      etaTerm :: Term bound free,
      etaType :: Term bound free,
      -- | The position in source from which this originates.
      etaPos :: !DWARFPosition
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
      lambdaPos :: !DWARFPosition
    }
  -- | A structure.  Structures can be named or ordered in the surface
  -- syntax.  Ordered structures are transliterated into named
  -- structures, with the fields "1", "2", and so on.
  | Record {
      -- | The bindings for this record.  These are introduction terms.
      recFields :: HashMap bound (Term bound free),
      -- | The position in source from which this originates.
      recPos :: !DWARFPosition
    }
  -- | A collection of one or more terms, each of which is
  -- bound to a name.  Each of the members of the group may reference
  -- eachother.
  | Fix {
      fixTerms :: HashMap bound (Scope bound (Term bound) free),
      -- | The position in source from which this originates.
      fixPos :: !DWARFPosition
    }
  -- | A computation value.  This is essentially a "frozen"
  -- computation.
  | Comp {
      compBody :: Comp bound free,
      -- | The position in source from which this originates.
      compPos :: !DWARFPosition
    }
  -- | Placeholder for a malformed term, allowing type checking to
  -- continue in spite of errors.
  | BadTerm !DWARFPosition

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
      valTerm :: Term bound free,
      -- | The position in source from which this originates.
      valPos :: !DWARFPosition
    }
  -- | Evaluate a computation value.  This allows execution of
  -- computations produced by terms.
  | Eval {
      -- | The computation value to evaluate
      evalTerm :: Term bound free,
      -- | The position in source from which this originates.
      evalPos :: !DWARFPosition
    }
  -- | Placeholder for a malformed command, allowing type checking to
  -- continue in spite of errors.
  | BadCmd !DWARFPosition

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
      seqPat :: Pattern bound (Term bound) free,
      -- | The type being bound.
      seqType :: Term bound free,
      -- | The command to execute.
      seqCmd :: Cmd bound free,
      -- | The next computation to execute.
      seqNext :: Scope bound (Comp bound) free,
      -- | The position in source from which this originates.
      seqPos :: !DWARFPosition
    }
  -- | Result of a computation. This is always the end of a sequence.
  | End {
      -- | The command to run to produce a result.
      endCmd :: Cmd bound free,
      -- | The position in source from which this originates.
      endPos :: !DWARFPosition
    }
  -- | Placeholder for a malformed computation, allowing type checking
  -- to continue in spite of errors.
  | BadComp !DWARFPosition

instance (Eq b, Eq1 t) => Eq1 (Pattern b t) where
  Deconstruct { deconstructBinds = binds1, deconstructStrict = strict1,
                deconstructConstructor = constructor1 } ==#
    Deconstruct { deconstructBinds = binds2, deconstructStrict = strict2,
                  deconstructConstructor = constructor2 } =
      (strict1 == strict2) && (constructor1 == constructor2) &&
      (binds1 == binds2)
  As { asName = name1, asBind = bind1 } ==#
    As { asName = name2, asBind = bind2 } =
      (name1 == name2) && (bind1 ==# bind2)
  Name { nameSym = name1 } ==# Name { nameSym = name2 } = name1 == name2
  Constant term1 ==# Constant term2 = term1 ==# term2
  _ ==# _ = False

instance Eq b => Eq1 (Case b) where
  Case { casePat = pat1, caseBody = body1 } ==#
    Case { casePat = pat2, caseBody = body2 } =
    pat1 ==# pat2 && body1 ==# body2

instance Eq b => Eq1 (Element b) where
  Element { elemType = ty1, elemPat = pat1, elemName = name1 } ==#
    Element { elemType = ty2, elemPat = pat2, elemName = name2 } =
    name1 == name2 && ty1 ==# ty2 && pat1 ==# pat2

instance Eq b => Eq1 (Term b) where
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
  Quantified { quantKind = kind1, quantType = ty1, quantCases = cases1 } ==#
    Quantified { quantKind = kind2, quantType = ty2, quantCases = cases2 } =
      kind1 == kind2 && ty1 ==# ty2 && cases1 ==# cases2
  Call { callArgs = args1, callFunc = func1 } ==#
    Call { callArgs = args2, callFunc = func2 } =
      args1 == args2 && func1 ==# func2
  Var { varSym = sym1 } ==# Var { varSym = sym2 } = sym1 == sym2
  Typed { typedTerm = term1, typedType = ty1 } ==#
    Typed { typedTerm = term2, typedType = ty2 } =
      term1 ==# term2 && ty1 ==# ty2
  Eta { etaTerm = term1, etaType = ty1 } ==#
    Eta { etaTerm = term2, etaType = ty2 } =
      term1 ==# term2 && ty1 ==# ty2
  Lambda { lambdaCases = cases1 } ==# Lambda { lambdaCases = cases2 } =
    cases1 == cases2
  Record { recFields = vals1 } ==# Record { recFields = vals2 } = vals1 == vals2
  Fix { fixTerms = terms1 } ==# Fix { fixTerms = terms2 } = terms1 == terms2
  Comp { compBody = body1 } ==# Comp { compBody = body2 } = body1 ==# body2
  BadTerm _ ==# BadTerm _ = True
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
  BadComp _ ==# BadComp _ = True
  _ ==# _ = False

instance (Eq b, Eq s, Eq1 t) => Eq (Pattern b t s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Case b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Element b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Term b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Cmd b s) where (==) = (==#)
instance (Eq b, Eq s) => Eq (Comp b s) where (==) = (==#)

keyOrd :: Ord a => (a, b) -> (a, b) -> Ordering
keyOrd (a1, _) (a2, _) = compare a1 a2

instance (Ord b, Ord1 t) => Ord1 (Pattern b t) where
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
  compare1 As { asName = name1, asBind = bind1 }
           As { asName = name2, asBind = bind2 } =
    case compare name1 name2 of
      EQ -> compare1 bind1 bind2
      out -> out
  compare1 As {} _ = GT
  compare1 _ As {} = LT
  compare1 Name { nameSym = name1 } Name { nameSym = name2 } =
    compare name1 name2
  compare1 Name {} _ = GT
  compare1 _ Name {} = LT
  compare1 (Constant term1) (Constant term2) = compare1 term1 term2

instance Ord b => Ord1 (Case b) where
  compare1 Case { casePat = pat1, caseBody = body1 }
           Case { casePat = pat2, caseBody = body2 } =
    case compare1 pat1 pat2 of
      EQ -> compare body1 body2
      out -> out

instance Ord b => Ord1 (Element b) where
  compare1 Element { elemName = name1, elemPat = pat1, elemType = ty1 }
           Element { elemName = name2, elemPat = pat2, elemType = ty2 } =
    case compare name1 name2 of
      EQ -> case compare1 ty1 ty2 of
        EQ -> compare1 pat1 pat2
        out -> out
      out -> out

instance Ord b => Ord1 (Term b) where
  compare1 FuncType { funcTypeArgs = argtys1, funcTypeRetTy = retty1 }
           FuncType { funcTypeArgs = argtys2, funcTypeRetTy = retty2 } =
    case compare1 retty1 retty2 of
      EQ -> compare1 argtys1 argtys2
      out -> out
  compare1 FuncType {} _ = GT
  compare1 _ FuncType {} = LT
  compare1 RecordType { recTypeBody = body1 }
    RecordType { recTypeBody = body2 } =
    compare1 body1 body2
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
  compare1 Call { callArgs = args1, callFunc = func1 }
           Call { callArgs = args2, callFunc = func2 } =
    case compare1 func1 func2 of
      EQ -> compare (sortBy keyOrd (HashMap.toList args1))
                    (sortBy keyOrd (HashMap.toList args2))
      out -> out
  compare1 Call {} _ = GT
  compare1 _ Call {} = LT
  compare1 Var { varSym = sym1 } Var { varSym = sym2 } = compare sym1 sym2
  compare1 Var {} _ = GT
  compare1 _ Var {} = LT
  compare1 Typed { typedTerm = term1, typedType = ty1 }
           Typed { typedTerm = term2, typedType = ty2 } =
    case compare1 term1 term2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 Typed {} _ = GT
  compare1 _ Typed {} = LT
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
  compare1 Fix { fixTerms = terms1 } Fix { fixTerms = terms2 } =
    compare (sortBy keyOrd (HashMap.toList terms1))
            (sortBy keyOrd (HashMap.toList terms2))
  compare1 Fix {} _ = GT
  compare1 _ Fix {} = LT
  compare1 Comp { compBody = body1 } Comp { compBody = body2 } =
    compare1 body1 body2
  compare1 Comp {} _ = GT
  compare1 _ Comp {} = LT
  compare1 (BadTerm _) (BadTerm _) = EQ

instance Ord b => Ord1 (Cmd b) where
  compare1 Value { valTerm = term1 } Value { valTerm = term2 } =
    compare1 term1 term2
  compare1 Value {} _ = GT
  compare1 _ Value {} = LT
  compare1 Eval { evalTerm = term1 } Eval { evalTerm = term2 } =
    compare1 term1 term2
  compare1 Eval {} _ = GT
  compare1 _ Eval {} = LT
  compare1 (BadCmd _) (BadCmd _) = EQ

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
  compare1 (BadComp _) (BadComp _) = EQ

instance (Ord b, Ord s, Ord1 t) => Ord (Pattern b t s) where
  compare = compare1
instance (Ord b, Ord s) => Ord (Case b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Element b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Term b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Cmd b s) where compare = compare1
instance (Ord b, Ord s) => Ord (Comp b s) where compare = compare1

instance (Hashable b, Hashable1 t, Ord b) =>
         Hashable1 (Pattern b t) where
  hashWithSalt1 s Deconstruct { deconstructConstructor = constructor,
                                deconstructBinds = binds,
                                deconstructStrict = strict } =
    (s `hashWithSalt` (1 :: Int) `hashWithSalt` constructor `hashWithSalt`
     strict) `hashWithSalt1` sortBy keyOrd (HashMap.toList binds)
  hashWithSalt1 s As { asName = name, asBind = bind } =
    (s `hashWithSalt` (2 :: Int) `hashWithSalt` name) `hashWithSalt1` bind
  hashWithSalt1 s Name { nameSym = name } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` name
  hashWithSalt1 s (Constant c) = s `hashWithSalt` (4 :: Int) `hashWithSalt1` c

instance (Hashable b, Ord b) => Hashable1 (Case b) where
  hashWithSalt1 s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt1` pat `hashWithSalt1` body

instance (Hashable b, Ord b) => Hashable1 (Element b) where
  hashWithSalt1 s Element { elemName = name, elemPat = pat, elemType = ty } =
    (s `hashWithSalt` name) `hashWithSalt1` pat `hashWithSalt1` ty

instance (Hashable b, Ord b) => Hashable1 (Term b) where
  hashWithSalt1 s FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` argtys `hashWithSalt` retty
  hashWithSalt1 s RecordType { recTypeBody = body } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` body
  hashWithSalt1 s RefineType { refineType = ty, refineCases = cases } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s CompType { compType = ty, compCases = cases } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Quantified { quantKind = Forall, quantType = ty,
                               quantCases = cases } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Quantified { quantKind = Exists, quantType = ty,
                               quantCases = cases } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt1` ty `hashWithSalt1` cases
  hashWithSalt1 s Call { callArgs = args, callFunc = func } =
    (s `hashWithSalt` (7 :: Int) `hashWithSalt`
     sortBy keyOrd (HashMap.toList args)) `hashWithSalt1` func
  hashWithSalt1 s Var { varSym = sym } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` sym
  hashWithSalt1 s Typed { typedTerm = term, typedType = ty } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Eta { etaTerm = term, etaType = ty } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Lambda { lambdaCases = cases } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt1` cases
  hashWithSalt1 s Record { recFields = vals } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt1`
    sortBy keyOrd (HashMap.toList vals)
  hashWithSalt1 s Fix { fixTerms = terms } =
    s `hashWithSalt` (13 :: Int) `hashWithSalt1`
    sortBy keyOrd (HashMap.toList terms)
  hashWithSalt1 s Comp { compBody = body } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1` body
  hashWithSalt1 s (BadTerm _) = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable1 (Cmd b) where
  hashWithSalt1 s Value { valTerm = term } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1` term
  hashWithSalt1 s Eval { evalTerm = term } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` term
  hashWithSalt1 s (BadCmd _) = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Ord b) => Hashable1 (Comp b) where
  hashWithSalt1 s Seq { seqCmd = cmd, seqNext = next,
                        seqType = ty, seqPat = pat } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1` ty `hashWithSalt1`
    pat `hashWithSalt1` cmd `hashWithSalt1` next
  hashWithSalt1 s End { endCmd = cmd } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` cmd
  hashWithSalt1 s (BadComp _) = s `hashWithSalt` (0 :: Int)

instance (Hashable b, Hashable1 t, Hashable a, Ord b) =>
         Hashable (Pattern b t a) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Case b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Element b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Term b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Cmd b s) where
  hashWithSalt = hashWithSalt1

instance (Hashable b, Hashable s, Ord b) => Hashable (Comp b s) where
  hashWithSalt = hashWithSalt1

instance Functor t => Functor (Pattern b t) where
  fmap f b @ Deconstruct { deconstructBinds = binds } =
    b { deconstructBinds = fmap (fmap f) binds }
  fmap f b @ As { asBind = bind } = b { asBind = fmap f bind }
  fmap _ b @ Name { nameSym = name } = b { nameSym = name }
  fmap f (Constant t) = Constant (fmap f t)

instance Functor (Case b) where
  fmap f c @ Case { casePat = pat, caseBody = body } =
    c { casePat = fmap f pat, caseBody = fmap f body }

instance Functor (Element b) where
  fmap f e @ Element { elemPat = pat, elemType = ty } =
    e { elemPat = fmap f pat, elemType = fmap f ty }

instance Functor (Term b) where
  fmap f t @ FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
    t { funcTypeArgs = fmap (fmap f) argtys, funcTypeRetTy = fmap f retty }
  fmap f t @ RecordType { recTypeBody = body } =
    t { recTypeBody = fmap (fmap f) body }
  fmap f t @ RefineType { refineType = ty, refineCases = cases } =
    t { refineType = fmap f ty, refineCases = fmap (fmap f) cases }
  fmap f t @ CompType { compType = ty, compCases = cases } =
    t { compType = fmap f ty, compCases = fmap (fmap f) cases }
  fmap f t @ Quantified { quantType = ty, quantCases = cases } =
    t { quantType = fmap f ty, quantCases = fmap (fmap f) cases }
  fmap f t @ Call { callArgs = args, callFunc = func } =
    t { callArgs = fmap (fmap f) args, callFunc = fmap f func }
  fmap f t @ Var { varSym = sym } = t { varSym = f sym }
  fmap f t @ Typed { typedTerm = term, typedType = ty } =
    t { typedTerm = fmap f term, typedType = fmap f ty }
  fmap f t @ Eta { etaTerm = term, etaType = ty } =
    t { etaTerm = fmap f term, etaType = fmap f ty }
  fmap f t @ Lambda { lambdaCases = cases } =
    t { lambdaCases = fmap (fmap f) cases }
  fmap f t @ Record { recFields = vals } = t { recFields = fmap (fmap f) vals }
  fmap f t @ Fix { fixTerms = terms } = t { fixTerms = fmap (fmap f) terms }
  fmap f t @ Comp { compBody = body } = t { compBody = fmap f body }
  fmap _ (BadTerm p) = BadTerm p

instance Functor (Cmd b) where
  fmap f c @ Eval { evalTerm = term } = c { evalTerm = fmap f term }
  fmap f c @ Value { valTerm = term } = c { valTerm = fmap f term }
  fmap _ (BadCmd p) = BadCmd p

instance Functor (Comp b) where
  fmap f c @ Seq { seqType = ty, seqPat = pat, seqCmd = cmd, seqNext = next } =
    c { seqCmd = fmap f cmd, seqNext = fmap f next,
        seqType = fmap f ty, seqPat = fmap f pat }
  fmap f c @ End { endCmd = cmd } = c { endCmd = fmap f cmd }
  fmap _ (BadComp p) = BadComp p

instance Foldable t => Foldable (Pattern b t) where
  foldMap f Deconstruct { deconstructBinds = binds } = foldMap (foldMap f) binds
  foldMap f As { asBind = bind } = foldMap f bind
  foldMap f (Constant t) = foldMap f t
  foldMap _ Name {} = mempty

instance Foldable (Case b) where
  foldMap f Case { casePat = pat, caseBody = body } =
    foldMap f pat `mappend` foldMap f body

instance Foldable (Element b) where
  foldMap f Element { elemPat = pat, elemType = ty } =
    foldMap f pat `mappend` foldMap f ty

instance Foldable (Term b) where
  foldMap f FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
    foldMap (foldMap f) argtys `mappend` foldMap f retty
  foldMap f RecordType { recTypeBody = body } = foldMap (foldMap f) body
  foldMap f RefineType { refineType = ty, refineCases = cases } =
    foldMap f ty `mappend` foldMap (foldMap f) cases
  foldMap f CompType { compType = ty, compCases = cases } =
    foldMap f ty `mappend` foldMap (foldMap f) cases
  foldMap f Quantified { quantType = ty, quantCases = cases } =
    foldMap f ty `mappend` foldMap (foldMap f) cases
  foldMap f Call { callArgs = args, callFunc = func } =
    foldMap (foldMap f) args `mappend` foldMap f func
  foldMap f Var { varSym = sym } = f sym
  foldMap f Typed { typedTerm = term, typedType = ty } =
    foldMap f term `mappend` foldMap f ty
  foldMap f Eta { etaTerm = term, etaType = ty } =
    foldMap f term `mappend` foldMap f ty
  foldMap f Lambda { lambdaCases = cases } = foldMap (foldMap f) cases
  foldMap f Record { recFields = vals } = foldMap (foldMap f) vals
  foldMap f Fix { fixTerms = terms } = foldMap (foldMap f) terms
  foldMap f Comp { compBody = body } = foldMap f body
  foldMap _ (BadTerm _) = mempty

instance Foldable (Cmd b) where
  foldMap f Value { valTerm = term } = foldMap f term
  foldMap f Eval { evalTerm = term } = foldMap f term
  foldMap _ (BadCmd _) = mempty

instance Foldable (Comp b) where
  foldMap f Seq { seqType = ty, seqPat = pat, seqCmd = cmd, seqNext = next } =
    foldMap f ty `mappend` foldMap f pat `mappend`
    foldMap f cmd `mappend` foldMap f next
  foldMap f End { endCmd = cmd } = foldMap f cmd
  foldMap _ (BadComp _) = mempty

instance Traversable t => Traversable (Pattern b t) where
  traverse f b @ Deconstruct { deconstructBinds = binds } =
    (\binds' -> b { deconstructBinds = binds' }) <$>
    traverse (traverse f) binds
  traverse _ b @ Name { nameSym = name } = pure (b { nameSym = name })
  traverse f b @ As { asBind = bind } =
    (\bind' -> b { asBind = bind' }) <$> traverse f bind
  traverse f (Constant t) = Constant <$> traverse f t

instance Traversable (Case b) where
  traverse f c @ Case { casePat = pat, caseBody = body } =
    (\pat' body' -> c { casePat = pat', caseBody = body' }) <$>
      traverse f pat <*> traverse f body

instance Traversable (Element b) where
  traverse f c @ Element { elemPat = pat, elemType = ty } =
    (\pat' ty' -> c { elemPat = pat', elemType = ty' }) <$>
      traverse f pat <*> traverse f ty

instance Traversable (Term b) where
  traverse f t @ FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } =
    (\argtys' retty' -> t { funcTypeArgs = argtys', funcTypeRetTy = retty' }) <$>
      traverse (traverse f) argtys <*> traverse f retty
  traverse f t @ RecordType { recTypeBody = body } =
    (\body' -> t { recTypeBody = body' }) <$> traverse (traverse f) body
  traverse f t @ RefineType { refineType = ty, refineCases = cases } =
    (\ty' cases' -> t { refineType = ty', refineCases = cases' }) <$>
      traverse f ty <*> traverse (traverse f) cases
  traverse f t @ CompType { compType = ty, compCases = cases } =
    (\ty' cases' -> t { compType = ty', compCases = cases' }) <$>
      traverse f ty <*> traverse (traverse f) cases
  traverse f t @ Quantified { quantType = ty, quantCases = cases } =
    (\ty' cases' -> t { quantType = ty', quantCases = cases' }) <$>
      traverse f ty <*> traverse (traverse f) cases
  traverse f t @ Call { callArgs = args, callFunc = func } =
    (\args' func' -> t { callArgs = args', callFunc = func' }) <$>
      traverse (traverse f) args <*> traverse f func
  traverse f t @ Var { varSym = sym } =
    (\sym' -> t { varSym = sym' }) <$> f sym
  traverse f t @ Typed { typedTerm = term, typedType = ty } =
    (\term' ty' -> t { typedTerm = term', typedType = ty' }) <$>
      traverse f term <*> traverse f ty
  traverse f t @ Eta { etaTerm = term, etaType = ty } =
    (\term' ty' -> t { etaTerm = term', etaType = ty' }) <$>
      traverse f term <*> traverse f ty
  traverse f t @ Lambda { lambdaCases = cases } =
    (\cases' -> t { lambdaCases = cases' }) <$> traverse (traverse f) cases
  traverse f t @ Record { recFields = vals } =
    (\vals' -> t { recFields = vals' }) <$> traverse (traverse f) vals
  traverse f t @ Fix { fixTerms = terms } =
    (\terms' -> t { fixTerms = terms' }) <$> traverse (traverse f) terms
  traverse f c @ Comp { compBody = body } =
    (\body' -> c { compBody = body' }) <$> traverse f body
  traverse _ (BadTerm p) = pure (BadTerm p)

instance Traversable (Cmd b) where
  traverse f c @ Value { valTerm = term } =
    (\term' -> c { valTerm = term' }) <$> traverse f term
  traverse f c @ Eval { evalTerm = term } =
    (\term' -> c { evalTerm = term' }) <$> traverse f term
  traverse _ (BadCmd c) = pure (BadCmd c)

instance Traversable (Comp b) where
  traverse f c @ Seq { seqCmd = cmd, seqNext = next,
                       seqType = ty, seqPat = pat } =
    (\ty' pat' cmd' next' -> c { seqCmd = cmd', seqNext = next',
                                 seqType = ty', seqPat = pat' }) <$>
      traverse f ty <*> traverse f pat <*> traverse f cmd <*> traverse f next
  traverse f c @ End { endCmd = cmd } =
    (\cmd' -> c { endCmd = cmd' }) <$> traverse f cmd
  traverse _ (BadComp p) = pure (BadComp p)

injectpos :: DWARFPosition
injectpos = Synthetic { synthDesc = Lazy.fromString "Monad return" }

instance MonadTrans (Pattern b) where
  lift = Constant

instance Bound (Pattern b) where
  b @ Deconstruct { deconstructBinds = binds } >>>= f =
    b { deconstructBinds = fmap (>>>= f) binds }
  b @ As { asBind = bind } >>>= f = b { asBind = bind >>>= f }
  b @ Name { nameSym = name } >>>= _ = b { nameSym = name }
  Constant t >>>= f = Constant (t >>= f)

instance Applicative (Term b) where
  pure = return
  (<*>) = ap

instance Applicative (Comp b) where
  pure = return
  (<*>) = ap

caseSubstTerm :: (a -> Term c b) -> Case c a -> Case c b
caseSubstTerm f c @ Case { casePat = pat, caseBody = body } =
  c { casePat = pat >>>= f, caseBody = body >>>= f }

elementSubstTerm :: (a -> Term c b) -> Element c a -> Element c b
elementSubstTerm f e @ Element { elemPat = pat, elemType = ty } =
  e { elemPat = pat >>>= f, elemType = ty >>>= f }

cmdSubstTerm :: (a -> Term c b) -> Cmd c a -> Cmd c b
cmdSubstTerm f c @ Value { valTerm = term } = c { valTerm = term >>= f }
cmdSubstTerm f c @ Eval { evalTerm = term } = c { evalTerm = term >>= f }
cmdSubstTerm _ (BadCmd p) = BadCmd p

compSubstTerm :: (a -> Term c b) -> Comp c a -> Comp c b
compSubstTerm f c @ Seq { seqCmd = cmd, seqNext = next,
                          seqType = ty, seqPat = pat } =
  c { seqType = ty >>= f, seqPat = pat >>>= f, seqCmd = cmdSubstTerm f cmd,
      seqNext = next >>>= compSubstTerm f . return }
compSubstTerm f c @ End { endCmd = cmd } =
  c { endCmd = cmdSubstTerm f cmd }
compSubstTerm _ (BadComp p) = BadComp p

instance Monad (Term b) where
  return sym = Var { varSym = sym, varPos = injectpos }

  t @ FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty } >>= f =
    t { funcTypeArgs = fmap (elementSubstTerm f) argtys,
        funcTypeRetTy = retty >>>= f }
  t @ RecordType { recTypeBody = body } >>= f =
    t { recTypeBody = fmap (elementSubstTerm f) body }
  t @ RefineType { refineType = ty, refineCases = cases } >>= f =
    t { refineType = ty >>= f, refineCases = fmap (caseSubstTerm f) cases }
  t @ CompType { compType = ty, compCases = cases } >>= f =
    t { compType = ty >>= f, compCases = fmap (caseSubstTerm f) cases }
  t @ Quantified { quantType = ty, quantCases = cases } >>= f =
    t { quantCases = fmap (caseSubstTerm f) cases, quantType = ty >>= f }
  t @ Call { callArgs = args, callFunc = func } >>= f =
    t { callArgs = fmap (>>= f) args, callFunc = func >>= f }
  Var { varSym = sym } >>= f = f sym
  t @ Typed { typedTerm = term, typedType = ty } >>= f =
    t { typedTerm = term >>= f, typedType = ty >>= f }
  t @ Lambda { lambdaCases = cases } >>= f =
    t { lambdaCases = fmap (caseSubstTerm f) cases }
  t @ Record { recFields = vals } >>= f = t { recFields = fmap (>>= f) vals }
  t @ Fix { fixTerms = terms } >>= f = t { fixTerms = fmap (>>>= f) terms }
  t @ Comp { compBody = body } >>= f = t { compBody = compSubstTerm f body }
  t @ Eta { etaTerm = term, etaType = ty } >>= f =
    t { etaTerm = term >>= f, etaType = ty >>= f }
  BadTerm p >>= _ = BadTerm p

termSubstComp :: (a -> Comp c b) -> a -> Term c b
termSubstComp f sym =
  case f sym of
    End { endCmd = Eval { evalTerm = term } } -> term
    End { endCmd = Value { valTerm = term } } -> term
    End { endCmd = BadCmd p } -> BadTerm p
    body @ Seq { seqPos = pos } -> Comp { compBody = body, compPos = pos }
    BadComp p -> BadTerm p

cmdSubstComp :: (a -> Comp c b) -> Cmd c a -> Cmd c b
cmdSubstComp f c @ Value { valTerm = term } =
  c { valTerm = term >>= termSubstComp f }
cmdSubstComp f c @ Eval { evalTerm = term } =
  c { evalTerm = term >>= termSubstComp f }
cmdSubstComp _ (BadCmd p) = BadCmd p

instance Monad (Comp b) where
  return sym =
    End { endCmd = Eval { evalTerm = Var { varSym = sym, varPos = injectpos },
                          evalPos = injectpos },
          endPos = injectpos }

  c @ Seq { seqType = ty, seqPat = pat, seqCmd = cmd, seqNext = next } >>= f =
    c { seqType = ty >>= termSubstComp f, seqNext = next >>>= f,
        seqPat = pat >>>= termSubstComp f, seqCmd = cmdSubstComp f cmd }
  c @ End { endCmd = cmd } >>= f = c { endCmd = cmdSubstComp f cmd }
  BadComp p >>= _ = BadComp p


{-
formatBind :: (Default b, Ord b, Eq b, Format b, Format s, Format (t s)) =>
              (b, t s) -> Doc
formatBind (name, bind) = name <+> equals <+> bind

instance Format Quantifier where
  format Forall = format "Forall"
  format Exists = format "Exists"

instance (Default b, Ord b, Eq b, Format b, Format s, Format (t s)) =>
         Format (Pattern b t s) where
  format Deconstruct { deconstructConstructor = constructor,
                       deconstructBinds = binds,
                       deconstructStrict = strict } =
    braceBlock "Deconstruct" [
        "constructor" <+> equals <+> constructor,
        "strict" <+> equals <+> strict,
        "binds" <+> equals <+>
        headlessBracketList (map formatBind (Map.assocs binds))
      ]
  format As { asBind = bind, asName = name } =
    braceBlock "As" [
        "bind" <+> equals <+> bind,
        "name" <+> equals <+> name
      ]
  format Name { nameSym = sym } =
    braceBlock "Name" [ "sym" <+> equals <+> sym ]
  format (Constant t) =   block 2 ("val" <+> lparen) (format t) rparen

instance (Default b, Ord b, Eq b, Format b, Format s) => Format (Case b s) where
  format Case { casePat = pat, caseBody = body } =
    braceBlock "Case" [
        "pat" <+> equals <+> pat,
        "body" <+> equals <+> body
      ]

instance (Default b, Ord b, Eq b, Format b, Format s) =>
         Format (Element b s) where
  format Element { elemPat = pat, elemType = ty, elemName = name } =
    braceBlock "Element" [
        "name" <+> equals <+> name,
        "pat" <+> equals <+> pat,
        "type" <+> equals <+> ty
      ]

instance (Default b, Ord b, Eq b, Format b, Format s) => Format (Term b s) where
  format FuncType { funcTypeArgs = args, funcTypeRetTy = ret } =
    braceBlock "FuncType" [
        "args" <+> equals <+> headlessBracketList args,
        "ret" <+> equals <+> ret
      ]
  format RecordType { recTypeBody = body } =
    braceBlock "RecordType" [ "body" <+> equals <+> headlessBracketList body ]
  format RefineType { refineType = ty, refineCases = cases } =
    braceBlock "RefineType" [
        "type" <+> equals <+> ty,
        "cases" <+> equals <+> headlessBracketList cases
      ]
  format CompType { compType = ty, compPat = pat, compSpec = spec } =
    braceBlock "CompType" [
        "type" <+> equals <+> ty,
        "pat" <+> equals <+> pat,
        "spec" <+> equals <+> spec
      ]
  format Quantified { quantKind = kind, quantType = ty, quantCases = cases } =
    braceBlock kind [
        "type" <+> equals <+> ty,
        "cases" <+> equals <+> headlessBracketList cases
      ]
  format Call { callFunc = func, callArgs = args } =
    braceBlock "Call" [
        "func" <+> equals <+> func,
        "args" <+> equals <+>
        headlessBracketList (map formatBind (Map.assocs args))
      ]
  format Var { varSym = sym } =
    braceBlock "Var" [ "name" <+> equals <+> format sym ]
  format Typed { typedTerm = term, typedType = ty } =
    braceBlock "Typed" [
        "term" <+> equals <+> term,
        "type" <+> equals <+> ty
      ]
  format Lambda { lambdaCases = cases } =
    braceBlock "Lambda" [ "cases" <+> equals <+> headlessBracketList cases ]
  format Record { recFields = vals } =
    bracketList "Record" (map formatBind (Map.assocs vals))
  format Fix { fixTerms = terms } =
    bracketList "Fix" (map formatBind (Map.assocs terms))
  format Comp { compBody = body } =
    braceBlock "Comp" ["compBody" <+> equals <+> body]
  format Eta { etaType = ty, etaTerm = term } =
    hang ("eta" <+> lparen <> term <> rparen <+> colon) 2
      (lparen <> ty <> rparen)
  format (BadTerm _) = format "BadTerm"

instance (Default b, Ord b, Eq b, Format b, Format s) => Format (Cmd b s) where
  format Value { valTerm = term } =
    braceBlock "Value" ["term" <+> equals <+> term]
  format Eval { evalTerm = term } =
    braceBlock "Eval" ["term" <+> equals <+> term]
  format (BadCmd _) = format "BadCmd"

instance (Default b, Ord b, Eq b, Format b, Format s) =>
         Format (Comp b s) where
  format Seq { seqType = ty, seqPat = pat, seqCmd = cmd, seqNext = next } =
    braceBlock "Seq" [
        "type" <+> equals <+> ty,
        "pat" <+> equals <+> pat,
        "cmd" <+> equals <+> cmd,
        "next" <+> equals <+> next
      ]
  format End { endCmd = cmd } =
    braceBlock "End" [ "cmd" <+> equals <+> format cmd ]
  format (BadComp _) = format "BadComp"

instance Show Quantifier where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s, Format (t s)) =>
         Show (Pattern b t s) where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s) => Show (Case b s) where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s) =>
         Show (Element b s) where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s) => Show (Term b s) where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s) => Show (Cmd b s) where
  show = show . format

instance (Default b, Ord b, Eq b, Format b, Format s) =>
         Show (Comp b s) where
  show = show . format

boundPicklerAttr :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text,
                     XmlPickler [(tag, text)] bound,
                     XmlPickler [(tag, text)] free) =>
                    PU [NodeG [] tag text] (Var bound free)
boundPicklerAttr =
  let
    revfunc (B pos) = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (B, revfunc) (xpElemAttrs (gxFromString "Bound") xpickle)

freePicklerAttr :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [(tag, text)] bound,
                    XmlPickler [(tag, text)] free) =>
                   PU [NodeG [] tag text] (Var bound free)
freePicklerAttr =
  let
    revfunc (F pos) = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (F, revfunc) (xpElemAttrs (gxFromString "Free") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound, XmlPickler [(tag, text)] free) =>
          XmlPickler [NodeG [] tag text] (Var bound free) where
  xpickle =
    let
      picker B {} = 0
      picker F {} = 1
    in
      xpAlt picker [ boundPicklerAttr, freePicklerAttr ]
-}

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
                       XmlPickler [NodeG [] tag text] free,
                       XmlPickler [NodeG [] tag text] (const free),
                       Hashable bound, Eq bound) =>
             PU [NodeG [] tag text] (Pattern bound const free)
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
              XmlPickler [NodeG [] tag text] free,
              XmlPickler [NodeG [] tag text] (const free),
              Hashable bound, Eq bound) =>
             PU [NodeG [] tag text] (Pattern bound const free)
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
                XmlPickler [NodeG [] tag text] free,
                XmlPickler [NodeG [] tag text] (const free),
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Pattern bound const free)
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
                    XmlPickler [NodeG [] tag text] free,
                    XmlPickler [NodeG [] tag text] (const free),
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Pattern bound const free)
constantPickler =
  let
    revfunc (Constant v) = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Constant, revfunc) (xpElemNodes (gxFromString "Constant") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          XmlPickler [NodeG [] tag text] (const free),
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Pattern bound const free) where
  xpickle =
    let
      picker Deconstruct {} = 0
      picker As {} = 1
      picker Name {} = 2
      picker Constant {} = 3
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
    xpWrap (\(sym, (pat, ty, pos)) -> Element { elemName = sym, elemPat = pat,
                                                elemType = ty, elemPos = pos },
            \Element { elemName = sym, elemPat = pat,
                       elemType = ty, elemPos = pos } -> (sym, (pat, ty, pos)))
           (xpElem (gxFromString "Element") xpickle
                   (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

funcTypePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [(tag, text)] bound,
                    XmlPickler [NodeG [] tag text] bound,
                    XmlPickler [NodeG [] tag text] free,
                    Hashable bound, Eq bound) =>
                   PU [NodeG [] tag text] (Term bound free)
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
                        (xpTriple (xpList (xpElemNodes (gxFromString "args")
                                                       xpickle))
                                  (xpElemNodes (gxFromString "retty") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

recordTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [(tag, text)] bound,
                      XmlPickler [NodeG [] tag text] bound,
                      XmlPickler [NodeG [] tag text] free,
                      Hashable bound, Eq bound) =>
                     PU [NodeG [] tag text] (Term bound free)
recordTypePickler =
  let
    revfunc RecordType { recTypeBody = body, recTypePos = pos } = (body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(body, pos) -> RecordType { recTypeBody = body, recTypePos = pos },
            revfunc)
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
                     PU [NodeG [] tag text] (Term bound free)
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
                   PU [NodeG [] tag text] (Term bound free)
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

quantifiedPickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [(tag, text)] bound,
                      XmlPickler [NodeG [] tag text] bound,
                      XmlPickler [NodeG [] tag text] free,
                      Hashable bound, Eq bound) =>
                     PU [NodeG [] tag text] (Term bound free)
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

callPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Term bound free)
callPickler =
  let
    revfunc Call { callFunc = func, callArgs = args,
                   callPos = pos } = (func, args, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(func, args, pos) -> Call { callFunc = func, callArgs = args,
                                         callPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "Call")
                        (xpTriple (xpElemNodes (gxFromString "func") xpickle)
                                  (xpElemNodes (gxFromString "args")
                                               (mapPickler "arg"))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

typedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [(tag, text)] bound,
                 XmlPickler [NodeG [] tag text] bound,
                 XmlPickler [NodeG [] tag text] free,
                 Hashable bound, Eq bound) =>
                PU [NodeG [] tag text] (Term bound free)
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
              PU [NodeG [] tag text] (Term bound free)
varPickler =
  let
    revfunc Var { varSym = sym, varPos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Var { varSym = sym, varPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Var")
                        (xpPair (xpElemNodes (gxFromString "sym") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

lambdaPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [(tag, text)] bound,
                  XmlPickler [NodeG [] tag text] bound,
                  XmlPickler [NodeG [] tag text] free,
                  Hashable bound, Eq bound) =>
                 PU [NodeG [] tag text] (Term bound free)
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
                 PU [NodeG [] tag text] (Term bound free)
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

fixPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [(tag, text)] bound,
               XmlPickler [NodeG [] tag text] bound,
               XmlPickler [NodeG [] tag text] free,
               Hashable bound, Eq bound) =>
              PU [NodeG [] tag text] (Term bound free)
fixPickler =
  let
    revfunc Fix { fixTerms = terms, fixPos = pos } = (terms, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(terms, pos) -> Fix { fixTerms = terms, fixPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Fix")
                        (xpPair (xpElemNodes (gxFromString "terms")
                                             (mapPickler "term"))
                                (xpElemNodes (gxFromString "pos") xpickle)))

compPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [(tag, text)] bound,
                XmlPickler [NodeG [] tag text] bound,
                XmlPickler [NodeG [] tag text] free,
                Hashable bound, Eq bound) =>
               PU [NodeG [] tag text] (Term bound free)
compPickler =
  let
    revfunc Comp { compBody = body, compPos = pos } = (body, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(body, pos) -> Comp { compBody = body, compPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Comp")
                        (xpPair (xpElemNodes (gxFromString "body") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

badTermPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] bound,
                   XmlPickler [NodeG [] tag text] free,
                   Hashable bound, Eq bound) =>
                  PU [NodeG [] tag text] (Term bound free)
badTermPickler =
  let
    revfunc (BadTerm pos) = pos
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (BadTerm, revfunc)
           (xpElemNodes (gxFromString "BadTerm")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [(tag, text)] bound,
          XmlPickler [NodeG [] tag text] bound,
          XmlPickler [NodeG [] tag text] free,
          Hashable bound, Eq bound) =>
         XmlPickler [NodeG [] tag text] (Term bound free) where
  xpickle =
    let
      picker FuncType {} = 0
      picker RecordType {} = 1
      picker RefineType {} = 2
      picker CompType {} = 3
      picker Quantified {} = 4
      picker Call {} = 5
      picker Typed {} = 6
      picker Var {} = 7
      picker Lambda {} = 8
      picker Record {} = 9
      picker Fix {} = 10
      picker Comp {} = 11
      picker BadTerm {} = 12
      picker Eta {} = error "Eta not supported"
    in
      xpAlt picker [ funcTypePickler, recordTypePickler, refineTypePickler,
                     compTypePickler, quantifiedPickler, callPickler,
                     typedPickler, varPickler, lambdaPickler, recordPickler,
                     fixPickler, compPickler, badTermPickler ]

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
