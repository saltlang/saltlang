-- Copyright (c) 2013 Eric McCorkle.
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
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- | The Salt core language.  Salt's surface syntax is transliterated
-- into Core, which is then type-checked and compiled.  Core is
-- generally not meant for human consumption.
module Language.Argent.Core.Syntax(
       Binding(..),
       Pattern(..),
       Term(..),
       Cmd(..),
       Comp(..)
       ) where

import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.Hash.ExtraInstances()
import Data.Map(Map)
import Data.Monoid
import Data.Pos
import Data.Set(Set)
import Data.Traversable
import Prelude hiding (foldr1)
import Prelude.Extras(Eq1(..), Ord1(..))
import Prelude.Extras.ExtraInstances()
import Test.QuickCheck

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A pattern binding.  Represents how to deconstruct a value and
-- bind it to variables.
data Binding b t s =
    -- | A deconstruction.  Takes a type apart.  Some types have
    -- constructors; nameless record types don't (they should use the
    -- "unused" symbol).
    Deconstruct {
      -- | The type constructor.  For nameless records, use the
      -- "unused" symbol.
      deconstructConstructor :: !b,
      -- | The fields in the record being bound.
      deconstructBinds :: Map b (Binding b t s),
      -- | Whether or not the binding is strict (ie. it omits some names)
      deconstructStrict :: !Bool,
      -- | The position in source from which this originates.
      deconstructPos :: !Pos
    }
    -- | An "as" binding.  Allows part of a pattern to be bound to a
    -- name, but further deconstructed by another pattern.  For
    -- example "x as (y, z)".
  | As {
      -- | The outer name, to which the entire datatype is bound.
      asName :: !b,
      -- | The inner binding, which further deconstructs the binding.
      asBind :: Binding b t s,
      -- | The position in source from which this originates.
      asPos :: !Pos
    }
    -- | A simple name binding.  This does the same thing as an as
    -- pattern, but does not further deconstruct the binding.
    -- 
    -- In the intended use case, we have a special symbol representing
    -- a wildcard (namely, the unused symbol), so we don't bother
    -- defining another constructor for it.
  | Name {
      -- | The bound variable type being bound.
      nameSym :: !b,
      -- | The position in source from which this originates.
      namePos :: !Pos
    }
    -- | A constant.  Constrains the binding to the given value.
  | Constant (t s)
    deriving (Ord, Eq)

-- | A pattern.  Consists of a pattern and a type.  The symbol unused
-- may occur in pattern terms, in which case it acts as a wildcard.
data Pattern b t s =
    Pattern {
      -- | The pattern for binding.  This is an introduction term.
      patternBind :: Binding b t s,
      -- | The type being bound.  This is an introduction term, which
      -- must be a type.
      patternType :: Term b s,
      -- | The position in source from which this originates.
      patternPos :: !Pos
    }
    deriving (Ord, Eq)

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
data Term b s =
  -- Types.  These do not support decidable equality.  As such, they
  -- cannot be the result of a computation, and cannot appear in
  -- patterns.

  -- | Dependent product type.  This is the type given to functions.
    ProdType {
      -- | The binding order for arguments.  This is used to determine
      -- the order in which to evaluate scopes.
      prodBindOrder :: [b],
      -- | The binding names and types of all arguments.  The first
      -- argument in the binding order is a degenerate scope; the
      -- remaining scopes may reference any previous arguments in the
      -- binding order, but not themselves or any future arguments.
      prodArgTys :: Map b (Scope b (Term b) s),
      -- | The return type of the function, which can reference the
      -- value of any argument by their binding name.
      prodRetTy :: Scope b (Term b) s,
      -- | The position in source from which this originates.
      prodTypePos :: !Pos
    }
  -- | Dependent sum type.  This is the type given to structures.
  | SumType {
      -- | The binding order for elements.  This is used to determine
      -- the order in which to evaluate the scopes.
      sumBindOrder :: [b],
      -- | The remaining elements of the sum type.  The first element
      -- in the binding order is a degenerate scope; the remaining
      -- scopes may reference any previous elements from the binding
      -- order, but not themselves or any future scopes.
      sumBody :: Map b (Scope b (Term b) s),
      -- | The position in source from which this originates.
      sumTypePos :: !Pos
    }
  -- | Refinement type.  This type represents all members of a type
  -- satisfying a given proposition.
  | RefineType {
      -- | The binder for values of the base type.
      refinePat :: Pattern b (Term b) s,
      -- | The proposition that members of this type must satisfy,
      -- which may reference bound variables from refinePat.
      refineProp :: Scope b (Term b) s,
      -- | The position in source from which this originates.
      refineTypePos :: !Pos
    }
  -- | Computation type.  This type represents a computation, and
  -- includes both its result type and a specification of its
  -- behavior.
  | CompType {
      -- | The binder for the result of this computation.
      compPat :: Pattern b (Term b) s,
      -- | The specification describing the computation's behavior.
      compSpec :: Scope b (Term b) s,
      -- | The position in source from which this originates.
      compTypePos :: !Pos
    }

  -- Propositions.  These do not support decidable equality.  As such,
  -- they cannot be the result of a computation, and cannot appear in
  -- a pattern.

  -- | Universal quantifier proposition.
  | Forall {
      -- | The bindings for the quantified proposition.  A list of
      -- (pattern, type) pairs.
      forallPats :: [Pattern b (Term b) s],
      -- | The core proposition.
      forallProp :: Scope b (Term b) s,
      -- | The position in source from which this originates.
      forallPos :: !Pos
    }
  -- | Existential quantifier proposition.
  | Exists {
      -- | The bindings for the quantified proposition.  A list of
      -- (pattern, type) pairs.
      existsPats :: [Pattern b (Term b) s],
      -- | The core proposition.
      existsProp :: Scope b (Term b) s,
      -- | The position in source from which this originates.
      existsPos :: !Pos
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
      -- This list must be sorted by the bound variable, and bound
      -- variables must be unique (in essence, this must be what you'd
      -- expect from Map.toList)
      callArgs :: Map b (Term b s),
      -- | The function being called.  This must be an elimination
      -- term.
      callFunc :: Term b s,
      -- | The position in source from which this originates.
      callPos :: !Pos
    }
  -- | A variable symbol.  Since we know the types of all variables,
  -- this is an elimination term.
  | Var {
      -- | The underlying symbol.
      varSym :: !s,
      -- | The position in source from which this originates.
      varPos :: !Pos
    }
  -- | A typed term.  This is an introduction term with an explicit
  -- type tag, which makes it an elimination term.
  | Typed {
      -- | The introduction term being typed.
      typedTerm :: Term b s,
      -- | The type of the introduction term.
      typedType :: Term b s,
      -- | The position in source from which this originates.
      typedPos :: !Pos
    }

  -- Introduction Terms.  These terms require a type in type checking.

  -- | An eta expansion.  This is present for type checking only.
  -- This represents a "frozen" substitution.
  | Eta {
      etaTerm :: Term b s,
      etaType :: Term b s,
      -- | The position in source from which this originates.
      etaPos :: !Pos
    }
  -- | A lambda expression.  Represents a function value.  Lambdas
  -- cannot appear in patterns, though they can be computed on.
  --
  -- Lambdas will ultimately need to be extended to support
  -- overloading, which adds an inherent multiple dispatch ability.
  -- This is obviousy highly nontrivial to implement.
  | Lambda {
      -- | The cases describing this function's behavior.
      lambdaCases :: [(Pattern b (Term b) s, Scope b (Term b) s)],
      -- | The position in source from which this originates.
      lambdaPos :: !Pos
    }
  -- | A structure.  Structures can be named or ordered in the surface
  -- syntax.  Ordered structures are transliterated into named
  -- structures, with the fields "1", "2", and so on.
  | Record {
      -- | The bindings for this record.  This list must be sorted by
      -- the bound variable, and bound variables must be unique (in
      -- essence, this must be what you'd expect from Map.toList).
      recVals :: Map b (Term b s),
      -- | The position in source from which this originates.
      recPos :: !Pos
    }
  -- | A collection of one or more computations, each of which is
  -- bound to a name.  Each of the members of the group may reference
  -- eachother.
  | Fix {
      fixComps :: Map b (Scope b (Comp b) s),
      -- | The position in source from which this originates.
      fixPos :: !Pos
    }
  -- | Placeholder for a malformed term, allowing type checking to
  -- continue in spite of errors.
  | BadTerm !Pos
    deriving (Ord, Eq)

-- | Commands.  These represent individual statements, or combinations
-- thereof, which do not bind variables.
--
-- We don't need alot of control flow structures.  Loops are handled
-- by the Fix structure, conditionals are handled by the pattern
-- matching inherent in Lambdas.  Widening and narrowing (ie typecase
-- and downcast) can be effectively handled by adding a multiple
-- dispatch capability).
--
-- The Core-to-LLVM compiler should therefore be smart enough to
-- figure out when it can implement a Fix as a loop, and when it can
-- inline lambdas, and when it can figure out dispatch decisions
-- statically.  This is also desirable in its own right.
data Cmd b s =
    Value {
      -- | The term representing the value of this command
      valTerm :: Term b s,
      -- | The position in source from which this originates.
      valPos :: !Pos
    }
  -- | Evaluate a computation value.  This allows execution of
  -- computations produced by terms.
  | Eval {
      -- | The computation value to evaluate
      evalTerm :: Term b s,
      -- | The position in source from which this originates.
      evalPos :: !Pos
    }
  -- | Placeholder for a malformed command, allowing type checking to
  -- continue in spite of errors.
  | BadCmd !Pos
    deriving (Ord, Eq)

-- | Computations.  Semantically, computations are generators for
-- sequences of atomic actions, which are not guaranteed to terminate,
-- or even run without error (in the case of unverified computations.
-- In terms of programming language structures, they represent
-- higher-order stateful procedures.
--
-- Obviously, computations do not admit decidable equality.
--
-- A raw computation is something akin to a function taking void in C,
-- or a monad in Haskell.  Stateful functions with arguments are
-- modeled as regular functions which produce a computation.
data Comp b s =
  -- | A sequential composition of terms.
    Seq {
      -- | The pattern to which to bind the result of seqCmd.
      seqPat :: Pattern b (Term b) s,
      -- | The command to execute.
      seqCmd :: Cmd b s,
      -- | The next computation to execute.
      seqNext :: Comp b s,
      -- | The position in source from which this originates.
      seqPos :: !Pos
    }
  -- | Result of a computation.  This is always the end of a sequence.
  | End {
      -- | The command to run to produce a result.
      endCmd :: Cmd b s,
      -- | The position in source from which this originates.
      endPos :: !Pos
    }
  -- | Placeholder for a malformed computation, allowing type checking
  -- to continue in spite of errors.
  | BadComp !Pos
    deriving (Ord, Eq)

eqBinds :: (Default b, Eq b, Eq s, Eq1 t) =>
           [(b, Binding b t s)] -> [(b, Binding b t s)] -> Bool
eqBinds ((name1, bind1) : binds1) ((name2, bind2) : binds2) =
  (name1 == name2) && (bind1 ==# bind2) && eqBinds binds1 binds2
eqBinds [] [] = True
eqBinds _ _ = False

eqCases :: (Default b, Eq b, Eq s, Eq1 t, Monad t) =>
           [(Pattern b t s, Scope b t s)] ->
           [(Pattern b t s, Scope b t s)] -> Bool
eqCases ((pat1, body1) : cases1) ((pat2, body2) : cases2) =
  (pat1 ==# pat2) && (body1 ==# body2) && eqCases cases1 cases2
eqCases [] [] = True
eqCases _ _ = False

compareBinds :: (Default b, Ord b, Ord s, Ord1 t) =>
                [(b, Binding b t s)] -> [(b, Binding b t s)] -> Ordering
compareBinds ((name1, bind1) : binds1) ((name2, bind2) : binds2) =
  case compare name1 name2 of
    EQ -> case compare1 bind1 bind2 of
      EQ -> compareBinds binds1 binds2
      out -> out
    out -> out
compareBinds [] [] = EQ
compareBinds [] _ = LT
compareBinds _ [] = GT

compareCases :: (Default b, Ord b, Ord s, Ord1 t, Monad t) =>
                [(Pattern b t s, Scope b t s)] ->
                [(Pattern b t s, Scope b t s)] -> Ordering
compareCases ((pat1, body1) : cases1) ((pat2, body2) : cases2) =
  case compare1 pat1 pat2 of
    EQ -> case compare1 body1 body2 of
      EQ -> compareCases cases1 cases2
      out -> out
    out -> out
compareCases [] [] = EQ
compareCases [] _ = LT
compareCases _ [] = GT

instance (Default b, Eq b, Eq1 t) => Eq1 (Binding b t) where
  Deconstruct { deconstructBinds = binds1, deconstructStrict = strict1,
                deconstructConstructor = constructor1 } ==#
    Deconstruct { deconstructBinds = binds2, deconstructStrict = strict2,
                  deconstructConstructor = constructor2 } =
      (strict1 == strict2) && (constructor1 == constructor2) &&
        eqBinds (Map.toAscList binds1) (Map.toAscList binds2)
  As { asName = name1, asBind = bind1 } ==#
    As { asName = name2, asBind = bind2 } =
      (name1 == name2) && (bind1 ==# bind2)
  Name { nameSym = name1 } ==# Name { nameSym = name2 } = name1 == name2
  Constant term1 ==# Constant term2 = term1 ==# term2
  _ ==# _ = False

instance (Default b, Eq b, Eq1 t) => Eq1 (Pattern b t) where
  Pattern { patternBind = bind1, patternType = ty1 } ==#
    Pattern { patternBind = bind2, patternType = ty2 } =
      (bind1 ==# bind2) && (ty1 ==# ty2)

instance (Default b, Eq b) => Eq1 (Term b) where
  ProdType { prodBindOrder = bindord1, prodArgTys = argtys1,
             prodRetTy = retty1 } ==#
    ProdType { prodBindOrder = bindord2, prodArgTys = argtys2,
               prodRetTy = retty2 } =
      (bindord1 == bindord2) && (argtys1 ==# argtys2) && (retty1 ==# retty2)
  SumType { sumBindOrder = bindord1, sumBody = body1 } ==#
    SumType { sumBindOrder = bindord2, sumBody = body2 } =
      (bindord1 == bindord2) && (body1 ==# body2)
  RefineType { refinePat = pat1, refineProp = prop1 } ==#
    RefineType { refinePat = pat2, refineProp = prop2 } =
      (pat1 ==# pat2) && (prop1 ==# prop2)
  CompType { compPat = pat1, compSpec = spec1 } ==#
    CompType { compPat = pat2, compSpec = spec2 } =
      (pat1 ==# pat2) && (spec1 ==# spec2)
  Forall { forallPats = pats1, forallProp = prop1 } ==#
    Forall { forallPats = pats2, forallProp = prop2 } =
      (pats1 ==# pats2) && (prop1 ==# prop2)
  Exists { existsPats = pats1, existsProp = prop1 } ==#
    Exists { existsPats = pats2, existsProp = prop2 } =
      (pats1 ==# pats2) && (prop1 ==# prop2)
  Call { callArgs = args1, callFunc = func1 } ==#
    Call { callArgs = args2, callFunc = func2 } =
      (args1 ==# args2) && (func1 ==# func2)
  Var { varSym = sym1 } ==# Var { varSym = sym2 } = sym1 == sym2
  Typed { typedTerm = term1, typedType = ty1 } ==#
    Typed { typedTerm = term2, typedType = ty2 } =
      (term1 ==# term2) && (ty1 ==# ty2)
  Eta { etaTerm = term1, etaType = ty1 } ==#
    Eta { etaTerm = term2, etaType = ty2 } =
      (term1 ==# term2) && (ty1 ==# ty2)
  Lambda { lambdaCases = cases1 } ==# Lambda { lambdaCases = cases2 } =
    eqCases cases1 cases2
  Record { recVals = vals1 } ==# Record { recVals = vals2 } = vals1 ==# vals2
  Fix { fixComps = comps1 } ==# Fix { fixComps = comps2 } = comps1 ==# comps2
  BadTerm _ ==# BadTerm _ = True
  _ ==# _ = False

instance (Default b, Eq b) => Eq1 (Cmd b) where
  Value { valTerm = term1 } ==# Value { valTerm = term2 } = term1 ==# term2
  Eval { evalTerm = term1 } ==# Eval { evalTerm = term2 } = term1 ==# term2
  BadCmd _ ==# BadCmd _ = True
  _ ==# _ = False

instance (Default b, Eq b) => Eq1 (Comp b) where
  Seq { seqPat = pat1, seqCmd = cmd1, seqNext = next1 } ==#
    Seq { seqPat = pat2, seqCmd = cmd2, seqNext = next2 } =
      (pat1 ==# pat2) && (cmd1 ==# cmd2) && (next1 ==# next2)
  End { endCmd = cmd1 } ==# End { endCmd = cmd2 } = cmd1 ==# cmd2
  BadComp _ ==# BadComp _ = True
  _ ==# _ = False

instance (Default b, Ord b, Ord1 t) => Ord1 (Binding b t) where
  compare1 Deconstruct { deconstructBinds = binds1, deconstructStrict = strict1,
                         deconstructConstructor = constructor1 }
           Deconstruct { deconstructBinds = binds2, deconstructStrict = strict2,
                         deconstructConstructor = constructor2 } =
    case compare strict1 strict2 of
      EQ -> case compare constructor1 constructor2 of
        EQ -> compareBinds (Map.toAscList binds1) (Map.toAscList binds2)
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

instance (Default b, Ord b, Ord1 t) => Ord1 (Pattern b t) where
  compare1 Pattern { patternBind = bind1, patternType = ty1 }
           Pattern { patternBind = bind2, patternType = ty2 } =
    case compare1 bind1 bind2 of
      EQ -> compare ty1 ty2
      out -> out

instance (Default b, Ord b) => Ord1 (Term b) where
  compare1 ProdType { prodBindOrder = bindord1, prodArgTys = argtys1,
                      prodRetTy = retty1 }
           ProdType { prodBindOrder = bindord2, prodArgTys = argtys2,
                      prodRetTy = retty2 } =
    case compare1 retty1 retty2 of
      EQ -> case compare bindord1 bindord2 of
        EQ -> compare1 argtys1 argtys2
        out -> out
      out -> out
  compare1 ProdType {} _ = GT
  compare1 _ ProdType {} = LT
  compare1 SumType { sumBindOrder = bindord1, sumBody = body1 }
           SumType { sumBindOrder = bindord2, sumBody = body2 } =
    case compare1 bindord1 bindord2 of
      EQ -> compare1 body1 body2
      out -> out
  compare1 SumType {} _ = GT
  compare1 _ SumType {} = LT
  compare1 RefineType { refinePat = pat1, refineProp = prop1 }
           RefineType { refinePat = pat2, refineProp = prop2 } =
    case compare1 pat1 pat2 of
      EQ -> compare1 prop1 prop2
      out -> out
  compare1 RefineType {} _ = GT
  compare1 _ RefineType {} = LT
  compare1 CompType { compPat = pat1, compSpec = spec1 }
           CompType { compPat = pat2, compSpec = spec2 } =
    case compare1 pat1 pat2 of
      EQ -> compare1 spec1 spec2
      out -> out
  compare1 CompType {} _ = GT
  compare1 _ CompType {} = LT
  compare1 Forall { forallPats = pats1, forallProp = prop1 }
           Forall { forallPats = pats2, forallProp = prop2 } =
    case compare1 pats1 pats2 of
      EQ -> compare1 prop1 prop2
      out -> out
  compare1 Forall {} _ = GT
  compare1 _ Forall {} = LT
  compare1 Exists { existsPats = pats1, existsProp = prop1 }
           Exists { existsPats = pats2, existsProp = prop2 } =
    case compare1 pats1 pats2 of
       EQ -> compare1 prop1 prop2
       out -> out
  compare1 Exists {} _ = GT
  compare1 _ Exists {} = LT
  compare1 Call { callArgs = args1, callFunc = func1 }
           Call { callArgs = args2, callFunc = func2 } =
    case compare1 func1 func2 of
      EQ -> compare1 args1 args2
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
    compareCases cases1 cases2
  compare1 Lambda {} _ = GT
  compare1 _ Lambda {} = LT
  compare1 Record { recVals = vals1 } Record { recVals = vals2 } =
    compare1 vals1 vals2
  compare1 Record {} _ = GT
  compare1 _ Record {} = LT
  compare1 Fix { fixComps = comps1 } Fix { fixComps = comps2 } =
    compare1 comps1 comps2
  compare1 Fix {} _ = GT
  compare1 _ Fix {} = LT
  compare1 (BadTerm _) (BadTerm _) = EQ

instance (Default b, Ord b) => Ord1 (Cmd b) where
  compare1 Value { valTerm = term1 } Value { valTerm = term2 } =
    compare1 term1 term2
  compare1 Value {} _ = GT
  compare1 _ Value {} = LT
  compare1 Eval { evalTerm = term1 } Eval { evalTerm = term2 } =
    compare1 term1 term2
  compare1 Eval {} _ = GT
  compare1 _ Eval {} = LT
  compare1 (BadCmd _) (BadCmd _) = EQ

instance (Default b, Ord b) => Ord1 (Comp b) where
  compare1 Seq { seqPat = pat1, seqCmd = cmd1, seqNext = next1 }
           Seq { seqPat = pat2, seqCmd = cmd2, seqNext = next2 } =
    case compare1 pat1 pat2 of
      EQ -> case compare1 cmd1 cmd2 of
        EQ -> compare1 next1 next2
        out -> out
      out -> out
  compare1 Seq {} _ = GT
  compare1 _ Seq {} = LT
  compare1 End { endCmd = cmd1 } End { endCmd = cmd2 } = compare1 cmd1 cmd2
  compare1 End {} _ = GT
  compare1 _ End {} = LT
  compare1 (BadComp _) (BadComp _) = EQ

instance Position (t s) => Position (Binding b t s) where
  pos Deconstruct { deconstructPos = p } = p
  pos As { asPos = p } = p
  pos Name { namePos = p } = p
  pos (Constant t) = pos t

instance Position (Pattern b t s) where
  pos Pattern { patternPos = p } = p

instance Position (Term b s) where
  pos ProdType { prodTypePos = p } = p
  pos SumType { sumTypePos = p } = p
  pos RefineType { refineTypePos = p } = p
  pos CompType { compTypePos = p } = p
  pos Forall { forallPos = p } = p
  pos Exists { existsPos = p } = p
  pos Call { callPos = p } = p
  pos Var { varPos = p } = p
  pos Typed { typedPos = p } = p
  pos Eta { etaPos = p } = p
  pos Lambda { lambdaPos = p } = p
  pos Record { recPos = p } = p
  pos Fix { fixPos = p } = p
  pos (BadTerm p) = p

instance Position (Cmd b s) where
  pos Eval { evalPos = p } = p
  pos Value { valPos = p } = p
  pos (BadCmd p) = p

instance Position (Comp b s) where
  pos Seq { seqPos = p } = p
  pos End { endPos = p } = p
  pos (BadComp p) = p

instance (Default b, Hashable b, Hashable1 t) => Hashable1 (Binding b t) where
  hashWithSalt1 s Deconstruct { deconstructConstructor = constructor,
                                deconstructBinds = binds,
                                deconstructStrict = strict } =
    (s `hashWithSalt` (1 :: Int) `hashWithSalt` constructor `hashWithSalt`
      strict) `hashWithSalt1` binds
  hashWithSalt1 s As { asName = name, asBind = bind } =
    (s `hashWithSalt` (2 :: Int) `hashWithSalt` name) `hashWithSalt1` bind
  hashWithSalt1 s Name { nameSym = name } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` name
  hashWithSalt1 s (Constant c) = (s `hashWithSalt` (4 :: Int)) `hashWithSalt1` c

instance (Default b, Hashable b, Hashable1 t) => Hashable1 (Pattern b t) where
  hashWithSalt1 s Pattern { patternBind = term, patternType = ty } =
    s `hashWithSalt1` term `hashWithSalt1` ty

instance (Default b, Hashable b) => Hashable1 (Term b) where
  hashWithSalt1 s ProdType { prodBindOrder = bindord, prodArgTys = argtys,
                             prodRetTy = retty } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` bindord `hashWithSalt`
    argtys `hashWithSalt` retty
  hashWithSalt1 s SumType { sumBindOrder = bindord, sumBody = body } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` bindord `hashWithSalt1` body
  hashWithSalt1 s RefineType { refinePat = pat, refineProp = prop } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt1` pat `hashWithSalt1` prop
  hashWithSalt1 s CompType { compPat = pat, compSpec = spec } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt1` pat `hashWithSalt1` spec
  hashWithSalt1 s Forall { forallPats = pats, forallProp = prop } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt1` pats `hashWithSalt1` prop
  hashWithSalt1 s Exists { existsPats = pats, existsProp = prop } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt1` pats `hashWithSalt1` prop
  hashWithSalt1 s Call { callArgs = args, callFunc = func } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt1` args `hashWithSalt1` func
  hashWithSalt1 s Var { varSym = sym } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` sym
  hashWithSalt1 s Typed { typedTerm = term, typedType = ty } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Eta { etaTerm = term, etaType = ty } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt1` term `hashWithSalt1` ty
  hashWithSalt1 s Lambda { lambdaCases = cases } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt1` cases
  hashWithSalt1 s Record { recVals = vals } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt1` vals
  hashWithSalt1 s Fix { fixComps = comps } =
    s `hashWithSalt` (13 :: Int) `hashWithSalt1` comps
  hashWithSalt1 s (BadTerm _) = s `hashWithSalt` (0 :: Int)

instance (Default b, Hashable b) => Hashable1 (Cmd b) where
  hashWithSalt1 s Value { valTerm = term } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1` term
  hashWithSalt1 s Eval { evalTerm = term } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` term
  hashWithSalt1 s (BadCmd _) = s `hashWithSalt` (0 :: Int)

instance (Default b, Hashable b) => Hashable1 (Comp b) where
  hashWithSalt1 s Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt1`
    pat `hashWithSalt1` cmd `hashWithSalt1` next
  hashWithSalt1 s End { endCmd = cmd } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt1` cmd
  hashWithSalt1 s (BadComp _) = s `hashWithSalt` (0 :: Int)

instance (Default b, Hashable b, Hashable1 t, Hashable s) =>
         Hashable (Binding b t s) where
  hashWithSalt = hashWithSalt1

instance (Default b, Hashable b, Hashable1 t, Hashable s) =>
         Hashable (Pattern b t s) where
  hashWithSalt = hashWithSalt1

instance (Default b, Hashable b, Hashable s) => Hashable (Term b s) where
  hashWithSalt = hashWithSalt1

instance (Default b, Hashable b, Hashable s) => Hashable (Cmd b s) where
  hashWithSalt = hashWithSalt1

instance (Default b, Hashable b, Hashable s) => Hashable (Comp b s) where
  hashWithSalt = hashWithSalt1

instance Functor t => Functor (Binding b t) where
  fmap f b @ Deconstruct { deconstructBinds = binds } =
    b { deconstructBinds = fmap (fmap f) binds }
  fmap f b @ As { asBind = bind } = b { asBind = fmap f bind }
  fmap _ b @ Name { nameSym = name } = b { nameSym = name }
  fmap f (Constant t) = Constant (fmap f t)

instance Functor t => Functor (Pattern b t) where
  fmap f p @ Pattern { patternBind = term, patternType = ty } =
    p { patternBind = fmap f term, patternType = fmap f ty }

instance Functor (Term b) where
  fmap f t @ ProdType { prodArgTys = argtys, prodRetTy = retty } =
    t { prodArgTys = fmap (fmap f) argtys, prodRetTy = fmap f retty }
  fmap f t @ SumType { sumBody = body } = t { sumBody = fmap (fmap f) body }
  fmap f t @ RefineType { refinePat = pat, refineProp = prop } =
    t { refinePat = fmap f pat, refineProp = fmap f prop }
  fmap f t @ CompType { compPat = pat, compSpec = spec } =
    t { compPat = fmap f pat, compSpec = fmap f spec }
  fmap f t @ Forall { forallPats = pats, forallProp = prop } =
    t { forallPats = fmap (fmap f) pats, forallProp = fmap f prop }
  fmap f t @ Exists { existsPats = pats, existsProp = prop } =
    t { existsPats = fmap (fmap f) pats, existsProp = fmap f prop }
  fmap f t @ Call { callArgs = args, callFunc = func } =
    t { callArgs = fmap (fmap f) args, callFunc = fmap f func }
  fmap f t @ Var { varSym = sym } = t { varSym = f sym }
  fmap f t @ Typed { typedTerm = term, typedType = ty } =
    t { typedTerm = fmap f term, typedType = fmap f ty }
  fmap f t @ Eta { etaTerm = term, etaType = ty } =
    t { etaTerm = fmap f term, etaType = fmap f ty }
  fmap f t @ Lambda { lambdaCases = cases } =
    t { lambdaCases = fmap (\(pat, body) -> (fmap f pat, fmap f body)) cases }
  fmap f t @ Record { recVals = vals } = t { recVals = fmap (fmap f) vals }
  fmap f t @ Fix { fixComps = comps } = t { fixComps = fmap (fmap f) comps }
  fmap _ (BadTerm p) = (BadTerm p)

instance Functor (Cmd b) where
  fmap f c @ Eval { evalTerm = term } = c { evalTerm = fmap f term }
  fmap f c @ Value { valTerm = term } = c { valTerm = fmap f term }
  fmap _ (BadCmd p) = BadCmd p

instance Functor (Comp b) where
  fmap f c @ Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
    c { seqPat = fmap f pat, seqCmd = fmap f cmd, seqNext = fmap f next }
  fmap f c @ End { endCmd = cmd } = c { endCmd = fmap f cmd }
  fmap _ (BadComp p) = BadComp p

instance Foldable t => Foldable (Binding b t) where
  foldMap f Deconstruct { deconstructBinds = binds } = foldMap (foldMap f) binds
  foldMap f As { asBind = bind } = foldMap f bind
  foldMap f (Constant t) = foldMap f t
  foldMap _ _ = mempty

instance Foldable t => Foldable (Pattern b t) where
  foldMap f Pattern { patternBind = term, patternType = ty } =
    foldMap f term `mappend` foldMap f ty

instance Foldable (Term b) where
  foldMap f ProdType { prodArgTys = argtys, prodRetTy = retty } =
    foldMap (foldMap f) argtys `mappend` foldMap f retty
  foldMap f SumType { sumBody = body } = foldMap (foldMap f) body
  foldMap f RefineType { refinePat = pat, refineProp = prop } =
    foldMap f pat `mappend` foldMap f prop
  foldMap f CompType { compPat = pat, compSpec = spec } =
    foldMap f pat `mappend` foldMap f spec
  foldMap f Forall { forallPats = pats, forallProp = prop } =
    foldMap (foldMap f) pats `mappend` foldMap f prop
  foldMap f Exists { existsPats = pats, existsProp = prop } =
    foldMap (foldMap f) pats `mappend` foldMap f prop
  foldMap f Call { callArgs = args, callFunc = func } =
    foldMap (foldMap f) args `mappend` foldMap f func
  foldMap f Var { varSym = sym } = f sym
  foldMap f Typed { typedTerm = term, typedType = ty } =
    foldMap f term `mappend` foldMap f ty
  foldMap f Eta { etaTerm = term, etaType = ty } =
    foldMap f term `mappend` foldMap f ty
  foldMap f Lambda { lambdaCases = cases } =
    foldMap (\(pat, body) -> foldMap f pat `mappend` foldMap f body) cases
  foldMap f Record { recVals = vals } = foldMap (foldMap f) vals
  foldMap f Fix { fixComps = comps } = foldMap (foldMap f) comps
  foldMap _ _ = mempty

instance Foldable (Cmd b) where
  foldMap f Value { valTerm = term } = foldMap f term
  foldMap f Eval { evalTerm = term } = foldMap f term
  foldMap _ _ = mempty

instance Foldable (Comp b) where
  foldMap f Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
    foldMap f pat `mappend` foldMap f cmd `mappend` foldMap f next
  foldMap f End { endCmd = cmd } = foldMap f cmd
  foldMap _ _ = mempty

instance Traversable t => Traversable (Binding b t) where
  traverse f b @ Deconstruct { deconstructBinds = binds } =
    (\binds' -> b { deconstructBinds = binds' }) <$>
    traverse (traverse f) binds
  traverse _ b @ Name { nameSym = name } = pure (b { nameSym = name })
  traverse f b @ As { asBind = bind } =
    (\bind' -> b { asBind = bind' }) <$> traverse f bind
  traverse f (Constant t) = Constant <$> traverse f t

instance Traversable t => Traversable (Pattern b t) where
  traverse f p @ Pattern { patternBind = term, patternType = ty } =
    (\term' ty' -> p { patternBind = term', patternType = ty' }) <$>
      traverse f term <*> traverse f ty

instance Traversable (Term b) where
  traverse f t @ ProdType { prodArgTys = argtys, prodRetTy = retty } =
    (\argtys' retty' -> t { prodArgTys = argtys', prodRetTy = retty' }) <$>
      traverse (traverse f) argtys <*> traverse f retty
  traverse f t @ SumType { sumBody = body } =
    (\body' -> t { sumBody = body' }) <$> traverse (traverse f) body
  traverse f t @ RefineType { refinePat = pat, refineProp = prop } =
    (\pat' prop' -> t { refinePat = pat', refineProp = prop' }) <$>
      traverse f pat <*> traverse f prop
  traverse f t @ CompType { compPat = pat, compSpec = spec } =
    (\pat' spec' -> t { compPat = pat', compSpec = spec' }) <$>
      traverse f pat <*> traverse f spec
  traverse f t @ Forall { forallPats = pats, forallProp = prop } =
    (\pats' prop' -> t { forallPats = pats', forallProp = prop' }) <$>
      traverse (traverse f) pats <*> traverse f prop
  traverse f t @ Exists { existsPats = pats, existsProp = prop } =
    (\pats' prop' -> t { existsPats = pats', existsProp = prop' }) <$>
      traverse (traverse f) pats <*> traverse f prop
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
    (\cases' -> t { lambdaCases = cases' }) <$>
      traverse (\(pat, body) -> (\pat' body' -> (pat', body')) <$>
                 traverse f pat <*> traverse f body) cases
  traverse f t @ Record { recVals = vals } =
    (\vals' -> t { recVals = vals' }) <$> traverse (traverse f) vals
  traverse f t @ Fix { fixComps = comps } =
    (\comps' -> t { fixComps = comps' }) <$> traverse (traverse f) comps
  traverse _ (BadTerm p) = pure (BadTerm p)

instance Traversable (Cmd b) where
  traverse f c @ Value { valTerm = term } =
    (\term' -> c { valTerm = term' }) <$> traverse f term
  traverse f c @ Eval { evalTerm = term } =
    (\term' -> c { evalTerm = term' }) <$> traverse f term
  traverse _ (BadCmd c) = pure (BadCmd c)

instance Traversable (Comp b) where
  traverse f c @ Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
    (\pat' cmd' next' -> c { seqPat = pat', seqCmd = cmd',
                             seqNext = next' }) <$>
      traverse f pat <*> traverse f cmd <*> traverse f next
  traverse f c @ End { endCmd = cmd } =
    (\cmd' -> c { endCmd = cmd' }) <$> traverse f cmd
  traverse _ (BadComp p) = pure (BadComp p)

injectpos :: Pos
injectpos = internal "Monad return"

substpos :: Pos
substpos = internal "Monad substitution"

instance MonadTrans (Binding b) where
  lift m = Constant m

instance Bound (Binding b) where
  b @ Deconstruct { deconstructBinds = binds } >>>= f =
    b { deconstructBinds = fmap (>>>= f) binds }
  b @ As { asBind = bind } >>>= f = b { asBind = bind >>>= f }
  b @ Name { nameSym = name } >>>= _ = b { nameSym = name }
  Constant t >>>= f = Constant (t >>= f)

instance Default b => Applicative (Term b) where
  pure = return
  (<*>) = ap

instance Default b => Applicative (Comp b) where
  pure = return
  (<*>) = ap

patSubstTerm :: Default c => (a -> Term c b) -> Pattern c (Term c) a ->
                Pattern c (Term c) b
patSubstTerm f p @ Pattern { patternBind = bind, patternType = ty } =
  p { patternBind = bind >>>= f, patternType = ty >>= f }

cmdSubstTerm :: Default c => (a -> Term c b) -> Cmd c a -> Cmd c b
cmdSubstTerm f c @ Value { valTerm = term } = c { valTerm = term >>= f }
cmdSubstTerm f c @ Eval { evalTerm = term } = c { evalTerm = term >>= f }
cmdSubstTerm _ (BadCmd p) = BadCmd p

compSubstTerm :: Default c => (a -> Term c b) -> Comp c a -> Comp c b
compSubstTerm f c @ Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
  c { seqPat = patSubstTerm f pat, seqCmd = cmdSubstTerm f cmd,
      seqNext = compSubstTerm f next }
compSubstTerm f c @ End { endCmd = cmd } =
  c { endCmd = cmdSubstTerm f cmd }
compSubstTerm _ (BadComp p) = BadComp p

instance Default b => Monad (Term b) where
  return sym = Var { varSym = sym, varPos = injectpos }

  t @ ProdType { prodArgTys = argtys, prodRetTy = retty } >>= f =
    t { prodArgTys = fmap (>>>= f) argtys, prodRetTy = retty >>>= f }
  t @ SumType { sumBody = body } >>= f =
    t { sumBody = fmap (>>>= f) body }
  t @ RefineType { refinePat = pat, refineProp = prop } >>= f =
    t { refinePat = patSubstTerm f pat, refineProp = prop >>>= f }
  t @ CompType { compPat = pat, compSpec = spec } >>= f =
    t { compPat = patSubstTerm f pat, compSpec = spec >>>= f }
  t @ Forall { forallPats = pats, forallProp = prop } >>= f =
    t { forallPats = fmap (patSubstTerm f) pats, forallProp = prop >>>= f }
  t @ Exists { existsPats = pats, existsProp = prop } >>= f =
    t { existsPats = fmap (patSubstTerm f) pats, existsProp = prop >>>= f }
  t @ Call { callArgs = args, callFunc = func } >>= f =
    t { callArgs = fmap (>>= f) args, callFunc = func >>= f }
  Var { varSym = sym } >>= f = f sym
  t @ Typed { typedTerm = term, typedType = ty } >>= f =
    t { typedTerm = term >>= f, typedType = ty >>= f }
  t @ Lambda { lambdaCases = cases } >>= f =
    t { lambdaCases = fmap (\(pat, body) -> (patSubstTerm f pat,
                                             body >>>= f)) cases }
  t @ Record { recVals = vals } >>= f = t { recVals = fmap (>>= f) vals }
  t @ Fix { fixComps = comps } >>= f =
    t { fixComps = fmap (>>>= compSubstTerm f . return) comps }
  t @ Eta { etaTerm = term, etaType = ty } >>= f =
    t { etaTerm = term >>= f, etaType = ty >>= f }
  BadTerm p >>= _ = BadTerm p

bindSubstComp :: Default c => (a -> Comp c b) -> Binding c (Term c) a ->
                Binding c (Term c) b
bindSubstComp f b @ Deconstruct { deconstructBinds = binds } =
  b { deconstructBinds = fmap (bindSubstComp f) binds }
bindSubstComp f b @ As { asBind = bind } =
  b { asBind = bindSubstComp f bind }
bindSubstComp _ b @ Name { nameSym = name } = b { nameSym = name }
bindSubstComp f (Constant t) = Constant (termSubstComp f t)


patSubstComp :: Default c => (a -> Comp c b) -> Pattern c (Term c) a ->
                Pattern c (Term c) b
patSubstComp f p @ Pattern { patternBind = bind, patternType = ty } =
  p { patternBind = bindSubstComp f bind, patternType = termSubstComp f ty }

termSubstComp :: Default c => (a -> Comp c b) -> Term c a -> Term c b
termSubstComp f t @ ProdType { prodArgTys = argtys, prodRetTy = retty } =
  t { prodArgTys = fmap (>>>= termSubstComp f . return) argtys,
      prodRetTy = retty >>>= termSubstComp f . return }
termSubstComp f t @ SumType { sumBody = body } =
  t { sumBody = fmap (>>>= termSubstComp f . return) body }
termSubstComp f t @ RefineType { refinePat = pat, refineProp = prop } =
  t { refineProp = prop >>>= termSubstComp f . return,
      refinePat = patSubstComp f pat }
termSubstComp f t @ CompType { compPat = pat, compSpec = spec } =
  t { compSpec = spec >>>= termSubstComp f . return,
      compPat = patSubstComp f pat }
termSubstComp f t @ Forall { forallPats = pats, forallProp = prop } =
  t { forallProp = prop >>>= termSubstComp f . return,
      forallPats = fmap (patSubstComp f) pats }
termSubstComp f t @ Exists { existsPats = pats, existsProp = prop } =
  t { existsProp = prop >>>= termSubstComp f . return,
      existsPats = fmap (patSubstComp f) pats }
termSubstComp f t @ Call { callArgs = args, callFunc = func } =
  t { callArgs = fmap (>>= termSubstComp f . return) args,
      callFunc = func >>= termSubstComp f . return }
termSubstComp f Var { varSym = sym } =
  Fix { fixComps = Map.singleton defaultVal (abstract (\_ -> Nothing) (f sym)),
        fixPos = substpos }
termSubstComp f t @ Typed { typedTerm = term, typedType = ty } =
  t { typedTerm = termSubstComp f term, typedType = termSubstComp f ty }
termSubstComp f t @ Lambda { lambdaCases = cases } =
  t { lambdaCases = fmap (\(pat, body) -> (patSubstComp f pat,
                                           body >>>= termSubstComp f . return))
                         cases }
termSubstComp f t @ Record { recVals = vals } =
  t { recVals = fmap (termSubstComp f) vals }
termSubstComp f t @ Fix { fixComps = comps } =
  t { fixComps = fmap (>>>= f) comps }
termSubstComp f t @ Eta { etaTerm = term, etaType = ty } =
    t { etaTerm = termSubstComp f term, etaType = termSubstComp f ty }
termSubstComp _ (BadTerm p) = BadTerm p

cmdSubstComp :: Default c => (a -> Comp c b) -> Cmd c a -> Cmd c b
cmdSubstComp f c @ Value { valTerm = term } =
  c { valTerm = termSubstComp f term }
cmdSubstComp f c @ Eval { evalTerm = term } =
  c { evalTerm = termSubstComp f term }
cmdSubstComp _ (BadCmd p) = BadCmd p

instance Default b => Monad (Comp b) where
  return sym =
    End { endCmd = Value { valTerm = Var { varSym = sym, varPos = injectpos },
                           valPos = injectpos },
          endPos = injectpos}
  c @ Seq { seqPat = pat, seqCmd = cmd, seqNext = next } >>= f =
    c { seqPat = patSubstComp f pat, seqNext = next >>= f,
        seqCmd = cmdSubstComp f cmd }
  c @ End { endCmd = cmd } >>= f = c { endCmd = cmdSubstComp f cmd }
  BadComp p >>= _ = BadComp p

arbitraryPos :: Pos
arbitraryPos = internal "arbitrary"

-- The top-level function takes a set of bound vars, and produces a
-- binding and a new set of bound vars.  This behavior is different
-- from the inner functions.
arbitraryBinding :: (Ord s, Arbitrary s) => Set s -> Int ->
                    Gen (Binding s (Term s) s, Set s)
arbitraryBinding scope insize
 | 0 < insize =
  let
    arbitraryBinding' vars size =
      let
        arbitraryNewName =
          suchThat arbitrary (\name -> not (Set.member name vars))

        arbitraryNameBinding =
          do
            var <- arbitraryNewName
            return (Name { nameSym = var, namePos = arbitraryPos },
                    Set.insert var vars)

        arbitraryAsBinding =
          do
            var <- arbitraryNewName
            (bind, vars') <- arbitraryBinding (Set.insert var vars) (size - 1)
            return (As { asName = var, asBind = bind, asPos = arbitraryPos },
                    vars')

        arbitraryConstantBinding =
          do
            term <- arbitraryTerm scope size
            return (Constant term, vars)

        arbitraryDeconstructBinding =
          let
            arbitraryBindings vars' size'
             | size > 0 =
              do
                thissize <- choose (0, size' - 1)
                bindvar <- arbitrary
                (bind, newvars) <-
                  arbitraryBinding' (Set.insert bindvar vars') thissize
                (binds, newvars') <-
                  arbitraryBindings newvars (size' - thissize - 1)
                return ((bindvar, bind) : binds, newvars')
             | otherwise = return ([], vars')
          in do
            strict <- arbitrary
            constructor <- elements (Set.toList scope)
            (binds, newvars) <- arbitraryBindings vars size
            return (Deconstruct { deconstructBinds = Map.fromList binds,
                                  deconstructStrict = strict,
                                  deconstructConstructor = constructor,
                                  deconstructPos = arbitraryPos }, newvars)
      in
        oneof [ arbitraryConstantBinding, arbitraryDeconstructBinding,
                arbitraryNameBinding, arbitraryAsBinding ]
  in do
    (bind, vars) <- arbitraryBinding' Set.empty insize
    return (bind, Set.union vars scope)
 | otherwise =
  do
    var <- arbitrary
    return (Name { nameSym = var, namePos = arbitraryPos },
            Set.insert var scope)

arbitraryPattern :: (Ord s, Arbitrary s) => Set s -> Int ->
                    Gen (Pattern s (Term s) s, Set s)
arbitraryPattern scope size
  | size > 1 =
    do
      tysize <- choose (0, size - 1)
      ty <- arbitraryTerm scope tysize
      (bind, newscope) <- arbitraryBinding scope (size - tysize - 1)
      return (Pattern { patternType = ty, patternBind = bind,
                        patternPos = arbitraryPos }, newscope)
  | otherwise =
    do
      ty <- arbitraryTerm scope 0
      (bind, newscope) <- arbitraryBinding scope 0
      return (Pattern { patternType = ty, patternBind = bind,
                        patternPos = arbitraryPos }, newscope)

arbitraryTerm :: (Ord s, Arbitrary s) => Set s -> Int -> Gen (Term s s)
arbitraryTerm bindings size =
  let
    genEntries size'
      | size' > 0 =
        do
          fieldsize <- choose (0, size' - 1)
          field <- arbitrary
          val <- arbitraryTerm bindings size'
          rest <- genEntries (size' - fieldsize - 1)
          return ((field, val) : rest)
      | otherwise = return []

    arbitraryCallTerm =
      do
        funcsize <- choose (0, size)
        func <- arbitraryTerm bindings funcsize
        entries <- genEntries (size - funcsize - 1)
        return Call { callFunc = func, callArgs = Map.fromList entries,
                      callPos = arbitraryPos }

    arbitraryRecordTerm =
      do
        entries <- genEntries size
        return Record { recVals = Map.fromList entries, recPos = arbitraryPos }

    arbitraryFreeVarTerm =
      do
        sym <- arbitrary
        return Var { varSym = sym, varPos = arbitraryPos }

    arbitraryBoundVarTerm =
      do
        sym <- elements (Set.elems bindings)
        return Var { varSym = sym, varPos = arbitraryPos }

    arbitraryTypedTerm =
      do
        tysize <- choose (0, size - 1)
        ty <- arbitraryTerm bindings tysize
        val <- arbitraryTerm bindings (size - tysize - 1)
        return Typed { typedTerm = val, typedType = ty,
                       typedPos = arbitraryPos }
  in if 0 > size
    then oneof [ arbitraryBoundVarTerm, arbitraryFreeVarTerm,
                 arbitraryTypedTerm, arbitraryRecordTerm,
                 arbitraryCallTerm ]
    else oneof [ arbitraryBoundVarTerm, arbitraryFreeVarTerm ]

arbitraryCmd :: (Ord s, Arbitrary s) => Set s -> Int -> Gen (Cmd s s)
arbitraryCmd scope size =
  let
    arbitraryEvalCmd size' =
      do
        term <- arbitraryTerm scope size'
        return Eval { evalTerm = term, evalPos = arbitraryPos }

    arbitraryValueCmd size' =
      do
        term <- arbitraryTerm scope size'
        return Value { valTerm = term, valPos = arbitraryPos }
  in
    oneof [ arbitraryEvalCmd size, arbitraryValueCmd size ]

arbitraryComp :: (Ord s, Arbitrary s) => Set s -> Int -> Gen (Comp s s)
arbitraryComp scope size
 | size > 0 =
  let
    arbitrarySeqComp =
      do
        patsize <- choose (0, size - 1)
        cmdsize <- choose (0, size - patsize - 1)
        cmd <- arbitraryCmd scope cmdsize
        (pat, newscope) <- arbitraryPattern scope patsize
        next <- arbitraryComp newscope (size - patsize - cmdsize - 1)
        return Seq { seqPat = pat, seqCmd = cmd, seqNext = next,
                     seqPos = arbitraryPos }

    arbitraryEndComp =
      do
        cmd <- arbitraryCmd scope size
        return End { endCmd = cmd, endPos = arbitraryPos }
  in
    oneof [ arbitraryEndComp, arbitrarySeqComp ]
 | otherwise =
  do
    cmd <- arbitraryCmd scope 0
    return End { endCmd = cmd, endPos = arbitraryPos }

instance (Ord s, Arbitrary s) => Arbitrary (Binding s (Term s) s) where
  arbitrary = sized (arbitraryBinding Set.empty) >>= return . fst

  shrink b @ Deconstruct { deconstructBinds = binds } =
    case Map.assocs binds of
      [(_, bind)] -> [bind]
      list -> map (\list' -> b { deconstructBinds = Map.fromList list' })
                  (shrink list)
  shrink b @ As { asBind = bind } =
    bind : (map (\bind' -> b { asBind = bind' }) (shrink bind))
  shrink Name {} = []
  shrink (Constant t) = map Constant (shrink t)

instance (Ord s, Arbitrary s) => Arbitrary (Pattern s (Term s) s) where
  arbitrary = sized (arbitraryPattern Set.empty) >>= return . fst

  shrink p @ Pattern { patternType = ty, patternBind = bind } =
    map (\ty' -> p { patternType = ty' }) (shrink ty) ++
    map (\bind' -> p { patternBind = bind' }) (shrink bind)

instance (Ord s, Arbitrary s) => Arbitrary (Term s s) where
  arbitrary = sized (arbitraryTerm Set.empty)

  shrink t @ Typed { typedType = ty, typedTerm = term } =
    map (\ty' -> t { typedType = ty' }) (shrink ty) ++
    map (\term' -> t { typedType = term' }) (shrink term)
  shrink Var {} = []

instance (Ord s, Arbitrary s) => Arbitrary (Cmd s s) where
  arbitrary = sized (arbitraryCmd Set.empty)

  shrink c @ Eval { evalTerm = term } =
    map (\term' -> c { evalTerm = term' }) (shrink term)
  shrink c @ Value { valTerm = term } =
    map (\term' -> c { valTerm = term' }) (shrink term)
  shrink (BadCmd _) = []

instance (Ord s, Arbitrary s) => Arbitrary (Comp s s) where
  arbitrary = sized (arbitraryComp Set.empty)

  shrink c @ Seq { seqPat = pat, seqCmd = cmd, seqNext = next } =
    map (\pat' -> c { seqPat = pat' }) (shrink pat) ++
    map (\cmd' -> c { seqCmd = cmd' }) (shrink cmd) ++
    map (\next' -> c { seqNext = next' }) (shrink next)
  shrink c @ End { endCmd = cmd } =
    map (\cmd' -> c { endCmd = cmd' }) (shrink cmd)
  shrink (BadComp _) = []