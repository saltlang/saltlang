-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- | The Argent core language.  Argent's surface syntax is
-- transliterated into Core, which is then type-checked and compiled.
-- Core is generally not meant for human consumption.
module Language.Argent.Core(
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
import Data.Foldable
import Data.Hash
import Data.Monoid
import Data.Pos
import Data.Traversable
import Prelude hiding (foldr1)
import Prelude.Extras(Eq1(..), Ord1(..))

-- | A pattern binding.  Represents how to deconstruct a value and
-- bind it to variables.
data Binding b t s =
    -- | A projection.  Binds to the names of a record.  This mirrors
    -- a Record term.
    Project {
      -- | The fields in the record being bound.
      projectBinds :: [(b, Binding b t s)],
      -- | Whether or not the binding is strict (ie. it omits some names)
      projectStrict :: !Bool,
      -- | The position in source from which this originates.
      projectPos :: !Pos
    }
    -- | A construction.  Binds to a datatype constructor.
    -- 
    -- Since this is a dependent language, then any set of orthonogal
    -- bijections can be a set of constructors (ie. a basis) for a
    -- datatype.
  | Construct {
      -- | The fields in the construction being bound.
      constructBinds :: [(b, Binding b t s)],
      -- | Whether or not the constructor is strict (ie. it omits some names)
      constructStrict :: !Bool,
      -- | The position in source from which this originates.
      constructPos :: !Pos
    }
    -- | An "as" binding.  Allows part of a pattern to be bound to a
    -- name, but further deconstructed by another pattern.  For
    -- example "x as (y, z)".
  | As {
      -- | The outer name, to which the entire datatype is bound.
      asName :: b,
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
      nameSym :: b,
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
      -- | The type of the first argument.
      prodInitArgTy :: Term b s,
      -- | The types of subsequent arguments, which can reference all
      -- previous arguments
      prodArgTys :: [Scope () (Term b) s],
      -- | The return type of the function.
      prodRetTy :: Scope () (Term b) s,
      -- | The position in source from which this originates.
      prodTypePos :: !Pos
    }
  -- | Dependent sum type.  This is the type given to structures.
  | SumType {
      -- | The first element of the sum type.
      sumInit :: Term b s,
      -- | The remaining elements of the sum type
      sumBody :: [Scope () (Term b) s],
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
      callArgs :: [(b, Term b s)],
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
      varSym :: s,
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
{-
  | Lambda {
      -- XXX Figure out how to represent lambdas, possibly as lists of
      -- pattern/scopes
      -- | The position in source from which this originates.
      lambdaPos :: !Pos
    }
-}
  -- | A structure.  Structures can be named or ordered in the surface
  -- syntax.  Ordered structures are transliterated into named
  -- structures, with the fields "1", "2", and so on.
  | Record {
      -- | The bindings for this record.
      recVals :: [(b, Term b s)],
      -- | The position in source from which this originates.
      recPos :: !Pos
    }
  -- | A collection of one or more computations, each of which is
  -- bound to a name.  Each of the members of the group may reference
  -- eachother.
  | Fix {
      fixComps :: [(Maybe b, Scope b (Comp b) s)],
      -- | The position in source from which this originates.
      fixPos :: !Pos
    }
  -- | Placeholder for a malformed term, allowing type checking to
  -- continue in spite of errors.
  | BadTerm !Pos
    deriving (Ord, Eq)

-- | Commands.  These represent individual statements, or combinations
-- thereof, which do not bind variables.
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
  -- XXX need some kind of a general form case statement, probably
  -- akin to Fortress' dispatch statement.
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

eqBinds :: (Eq b, Eq s, Eq1 t) =>
           [(b, Binding b t s)] -> [(b, Binding b t s)] -> Bool
eqBinds ((name1, bind1) : binds1) ((name2, bind2) : binds2) =
  (name1 == name2) && (bind1 ==# bind2) && eqBinds binds1 binds2
eqBinds [] [] = True
eqBinds _ _ = False

compareBinds :: (Ord b, Ord s, Ord1 t) =>
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

instance (Eq b, Eq1 t) => Eq1 (Binding b t) where
  Project { projectBinds = binds1, projectStrict = strict1 } ==#
    Project { projectBinds = binds2, projectStrict = strict2 } =
      (eqBinds binds1 binds2) && (strict1 == strict2)
  Construct { constructBinds = binds1, constructStrict = strict1 } ==#
    Construct { constructBinds = binds2, constructStrict = strict2 } =
      (eqBinds binds1 binds2) && (strict1 == strict2)
  As { asName = name1, asBind = bind1 } ==#
    As { asName = name2, asBind = bind2 } =
      (name1 == name2) && (bind1 ==# bind2)
  Name { nameSym = name1 } ==# Name { nameSym = name2 } = name1 == name2
  Constant term1 ==# Constant term2 = term1 ==# term2
  _ ==# _ = False

instance (Eq b, Eq1 t) => Eq1 (Pattern b t) where
  Pattern { patternBind = bind1, patternType = ty1 } ==#
    Pattern { patternBind = bind2, patternType = ty2 } =
      (bind1 ==# bind2) && (ty1 ==# ty2)

instance Eq b => Eq1 (Term b) where
  ProdType { prodInitArgTy = initty1, prodArgTys = argtys1,
             prodRetTy = retty1 } ==#
    ProdType { prodInitArgTy = initty2, prodArgTys = argtys2,
               prodRetTy = retty2 } =
      (initty1 ==# initty2) && (argtys1 ==# argtys2) && (retty1 ==# retty2)
  SumType { sumInit = init1, sumBody = body1 } ==#
    SumType { sumInit = init2, sumBody = body2 } =
      (init1 ==# init2) && (body1 ==# body2)
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
  Record { recVals = vals1 } ==# Record { recVals = vals2 } = vals1 ==# vals2
  Fix { fixComps = comps1 } ==# Fix { fixComps = comps2 } = comps1 ==# comps2
  BadTerm _ ==# BadTerm _ = True
  _ ==# _ = False

instance Eq b => Eq1 (Cmd b) where
  Value { valTerm = term1 } ==# Value { valTerm = term2 } = term1 ==# term2
  Eval { evalTerm = term1 } ==# Eval { evalTerm = term2 } = term1 ==# term2
  BadCmd _ ==# BadCmd _ = True
  _ ==# _ = False

instance Eq b => Eq1 (Comp b) where
  Seq { seqPat = pat1, seqCmd = cmd1, seqNext = next1 } ==#
    Seq { seqPat = pat2, seqCmd = cmd2, seqNext = next2 } =
      (pat1 ==# pat2) && (cmd1 ==# cmd2) && (next1 ==# next2)
  End { endCmd = cmd1 } ==# End { endCmd = cmd2 } = cmd1 ==# cmd2
  BadComp _ ==# BadComp _ = True
  _ ==# _ = False

instance (Ord b, Ord1 t) => Ord1 (Binding b t) where
  compare1 (Project { projectBinds = binds1, projectStrict = strict1 })
           (Project { projectBinds = binds2, projectStrict = strict2 }) =
    case compare strict1 strict2 of
      EQ -> compareBinds binds1 binds2
      out -> out
  compare1 (Project {}) _ = GT
  compare1 _ (Project {}) = LT
  compare1 (Construct { constructBinds = binds1, constructStrict = strict1 })
           (Construct { constructBinds = binds2, constructStrict = strict2 }) =
    case compare strict1 strict2 of
      EQ -> compareBinds binds1 binds2
      out -> out
  compare1 (Construct {}) _ = GT
  compare1 _ (Construct {}) = LT
  compare1 (As { asName = name1, asBind = bind1 })
           (As { asName = name2, asBind = bind2 }) =
    case compare name1 name2 of
      EQ -> compare1 bind1 bind2
      out -> out
  compare1 (As {}) _ = GT
  compare1 _ (As {}) = LT
  compare1 (Name { nameSym = name1 }) (Name { nameSym = name2 }) =
    compare name1 name2
  compare1 (Name {}) _ = GT
  compare1 _ (Name {}) = LT
  compare1 (Constant term1) (Constant term2) = compare1 term1 term2

instance (Ord b, Ord1 t) => Ord1 (Pattern b t) where
  compare1 (Pattern { patternBind = bind1, patternType = ty1 })
           (Pattern { patternBind = bind2, patternType = ty2 }) =
    case compare1 bind1 bind2 of
      EQ -> compare ty1 ty2
      out -> out

instance Ord b => Ord1 (Term b) where
  compare1 (ProdType { prodInitArgTy = initty1, prodArgTys = argtys1,
                       prodRetTy = retty1 })
           (ProdType { prodInitArgTy = initty2, prodArgTys = argtys2,
                       prodRetTy = retty2 }) =
    case compare1 initty1 initty2 of
      EQ -> case compare1 argtys1 argtys2 of
        EQ -> compare1 retty1 retty2
        out -> out
      out -> out
  compare1 (ProdType {}) _ = GT
  compare1 _ (ProdType {}) = LT
  compare1 (SumType { sumInit = init1, sumBody = body1 })
           (SumType { sumInit = init2, sumBody = body2 }) =
    case compare1 init1 init2 of
      EQ -> compare1 body1 body2
      out -> out
  compare1 (SumType {}) _ = GT
  compare1 _ (SumType {}) = LT
  compare1 (RefineType { refinePat = pat1, refineProp = prop1 })
           (RefineType { refinePat = pat2, refineProp = prop2 }) =
    case compare1 pat1 pat2 of
      EQ -> compare1 prop1 prop2
      out -> out
  compare1 (RefineType {}) _ = GT
  compare1 _ (RefineType {}) = LT
  compare1 (CompType { compPat = pat1, compSpec = spec1 })
           (CompType { compPat = pat2, compSpec = spec2 }) =
    case compare1 pat1 pat2 of
      EQ -> compare1 spec1 spec2
      out -> out
  compare1 (CompType {}) _ = GT
  compare1 _ (CompType {}) = LT
  compare1 (Forall { forallPats = pats1, forallProp = prop1 })
           (Forall { forallPats = pats2, forallProp = prop2 }) =
    case compare1 pats1 pats2 of
      EQ -> compare1 prop1 prop2
      out -> out
  compare1 (Forall {}) _ = GT
  compare1 _ (Forall {}) = LT
  compare1 (Exists { existsPats = pats1, existsProp = prop1 })
           (Exists { existsPats = pats2, existsProp = prop2 }) =
    case compare1 pats1 pats2 of
       EQ -> compare1 prop1 prop2
       out -> out
  compare1 (Exists {}) _ = GT
  compare1 _ (Exists {}) = LT
  compare1 (Call { callArgs = args1, callFunc = func1 })
           (Call { callArgs = args2, callFunc = func2 }) =
    case compare1 func1 func2 of
      EQ -> compare1 args1 args2
      out -> out
  compare1 (Call {}) _ = GT
  compare1 _ (Call {}) = LT
  compare1 (Var { varSym = sym1 }) (Var { varSym = sym2 }) = compare sym1 sym2
  compare1 (Var {}) _ = GT
  compare1 _ (Var {}) = LT
  compare1 (Typed { typedTerm = term1, typedType = ty1 })
           (Typed { typedTerm = term2, typedType = ty2 }) =
    case compare1 term1 term2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 (Typed {}) _ = GT
  compare1 _ (Typed {}) = LT
  compare1 (Eta { etaTerm = term1, etaType = ty1 })
           (Eta { etaTerm = term2, etaType = ty2 }) =
    case compare1 term1 term2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 (Eta {}) _ = GT
  compare1 _ (Eta {}) = LT
  compare1 (Record { recVals = vals1 }) (Record { recVals = vals2 }) =
    compare1 vals1 vals2
  compare1 (Record {}) _ = GT
  compare1 _ (Record {}) = LT
  compare1 (Fix { fixComps = comps1 }) (Fix { fixComps = comps2 }) =
    compare1 comps1 comps2
  compare1 (Fix {}) _ = GT
  compare1 _ (Fix {}) = LT
  compare1 (BadTerm _) (BadTerm _) = EQ

instance Ord b => Ord1 (Cmd b) where
  compare1 (Value { valTerm = term1 }) (Value { valTerm = term2 }) =
    compare1 term1 term2
  compare1 (Value {}) _ = GT
  compare1 _ (Value {}) = LT
  compare1 (Eval { evalTerm = term1 }) (Eval { evalTerm = term2 }) =
    compare1 term1 term2
  compare1 (Eval {}) _ = GT
  compare1 _ (Eval {}) = LT
  compare1 (BadCmd _) (BadCmd _) = EQ

instance Ord b => Ord1 (Comp b) where
  compare1 (Seq { seqPat = pat1, seqCmd = cmd1, seqNext = next1 })
          (Seq { seqPat = pat2, seqCmd = cmd2, seqNext = next2 }) =
    case compare1 pat1 pat2 of
      EQ -> case compare1 cmd1 cmd2 of
        EQ -> compare1 next1 next2
        out -> out
      out -> out
  compare1 (Seq {}) _ = GT
  compare1 _ (Seq {}) = LT
  compare1 (End { endCmd = cmd1 }) (End { endCmd = cmd2 }) = compare1 cmd1 cmd2
  compare1 (End {}) _ = GT
  compare1 _ (End {}) = LT
  compare1 (BadComp _) (BadComp _) = EQ

instance Position (t s) => Position (Binding b t s) where
  pos (Project { projectPos = p }) = p
  pos (Construct { constructPos = p }) = p
  pos (As { asPos = p }) = p
  pos (Name { namePos = p }) = p
  pos (Constant t) = pos t

instance Position (Pattern b t s) where
  pos (Pattern { patternPos = p }) = p

instance Position (Term b s) where
  pos (ProdType { prodTypePos = p }) = p
  pos (SumType { sumTypePos = p }) = p
  pos (RefineType { refineTypePos = p }) = p
  pos (CompType { compTypePos = p }) = p
  pos (Forall { forallPos = p }) = p
  pos (Exists { existsPos = p }) = p
  pos (Call { callPos = p }) = p
  pos (Var { varPos = p }) = p
  pos (Typed { typedPos = p }) = p
  pos (Eta { etaPos = p }) = p
--  pos (Lambda { lambdaPos = p }) = p
  pos (Record { recPos = p }) = p
  pos (Fix { fixPos = p }) = p
  pos (BadTerm p) = p

instance Position (Cmd b s) where
  pos (Eval { evalPos = p }) = p
  pos (Value { valPos = p }) = p
  pos (BadCmd p) = p

instance Position (Comp b s) where
  pos (Seq { seqPos = p }) = p
  pos (End { endPos = p }) = p
  pos (BadComp p) = p

-- XXX orphan instances of hashable for Bound structures.  Move these
-- into a separate place.
instance (Hashable b, Hashable a) => Hashable (Var b a) where
  hash (B b) = hash b
  hash (F a) = hash a

instance (Hashable b, Hashable (f (Var b a)), Hashable a, Monad f) =>
         Hashable (Scope b f a) where
  hash s = hash (fromScope s)

instance (Hashable b, Hashable s, Hashable (t s)) =>
         Hashable (Binding b t s) where
  hash (Project { projectBinds = binds, projectStrict = strict }) =
    hashInt 1 `combine` hash strict `combine` hash binds
  hash (Construct { constructBinds = binds, constructStrict = strict }) =
    hashInt 2 `combine` hash strict `combine` hash binds
  hash (As { asName = name, asBind = bind }) =
    hashInt 3 `combine` hash name `combine` hash bind
  hash (Name { nameSym = name }) = hashInt 4 `combine` hash name
  hash (Constant c) = hashInt 5 `combine` hash c

instance (Hashable b, Hashable s, Hashable (t s)) =>
         Hashable (Pattern b t s) where
  hash (Pattern { patternBind = term, patternType = ty }) =
    hash term `combine` hash ty

instance (Hashable b, Hashable s) => Hashable (Term b s) where
  hash (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                   prodRetTy = retty }) =
    hashInt 1 `combine` hash initty `combine`
    hash argtys `combine` hash retty
  hash (SumType { sumInit = initty, sumBody = body }) =
    hashInt 2 `combine` hash initty `combine` hash body
  hash (RefineType { refinePat = pat, refineProp = prop }) =
    hashInt 3 `combine` hash pat `combine` hash prop
  hash (CompType { compPat = pat, compSpec = spec }) =
    hashInt 4 `combine` hash pat `combine` hash spec
  hash (Forall { forallPats = pats, forallProp = prop }) =
    hashInt 5 `combine` hash pats `combine` hash prop
  hash (Exists { existsPats = pats, existsProp = prop }) =
    hashInt 6 `combine` hash pats `combine` hash prop
  hash (Call { callArgs = args, callFunc = func }) =
    hashInt 7 `combine` hash args `combine` hash func
  hash (Var { varSym = sym }) = hashInt 8 `combine` hash sym
  hash (Typed { typedTerm = term, typedType = ty }) =
    hashInt 9 `combine` hash term `combine` hash ty
  hash (Eta { etaTerm = term, etaType = ty }) =
    hashInt 10 `combine` hash term `combine` hash ty
--  hash (Lambda {}) = hashInt 11
  hash (Record { recVals = vals }) = hashInt 12 `combine` hash vals
  hash (Fix { fixComps = comps }) =
    hashInt 13 `combine` hash comps
  hash (BadTerm _) = hashInt 0

instance (Hashable b, Hashable s) => Hashable (Cmd b s) where
  hash (Value { valTerm = term }) = hashInt 1 `combine` hash term
  hash (Eval { evalTerm = term }) = hashInt 2 `combine` hash term
  hash (BadCmd _) = hashInt 0

instance (Hashable b, Hashable s) => Hashable (Comp b s) where
  hash (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) =
    hashInt 1 `combine` hash pat `combine` hash cmd `combine` hash next
  hash (End { endCmd = cmd }) = hashInt 2 `combine` hash cmd
  hash (BadComp _) = hashInt 0

instance Functor t => Functor (Binding b t) where
  fmap f b @ (Project { projectBinds = binds }) =
    b { projectBinds = fmap (\(name, bind) -> (name, fmap f bind)) binds }
  fmap f b @ (Construct { constructBinds = binds }) =
    b { constructBinds = fmap (\(name, bind) -> (name, fmap f bind)) binds }
  fmap f b @ (As { asBind = bind }) = b { asBind = fmap f bind }
  fmap _ b @ (Name { nameSym = name }) = b { nameSym = name }
  fmap f (Constant t) = Constant (fmap f t)

instance Functor t => Functor (Pattern b t) where
  fmap f p @ (Pattern { patternBind = term, patternType = ty }) =
    p { patternBind = fmap f term, patternType = fmap f ty }

instance Functor (Term b) where
  fmap f t @ (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                         prodRetTy = retty }) =
    t { prodInitArgTy = fmap f initty, prodArgTys = fmap (fmap f) argtys,
        prodRetTy = fmap f retty }
  fmap f t @ (SumType { sumInit = initty, sumBody = body }) =
    t { sumInit = fmap f initty, sumBody = fmap (fmap f) body }
  fmap f t @ (RefineType { refinePat = pat, refineProp = prop }) =
    t { refinePat = fmap f pat, refineProp = fmap f prop }
  fmap f t @ (CompType { compPat = pat, compSpec = spec }) =
    t { compPat = fmap f pat, compSpec = fmap f spec }
  fmap f t @ (Forall { forallPats = pats, forallProp = prop }) =
    t { forallPats = fmap (fmap f) pats, forallProp = fmap f prop }
  fmap f t @ (Exists { existsPats = pats, existsProp = prop }) =
    t { existsPats = fmap (fmap f) pats, existsProp = fmap f prop }
  fmap f t @ (Call { callArgs = args, callFunc = func }) =
    t { callArgs = fmap (\(name, arg) -> (name, fmap f arg)) args,
        callFunc = fmap f func }
  fmap f t @ (Var { varSym = sym }) = t { varSym = f sym }
  fmap f t @ (Typed { typedTerm = term, typedType = ty }) =
    t { typedTerm = fmap f term, typedType = fmap f ty }
  fmap f t @ (Eta { etaTerm = term, etaType = ty }) =
    t { etaTerm = fmap f term, etaType = fmap f ty }
--  fmap f t @ (Lambda {}) = t {}
  fmap f t @ (Record { recVals = vals }) =
    t { recVals = fmap (\(name, term) -> (name, fmap f term)) vals }
  fmap f t @ (Fix { fixComps = comps }) =
    t { fixComps = fmap (fmap (fmap f)) comps }
  fmap _ (BadTerm p) = (BadTerm p)

instance Functor (Cmd b) where
  fmap f c @ (Eval { evalTerm = term }) = c { evalTerm = fmap f term }
  fmap f c @ (Value { valTerm = term }) = c { valTerm = fmap f term }
  fmap _ (BadCmd p) = BadCmd p

instance Functor (Comp b) where
  fmap f c @ (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) =
    c { seqPat = fmap f pat, seqCmd = fmap f cmd, seqNext = fmap f next }
  fmap f c @ (End { endCmd = cmd }) = c { endCmd = fmap f cmd }
  fmap _ (BadComp p) = BadComp p

instance Foldable t => Foldable (Binding b t) where
  foldMap f (Project { projectBinds = binds }) =
    foldMap (foldMap f . snd) binds
  foldMap f (Construct { constructBinds = binds }) =
    foldMap (foldMap f . snd) binds
  foldMap f (As { asBind = bind }) = foldMap f bind
  foldMap f (Constant t) = foldMap f t
  foldMap _ _ = mempty

instance Foldable t => Foldable (Pattern b t) where
  foldMap f (Pattern { patternBind = term, patternType = ty }) =
    foldMap f term `mappend` foldMap f ty

instance Foldable (Term b) where
  foldMap f (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                        prodRetTy = retty }) =
    foldMap f initty `mappend` foldMap (foldMap f) argtys `mappend`
    foldMap f retty
  foldMap f (SumType { sumInit = initty, sumBody = body }) =
    foldMap f initty `mappend` foldMap (foldMap f) body
  foldMap f (RefineType { refinePat = pat, refineProp = prop }) =
    foldMap f pat `mappend` foldMap f prop
  foldMap f (CompType { compPat = pat, compSpec = spec }) =
    foldMap f pat `mappend` foldMap f spec
  foldMap f (Forall { forallPats = pats, forallProp = prop }) =
    foldMap (foldMap f) pats `mappend` foldMap f prop
  foldMap f (Exists { existsPats = pats, existsProp = prop }) =
    foldMap (foldMap f) pats `mappend` foldMap f prop
  foldMap f (Call { callArgs = args, callFunc = func }) =
    foldMap (\(_, term) -> foldMap f term) args `mappend` foldMap f func
  foldMap f (Var { varSym = sym }) = f sym
  foldMap f (Typed { typedTerm = term, typedType = ty }) =
    foldMap f term `mappend` foldMap f ty
  foldMap f (Eta { etaTerm = term, etaType = ty }) =
    foldMap f term `mappend` foldMap f ty
--  foldMap f t @ (Lambda {}) = t {}
  foldMap f (Record { recVals = vals }) =
    foldMap (\(_, term) -> foldMap f term) vals
  foldMap f (Fix { fixComps = comps }) =
    foldMap (\(_, comp) -> foldMap f comp) comps
  foldMap _ _ = mempty

instance Foldable (Cmd b) where
  foldMap f (Value { valTerm = term }) = foldMap f term
  foldMap f (Eval { evalTerm = term }) = foldMap f term
  foldMap _ _ = mempty

instance Foldable (Comp b) where
  foldMap f (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) =
    foldMap f pat `mappend` foldMap f cmd `mappend` foldMap f next
  foldMap f (End { endCmd = cmd }) = foldMap f cmd
  foldMap _ _ = mempty

instance Traversable t => Traversable (Binding b t) where
  traverse f b @ (Project { projectBinds = binds }) =
    (\binds' -> b { projectBinds = binds' }) <$>
      traverse (\(name, bind) ->
                 (\bind' -> (name, bind')) <$>
                   traverse f bind) binds
  traverse f b @ (Construct { constructBinds = binds }) =
    (\binds' -> b { constructBinds = binds' }) <$>
      traverse (\(name, bind) ->
                 (\bind' -> (name, bind')) <$>
                   traverse f bind) binds
  traverse f b @ (As { asBind = bind }) =
    (\bind' -> b { asBind = bind' }) <$> traverse f bind
  traverse f (Constant t) = Constant <$> traverse f t
  traverse _ b @ (Name { nameSym = name }) = pure (b { nameSym = name })

instance Traversable t => Traversable (Pattern b t) where
  traverse f p @ (Pattern { patternBind = term, patternType = ty }) =
    (\term' ty' -> p { patternBind = term', patternType = ty' }) <$>
      traverse f term <*> traverse f ty

instance Traversable (Term b) where
  traverse f t @ (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                             prodRetTy = retty }) =
    (\initty' argtys' retty' -> t { prodInitArgTy = initty',
                                    prodArgTys = argtys',
                                    prodRetTy = retty' }) <$>
      traverse f initty <*> traverse (traverse f) argtys <*> traverse f retty
  traverse f t @ (SumType { sumInit = initty, sumBody = body }) =
    (\initty' body' -> t { sumInit = initty', sumBody = body' }) <$>
      traverse f initty <*> traverse (traverse f) body
  traverse f t @ (RefineType { refinePat = pat, refineProp = prop }) =
    (\pat' prop' -> t { refinePat = pat', refineProp = prop' }) <$>
      traverse f pat <*> traverse f prop
  traverse f t @ (CompType { compPat = pat, compSpec = spec }) =
    (\pat' spec' -> t { compPat = pat', compSpec = spec' }) <$>
      traverse f pat <*> traverse f spec
  traverse f t @ (Forall { forallPats = pats, forallProp = prop }) =
    (\pats' prop' -> t { forallPats = pats', forallProp = prop' }) <$>
      traverse (traverse f) pats <*> traverse f prop
  traverse f t @ (Exists { existsPats = pats, existsProp = prop }) =
    (\pats' prop' -> t { existsPats = pats', existsProp = prop' }) <$>
      traverse (traverse f) pats <*> traverse f prop
  traverse f t @ (Call { callArgs = args, callFunc = func }) =
    (\args' func' -> t { callArgs = args', callFunc = func' }) <$>
      traverse (\(name, arg) -> (\arg' -> (name, arg')) <$>
                 traverse f arg) args <*>
      traverse f func
  traverse f t @ (Var { varSym = sym }) =
    (\sym' -> t { varSym = sym' }) <$> f sym
  traverse f t @ (Typed { typedTerm = term, typedType = ty }) =
    (\term' ty' -> t { typedTerm = term', typedType = ty' }) <$>
      traverse f term <*> traverse f ty
  traverse f t @ (Eta { etaTerm = term, etaType = ty }) =
    (\term' ty' -> t { etaTerm = term', etaType = ty' }) <$>
      traverse f term <*> traverse f ty
--  fmap f t @ (Lambda {}) = t {}
  traverse f t @ (Record { recVals = vals }) =
    (\vals' -> t { recVals = vals' }) <$>
      traverse (\(name, term) ->
                 (\term' -> (name, term')) <$>
                   traverse f term) vals
  traverse f t @ (Fix { fixComps = comps }) =
    (\comps' -> t { fixComps = comps' }) <$>
      traverse (\(name, comp) -> (\comp' -> (name, comp')) <$>
                 traverse f comp) comps
  traverse _ (BadTerm p) = pure (BadTerm p)

instance Traversable (Cmd b) where
  traverse f c @ (Value { valTerm = term }) =
    (\term' -> c { valTerm = term' }) <$> traverse f term
  traverse f c @ (Eval { evalTerm = term }) =
    (\term' -> c { evalTerm = term' }) <$> traverse f term
  traverse _ (BadCmd c) = pure (BadCmd c)

instance Traversable (Comp b) where
  traverse f c @ (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) =
    (\pat' cmd' next' -> c { seqPat = pat', seqCmd = cmd',
                             seqNext = next' }) <$>
      traverse f pat <*> traverse f cmd <*> traverse f next
  traverse f c @ (End { endCmd = cmd }) =
    (\cmd' -> c { endCmd = cmd' }) <$> traverse f cmd
  traverse _ (BadComp p) = pure (BadComp p)

injectpos :: Pos
injectpos = internal "Monad return"

substpos :: Pos
substpos = internal "Monad substitution"

instance MonadTrans (Binding b) where
  lift m = Constant m

instance Bound (Binding b) where
  b @ (Project { projectBinds = binds }) >>>= f =
    b { projectBinds = fmap (\(name, bind) -> (name, bind >>>= f)) binds }
  b @ (Construct { constructBinds = binds }) >>>= f =
    b { constructBinds = fmap (\(name, bind) -> (name, bind >>>= f)) binds }
  b @ (As { asBind = bind }) >>>= f = b { asBind = bind >>>= f }
  b @ (Name { nameSym = name }) >>>= _ = b { nameSym = name }
  Constant t >>>= f = Constant (t >>= f)

instance Applicative (Term b) where
  pure = return
  (<*>) = ap

instance Applicative (Comp b) where
  pure = return
  (<*>) = ap

patSubstTerm :: (a -> Term c b) -> Pattern c (Term c) a ->
                Pattern c (Term c) b
patSubstTerm f p @ (Pattern { patternBind = bind, patternType = ty }) =
  p { patternBind = bind >>>= f, patternType = ty >>= f }

cmdSubstTerm :: (a -> Term c b) -> Cmd c a -> Cmd c b
cmdSubstTerm f c @ (Value { valTerm = term }) = c { valTerm = term >>= f }
cmdSubstTerm f c @ (Eval { evalTerm = term }) = c { evalTerm = term >>= f }
cmdSubstTerm _ (BadCmd p) = BadCmd p

compSubstTerm :: (a -> Term c b) -> Comp c a -> Comp c b
compSubstTerm f c @ (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) =
  c { seqPat = patSubstTerm f pat, seqCmd = cmdSubstTerm f cmd,
      seqNext = compSubstTerm f next }
compSubstTerm f c @ (End { endCmd = cmd }) =
  c { endCmd = cmdSubstTerm f cmd }
compSubstTerm _ (BadComp p) = BadComp p

instance Monad (Term b) where
  return sym = Var { varSym = sym, varPos = injectpos }

  t @ (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                  prodRetTy = retty }) >>= f =
    t { prodInitArgTy = initty >>= f, prodArgTys = fmap (>>>= f) argtys,
        prodRetTy = retty >>>= f }
  t @ (SumType { sumInit = initty, sumBody = body }) >>= f =
    t { sumInit = initty >>= f, sumBody = fmap (>>>= f) body }
  t @ (RefineType { refinePat = pat, refineProp = prop }) >>= f =
    t { refinePat = patSubstTerm f pat, refineProp = prop >>>= f }
  t @ (CompType { compPat = pat, compSpec = spec }) >>= f =
    t { compPat = patSubstTerm f pat, compSpec = spec >>>= f }
  t @ (Forall { forallPats = pats, forallProp = prop }) >>= f =
    t { forallPats = fmap (patSubstTerm f) pats, forallProp = prop >>>= f }
  t @ (Exists { existsPats = pats, existsProp = prop }) >>= f =
    t { existsPats = fmap (patSubstTerm f) pats, existsProp = prop >>>= f }
  t @ (Call { callArgs = args, callFunc = func }) >>= f =
    t { callArgs = fmap (\(name, arg) -> (name, arg >>= f)) args,
        callFunc = func >>= f }
  Var { varSym = sym } >>= f = f sym
  t @ (Typed { typedTerm = term, typedType = ty }) >>= f =
    t { typedTerm = term >>= f, typedType = ty >>= f }
--  fmap f t @ (Lambda {}) = t {}
  t @ (Record { recVals = vals }) >>= f =
    t { recVals = fmap (\(name, term) -> (name, term >>= f)) vals }
  t @ (Fix { fixComps = comps }) >>= f =
    t { fixComps = fmap (\(name, comp) ->
                          (name, comp >>>= compSubstTerm f . return)) comps }
  t @ (Eta { etaTerm = term, etaType = ty }) >>= f =
    t { etaTerm = term >>= f, etaType = ty >>= f }
  BadTerm p >>= _ = BadTerm p

bindSubstComp :: (a -> Comp c b) -> Binding c (Term c) a ->
                Binding c (Term c) b
bindSubstComp f b @ (Project { projectBinds = binds }) =
  b { projectBinds = fmap (\(name, bind) ->
                            (name, bindSubstComp f bind)) binds }
bindSubstComp f b @ (Construct { constructBinds = binds }) =
  b { constructBinds = fmap (\(name, bind) ->
                              (name, bindSubstComp f bind)) binds }
bindSubstComp f b @ (As { asBind = bind }) =
  b { asBind = bindSubstComp f bind }
bindSubstComp _ b @ (Name { nameSym = name }) = b { nameSym = name }
bindSubstComp f (Constant t) = Constant (termSubstComp f t)


patSubstComp :: (a -> Comp c b) -> Pattern c (Term c) a ->
                Pattern c (Term c) b
patSubstComp f p @ (Pattern { patternBind = bind, patternType = ty }) =
  p { patternBind = bindSubstComp f bind, patternType = termSubstComp f ty }

termSubstComp :: (a -> Comp c b) -> Term c a -> Term c b
termSubstComp f t @ (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                                prodRetTy = retty }) =
  t { prodInitArgTy = termSubstComp f initty,
      prodArgTys = fmap (>>>= termSubstComp f . return) argtys,
      prodRetTy = retty >>>= termSubstComp f . return }
termSubstComp f t @ (SumType { sumInit = initty, sumBody = body }) =
  t { sumInit = termSubstComp f initty,
      sumBody = fmap (>>>= termSubstComp f . return) body }
termSubstComp f t @ (RefineType { refinePat = pat, refineProp = prop }) =
  t { refineProp = prop >>>= termSubstComp f . return,
      refinePat = patSubstComp f pat }
termSubstComp f t @ (CompType { compPat = pat, compSpec = spec }) =
  t { compSpec = spec >>>= termSubstComp f . return,
      compPat = patSubstComp f pat }
termSubstComp f t @ (Forall { forallPats = pats, forallProp = prop }) =
  t { forallProp = prop >>>= termSubstComp f . return,
      forallPats = fmap (patSubstComp f) pats }
termSubstComp f t @ (Exists { existsPats = pats, existsProp = prop }) =
  t { existsProp = prop >>>= termSubstComp f . return,
      existsPats = fmap (patSubstComp f) pats }
termSubstComp f t @ (Call { callArgs = args, callFunc = func }) =
  t { callArgs = fmap (\(name, arg) ->
                        (name, arg >>= termSubstComp f . return)) args,
      callFunc = func >>= termSubstComp f . return }
termSubstComp f (Var { varSym = sym }) =
  Fix { fixComps = [(Nothing, abstract (\_ -> Nothing) (f sym))],
        fixPos = substpos }
termSubstComp f t @ (Typed { typedTerm = term, typedType = ty }) =
  t { typedTerm = termSubstComp f term, typedType = termSubstComp f ty }
--  fmap f t @ (Lambda {}) = t {}
termSubstComp f t @ (Record { recVals = vals }) =
  t { recVals = fmap (\(name, term) -> (name, termSubstComp f term)) vals }
termSubstComp f t @ (Fix { fixComps = comps }) =
  t { fixComps = fmap (\(name, comp) -> (name, comp >>>= f)) comps }
termSubstComp f t @ (Eta { etaTerm = term, etaType = ty }) =
    t { etaTerm = termSubstComp f term, etaType = termSubstComp f ty }
termSubstComp _ (BadTerm p) = BadTerm p

cmdSubstComp :: (a -> Comp c b) -> Cmd c a -> Cmd c b
cmdSubstComp f c @ (Value { valTerm = term }) =
  c { valTerm = termSubstComp f term }
cmdSubstComp f c @ (Eval { evalTerm = term }) =
  c { evalTerm = termSubstComp f term }
cmdSubstComp _ (BadCmd p) = BadCmd p

instance Monad (Comp b) where
  return sym =
    End { endCmd = Value { valTerm = Var { varSym = sym, varPos = injectpos },
                           valPos = injectpos },
          endPos = injectpos}

  c @ (Seq { seqPat = pat, seqCmd = cmd, seqNext = next }) >>= f =
    c { seqPat = patSubstComp f pat, seqNext = next >>= f,
        seqCmd = cmdSubstComp f cmd }
  c @ (End { endCmd = cmd }) >>= f = c { endCmd = cmdSubstComp f cmd }
  BadComp p >>= _ = BadComp p
