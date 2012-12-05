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

-- | The Argent core language.  Argent's surface syntax is
-- transliterated into Core, which is then type-checked and compiled.
-- Core is generally not meant for human consumption.
module Language.Argent.Core(
       Term(..),
       Cmd(..),
       Comp(..)
       ) where

import Bound
import Data.Pos
import Data.Hash

-- | A binder.  Consists of a pattern and a type.  The symbol unused
-- may occur in pattern terms, in which case it acts as a wildcard.
data Bind s = Bind {
    -- | The pattern for binding.  This is an introduction term.
    bindPat :: Term s,
    -- | The type being bound.  This is an introduction term, which
    -- must be a type.
    bindType :: Term s,
      -- | The position in source from which this originates.
    bindPos :: !Pos
  }
  deriving (Ord, Eq)

-- | Terms.
data Term s =
  -- Types.  These do not support decidable equality.  As such, they
  -- cannot be the result of a computation, and cannot appear in
  -- patterns.

  -- | Dependent product type.  This is the type given to functions.
    ProdType {
      -- | The type of the first argument.
      prodInitArgTy :: Term s,
      -- | The types of subsequent arguments, which can reference all
      -- previous arguments
      prodArgTys :: [Scope () Term s],
      -- | The return type of the function.
      prodRetTy :: Scope () Term s,
      -- | The position in source from which this originates.
      prodTypePos :: !Pos
    }
  -- | Dependent sum type.  This is the type given to structures.
  | SumType {
      -- | The first element of the sum type.
      sumInit :: Term s,
      -- | The remaining elements of the sum type
      sumBody :: [Scope () Term s],
      -- | The position in source from which this originates.
      sumTypePos :: !Pos
    }
  -- | Refinement type.  This type represents all members of a type
  -- satisfying a given proposition.
  | RefineType {
      -- | The binder for values of the base type.
      refineBind :: Term s,
      -- | The proposition that members of this type must satisfy,
      -- which may reference bound variables from refineBindPat.
      refineProp :: Scope s Term s,
      -- | The position in source from which this originates.
      refineTypePos :: !Pos
    }
  -- | Computation type.  This type represents a computation, and
  -- includes both its result type and a specification of its
  -- behavior.
  | CompType {
      -- | The binder for the result of this computation.
      compBind :: Bind s,
      -- | The specification describing the computation's behavior.
      compSpec :: Term s,
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
      forallBinds :: [Bind s],
      -- | The core proposition.
      forallProp :: Scope s Term s,
      -- | The position in source from which this originates.
      forallPos :: !Pos
    }
  -- | Existential quantifier proposition.
  | Exists {
      -- | The bindings for the quantified proposition.  A list of
      -- (pattern, type) pairs.
      existsBinds :: [Bind s],
      -- | The core proposition.
      existsProp :: Scope s Term s,
      -- | The position in source from which this originates.
      existsPos :: !Pos
    }

  -- Elimination Terms.  These terms generate a type in type checking.

  -- | Call term.  Represents a call to a function.  The type of the
  -- term comes from the called function's return type.
  | Call {
      -- | The arguments to the call.  These are introduction terms.
      callArgs :: [Term s],
      -- | The function being called.  This must be an elimination
      -- term.
      callFunc :: Term s,
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
      typedTerm :: Term s,
      -- | The type of the introduction term.
      typedType :: Term s,
      -- | The position in source from which this originates.
      typedPos :: !Pos
    }

  -- Introduction Terms.  These terms require a type in type checking.

  -- | An eta expansion.  This is present for type checking only.
  -- This represents a "frozen" substitution.
  | Eta {
      etaTerm :: Term s,
      etaType :: Term s,
      -- | The position in source from which this originates.
      etaPos :: !Pos
    }
  -- | A lambda expression.  Represents a function value.  Lambdas
  -- cannot appear in patterns, though they can be computed on.
  | Lambda {
      -- | The position in source from which this originates.
      lambdaPos :: !Pos
    }
  -- | A collection of one or more computations, each of which is
  -- bound to a name.  Each of the members of the group may reference
  -- eachother.
  | Fix {
      fixComps :: [(s, Scope s Comp s)],
      -- | The position in source from which this originates.
      fixPos :: !Pos
    }
  -- | Placeholder for a malformed term, allowing type checking to
  -- continue in spite of errors.
  | BadTerm !Pos
    deriving (Eq, Ord)

-- | Commands
data Cmd s =
  -- | Evaluate a computation value.  This allows execution of
  -- computations produced by terms.
    Eval {
      -- | The computation value to evaluate
      evalTerm :: Term s,
      -- | The position in source from which this originates.
      evalPos :: !Pos
    }
  -- | Placeholder for a malformed command, allowing type checking to
  -- continue in spite of errors.
  | BadCmd !Pos
    deriving (Eq, Ord)

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
data Comp s =
  -- | A sequential composition of terms.
    Seq {
      -- | The pattern to which to bind the result of seqCmd.
      seqBind :: Bind s,
      -- | The command to execute.
      seqCmd :: Cmd s,
      -- | The next computation to execute.
      seqNext :: Comp s,
      -- | The position in source from which this originates.
      seqPos :: !Pos
    }
  -- | Result of a computation.  This is always the end of a sequence.
  | End {
      -- | The command to run to produce a result.
      endCmd :: Cmd s,
      -- | The position in source from which this originates.
      endPos :: !Pos
    }
  -- | Placeholder for a malformed computation, allowing type checking
  -- to continue in spite of errors.
  | BadComp !Pos
    deriving (Ord, Eq)

instance Position (Bind s) where
  pos (Bind { bindPos = p }) = p

instance Position (Term s) where
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
  pos (Lambda { lambdaPos = p }) = p
  pos (Fix { fixPos = p }) = p
  pos (BadTerm p) = p

instance Position (Cmd s) where
  pos (Eval { evalPos = p }) = p
  pos (BadCmd p) = p

instance Position (Comp s) where
  pos (Seq { seqPos = p }) = p
  pos (End { endPos = p }) = p
  pos (BadComp p) = p

instance Hashable s => Hashable (Bind s) where
  hash (Bind { bindPat = pat, bindType = ty }) = hash pat `combine` hash ty

instance Hashable s => Hashable (Term s) where
  hash (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                   prodRetTy = retty }) =
    hashInt 1 `combine` hash initty `combine` argtys `combine` retty
  hash (SumType { sumInit = init, sumBody = body }) =
    hashInt 2 `combine` hash init `combine` hash body
  hash (RefineType { refineBind = bind, refineProp = prop }) =
    hashInt 3 `combine` hash bind `combine` hash prop
  hash (CompType { compBind = bind, compSpec = spec }) =
    hashInt 4 `combine` hash bind `combine` hash spec
  hash (Forall { forallBinds = binds, forallProp = prop }) =
    hashInt 5 `combine` hash binds `combine` hash prop
  hash (Exists { existsBinds = binds, existsProp = prop }) =
    hashInt 6 `combine` hash binds `combine` hash prop
  hash (Call { callArgs = args, callFunc = func }) =
    hashInt 7 `combine` hash args `combine` hash func
  hash (Var { varSym = sym }) = hashInt 8 `combine` hash sym
  hash (Typed { typedTerm = term, typedType = ty }) =
    hashInt 9 `combine` hash term `combine` hash ty
  hash (Eta { etaTerm = term, etaType = ty }) =
    hashInt 10 `combine` hash term `combine` hash ty
  hash (Lambda {}) =
    hashInt 11
  hash (Fix { fixComps = comps }) =
    hashInt 12 `combine` hash comps
  hash (BadTerm _) = 0

instance Hashable s => Hashable (Cmd s) where
  hash (Eval { evalTerm = term }) = hash term
  hash (BadCmd _) = hashInt 0

instance Hashable s => Hashable (Comp s) where
  hash (Seq { seqBind = bind, seqCmd = cmd, seqNext = next }) =
    hashInt 1 `combine` hash bind `combine` hash cmd `combine` hash next
  hash (End { endCmd = cmd }) = hashInt 2 `combine` hash cmd
  hash (BadComp _) = hashInt 0

instance Functor Bind where
  fmap f b @ (Bind { bindPat = pat, bindType = ty }) =
    b { bindPat = fmap f pat, bindType = fmap f pat }

instance Functor Term where
  fmap f t @ (ProdType { prodInitArgTy = initty, prodArgTys = argtys,
                         prodRetTy = retty }) =
    t { prodInitArgTy = fmap f initty, prodArgTys = fmap f argtys,
        prodRetTy = fmap f retty }
  fmap f t @ (SumType { sumInit = init, sumBody = body }) =
    t { sumInit = fmap f init, sumBody = fmap f body }
  fmap f t @ (RefineType { refineBind = bind, refineProp = prop }) =
    t { refineBind = fmap f bind, refineProp = fmap f prop }
  fmap f t @ (CompType { compBind = bind, compSpec = spec }) =
    t { compBind = fmap f bind, compSpec = fmap f spec }
  fmap f t @ (Forall { forallBinds = binds, forallProp = prop }) =
    t { forallBinds = fmap f binds, forallProp = fmap f prop }
  fmap f t @ (Exists { forallBinds = binds, forallProp = prop }) =
    t { existsBinds = fmap f binds, existsProp = fmap f prop }
  fmap f t @ (Call { callArgs = args, callFunc = func }) =
    t { callArgs = fmap f args, callFunc = fmap f func }
  fmap f t @ (Var { varSym = sym }) = t { varSym = f sym }
  fmap f t @ (Typed { typedTerm = term, typedType = ty }) =
    t { typedTerm = fmap f term, typedType = fmap f ty }
  fmap f t @ (Eta { etaTerm = term, etaType = ty }) =
    t { etaTerm = fmap f term, etaType = fmap f ty }
--  fmap f t @ (Lambda {}) = t {}
  fmap f t @ (Fix { fixComps = comps }) = t { fixComps = fmap f comps }
  fmap _ t = t

instance Functor Cmd where
  fmap f c @ (Eval { evalTerm = term }) = c { evalTerm = fmap f term }
  fmap _ c = c

instance Functor Comp where
  fmap f c @ (Seq { seqBind = bind, seqCmd = cmd, seqNext = next }) =
    c { seqBind = fmap f bind, seqCmd = fmap f cmd, seqNext = fmap f next }
  fmap f c @ (End { endCmd = cmd }) = c { endCmd = fmap f cmd }
  fmap _ c = c