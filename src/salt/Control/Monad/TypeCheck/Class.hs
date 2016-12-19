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

-- | A module defining a monad class for implementing the type checker
module Control.Monad.TypeCheck.Class(
       MonadTypeCheck(..),
       MonadElimSynth(..),
       MonadIntroCheck(..)
       ) where

import Data.Symbol
import Language.Salt.Core.Syntax
import Language.Salt.Message

class Monad m => MonadTypeCheck m where
  -- | Synthesize the type of an 'Elim' terms.  These terms produce a
  -- type during type checking, as opposed to 'Intro' terms, which
  -- require a type against which to check.
  synthElim :: Elim Symbol Symbol
            -- ^ The term whose type to synthesize.
            -> m (Intro Symbol Symbol)
            -- ^ The type of the checked term.

  checkIntro :: Intro Symbol Symbol
             -- ^ The term to check.
             -> Intro Symbol Symbol
             -- ^ The type against which to check.
             -> m ()

  checkCase :: Case Symbol Symbol
            -- ^ The case to check.
            -> Intro Symbol Symbol
            -- ^ The type against which to check.
            -> m ()

  checkProp :: Intro Symbol Symbol
            -> m ()

  -- | Check that a given 'Intro' term is a type of some rank.
  checkType :: Intro Symbol Symbol
            -- ^ The term to check.
            -> m ()

  -- | Get the best supertype of a type that has a functional form.
  -- In essence, this computes the join of the argument type with @top
  -- -> bottom@.
  typeJoinFunc :: Intro Symbol Symbol
               -- ^ The type for which to get a function form.
               -> m (Intro Symbol Symbol)

  -- | Check that the first type is a subtype of the second.  The term
  -- must also be supplied, so as to be able to generate proof
  -- obligations for refinement types.
  checkSubtype :: Intro Symbol Symbol
               -- ^ The term having the supposed subtype.
               -> Intro Symbol Symbol
               -- ^ The supposed subtype.
               -> Intro Symbol Symbol
               -- ^ The supposed supertype.
               -> m ()

  -- | Enter an assertion into the truth environment.
  assertion :: Intro Symbol Symbol
            -> m ()

  -- | Report a type error through the speculative type checking
  -- mechanism.  If this is called wrapped in a 'speculative' call,
  -- then any 'Message' whose severity is 'Error' will instead cause
  -- the speculative action to fail.  Any 'Message' of lower severity
  -- will be reported only if the speculative action succeeds.
  typeError :: Message
            -- ^ The 'Message' to report.
            -> m ()

  -- | Speculatively perform an action.  This will squealch all error
  -- reporting through 'typeError'; any errors logged in this way will
  -- instead cause the result of the speculative action to fail.
  -- Errors not reported through 'typeError' (such as internal errors)
  -- will be reported normally.
  --
  -- Any assertions or messages of lower severity than errors will
  -- only take effect if the speculative action succeeds.  If it
  -- fails, they will be cancelled.
  speculative :: m a
              -- ^ Action to perform speculatively.
              -> m (Maybe a)
              -- ^ 'Just' result on success, 'Nothing' if the action
              -- fails.

-- | Class of monads that implement type synthesis rules for 'Elim'
-- terms.
class MonadTypeCheck m => MonadElimSynth m where
  -- | Apply the synthCall rule to synthesize the type of a 'Call'
  -- term.  This will cause an internal error if the goal is anything
  -- other than a 'Call'.
  --
  -- The formal statement of the type rule is as follows:
  --
  -- >  E |- f => Pi(argtys, retty)   E |- arg <= Sigma(argtys)
  -- > ---------------------------------------------------------
  -- >           E |- f arg => subst(arg, argty, retty)
  synthCall :: m (Intro Symbol Symbol)
            -- ^ The type of the 'Call' term.

  -- | Apply the synthTyped rule to synthesize the type of a 'Typed'
  -- term.  This will cause an internal error if the goal is anything
  -- other than a 'Typed'.
  --
  -- The formal statement of the type rule is as follows:
  --
  -- >  E |- ty <= type   E |- term <= ty
  -- > -----------------------------------
  -- >       E |- term : ty => ty
  synthTyped :: m (Intro Symbol Symbol)
             -- ^ The type of the 'Typed' term.

  -- | Apply the synthVar rule to synthesize the type of a 'Var' term.
  -- This will cause an internal error if the goal is anything other
  -- than a 'Var'.
  --
  -- The formal statement of the type rule is as follows:
  --
  -- >
  -- > -----------------
  -- >  x : t |- x => t
  synthVar :: m (Intro Symbol Symbol)
           -- ^ The type of the 'Var' term.

class MonadTypeCheck m => MonadIntroCheck m where
  -- | Apply the checkElim rule to the current goal state.  This will
  -- generate an actual type using type synthesis and check that it is
  -- a subtype of the expected type.  This will cause an internal
  -- error if the goal term is anything other than an 'Elim'.
  --
  -- The formal statement of the type rule is as follows:
  --
  -- >  E |- term => ty'   (term : ty') :> ty
  -- > ---------------------------------------
  -- >            E |- term <= ty
  checkElim :: m ()

  -- | Apply the checkQuantified rule to the current goal state.  This
  -- will assert that the goal type is a subtype of prop, that the
  -- quantifier type is a well-formed type, and that the cases all
  -- have the type of a function from the quantifier type to prop.
  -- This will cause an internal error if the goal term is anything
  -- other than an 'Elim'.
  --
  -- The formal statement of the type rule is as follows:
  --
  -- >  ty :> prop   E |- qty <= type   E |- case_i <= Pi(qty, ty)
  -- > ------------------------------------------------------------
  -- >               E |- quant qty. [case_i] <= ty
  checkQuantified :: m ()

  -- | Apply the checkLambda rule to the current goal.  This check a
  -- lambda term's cases against the expected type, and check for case
  -- completeness.  This will cause an internal error if the goal term is
  -- anything other than a 'Lambda'.
  --
  -- Corresponds to the following type rule:
  --
  -- >   E |- case_i <= ty
  -- > ----------------------
  -- >  E |- \[case_i] <= ty
  checkLambda :: m ()
