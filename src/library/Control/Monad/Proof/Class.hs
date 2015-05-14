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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | A module defining a monad class for recording or checking proofs.
module Control.Monad.Proof.Class(
       MonadProof(..)
       ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map(Map)
import Data.Pos
import Language.Salt.Core.Syntax

-- | This is a monad class containing rules for the five axioms of the
-- intuitionistic predicate logic upon which the Salt language is
-- built.
--
-- This class is only designed with checking or recording proofs in
-- mind, not synthesizing them (ie in an automated prover).  Hence the
-- monad produces no usable value, the assumption being that whatever
-- artifact is produced is contained within the monad.
--
-- One use of this is to simply assert the applicability of these
-- rules, then update the truth and type contexts and the goals
-- accordingly, and emit error messages whenever a rule fails to
-- apply.
--
-- Another possible use is simply to emit proof scripts for Coq or a
-- similar prover.  The type checker can use this to produce a "flight
-- record", or to sanity-check itself with the proof checker as it
-- goes.
--
-- An automated prover could likewise use this as a separate module to
-- encapsulate its internal state, and die with an error if a rule
-- fails to apply (presumably, it would have another monad class that
-- allows it to examine the goal and context state to determine if a
-- rule is applicable).
--
-- The names of these axioms are borrowed in part
-- from Coq tactics.
class Monad m => MonadProof sym m where
  -- |
  --   -------------
  --    Env, P |- P
  assumption :: Pos
             -- ^ The position from which this originates.
             -> sym
             -- ^ The name of the equivalent proposition in the truth
             -- environment
             -> m ()

  -- |   Env, P |- Q
  --   ---------------
  --    Env |- P -> Q
  intro :: Pos
        -- ^ The position from which this originates.
        -> sym
        -- ^ The name in the truth environment to give the left hand side
        -> m ()

  -- |  Env |- P -> Q    Env |- P
  --   ---------------------------
  --            Env |- Q
  cut :: Pos
      -- ^ The position from which this originates.
      -> Term sym sym
      -- ^ The proposition by which to do the cut
      -> m ()

  -- |  Env |- forall (pattern) : T. P   Env |- V : T
  --   -----------------------------------------------
  --                Env |- [V/(pattern)]P
  apply :: Pos
        -- ^ The position from which this originates.
        -> Term sym sym
        -- ^ The proposition to apply
        -> [Term sym sym]
        -- ^ The argument to the application
        -> m ()

  -- |  Env, x_1 : T_1 ... x_n : T_n |- P
  --   -----------------------------------
  --     Env |- forall (pattern) : T. P
  introVars :: Pos
            -- ^ The position from which this originates.
            -> [Map sym sym]
            -- ^ A list of symbol renaming functions, one for each
            -- case, that possibly rename all symbols bound by that
            -- case.  Must be as many elements in the list as there
            -- are cases in the predicate.
            -> m ()

instance MonadProof sym m => MonadProof sym (ReaderT s m) where
  assumption p = lift . assumption p
  intro p = lift. intro p
  introVars p = lift . introVars p
  cut p = lift . cut p
  apply p prop = lift . apply p prop

instance MonadProof sym m => MonadProof sym (StateT s m) where
  assumption p = lift . assumption p
  intro p = lift. intro p
  introVars p = lift . introVars p
  cut p = lift . cut p
  apply p prop = lift . apply p prop

instance (Monoid s, MonadProof sym m) => MonadProof sym (WriterT s m) where
  assumption p = lift . assumption p
  intro p = lift. intro p
  introVars p = lift . introVars p
  cut p = lift . cut p
  apply p prop = lift . apply p prop
