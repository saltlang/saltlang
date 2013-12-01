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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | A module defining a monad class for recording or checking proofs.
module Control.Monad.Proof.Class(
       MonadProtoProof(..),
       MonadProof(..)
       ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
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
class MonadProtoProof m => MonadProof sym m where
  -- |
  --   -------------
  --    Env, P |- P
  exact :: Pos
        -- ^ The position from which this originates.
        -> sym
        -- ^ The name of the equivalent proposition in the truth environment
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
  --                 Env |- [V/(pattern)]P
  apply :: Pos
        -- ^ The position from which this originates.
        -> Term sym sym
        -- ^ The proposition to apply
        -> Term sym sym
        -- ^ The arguments to the application
        -> m ()

-- | This class contains proof rules that don't depend on a symbol
-- type.
class Monad m => MonadProtoProof m where
  -- |  Env, x_1 : T_1 ... x_n : T_n |- P
  --   -----------------------------------
  --     Env |- forall (pattern) : T. P
  introVars :: Pos
            -- ^ The position from which this originates.
            -> m ()

instance MonadProtoProof m => MonadProtoProof (ReaderT s m) where
  introVars = lift . introVars

instance MonadProof sym m => MonadProof sym (ReaderT s m) where
  exact p = lift . exact p
  intro p = lift. intro p
  cut p = lift . cut p
  apply p prop = lift . apply p prop

instance MonadProtoProof m => MonadProtoProof (StateT s m) where
  introVars = lift . introVars

instance MonadProof sym m => MonadProof sym (StateT s m) where
  exact p = lift . exact p
  intro p = lift. intro p
  cut p = lift . cut p
  apply p prop = lift . apply p prop

instance (Monoid s, MonadProtoProof m) => MonadProtoProof (WriterT s m) where
  introVars = lift . introVars

instance (Monoid s, MonadProof sym m) => MonadProof sym (WriterT s m) where
  exact p = lift . exact p
  intro p = lift. intro p
  cut p = lift . cut p
  apply p prop = lift . apply p prop
