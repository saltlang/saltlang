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
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module defining a monad class for the proof checker.
module Control.Monad.ProofCheck.Class(
       MonadProofCheck(..)
       ) where

import Data.Map
import Language.Salt.Core.Syntax

-- | This is a monad class containing rules for the five axioms of the
-- intuitionistic predicate logic upon which the Salt language is
-- built.
-- 
-- This class is only designed with *checking* proofs in mind, not
-- synthesizing them.  Hence the monad produces no usable value, the
-- assumption being that whatever artifact is produced is contained
-- within the monad.
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
class Monad m => MonadProofCheck sym m where
  -- |
  --   -------------
  --    Env, P |- P
  exact :: sym
        -- ^ The name of the equivalent proposition in the truth environment
        -> m ()

  -- |   Env, P |- Q
  --   ---------------
  --    Env |- P -> Q
  intro :: sym
        -- ^ The name in the truth environment to give the left hand side
        -> m ()

  -- |  Env, x_1 : T_1 ... x_n : T_n |- P
  --   -----------------------------------
  --     Env |- forall (pattern) : T. P
  introVars :: m ()

  -- |  Env |- P -> Q    Env |- P
  --   ---------------------------
  --            Env |- Q
  cut :: Term sym sym
      -- ^ The proposition by which to do the cut
      -> m ()

  -- |  Env |- forall (pattern) : T. P   Env |- V : T
  --   -----------------------------------------------
  --                 Env |- [V/(pattern)]P
  apply :: Map sym (Term sym sym)
        -- ^ The arguments to the application
        -> m ()
