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
module Control.Monad.ProofErrors.Class(
       MonadProofErrors(..)
       ) where

import Data.Pos
import Language.Salt.Core.Syntax

class Monad m => MonadProofErrors sym m where
  -- | Log an error when using exact with an undefined symbol.
  undefProp :: Pos
            -- ^ The position from which this arises.
            -> sym
            -- ^ The name of the undefined proposition.
            -> m ()
  -- | Log an error when using exact, but the proposition in the truth
  -- environment doesn't match the goal.
  exactMismatch :: Pos
                -- ^ The position from which this arises.
                -> sym
                -- ^ The name of the proposition in the proof environment.
                -> Term sym sym
                -- ^ The proposition from the proof environment.
                -> Term sym sym
                -- ^ The goal proposition.
                -> m ()
  -- | Log an error when using intro, but the goal isn't an implication.
  introMismatch :: Pos
                -- ^ The position from which this arises.
                -> Term sym sym
                -- ^ The goal proposition.
                -> m ()
  -- | Log an error when using introVar, but the goal isn't a forall.
  introVarMismatch :: Pos
                   -- ^ The position from which this arises.
                   -> Term sym sym
                   -- ^ The goal proposition.
                   -> m ()
