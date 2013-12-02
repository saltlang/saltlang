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

-- | A module containing representations of the state of a proof.
module Language.Salt.Core.Proofs.ProofState(
       ProofState(..),
       Goal(..)
       ) where

import Data.Map
import Language.Salt.Core.Syntax

-- | A representation of a proof state.
data ProofState sym =
  ProofState {
    -- | The goals that need to be proven.
    proofGoals :: [Goal sym]
  }

-- | A representation of an individual proof goal.
data Goal sym =
  Goal {
    -- | The current goal that needs to be proven.
    goalProp :: Term sym sym,
    -- | The current known truths.
    goalTruthCtx :: Map sym (Term sym sym),
    -- | The current known types.
    goalTypeCtx :: Map sym (Term sym sym)
  }
