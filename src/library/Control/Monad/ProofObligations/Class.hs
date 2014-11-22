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

-- | A class of monads for outputting proof obligations.
module Control.Monad.ProofObligations.Class(
       MonadProofObligations(..)
       ) where

import Language.Salt.Core.Syntax

-- | Monad class for outputting proof obligations during type
-- checking.
class Monad m => MonadProofObligations m where
  -- | Emit a proof obligation.
  proofObligation :: Term bound free
                  -- ^ The proposition that needs to be proven.
                  -> m ()
