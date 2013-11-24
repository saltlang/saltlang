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

-- | A module defining a monad class for implementing the type checker
module Control.Monad.TypeCheck.Class(
       MonadTypeCheck(..)
       ) where

import Control.Monad.ProofObligations.Class
import Control.Monad.TypeErrors.Class
import Language.Salt.Core.Syntax

-- | A monad class encapsulating all context information needed by the
-- type checker
class (MonadProofObligations m, MonadTypeErrors s m) =>
      MonadTypeCheck s m where
  -- | The type of propositions
  propType :: m (Term sym sym)
  -- | The type of types
  typeType :: m (Term sym sym)
