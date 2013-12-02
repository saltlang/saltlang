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

module Control.Monad.ProofNames.Class(
       MonadProofNames(..)
       ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Language.Salt.Core.Syntax

-- | A monad class for names used by the proof checker.
class Monad m => MonadProofNames sym m where
  -- | The term representing the implies proposition.
  impliesProp :: m (Term sym sym)
  -- | The symbol for the name "premise", an argument to the implies
  -- function.
  premiseName :: m sym
  -- | The symbol for the name "consequence", an argument to the
  -- implies function.
  consequenceName :: m sym

instance MonadProofNames sym m => MonadProofNames sym (ReaderT s m) where
  impliesProp = lift impliesProp
  premiseName = lift premiseName
  consequenceName = lift consequenceName

instance MonadProofNames sym m => MonadProofNames sym (StateT s m) where
  impliesProp = lift impliesProp
  premiseName = lift premiseName
  consequenceName = lift consequenceName

instance (Monoid s, MonadProofNames sym m) =>
         MonadProofNames sym (WriterT s m) where
  impliesProp = lift impliesProp
  premiseName = lift premiseName
  consequenceName = lift consequenceName

