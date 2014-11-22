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

-- | A module defining a monad class for the proof checker.
module Control.Monad.ProofErrors.Class(
       MonadProtoProofErrors(..),
       MonadProofErrors(..)
       ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Pos
import Language.Salt.Core.Syntax

-- | A monad class for proof-checking error messages that don't depend
-- on the symbol type.
class Monad m => MonadProtoProofErrors m where
  -- | Log an error message if a proof terminates while still incomplete.
  incomplete :: m ()
  -- | Log an error message if a proof script continues after the
  -- proof is complete.
  complete :: m ()

-- | A monad class for the error messages that can arise during proof checking.
class MonadProtoProofErrors m => MonadProofErrors sym m where
  -- | Log an error when using exact with an undefined symbol.
  undefProp :: Pos
            -- ^ The position from which this arises.
            -> sym
            -- ^ The name of the undefined proposition.
            -> m ()
  -- | Log an error when using exact, but the proposition in the truth
  -- environment doesn't match the goal.
  applyMismatch :: Pos
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
  introVarsMismatch :: Pos
                    -- ^ The position from which this arises.
                    -> Term sym sym
                    -- ^ The goal proposition.
                    -> m ()
  -- | Log an error message when using apply, but the proposition
  -- isn't a forall.
  applyWithMismatch :: Pos
                    -- ^ The position from which this arises.
                    -> Term sym sym
                    -- ^ The goal proposition.
                    -> m ()

instance MonadProtoProofErrors m => MonadProtoProofErrors (ReaderT s m) where
  incomplete = lift incomplete
  complete = lift complete

instance MonadProofErrors sym m => MonadProofErrors sym (ReaderT s m) where
  undefProp p = lift . undefProp p
  applyMismatch p sym prop = lift . applyMismatch p sym prop
  introMismatch p = lift . introMismatch p
  introVarsMismatch p = lift . introVarsMismatch p
  applyWithMismatch p = lift . applyWithMismatch p

instance MonadProtoProofErrors m => MonadProtoProofErrors (StateT s m) where
  incomplete = lift incomplete
  complete = lift complete

instance MonadProofErrors sym m => MonadProofErrors sym (StateT s m) where
  undefProp p = lift . undefProp p
  applyMismatch p sym prop = lift . applyMismatch p sym prop
  introMismatch p = lift . introMismatch p
  introVarsMismatch p = lift . introVarsMismatch p
  applyWithMismatch p = lift . applyWithMismatch p

instance (Monoid s, MonadProtoProofErrors m) =>
         MonadProtoProofErrors (WriterT s m) where
  incomplete = lift incomplete
  complete = lift complete

instance (Monoid s, MonadProofErrors sym m) =>
         MonadProofErrors sym (WriterT s m) where
  undefProp p = lift . undefProp p
  applyMismatch p sym prop = lift . applyMismatch p sym prop
  introMismatch p = lift . introMismatch p
  introVarsMismatch p = lift . introVarsMismatch p
  applyWithMismatch p = lift . applyWithMismatch p
