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
