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
