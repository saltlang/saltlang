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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- | A module defining a monad and transformer that record proof scripts.
module Control.Monad.ProofRecorder(
       MonadProof(..),
       ProofRecorderT,
       ProofRecorder,
       runProofRecorderT,
       runProofRecorder
       ) where

import Control.Monad.Proof.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map(Map)
import Data.Pos
import Language.Salt.Core.Proofs.ProofScript
import Language.Salt.Core.Syntax

-- | A simple monad transformer for recording proofs.  This is
-- essentially a wrapper around a WriterT.
newtype ProofRecorderT sym m a =
  ProofRecorderT ((WriterT (ProofScript sym) m) a)
type ProofRecorder sym a = ProofRecorderT sym IO a

unpackProofRecorderT :: ProofRecorderT sym m a
                     -> (WriterT (ProofScript sym) m) a
unpackProofRecorderT (ProofRecorderT w) = w

-- | Execute a proof recorder monad, and return the result along with
-- the recorded proof.
runProofRecorderT :: Monad m =>
                     ProofRecorderT sym m a
                  -- ^ The monad to execute.
                  -> m (a, ProofScript sym)
                  -- ^ The result, and the recorded proof.
runProofRecorderT = runWriterT . unpackProofRecorderT

-- | Execute a proof recorder monad, and return the result along with
-- the recorded proof.
runProofRecorder :: ProofRecorder sym a
                 -- ^ The monad to execute.
                 -> IO (a, ProofScript sym)
                 -- ^ The result, and the recorded proof.
runProofRecorder = runProofRecorderT

assumption' :: Monad m => Pos -> sym -> (WriterT (ProofScript sym) m) ()
assumption' p name = tell [Assumption { assumptionName = name,
                                        assumptionPos = p }]

intro' :: Monad m => Pos -> sym -> (WriterT (ProofScript sym) m) ()
intro' p name = tell [Intro { introName = name, introPos = p }]

introVars' :: Monad m => Pos -> [Map sym sym] ->
              (WriterT (ProofScript sym) m) ()
introVars' p namemaps =
  tell [IntroVars { introVarsMaps = namemaps, introVarsPos = p }]

cut' :: Monad m => Pos -> Term sym sym -> (WriterT (ProofScript sym) m) ()
cut' p prop = tell [Cut { cutProp = prop, cutPos = p }]

apply' :: Monad m => Pos -> Term sym sym -> [Term sym sym] ->
              (WriterT (ProofScript sym) m) ()
apply' p prop args = tell [Apply { applyProp = prop, applyArgs = args,
                                   applyPos = p }]

instance Monad m => Monad (ProofRecorderT sym m) where
  return = ProofRecorderT . return
  (ProofRecorderT m) >>= f = ProofRecorderT $ m >>= unpackProofRecorderT . f

instance Monad m => MonadProof sym (ProofRecorderT sym m) where
  assumption p = ProofRecorderT . assumption' p
  intro p = ProofRecorderT . intro' p
  introVars p = ProofRecorderT . introVars' p
  cut p = ProofRecorderT . cut' p
  apply p prop = ProofRecorderT . apply' p prop

instance MonadIO m => MonadIO (ProofRecorderT sym m) where
  liftIO = ProofRecorderT . liftIO

instance MonadTrans (ProofRecorderT w) where
  lift = ProofRecorderT . lift

instance MonadState s m => MonadState s (ProofRecorderT w m) where
  get = lift get
  put = lift . put
{-
instance MonadReader s m => MonadReader s (ProofRecorderT w m) where
  ask = lift ask
  local func (ProofRecorderT m) = ProofRecorderT ((lift (local func)) m)
-}
