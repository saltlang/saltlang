-- Copyright (c) 2013, 2014 Eric McCorkle.
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

apply' :: Monad m => Pos -> sym -> (WriterT (ProofScript sym) m) ()
apply' p name = tell [Apply { applyName = name, applyPos = p }]

intro' :: Monad m => Pos -> sym -> (WriterT (ProofScript sym) m) ()
intro' p name = tell [Intro { introName = name, introPos = p }]

introVars' :: Monad m => Pos -> [Map sym sym] ->
              (WriterT (ProofScript sym) m) ()
introVars' p namemaps =
  tell [IntroVars { introVarsMaps = namemaps, introVarsPos = p }]

cut' :: Monad m => Pos -> Term sym sym -> (WriterT (ProofScript sym) m) ()
cut' p prop = tell [Cut { cutProp = prop, cutPos = p }]

applyWith' :: Monad m => Pos -> Term sym sym -> [Term sym sym] ->
              (WriterT (ProofScript sym) m) ()
applyWith' p prop args = tell [ApplyWith { applyWithProp = prop,
                                           applyWithArgs = args,
                                           applyWithPos = p }]

instance Monad m => Monad (ProofRecorderT sym m) where
  return = ProofRecorderT . return
  (ProofRecorderT m) >>= f = ProofRecorderT $ m >>= unpackProofRecorderT . f

instance Monad m => MonadProof sym (ProofRecorderT sym m) where
  apply p = ProofRecorderT . apply' p
  intro p = ProofRecorderT . intro' p
  introVars p = ProofRecorderT . introVars' p
  cut p = ProofRecorderT . cut' p
  applyWith p prop = ProofRecorderT . applyWith' p prop

instance MonadIO m => MonadIO (ProofRecorderT sym m) where
  liftIO = ProofRecorderT . liftIO

instance MonadTrans (ProofRecorderT w) where
  lift = ProofRecorderT . lift

instance MonadState s m => MonadState s (ProofRecorderT w m) where
  get = lift get
  put = lift . put
