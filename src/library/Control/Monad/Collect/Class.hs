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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | Defines a monad class that provides functionality for the Collect
-- phase.
module Control.Monad.Collect.Class(
       MonadCollect(..)
       ) where

import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.FileLoader
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.List
import Control.Monad.MemoryLoader
import Control.Monad.Messages
import Control.Monad.Positions
import Control.Monad.Reader
import Control.Monad.SkipComments
import Control.Monad.SourceFiles
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Writer
import Data.Symbol
import Language.Salt.Surface.Common
import Language.Salt.Surface.Syntax(Component)

-- | Monad class providing functionality for the Collect phase.
class Monad m => MonadCollect m where
  -- | Get a fresh 'ScopeID'
  scopeID :: m ScopeID
  -- | Finish collecting a component
  addComponent :: [Symbol]
               -- ^ The component name.
               -> Component
               -- ^ The component body.
               -> m ()
  -- | Check if a component exists.
  componentExists :: [Symbol]
                  -- ^ The component name
                  -> m Bool

instance MonadCollect m => MonadCollect (CommentBufferT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (CommentsT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ContT c m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m) => MonadCollect (ExceptT e m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (GenposT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (GensymT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (KeywordsT pos tok m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ListT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (MemoryLoaderT info m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m, Monoid msgs) =>
         MonadCollect (MessagesT msgs msg m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (PositionsT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ReaderT r m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SkipCommentsT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SourceFilesT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (FileLoaderT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (StateT s m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SymbolsT m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m, Monoid w) => MonadCollect (WriterT w m) where
  scopeID = lift scopeID
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists
