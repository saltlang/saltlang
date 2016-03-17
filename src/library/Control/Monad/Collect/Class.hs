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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Defines a monad class that provides functionality for the Collect
-- phase.
module Control.Monad.Collect.Class(
       MonadCollectBase(..),
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
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.Symbol
import Language.Salt.Surface.Common
import Language.Salt.Surface.Syntax(Component)

-- | Monad class providing functionality for the Collect phase.
class Monad m => MonadCollectBase m where
  -- | Get a fresh 'ScopeID'
  scopeID :: m ScopeID
  -- | Check if a component exists.
  componentExists :: [Symbol]
                  -- ^ The component name
                  -> m Bool

-- | Monad class providing ability to add components in the Collect phase.
class MonadCollectBase m => MonadCollect expty m where
  -- | Finish collecting a component
  addComponent :: [Symbol]
               -- ^ The component name.
               -> Component expty
               -- ^ The component body.
               -> m ()

instance MonadCollectBase m => MonadCollectBase (CommentBufferT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (CommentBufferT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (CommentsT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (CommentsT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (ContT c m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (ContT c m) where
  addComponent cname = lift . addComponent cname

instance (MonadCollectBase m) => MonadCollectBase (ExceptT e m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance (MonadCollect expty m) => MonadCollect expty (ExceptT e m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (GenposT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (GenposT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (GensymT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (GensymT m) where
  addComponent cname = lift . addComponent cname

instance (MonadCollectBase m, Monoid w) => MonadCollectBase (JournalT w m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance (MonadCollect expty m, Monoid w) =>
         MonadCollect expty (JournalT w m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (KeywordsT pos tok m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (KeywordsT pos tok m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (ListT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (ListT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (MemoryLoaderT info m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (MemoryLoaderT info m) where
  addComponent cname = lift . addComponent cname

instance (MonadCollectBase m, Monoid msgs) =>
         MonadCollectBase (MessagesT msgs msg m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance (MonadCollect expty m, Monoid msgs) =>
         MonadCollect expty (MessagesT msgs msg m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (PositionsT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (PositionsT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (ReaderT r m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (ReaderT r m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (SkipCommentsT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (SkipCommentsT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (SourceFilesT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (SourceFilesT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (FileLoaderT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (FileLoaderT m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (StateT s m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (StateT s m) where
  addComponent cname = lift . addComponent cname

instance MonadCollectBase m => MonadCollectBase (SymbolsT m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance MonadCollect expty m => MonadCollect expty (SymbolsT m) where
  addComponent cname = lift . addComponent cname

instance (MonadCollectBase m, Monoid w) => MonadCollectBase (WriterT w m) where
  scopeID = lift scopeID
  componentExists = lift . componentExists

instance (MonadCollect expty m, Monoid w) =>
         MonadCollect expty (WriterT w m) where
  addComponent cname = lift . addComponent cname
