-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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

-- | Monad class for generating and managing type rank variables.
module Control.Monad.TypeRanks.Class(
       MonadTypeRanks(..)
       ) where

import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.FileArtifacts
import Control.Monad.FileLoader
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.GraphBuilder
import Control.Monad.Keywords
import Control.Monad.List
import Control.Monad.MemoryArtifacts
import Control.Monad.MemoryLoader
import Control.Monad.Messages
import Control.Monad.Positions
import Control.Monad.Reader
import Control.Monad.SkipComments
import Control.Monad.ScopeBuilder
import Control.Monad.SourceBuffer
import Control.Monad.SourceFiles
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Language.Salt.Core.Syntax

-- | A monad class for generating and managing type rank variables.
-- This provides a way to get fresh type rank variables on demand.
class Monad m => MonadTypeRanks m where
  -- | Get a fresh rank variable
  rankvar :: m RankID

instance MonadTypeRanks m => MonadTypeRanks (CommentBufferT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (CommentsT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (ContT c m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (ExceptT e m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (FileArtifactsT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (FileLoaderT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (GenposT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (GensymT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (GraphBuilderT n e m) where
  rankvar = lift rankvar

instance (MonadTypeRanks m, Monoid w) => MonadTypeRanks (JournalT w m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (KeywordsT pos tok m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (ListT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (MemoryArtifactsT info m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (MemoryLoaderT info m) where
  rankvar = lift rankvar

instance (MonadTypeRanks m, Monoid msgs) =>
         MonadTypeRanks (MessagesT msgs msg m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (PositionsT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (ReaderT r m) where
  rankvar = lift rankvar

instance MonadTypeRanks m =>
         MonadTypeRanks (ScopeBuilderT tmpscope scope m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (SkipCommentsT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (SourceFilesT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (SourceBufferT m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (StateT s m) where
  rankvar = lift rankvar

instance MonadTypeRanks m => MonadTypeRanks (SymbolsT m) where
  rankvar = lift rankvar

instance (MonadTypeRanks m, Monoid w) => MonadTypeRanks (WriterT w m) where
  rankvar = lift rankvar
