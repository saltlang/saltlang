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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Monad class for getting 'Ref's to compiler-referenced definitions
-- after resolution.  Prior to resolution, these symbols will be
-- carried forward through the pipeline in a 'Refs' structure.
module Control.Monad.Refs.Class(
       MonadRefs(..)
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
import Language.Salt.Surface.Syntax
import Language.Salt.Refs

class Monad m => MonadFieldNames m where
  -- | Get all the compiler-used field names.
  getFieldNames :: m (FieldNames FieldName)

  -- | Get the 'FieldName' for the field @arg@.
  getArgField :: m FieldName
  getArgField = fmap fieldNameArg getFieldNames

-- | Class of monads providing access to the definitions used directly
-- by the compiler.
class MonadFieldNames m => MonadRefs refty m  where
  -- | Get all the compiler-referenced definitions.
  getRefs :: m (Refs refty)

  -- | Get a reference to the compose function.
  getComposeRef :: m refty
  getComposeRef = fmap refCompose getRefs

instance MonadRefs refty m => MonadRefs refty (CommentBufferT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (CommentsT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (ContT c m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (ExceptT e m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (FileArtifactsT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (FileLoaderT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (GenposT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (GensymT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (GraphBuilderT n e m) where
  getRefs = lift getRefs

instance (MonadRefs refty m, Monoid w) => MonadRefs refty (JournalT w m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (KeywordsT pos tok m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (ListT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (MemoryArtifactsT info m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (MemoryLoaderT info m) where
  getRefs = lift getRefs

instance (MonadRefs refty m, Monoid msgs) =>
         MonadRefs refty (MessagesT msgs msg m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (PositionsT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (ReaderT r m) where
  getRefs = lift getRefs

instance MonadRefs refty m =>
         MonadRefs refty (ScopeBuilderT tmpscope scope m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (SkipCommentsT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (SourceFilesT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (SourceBufferT m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (StateT s m) where
  getRefs = lift getRefs

instance MonadRefs refty m => MonadRefs refty (SymbolsT m) where
  getRefs = lift getRefs

instance (MonadRefs refty m, Monoid w) => MonadRefs refty (WriterT w m) where
  getRefs = lift getRefs

instance MonadFieldNames m => MonadFieldNames (CommentBufferT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (CommentsT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (ContT c m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (ExceptT e m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (FileArtifactsT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (FileLoaderT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (GenposT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (GensymT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (GraphBuilderT n e m) where
  getFieldNames = lift getFieldNames

instance (MonadFieldNames m, Monoid w) => MonadFieldNames (JournalT w m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (KeywordsT pos tok m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (ListT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (MemoryArtifactsT info m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (MemoryLoaderT info m) where
  getFieldNames = lift getFieldNames

instance (MonadFieldNames m, Monoid msgs) =>
         MonadFieldNames (MessagesT msgs msg m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (PositionsT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (ReaderT r m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m =>
         MonadFieldNames (ScopeBuilderT tmpscope scope m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (SkipCommentsT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (SourceFilesT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (SourceBufferT m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (StateT s m) where
  getFieldNames = lift getFieldNames

instance MonadFieldNames m => MonadFieldNames (SymbolsT m) where
  getFieldNames = lift getFieldNames

instance (MonadFieldNames m, Monoid w) => MonadFieldNames (WriterT w m) where
  getFieldNames = lift getFieldNames
