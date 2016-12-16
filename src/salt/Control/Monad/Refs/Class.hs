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
import Data.ScopeID
import Language.Salt.Surface.Syntax
import Language.Salt.Refs

-- | Class of monads providing access to the definitions used directly
-- by the compiler.
class Monad m => MonadRefs m where
  -- | Get all the compiler-referenced definitions.
  getRefs :: m (Refs ScopeID Ref)

  -- | Get a reference to the compose function.
  getComposeRef :: m Ref
  getComposeRef = fmap refCompose getRefs

instance MonadRefs m => MonadRefs (CommentBufferT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (CommentsT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (ContT c m) where
  getRefs = lift getRefs

instance (MonadRefs m) => MonadRefs (ExceptT e m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (FileArtifactsT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (FileLoaderT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (GenposT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (GensymT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (GraphBuilderT n e m) where
  getRefs = lift getRefs

instance (MonadRefs m, Monoid w) => MonadRefs (JournalT w m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (KeywordsT pos tok m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (ListT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (MemoryArtifactsT info m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (MemoryLoaderT info m) where
  getRefs = lift getRefs

instance (MonadRefs m, Monoid msgs) => MonadRefs (MessagesT msgs msg m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (PositionsT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (ReaderT r m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (ScopeBuilderT tmpscope scope m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (SkipCommentsT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (SourceFilesT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (SourceBufferT m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (StateT s m) where
  getRefs = lift getRefs

instance MonadRefs m => MonadRefs (SymbolsT m) where
  getRefs = lift getRefs

instance (MonadRefs m, Monoid w) => MonadRefs (WriterT w m) where
  getRefs = lift getRefs
