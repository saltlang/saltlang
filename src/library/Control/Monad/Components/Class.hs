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

-- | Defines a monad class that provides access to components (the
-- results of the Collect phase).
module Control.Monad.Components.Class(
       MonadComponents(..)
       ) where

import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Cont
import Control.Monad.Error
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
import Language.Salt.Surface.Syntax

-- | Monad class providing access to components.
class Monad m => MonadComponents m where
  -- | Get access to the scope defined in a component.
  component :: [Symbol]
            -- ^ The component name.
            -> m Component
  -- | Get all components that have been defined.
  components :: m [([Symbol], Component)]

instance MonadComponents m => MonadComponents (CommentBufferT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (CommentsT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (ContT c m) where
  component = lift . component
  components = lift components

instance (MonadComponents m, Error e) => MonadComponents (ErrorT e m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (GenposT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (GensymT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (KeywordsT pos tok m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (ListT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (MemoryLoaderT info m) where
  component = lift . component
  components = lift components

instance (MonadComponents m, Monoid msgs) =>
         MonadComponents (MessagesT msgs msg m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (PositionsT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (ReaderT r m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (SkipCommentsT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (SourceFilesT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (FileLoaderT m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (StateT s m) where
  component = lift . component
  components = lift components

instance MonadComponents m => MonadComponents (SymbolsT m) where
  component = lift . component
  components = lift components

instance (MonadComponents m, Monoid w) => MonadComponents (WriterT w m) where
  component = lift . component
  components = lift components
