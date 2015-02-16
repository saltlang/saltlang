-- Copyright (c) 2015 Eric McCorkle.
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
import Control.Monad.SourceLoader
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Writer
import Language.Salt.Surface.Syntax

import Data.ByteString as Strict

-- | Monad class providing access to components.
class Monad m => MonadComponents m where
  -- | Get access to the scope defined in a component.
  component :: Strict.ByteString
            -- ^ The component name.
            -> m Scope

instance MonadComponents m => MonadComponents (CommentBufferT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (CommentsT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (ContT c m) where
  component = lift . component

instance (MonadComponents m, Error e) => MonadComponents (ErrorT e m) where
  component = lift . component

instance MonadComponents m => MonadComponents (GenposT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (GensymT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (KeywordsT tok m) where
  component = lift . component

instance MonadComponents m => MonadComponents (ListT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (MemoryLoaderT info m) where
  component = lift . component

instance (MonadComponents m, Monoid msgs) =>
         MonadComponents (MessagesT msgs msg m) where
  component = lift . component

instance MonadComponents m => MonadComponents (PositionsT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (ReaderT r m) where
  component = lift . component

instance MonadComponents m => MonadComponents (SkipCommentsT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (SourceFilesT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (SourceLoaderT m) where
  component = lift . component

instance MonadComponents m => MonadComponents (StateT s m) where
  component = lift . component

instance MonadComponents m => MonadComponents (SymbolsT m) where
  component = lift . component

instance (MonadComponents m, Monoid w) => MonadComponents (WriterT w m) where
  component = lift . component
