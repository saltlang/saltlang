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

-- | Defines a monad class that provides functionality for the Collect
-- phase.
module Control.Monad.Collect.Class(
       MonadCollect(..)
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

-- | Monad class providing functionality for the Collect phase.
class Monad m => MonadCollect m where
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
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (CommentsT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ContT c m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m, Error e) => MonadCollect (ErrorT e m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (GenposT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (GensymT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (KeywordsT pos tok m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ListT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (MemoryLoaderT info m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m, Monoid msgs) =>
         MonadCollect (MessagesT msgs msg m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (PositionsT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (ReaderT r m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SkipCommentsT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SourceFilesT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (FileLoaderT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (StateT s m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance MonadCollect m => MonadCollect (SymbolsT m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists

instance (MonadCollect m, Monoid w) => MonadCollect (WriterT w m) where
  addComponent cname = lift . addComponent cname
  componentExists = lift . componentExists
