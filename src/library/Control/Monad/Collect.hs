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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.Collect(
       MonadCollect(..),
       CollectT,
       Collect,
       runCollectT,
       mapCollectT,
       runCollect
       ) where

import Control.Applicative
import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Collect.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.Reader
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Writer
import Data.HashTable.IO(BasicHashTable)
import Data.Maybe
import Data.Symbol
import Language.Salt.Surface.Syntax

import qualified Data.HashTable.IO as HashTable

type Table = BasicHashTable [Symbol] Scope

newtype CollectT m a = CollectT { unpackCollectT :: ReaderT Table m a }

type Collect = CollectT IO

runCollectT :: MonadIO m =>
               CollectT m a
            -- ^ The @CollectT@ monad transformer to execute.
            -> m a
runCollectT c =
  do
    tab <- liftIO HashTable.new
    runReaderT (unpackCollectT c) tab

runCollect :: Collect a
           -- ^ The @Collect@ monad to execute.
           -> IO a
runCollect = runCollectT

mapCollectT :: (Monad m, Monad n) =>
               (m a -> n b) -> CollectT m a -> CollectT n b
mapCollectT f = CollectT . mapReaderT f . unpackCollectT

addComponent' :: MonadIO m => [Symbol] -> Scope -> ReaderT Table m ()
addComponent' cname component=
  do
    tab <- ask
    liftIO (HashTable.insert tab cname component)

componentExists' :: MonadIO m => [Symbol] -> ReaderT Table m Bool
componentExists' cname =
  do
    tab <- ask
    res <- liftIO (HashTable.lookup tab cname)
    return $! isJust res

instance Monad m => Monad (CollectT m) where
  return = CollectT . return
  s >>= f = CollectT $ unpackCollectT s >>= unpackCollectT . f

instance Monad m => Applicative (CollectT m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (CollectT m) where
  empty = lift empty
  s1 <|> s2 = CollectT (unpackCollectT s1 <|> unpackCollectT s2)

instance Functor (CollectT m) where
  fmap = fmap

instance MonadIO m => MonadCollect (CollectT m) where
  addComponent cname = CollectT . addComponent' cname
  componentExists = CollectT . componentExists'

instance MonadIO m => MonadIO (CollectT m) where
  liftIO = CollectT . liftIO

instance MonadTrans CollectT where
  lift = CollectT . lift

instance MonadCommentBuffer m => MonadCommentBuffer (CollectT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (CollectT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (CollectT m) where
  callCC f = CollectT (callCC (\c -> unpackCollectT (f (CollectT . c))))

instance (Error e, MonadError e m) => MonadError e (CollectT m) where
  throwError = lift . throwError
  m `catchError` h =
    CollectT (unpackCollectT m `catchError` (unpackCollectT . h))

instance MonadGenpos m => MonadGenpos (CollectT m) where
  position = lift . position

instance MonadGensym m => MonadGensym (CollectT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords t m => MonadKeywords t (CollectT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (CollectT m) where
  message = lift . message

instance MonadLoader path info m => MonadLoader path info (CollectT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (CollectT m) where
  positionInfo = lift . positionInfo

instance MonadSourceFiles m => MonadSourceFiles (CollectT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (CollectT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (CollectT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (CollectT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (CollectT m) where
  ask = lift ask
  local f = mapCollectT (local f)

instance MonadWriter w m => MonadWriter w (CollectT m) where
  tell = lift . tell
  listen = mapCollectT listen
  pass = mapCollectT pass

instance MonadPlus m => MonadPlus (CollectT m) where
  mzero = lift mzero
  mplus s1 s2 = CollectT (mplus (unpackCollectT s1)
                          (unpackCollectT s2))

instance MonadFix m => MonadFix (CollectT m) where
  mfix f = CollectT (mfix (unpackCollectT . f))
