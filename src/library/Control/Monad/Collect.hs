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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.Collect(
       MonadCollect(..),
       CollectT,
       Collect,
       runCollectTComponentsT,
       runCollectT,
       runCollect
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Components
import Control.Monad.Collect.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Journal
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.Reader
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.State
import Control.Monad.Symbols
import Data.HashTable.IO(BasicHashTable)
import Data.Maybe
import Data.Symbol
import Language.Salt.Surface.Common
import Language.Salt.Surface.Syntax

import qualified Data.HashTable.IO as HashTable

type Table = BasicHashTable [Symbol] Component

newtype CollectT m a =
  CollectT { unpackCollectT :: StateT ScopeID (ReaderT Table m) a }

type Collect = CollectT IO

runCollectTComponentsT :: MonadIO m =>
                          CollectT m a
                       -- ^ The @CollectT@ monad transformer to execute.
                       -> (a -> ComponentsT m b)
                       -- ^ The @ComponentsT@ monad transformer to
                       -- execute with the results of the @CollectT@
                       -- monad transformer.
                       -> m b
runCollectTComponentsT collect comps =
  do
    tab <- liftIO HashTable.new
    (res, _) <- runReaderT (runStateT (unpackCollectT collect) firstScopeID) tab
    runComponentsT (comps res) tab

runCollectT :: MonadIO m =>
               CollectT m a
            -- ^ The @CollectT@ monad transformer to execute.
            -> m a
runCollectT c =
  do
    tab <- liftIO HashTable.new
    (out, _) <- runReaderT (runStateT (unpackCollectT c) firstScopeID) tab
    return out

runCollect :: Collect a
           -- ^ The @Collect@ monad to execute.
           -> IO a
runCollect = runCollectT

scopeID' :: MonadIO m => StateT ScopeID (ReaderT Table m) ScopeID
scopeID' =
  do
    n <- get
    put $! succ n
    return n

addComponent' :: MonadIO m => [Symbol] -> Component ->
                 StateT ScopeID (ReaderT Table m) ()
addComponent' cname comp =
  do
    tab <- ask
    liftIO (HashTable.insert tab cname comp)

componentExists' :: MonadIO m =>
                    [Symbol] -> StateT ScopeID (ReaderT Table m) Bool
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

instance (MonadPlus m, Alternative m) => Alternative (CollectT m) where
  empty = lift empty
  s1 <|> s2 = CollectT (unpackCollectT s1 <|> unpackCollectT s2)

instance Functor (CollectT m) where
  fmap = fmap

instance MonadIO m => MonadCollect (CollectT m) where
  scopeID = CollectT scopeID'
  addComponent cname = CollectT . addComponent' cname
  componentExists = CollectT . componentExists'

instance MonadIO m => MonadIO (CollectT m) where
  liftIO = CollectT . liftIO

instance MonadTrans CollectT where
  lift = CollectT . lift . lift

instance MonadArtifacts path m => MonadArtifacts path (CollectT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

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

instance (MonadError e m) => MonadError e (CollectT m) where
  throwError = lift . throwError
  m `catchError` h =
    CollectT (unpackCollectT m `catchError` (unpackCollectT . h))

instance MonadGenpos m => MonadGenpos (CollectT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (CollectT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (CollectT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (CollectT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (CollectT m) where
  message = lift . message

instance MonadLoader path info m => MonadLoader path info (CollectT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (CollectT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

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

instance MonadPlus m => MonadPlus (CollectT m) where
  mzero = lift mzero
  mplus s1 s2 = CollectT (mplus (unpackCollectT s1)
                          (unpackCollectT s2))

instance MonadFix m => MonadFix (CollectT m) where
  mfix f = CollectT (mfix (unpackCollectT . f))
