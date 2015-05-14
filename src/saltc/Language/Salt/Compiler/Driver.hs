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

module Language.Salt.Compiler.Driver(
       run
       ) where

import Control.Monad.Collect
import Control.Monad.FileArtifacts
import Control.Monad.FileLoader
import Control.Monad.Frontend
import Control.Monad.Messages
import Data.Array
import Data.Message
import Language.Salt.Compiler.Options
import Language.Salt.Compiler.Stages
import Language.Salt.Surface.Token
import Prelude hiding (lex)
import System.IO

import qualified Data.ByteString as Strict

-- Later on, have some sort of compiler products structure.  Also,
-- have run call a compile function that generates compiler products.

-- Also, have a frontend and backend function, that builds a frontend
-- and backend pipeline based on data in options.

-- | Run the compiler with the given options.
run :: Options -> IO ()
run opts @ Options { optInputs = inputs, optStages = stages,
                     optDistDir = distdiropt,
                     optSrcDirs = srcdirs } =
  let
    distdir = case distdiropt of
      Just val -> val
      Nothing -> Strict.empty
  in case bounds stages of
    (Lexer, Lexer) ->
      let
        loader = runFileArtifactsT (lex opts inputs) distdir
        msgs = runFileLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (Lexer, Parser) ->
      let
        loader = runFileArtifactsT (parse opts inputs) distdir
        msgs = runFileLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (Lexer, Collect) ->
      let
        artifacts = runCollectTComponentsT (collect opts inputs)
                                           (const $! dumpSurface opts)
        loader = runFileArtifactsT artifacts distdir
        msgs = runFileLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (_, _) -> error "Stages array does not begin with Lexer"
