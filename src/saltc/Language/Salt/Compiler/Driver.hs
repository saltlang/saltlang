-- Copyright (c) 2014 Eric McCorkle.
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

module Language.Salt.Compiler.Driver(
       run
       ) where

import Control.Monad.Collect
import Control.Monad.FileArtifacts
import Control.Monad.Frontend
import Control.Monad.Messages
import Control.Monad.SourceLoader
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
        msgs = runSourceLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (Lexer, Parser) ->
      let
        loader = runFileArtifactsT (parse opts inputs) distdir
        msgs = runSourceLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (Lexer, Collect) ->
      let
        artifacts = runCollectT (collect opts inputs)
        loader = runFileArtifactsT artifacts distdir
        msgs = runSourceLoaderT loader srcdirs
        front = putMessagesT stderr Error msgs
      in do
        _ <- runFrontendT front keywords
        return ()
    (_, _) -> error "Stages array does not begin with Lexer"
