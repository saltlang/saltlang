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

module Main(
       main
       ) where

import Control.Monad.Frontend
import Control.Monad.Messages
import Data.Message
import Language.Salt.Compiler.Stages
import Language.Salt.Surface.Token
import System.Environment
import System.IO

frontend :: [String] -> IO ()
frontend [] =
  let
    front = putMessagesT stderr Error lexOnlyStdin
  in do
    _ <- runFrontendT front keywords
    return ()

frontend args =
  let
    run = mapM_ lexOnly args
    front = putMessagesT stderr Error run
  in do
    _ <- runFrontendT front keywords
    return ()

main :: IO ()
main =
  do
    args <- getArgs
    frontend args
