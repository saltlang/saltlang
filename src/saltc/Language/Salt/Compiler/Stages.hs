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

module Language.Salt.Compiler.Stages(
       printTokens,
       lexOnly,
       lexOnlyStdin
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Control.Monad.Trans
import Language.Salt.Surface.Token
import Language.Salt.Surface.Lexer
import System.IO
import Text.Format

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy

-- | Print out all tokens as text to the given handle
printTokens :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
               Handle -> Strict.ByteString -> [Token] -> m ()
printTokens handle fname tokens =
  do
    tokdocs <- mapM formatM tokens
    liftIO (putFast handle (vcat (string "Tokens for" <+> bytestring fname <>
                                  colon : tokdocs) <> line))

lexOnly :: FilePath -> Frontend ()
lexOnly fname =
  let
    fnamebstr = Strict.fromString fname
  in do
    input <- liftIO (Lazy.readFile fname)
    output <- liftIO (openFile (fname ++ ".tokens") WriteMode)
    lexresult <- lexFile fnamebstr input
    printTokens output fnamebstr lexresult
    liftIO (hClose output)

lexOnlyStdin :: Frontend ()
lexOnlyStdin =
  let
    stdinname = Strict.fromString "stdin"
  in do
    input <- liftIO Lazy.getContents
    lexresult <- lexFile stdinname input
    printTokens stdout stdinname lexresult
