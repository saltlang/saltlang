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
       lexOnly,
       ) where

import Control.Monad.Trans
import Language.Salt.Surface.Lexer

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy

lexOnly :: Bool -> Bool -> [FilePath] -> Frontend ()
lexOnly savetext savexml =
  let
    lexOnlyFile :: FilePath -> Frontend ()
    lexOnlyFile fname  =
      let
        fnamebstr = Strict.fromString fname

        textpath = if savetext then Just $! fname ++ ".tokens" else Nothing
        xmlpath = if savexml then Just $! fname ++ ".tokens.xml" else Nothing
      in do
        input <- liftIO (Lazy.readFile fname)
        runLexer lexRemaining textpath xmlpath fnamebstr input
  in
    mapM_ lexOnlyFile
