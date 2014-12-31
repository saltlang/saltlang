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
       lex,
       parse
       ) where

import Control.Monad
import Control.Monad.Positions
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.Array
import Language.Salt.Compiler.Options
import Language.Salt.Frontend
import Language.Salt.Surface.AST
import Language.Salt.Surface.Lexer hiding (lex)
import Language.Salt.Surface.Parser
import Language.Salt.Surface.Token
import Prelude hiding (lex)
import System.IO
import Text.FormatM hiding (Options)
import Text.XML.Expat.Pickle

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Text.XML.Expat.Format as XML

-- | Print out all tokens as text to the given handle
printTextTokens :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
                   Handle -> Strict.ByteString -> [Token] -> m ()
printTextTokens handle fname tokens =
  let
    doc tokdocs = vcat (string "Tokens for" <+> bytestring fname <>
                        colon : tokdocs) <> line
  in do
    tokdocs <- mapM formatM tokens
    liftIO (putFast handle (doc tokdocs))

printXMLTokens :: (MonadIO m) => Handle -> Strict.ByteString -> [Token] -> m ()
printXMLTokens handle fname tokens =
  let
    pickler = xpRoot (xpElem (Strict.fromString "tokens")
                             (xpAttrFixed (Strict.fromString "filename")
                                          fname)
                             (xpList xpickle))
    xmltree = XML.indent 2 (pickleTree pickler ((), tokens))
  in
    liftIO (Lazy.hPutStr handle (XML.format xmltree))

printTokens :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
               Save -> FilePath -> [Token] -> m ()
printTokens Save { saveText = savetext, saveXML = savexml } fname toks =
  do
    when savetext
      (do
         output <- liftIO (openFile (fname ++ ".tokens") WriteMode)
         printTextTokens output (Strict.fromString fname) (reverse toks)
         liftIO (hClose output))
    when savexml
      (do
         output <- liftIO (openFile (fname ++ ".tokens.xml")
                                    WriteMode)
         printXMLTokens output (Strict.fromString fname) (reverse toks)
         liftIO (hClose output))

printTextAST :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
                Handle -> AST -> m ()
printTextAST handle ast =
  do
    astdoc <- formatM ast
    liftIO (putOptimal handle 120 False astdoc)

printDotAST :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
                Handle -> AST -> m ()
printDotAST handle ast =
  do
    astdoc <- astDot ast
    liftIO (putFast handle astdoc)

printXMLAST :: (MonadIO m) => Handle -> Strict.ByteString -> AST -> m ()
printXMLAST handle fname ast =
  let
    pickler = xpRoot (xpElem (Strict.fromString "file")
                             (xpAttrFixed (Strict.fromString "name") fname)
                             xpickle)
    xmltree = XML.indent 2 (pickleTree pickler ((), ast))
  in
    liftIO (Lazy.hPutStr handle (XML.format xmltree))

printAST :: (MonadIO m, MonadPositions m, MonadSymbols m) =>
            Save -> FilePath -> AST -> m ()
printAST Save { saveXML = savexml, saveText = savetxt, saveDot = savedot }
         fname ast =
  do
    when savetxt
      (do
         output <- liftIO (openFile (fname ++ ".ast") WriteMode)
         printTextAST output ast
         liftIO (hClose output))
    when savedot
      (do
         output <- liftIO (openFile (fname ++ ".ast.dot") WriteMode)
         printDotAST output ast
         liftIO (hClose output))
    when savexml
      (do
         output <- liftIO (openFile (fname ++ ".ast.xml") WriteMode)
         printXMLAST output (Strict.fromString fname) ast
         liftIO (hClose output))

-- | Just run the lexer, without the parser.
lex :: Options
    -- ^ Compiler options.
    -> [FilePath]
    -- ^ Files to lex
    -> Frontend ()
lex Options { optStages = stages } =
  let
    lexOnlyFile :: FilePath -> Frontend ()
    lexOnlyFile =
      case stages ! Lexer of
        Save { saveText = False, saveXML = False } ->
          \fname ->
            let
              fnamebstr = Strict.fromString fname
            in do
              input <- liftIO (Lazy.readFile fname)
              runLexerNoTokens lexRemaining fnamebstr input
        save ->
          \fname ->
            let
              fnamebstr = Strict.fromString fname
            in do
              input <- liftIO (Lazy.readFile fname)
              (_, toks) <- runLexer lexRemaining fnamebstr input
              printTokens save fname toks
  in
    mapM_ lexOnlyFile

-- Run the parser, produce an AST
parse :: Options
      -- ^ Compiler options.
      -> [FilePath]
      -- ^ Files to parse.
      -> Frontend ()
parse Options { optStages = stages } =
  let
    saveast = stages ! Parser

    parseFile :: FilePath -> Frontend (Maybe AST)
    parseFile =
      case stages ! Lexer of
        Save { saveText = False, saveXML = False } ->
          \fname ->
            let
              fnamebstr = Strict.fromString fname
            in do
              input <- liftIO (Lazy.readFile fname)
              out <- parserNoTokens fnamebstr input
              case out of
                Just ast ->
                  do
                    printAST saveast fname ast
                    return out
                Nothing -> return out
        savetokens ->
          \fname ->
            let
              fnamebstr = Strict.fromString fname
            in do
              input <- liftIO (Lazy.readFile fname)
              (out, tokens) <- parser fnamebstr input
              printTokens savetokens fname tokens
              case out of
                Just ast ->
                  do
                    printAST saveast fname ast
                    return out
                Nothing -> return out
  in
    mapM_ parseFile
