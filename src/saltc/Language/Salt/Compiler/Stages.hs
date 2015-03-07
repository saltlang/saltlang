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
{-# LANGUAGE FlexibleContexts #-}

module Language.Salt.Compiler.Stages(
       lex,
       parse,
--       collect
       ) where

import Blaze.ByteString.Builder
import Control.Monad
import Control.Monad.Artifacts.Class
import Control.Monad.FileArtifacts
import Control.Monad.Messages
import Control.Monad.Positions
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.Array
import Language.Salt.Compiler.Options
import Language.Salt.Frontend
import Language.Salt.Message
import Language.Salt.Surface.AST
import Language.Salt.Surface.Lexer hiding (lex)
import Language.Salt.Surface.Parser
import Language.Salt.Surface.Token
import Prelude hiding (lex)
import System.IO.Error
import System.FilePath
import Text.Format hiding (Options, concat)
import Text.FormatM hiding (Options)
import Text.XML.Expat.Pickle

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Text.XML.Expat.Format as XML

createArtifact :: (MonadMessages Message m,
                   MonadArtifacts Strict.ByteString m) =>
                  Strict.ByteString -> Builder -> m ()
createArtifact fname builder =
  do
    res <- artifact fname builder
    case res of
      Nothing -> return ()
      Just err ->
        cannotCreateFile fname (Strict.fromString $! ioeGetErrorString err)

createLazyBytestringArtifact :: (MonadMessages Message m,
                                 MonadArtifacts Strict.ByteString m) =>
                                Strict.ByteString -> Lazy.ByteString -> m ()
createLazyBytestringArtifact fname builder =
  do
    res <- artifactLazyBytestring fname builder
    case res of
      Nothing -> return ()
      Just err ->
        cannotCreateFile fname (Strict.fromString $! ioeGetErrorString err)

printTextTokens :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                    MonadArtifacts Strict.ByteString m) =>
                   Strict.ByteString -> [Token] -> m ()
printTextTokens fname tokens =
  let
    tokfile = Strict.append fname (Strict.fromString (extSeparator : "tokens"))
    doc tokdocs = vcat (string "Tokens for" <+> bytestring fname <>
                        colon : tokdocs) <> line
  in do
    tokdocs <- mapM formatM tokens
    createArtifact tokfile (buildFast (doc tokdocs))

printXMLTokens :: (MonadArtifacts Strict.ByteString m,
                   MonadMessages Message m) =>
                  Strict.ByteString -> [Token] -> m ()
printXMLTokens fname tokens =
  let
    xmlfile = Strict.append fname (Strict.fromString (extSeparator : "tokens" ++
                                                      extSeparator : "xml" ))
    pickler = xpRoot (xpElem (Strict.fromString "tokens")
                             (xpAttrFixed (Strict.fromString "filename")
                                          fname)
                             (xpList xpickle))
    xmltree = XML.indent 2 (pickleTree pickler ((), tokens))
  in
    createLazyBytestringArtifact xmlfile (XML.format xmltree)

printTokens :: (MonadIO m, MonadPositions m, MonadMessages Message m,
                MonadSymbols m, MonadArtifacts Strict.ByteString m) =>
               Save -> Strict.ByteString -> [Token] -> m ()
printTokens Save { saveText = savetext, saveXML = savexml } fname toks =
  do
    when savetext (printTextTokens fname (reverse toks))
    when savexml (printXMLTokens fname (reverse toks))

printTextAST :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                 MonadArtifacts Strict.ByteString m) =>
                Strict.ByteString -> AST -> m ()
printTextAST fname ast =
  let
    astfile = Strict.append fname (Strict.fromString (extSeparator : "ast"))
  in do
    astdoc <- formatM ast
    createArtifact astfile (buildOptimal 120 False astdoc)

printDotAST :: (MonadArtifacts Strict.ByteString m, MonadMessages Message m,
                MonadPositions m, MonadSymbols m) =>
                Strict.ByteString -> AST -> m ()
printDotAST fname ast =
  let
    dotfile = Strict.append fname (Strict.fromString (extSeparator : "ast" ++
                                                      extSeparator : "dot"))
  in do
    astdoc <- astDot ast
    createArtifact dotfile (buildFast astdoc)

printXMLAST :: (MonadArtifacts Strict.ByteString m, MonadMessages Message m) =>
               Strict.ByteString -> AST -> m ()
printXMLAST fname ast =
  let
    xmlfile = Strict.append fname (Strict.fromString (extSeparator : "ast" ++
                                                      extSeparator : "xml"))
    pickler = xpRoot (xpElem (Strict.fromString "file")
                             (xpAttrFixed (Strict.fromString "name") fname)
                             xpickle)
    xmltree = XML.indent 2 (pickleTree pickler ((), ast))
  in
    createLazyBytestringArtifact xmlfile (XML.format xmltree)

printAST :: (MonadIO m, MonadPositions m, MonadSymbols m,
             MonadMessages Message m, MonadArtifacts Strict.ByteString m) =>
            Save -> Strict.ByteString -> AST -> m ()
printAST Save { saveXML = savexml, saveText = savetxt, saveDot = savedot }
         fname ast =
  do
    when savetxt (printTextAST fname ast)
    when savedot (printDotAST fname ast)
    when savexml (printXMLAST fname ast)

-- | Just run the lexer, without the parser.
lex :: Options
    -- ^ Compiler options.
    -> [Strict.ByteString]
    -- ^ Files to lex.
    -> FileArtifactsT Frontend ()
lex Options { optStages = stages } =
  let
    lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
    lexOnlyFile =
      case stages ! Lexer of
        Save { saveText = False, saveXML = False } ->
          \fname ->
            let
              fnamestr = Strict.toString fname
            in do
              input <- liftIO (Lazy.readFile fnamestr)
              lift (runLexerNoTokens lexRemaining fname input)
        save ->
          \fname ->
            let
              fnamestr = Strict.toString fname
            in do
              input <- liftIO (Lazy.readFile fnamestr)
              (_, toks) <- lift (runLexer lexRemaining fname input)
              printTokens save fname toks
  in
    mapM_ lexOnlyFile

-- | Run the parser, produce an AST
parse :: Options
      -- ^ Compiler options.
      -> [Strict.ByteString]
      -- ^ Files to parse.
      -> FileArtifactsT Frontend ()
parse Options { optStages = stages } =
  let
    saveast = stages ! Parser

    parseFile :: Strict.ByteString -> FileArtifactsT Frontend (Maybe AST)
    parseFile =
      case stages ! Lexer of
        Save { saveText = False, saveXML = False } ->
          \fname ->
            let
              fnamestr = Strict.toString fname
            in do
              input <- liftIO (Lazy.readFile fnamestr)
              out <- lift (parserNoTokens fname input)
              case out of
                Just ast ->
                  do
                    printAST saveast fname ast
                    return out
                Nothing -> return out
        savetokens ->
          \fname ->
            let
              fnamestr = Strict.toString fname
            in do
              input <- liftIO (Lazy.readFile fnamestr)
              (out, tokens) <- lift (parser fname input)
              printTokens savetokens fname tokens
              case out of
                Just ast ->
                  do
                    printAST saveast fname ast
                    return out
                Nothing -> return out
  in
    mapM_ parseFile
{-
collect :: Options
        -- ^ Compiler options
        -> [Strict.ByteString]
        -- ^ Inputs to parse.
        -> CollectT (SourceLoaderT (Frontend ()))
-}
