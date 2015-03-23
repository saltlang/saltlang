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
       collect,
       dumpSurface
       ) where

import Blaze.ByteString.Builder
import Control.Monad
import Control.Monad.Artifacts.Class
import Control.Monad.Collect
import Control.Monad.Components
import Control.Monad.FileArtifacts
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Messages
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.Array hiding (accum)
import Data.Position
import Data.Symbol
import Language.Salt.Compiler.Options
import Language.Salt.Frontend
import Language.Salt.Message
import Language.Salt.Surface.AST(AST, astDot)
import Language.Salt.Surface.Collect
import Language.Salt.Surface.Lexer hiding (lex)
import Language.Salt.Surface.Parser
import Language.Salt.Surface.Syntax(Component)
import Language.Salt.Surface.Token
import Prelude hiding (lex)
import System.IO.Error
import System.FilePath
import Text.Format hiding (Options, concat)
import Text.FormatM hiding (Options)
import Text.XML.Expat.Pickle

import qualified Data.ByteString as Strict hiding (drop, break)
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

tokensExt :: Strict.ByteString
tokensExt = Strict.fromString $! extSeparator : "tokens"

astExt :: Strict.ByteString
astExt = Strict.fromString $! extSeparator : "ast"

surfaceExt :: Strict.ByteString
surfaceExt = Strict.fromString $! extSeparator : "surface"

xmlExt :: Strict.ByteString
xmlExt = Strict.fromString $! extSeparator : "xml"

dotExt :: Strict.ByteString
dotExt = Strict.fromString $! extSeparator : "dot"

printTextTokens :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                    MonadArtifacts Strict.ByteString m) =>
                   Strict.ByteString -> [Token] -> m ()
printTextTokens fname tokens =
  let
    tokfile = Strict.append fname tokensExt
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
    xmlfile = Strict.concat [fname, tokensExt, xmlExt ]
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
    astfile = Strict.append fname astExt
  in do
    astdoc <- formatM ast
    createArtifact astfile (buildOptimal 120 False astdoc)

printDotAST :: (MonadArtifacts Strict.ByteString m, MonadMessages Message m,
                MonadPositions m, MonadSymbols m) =>
                Strict.ByteString -> AST -> m ()
printDotAST fname ast =
  let
    dotfile = Strict.concat [fname, astExt, dotExt]
  in do
    astdoc <- astDot ast
    createArtifact dotfile (buildFast astdoc)

printXMLAST :: (MonadArtifacts Strict.ByteString m, MonadMessages Message m) =>
               Strict.ByteString -> AST -> m ()
printXMLAST fname ast =
  let
    xmlfile = Strict.concat [fname, astExt, xmlExt]
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

printTextSurface :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                     MonadArtifacts Strict.ByteString m) =>
                    Strict.ByteString -> Component -> m ()
printTextSurface fname scope =
  let
    surfacefile = Strict.append fname surfaceExt
  in do
    surfacedoc <- formatM scope
    createArtifact surfacefile (buildOptimal 120 False surfacedoc)

printXMLSurface :: (MonadArtifacts Strict.ByteString m, MonadMessages Message m) =>
                   Strict.ByteString -> Component -> m ()
printXMLSurface fname scope =
  let
    xmlfile = Strict.concat [fname, surfaceExt, xmlExt]
    pickler = xpRoot (xpElem (Strict.fromString "file")
                             (xpAttrFixed (Strict.fromString "name") fname)
                             xpickle)
    xmltree = XML.indent 2 (pickleTree pickler ((), scope))
  in
    createLazyBytestringArtifact xmlfile (XML.format xmltree)

printSurface :: (MonadIO m, MonadPositions m, MonadSymbols m,
                 MonadMessages Message m, MonadArtifacts Strict.ByteString m) =>
                Save -> Strict.ByteString -> Component -> m ()
printSurface Save { saveXML = savexml, saveText = savetxt }
             fname ast =
  do
    when savetxt (printTextSurface fname ast)
    when savexml (printXMLSurface fname ast)

getComponentName :: MonadGensym m =>
                    Strict.ByteString -> m [Symbol]
getComponentName =
  let
    getComponentName' :: MonadGensym m =>
                         [Symbol] -> Strict.ByteString -> m [Symbol]
    getComponentName' accum bstr
      | Strict.null bstr = return $! reverse accum
      | otherwise =
        let
          (first, rest) = Strict.break (== '.') bstr
        in do
          sym <- symbol first
          if Strict.null rest
            then return $! reverse (sym : accum)
            else getComponentName' (sym : accum) (Strict.drop 1 rest)
  in
    getComponentName' []

-- | Just run the lexer, without the parser.
lex :: Options
    -- ^ Compiler options.
    -> [Strict.ByteString]
    -- ^ Files to lex.
    -> FileArtifactsT Frontend ()
lex Options { optStages = stages, optComponents = True }
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      save = stages ! Lexer

      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile namestr =
        do
          cname <- getComponentName namestr
          pos <- cmdLine
          (fname, input) <- loadComponent cname pos
          case input of
            Nothing -> return ()
            Just content ->
              do
                (_, toks) <- lift (runLexer lexRemaining fname content)
                printTokens save fname toks
    in
      mapM_ lexOnlyFile
  | otherwise =
    let
      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile namestr =
        do
          cname <- getComponentName namestr
          pos <- cmdLine
          (fname, input) <- loadComponent cname pos
          case input of
            Nothing -> return ()
            Just content ->
              lift (runLexerNoTokens lexRemaining fname content)
    in
      mapM_ lexOnlyFile
lex Options { optStages = stages, optComponents = False }
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      save = stages ! Lexer

      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile fname =
        do
          pos <- cmdLine
          input <- loadFile fname pos
          case input of
            Nothing -> return ()
            Just content ->
              do
                (_, toks) <- lift (runLexer lexRemaining fname content)
                printTokens save fname toks
    in
      mapM_ lexOnlyFile
  | otherwise =
    let
      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile fname =
        do
          pos <- cmdLine
          input <- loadFile fname pos
          case input of
            Nothing -> return ()
            Just content ->
              lift (runLexerNoTokens lexRemaining fname content)
    in
      mapM_ lexOnlyFile

-- | Run the parser, produce an AST
parse :: Options
      -- ^ Compiler options.
      -> [Strict.ByteString]
      -- ^ Files to parse.
      -> FileArtifactsT Frontend ()
parse Options { optStages = stages, optComponents = True } names
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      savetokens = stages ! Lexer
      saveast = stages ! Parser

      parseFile :: Position -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos namestr =
        do
          cname <- getComponentName namestr
          (fname, input) <- loadComponent cname pos
          case input of
            Nothing -> return Nothing
            Just content ->
              do
                (out, tokens) <- lift (parser fname content)
                printTokens savetokens fname tokens
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in do
      pos <- cmdLine
      mapM_ (parseFile pos) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: Position -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos namestr =
        do
          cname <- getComponentName namestr
          (fname, input) <- loadComponent cname pos
          case input of
            Nothing -> return Nothing
            Just content ->
              do
                out <- lift (parserNoTokens fname content)
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in do
      pos <- cmdLine
      mapM_ (parseFile pos) names
parse Options { optStages = stages, optComponents = False } names
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      savetokens = stages ! Lexer
      saveast = stages ! Parser

      parseFile :: Position -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos fname =
        do
          input <- loadFile fname pos
          case input of
            Nothing -> return Nothing
            Just content ->
              do
                (out, tokens) <- lift (parser fname content)
                printTokens savetokens fname tokens
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in do
      pos <- cmdLine
      mapM_ (parseFile pos) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: Position -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos fname =
        do
          input <- loadFile fname pos
          case input of
            Nothing -> return Nothing
            Just content ->
              do
                out <- lift (parserNoTokens fname content)
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in do
      pos <- cmdLine
      mapM_ (parseFile pos) names

dumpSurface :: Options -> ComponentsT (FileArtifactsT Frontend) ()
dumpSurface Options { optStages = stages } =
  let
    savesurface = stages ! Collect

    mapfun :: ([Symbol], Component) -> ComponentsT (FileArtifactsT Frontend) ()
    mapfun (cname, scope) =
      do
        fname <- componentFileName cname
        printSurface savesurface fname scope
  in
    when (saveXML savesurface || saveText savesurface)
      (do
         comps <- components
         mapM_ mapfun comps)

-- | Run the collect phase.
collect :: Options
        -- ^ Compiler options
        -> [Strict.ByteString]
        -- ^ Inputs to parse.
        -> CollectT (FileArtifactsT Frontend) ()
collect Options { optStages = stages, optComponents = compnames } names
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      savetokens = stages ! Lexer
      saveast = stages ! Parser

      parseFile :: Strict.ByteString -> Lazy.ByteString ->
                   CollectT (FileArtifactsT Frontend) (Maybe AST)
      parseFile fname content =
        do
          (out, tokens) <- lift (lift (parser fname content))
          lift (printTokens savetokens fname tokens)
          case out of
            Just ast ->
              do
                lift (printAST saveast fname ast)
                return out
            Nothing -> return out
    in do
      pos <- cmdLine
      if compnames
        then do
          cnames <- mapM getComponentName names
          mapM_ (collectComponent parseFile pos) cnames
        else mapM_ (collectFile parseFile pos) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: Strict.ByteString -> Lazy.ByteString ->
                   CollectT (FileArtifactsT Frontend) (Maybe AST)
      parseFile fname content =
        do
          out <- lift (lift (parserNoTokens fname content))
          case out of
            Just ast ->
              do
                lift (printAST saveast fname ast)
                return out
            Nothing -> return out
    in do
      pos <- cmdLine
      if compnames
        then do
          cnames <- mapM getComponentName names
          mapM_ (collectComponent parseFile pos) cnames
        else mapM_ (collectFile parseFile pos) names
