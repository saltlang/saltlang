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
import Data.Position.BasicPosition
import Data.Position.Filename
import Data.Symbol
import Language.Salt.Compiler.Options
import Language.Salt.Frontend
import Language.Salt.Message
import Language.Salt.Surface.AST(AST, astDot)
import Language.Salt.Surface.Collect
import Language.Salt.Surface.Lexer hiding (lex)
import Language.Salt.Surface.Parser
import Language.Salt.Surface.Syntax(Component, Exp)
import Language.Salt.Surface.Token
import Prelude hiding (lex)
import System.IO.Error
import System.FilePath
import Text.Format hiding (Options, concat)
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
               Save -> Filename -> [Token] -> m ()
printTokens Save { saveText = savetext, saveXML = savexml } fname toks =
  do
    FileInfo { fileInfoName = fstr } <- fileInfo fname
    when savetext (printTextTokens fstr (reverse toks))
    when savexml (printXMLTokens fstr (reverse toks))

printTextAST :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                 MonadArtifacts Strict.ByteString m) =>
                Strict.ByteString -> AST -> m ()
printTextAST fname ast =
  let
    astfile = Strict.append fname astExt
  in do
    astdoc <- formatM ast
    createArtifact astfile (buildGreedy 4 120 False astdoc)

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
            Save -> Filename -> AST -> m ()
printAST Save { saveXML = savexml, saveText = savetxt, saveDot = savedot }
         fname ast =
  do
    FileInfo { fileInfoName = fstr } <- fileInfo fname
    when savetxt (printTextAST fstr ast)
    when savedot (printDotAST fstr ast)
    when savexml (printXMLAST fstr ast)

printTextSurface :: (MonadPositions m, MonadSymbols m, MonadMessages Message m,
                     MonadArtifacts Strict.ByteString m) =>
                    Strict.ByteString -> Component (Exp Symbol) -> m ()
printTextSurface fname scope =
  let
    surfacefile = Strict.append fname surfaceExt
  in do
    surfacedoc <- formatM scope
    createArtifact surfacefile (buildGreedy 4 120 False surfacedoc)

printXMLSurface :: (MonadArtifacts Strict.ByteString m,
                    MonadMessages Message m) =>
                   Strict.ByteString -> Component (Exp Symbol) -> m ()
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
                Save -> Strict.ByteString -> Component (Exp Symbol) -> m ()
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
          res <- loadComponent cname CmdLine
          case res of
            Nothing -> return ()
            Just (fname, content) ->
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
          res <- loadComponent cname CmdLine
          case res of
            Nothing -> return ()
            Just (fname, content) ->
              lift (runLexerNoTokens lexRemaining fname content)
    in
      mapM_ lexOnlyFile
lex Options { optStages = stages, optComponents = False }
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      save = stages ! Lexer

      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile fstr =
        do
          res <- loadFile fstr CmdLine
          case res of
            Nothing -> return ()
            Just (fname, content) ->
              do
                (_, toks) <- lift (runLexer lexRemaining fname content)
                printTokens save fname toks
    in
      mapM_ lexOnlyFile
  | otherwise =
    let
      lexOnlyFile :: Strict.ByteString -> FileArtifactsT Frontend ()
      lexOnlyFile fstr =
        do
          res <- loadFile fstr CmdLine
          case res of
            Nothing -> return ()
            Just (fname, content) ->
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

      parseFile :: BasicPosition -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos namestr =
        do
          cname <- getComponentName namestr
          res <- loadComponent cname pos
          case res of
            Nothing -> return Nothing
            Just (fname, content) ->
              do
                (out, tokens) <- lift (parser fname content)
                printTokens savetokens fname tokens
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in
      mapM_ (parseFile CmdLine) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: BasicPosition -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos namestr =
        do
          cname <- getComponentName namestr
          res <- loadComponent cname pos
          case res of
            Nothing -> return Nothing
            Just (fname, content) ->
              do
                out <- lift (parserNoTokens fname content)
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in
      mapM_ (parseFile CmdLine) names
parse Options { optStages = stages, optComponents = False } names
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      savetokens = stages ! Lexer
      saveast = stages ! Parser

      parseFile :: BasicPosition -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos fstr =
        do
          res <- loadFile fstr pos
          case res of
            Nothing -> return Nothing
            Just (fname, content) ->
              do
                (out, tokens) <- lift (parser fname content)
                printTokens savetokens fname tokens
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in
      mapM_ (parseFile CmdLine) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: BasicPosition -> Strict.ByteString ->
                   FileArtifactsT Frontend (Maybe AST)
      parseFile pos fstr =
        do
          res <- loadFile fstr pos
          case res of
            Nothing -> return Nothing
            Just (fname, content) ->
              do
                out <- lift (parserNoTokens fname content)
                case out of
                  Just ast ->
                    do
                      printAST saveast fname ast
                      return out
                  Nothing -> return out
    in
      mapM_ (parseFile CmdLine) names

dumpSurface :: Options -> ComponentsT (Exp Symbol) (FileArtifactsT Frontend) ()
dumpSurface Options { optStages = stages } =
  let
    savesurface = stages ! Collect

    mapfun :: ([Symbol], Component (Exp Symbol)) ->
              ComponentsT (Exp Symbol) (FileArtifactsT Frontend) ()
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
        -> CollectT (Exp Symbol) (FileArtifactsT Frontend) ()
collect Options { optStages = stages, optComponents = compnames } names
  | saveText (stages ! Lexer) || saveXML (stages ! Lexer) =
    let
      savetokens = stages ! Lexer
      saveast = stages ! Parser

      parseFile :: Filename -> Lazy.ByteString ->
                   CollectT (Exp Symbol) (FileArtifactsT Frontend) (Maybe AST)
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
    in
      if compnames
        then do
          cnames <- mapM getComponentName names
          mapM_ (collectComponent parseFile CmdLine) cnames
        else mapM_ (collectFile parseFile CmdLine) names
  | otherwise =
    let
      saveast = stages ! Parser

      parseFile :: Filename -> Lazy.ByteString ->
                   CollectT (Exp Symbol) (FileArtifactsT Frontend) (Maybe AST)
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
      if compnames
        then do
          cnames <- mapM getComponentName names
          mapM_ (collectComponent parseFile CmdLine) cnames
        else mapM_ (collectFile parseFile CmdLine) names
