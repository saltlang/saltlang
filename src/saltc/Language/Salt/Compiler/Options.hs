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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module Language.Salt.Compiler.Options(
       Options(..),
       Save(..),
       Stage(..),
       optionsDesc,
       options
       ) where

import Data.Array
import Data.Monoid hiding (All)
import System.Console.GetOpt
import System.FilePath

import Data.ByteString.UTF8 as Strict

version :: String
version = "saltc compiler, development version\n"

-- | Options for the compiler.
data Options =
  Options {
    -- | Input. Might be file names or components, depending on
    -- 'optComponents'.
    optInputs :: ![Strict.ByteString],
    -- | An array of all stages to run, and which structures to save.
    optStages :: !(Array Stage Save),
    -- | Whether inputs are file names or component names.
    optComponents :: !Bool,
    -- | Soruce directories to search.
    optSrcDirs :: ![Strict.ByteString],
    -- | Output directory to use, or @Nothing@ if there is no output
    -- directory.
    optDistDir :: !(Maybe Strict.ByteString)
  }
  deriving Show

-- | What to save at a given stage.
data Save =
  Save {
    -- | Save a textual representation.
    saveText :: !Bool,
    -- | Save an XML representation.
    saveXML :: !Bool,
    -- | Save a graphviz representation.
    saveDot :: !Bool
  }
  deriving (Eq, Show)

-- | Datatype representing compiler stages.
data Stage =
    Lexer
  | Parser
  | Collect
    deriving (Eq, Ord, Enum, Ix, Show)

firstStage :: Stage
firstStage = Lexer

lastStage :: Stage
lastStage = Collect

data Singular a =
    Default
  | Single !a
  | Multiple
    deriving Eq

-- | Intermediate command-line arguments structure.  This is used by
-- 'getOpt' to record command-line actions.
data Args =
    Args {
      -- | Whether to save tokens.
      argTokensSave :: !Save,
      -- | Whether to save the ASTs.
      argASTSave :: !Save,
      -- | Whether to save the surface syntax.
      argSurfaceSave :: !Save,
      -- | Run every stage up to this one.
      argLastStage :: !(Singular Stage),
      -- | The directory to use for storing generated files.
      argDistDir :: !(Singular FilePath),
      -- | The source path.
      argSrcDirs :: ![FilePath],
      -- | @False@ means the names are file names, @True@ means
      -- component names.
      argComponents :: !Bool
    }
  | Error {
      -- | Current error messages.
      errMsgs :: ![String],
      -- | Whether or not we've seen a dist dir argument
      errDistDir :: !Bool,
      -- | Whether or not we've seen a last stage argument
      errLastStage :: !Bool
    }
  | Version
  | Usage
    deriving Eq

instance Monoid (Singular a) where
  mempty = Default

  mappend d Default = d
  mappend Default d = d
  mappend Multiple _ = Multiple
  mappend _ Multiple = Multiple
  mappend (Single _) (Single _) = Multiple

instance Monoid Save where
  mempty = Save { saveText = False, saveXML = False, saveDot = False }
  mappend Save { saveText = text1, saveXML = xml1, saveDot = dot1 }
          Save { saveText = text2, saveXML = xml2, saveDot = dot2 } =
    Save { saveText = text1 || text2, saveXML = xml1 || xml2,
           saveDot = dot1 || dot2 }

instance Monoid Args where
  mempty = Args { argTokensSave = mempty, argASTSave = mempty,
                  argLastStage = mempty, argSurfaceSave = mempty,
                  argDistDir = Default, argSrcDirs = [],
                  argComponents = False }

  mappend Error { errMsgs = msgs1, errDistDir = distdir1,
                  errLastStage = last1 }
          Error { errMsgs = msgs2, errDistDir = distdir2,
                  errLastStage = last2 } =
    Error { errMsgs = msgs1 ++ msgs2, errDistDir = distdir1 || distdir2,
            errLastStage = last1 || last2 }
  mappend e @ Error {} Usage = e
  mappend Usage e @ Error {} = e
  mappend Usage _ = Usage
  mappend _ Usage = Usage
  mappend e @ Error {} Version = e
  mappend Version e @ Error {} = e
  mappend e @ Error { errDistDir = distdir1, errLastStage = last1 }
          Args { argDistDir = distdir2, argLastStage = last2 } =
    e { errDistDir = distdir1 || distdir2 /= Default,
        errLastStage = last1 || last2 /= Default }
  mappend Args { argDistDir = distdir1, argLastStage = last1 }
          e @ Error { errDistDir = distdir2, errLastStage = last2 } =
    e { errDistDir = distdir2 || distdir1 /= Default,
        errLastStage = last2 || last1 /= Default }
  mappend Args { argTokensSave = toksave1, argASTSave = astsave1,
                 argSurfaceSave = surfacesave1, argLastStage = laststage1,
                 argDistDir = distdir1, argSrcDirs = srcpath1,
                 argComponents = components1 }
          Args { argTokensSave = toksave2, argASTSave = astsave2,
                 argSurfaceSave = surfacesave2, argLastStage = laststage2,
                 argDistDir = distdir2, argSrcDirs = srcpath2,
                 argComponents = components2 } =
    let
      laststage = laststage1 <> laststage2
      distdir = distdir1 <> distdir2
      temp = Args { argTokensSave = toksave1 <> toksave2,
                    argASTSave = astsave1 <> astsave2,
                    argSurfaceSave = surfacesave1 <> surfacesave2,
                    argLastStage = laststage, argDistDir = distdir,
                    argComponents = components1 || components2,
                    argSrcDirs = srcpath1 <> srcpath2 }
      componentErrs =
        if laststage == Multiple
          then ["Multiple ending stages\n"]
          else []
      distdirErrs =
        if distdir == Multiple
          then "Multiple output directories\n" : componentErrs
          else componentErrs
    in
      if null distdirErrs
        then temp
        else Error { errMsgs = distdirErrs, errDistDir = distdir /= Default,
                     errLastStage = laststage /= Default }
  mappend Version Version = Version
  mappend Version args @ Args { argDistDir = distdir, argLastStage = laststage }
    | args == mempty = Version
    | otherwise = Error { errMsgs = ["extra arguments when reporting version\n"],
                          errDistDir = distdir /= Default,
                          errLastStage = laststage /= Default }
  mappend args @ Args { argDistDir = distdir, argLastStage = laststage } Version
    | args == mempty = Version
    | otherwise = Error { errMsgs = ["extra arguments when reporting version\n"],
                          errDistDir = distdir /= Default,
                          errLastStage = laststage /= Default }

setSaveText :: Save
setSaveText = mempty { saveText = True }

keepText :: Maybe String -> Args
keepText Nothing = mempty { argTokensSave = setSaveText,
                            argASTSave = setSaveText,
                            argSurfaceSave = setSaveText }
keepText (Just "all") =
  mempty { argTokensSave = setSaveText, argASTSave = setSaveText,
           argSurfaceSave = setSaveText }
keepText (Just "tokens") =
  mempty { argTokensSave = setSaveText }
keepText (Just "ast") =
  mempty { argASTSave = setSaveText }
keepText (Just "surface") =
  mempty { argSurfaceSave = setSaveText }
keepText (Just txt) =
  Error { errMsgs = ["no compiler structure named " ++ txt ++ "\n"],
          errDistDir = False, errLastStage = False }

setSaveXML :: Save
setSaveXML = mempty { saveXML = True }

keepXML :: Maybe String -> Args
keepXML Nothing = mempty { argTokensSave = setSaveXML,
                           argASTSave = setSaveXML,
                           argSurfaceSave = setSaveXML }
keepXML (Just "all") =
  mempty { argTokensSave = setSaveXML, argASTSave = setSaveXML,
           argSurfaceSave = setSaveXML }
keepXML (Just "tokens") =
  mempty { argTokensSave = setSaveXML }
keepXML (Just "ast") =
  mempty { argASTSave = setSaveXML }
keepXML (Just "surface") =
  mempty { argSurfaceSave = setSaveXML }
keepXML (Just txt) =
  Error { errMsgs = ["no compiler structure named " ++ txt ++ "\n"],
          errDistDir = False, errLastStage = False }

setSaveDot :: Save
setSaveDot = mempty { saveDot = True }

keepDot :: Maybe String -> Args
keepDot Nothing = mempty { argASTSave = setSaveDot,
                           argSurfaceSave = setSaveDot }
keepDot (Just "all") = mempty { argASTSave = setSaveDot,
                                argSurfaceSave = setSaveDot }
keepDot (Just "tokens") =
  Error { errMsgs = ["tokens have no graphviz representation"],
          errDistDir = False, errLastStage = False }
keepDot (Just "ast") =
  mempty { argASTSave = setSaveDot }
keepDot (Just "surface") =
  mempty { argSurfaceSave = setSaveDot }
keepDot (Just txt) =
  Error { errMsgs = ["no compiler structure named " ++ txt ++ "\n"],
          errDistDir = False, errLastStage = False }

distDir :: String -> Args
distDir dir = mempty { argDistDir = Single dir }

stopAfter :: String -> Args
stopAfter "lexer" = mempty { argLastStage = Single Lexer }
stopAfter "parser" = mempty { argLastStage = Single Parser }
stopAfter "collect" = mempty { argLastStage = Single Collect }
stopAfter txt =
  Error { errMsgs = ["no compiler stage named " ++ txt ++ "\n"],
          errDistDir = False, errLastStage = True }

srcPath :: String -> Args
srcPath path = mempty { argSrcDirs = splitSearchPath path }

setComponentNames :: Args
setComponentNames = mempty { argComponents = True }

optionsDesc :: [OptDescr Args]
optionsDesc = [
    Option "d" ["dist-dir"] (ReqArg distDir "DIR")
      "directory for generated files",
    Option "s" ["src-path"] (ReqArg srcPath "DIR")
      "search path for source files",
    Option "V" ["version"] (NoArg Version)
      "display version number",
    Option "C" ["component-names"] (NoArg setComponentNames)
      "supply component names instead of file names",
    Option [] ["help"] (NoArg Usage) "print usage information",

    Option [] ["stop-after"] (ReqArg stopAfter "STAGE")
      "stop after compiler stage",
    Option [] ["keep"] (OptArg keepText "STAGE")
      "save intermediate structures as text",
    Option [] ["keep-xml"] (OptArg keepXML "STAGE")
      "save intermediate structures as text",
    Option [] ["keep-dot"] (OptArg keepDot "STAGE")
      "save intermediate structures as graphviz"
  ]

-- | Get compiler options from the command line.
options :: [String] -> Either [String] Options
options strargs =
  let
    (args, unmatched, errs) = getOpt Permute optionsDesc strargs
  in case mconcat args of
    Version
      | null unmatched -> Left [version]
      | otherwise -> Left ["extra arguments when reporting version\n"]
    Error { errMsgs = errs' } -> Left (errs ++ errs')
    Usage -> Left []
    Args { argLastStage = lastStageArg, argTokensSave = tokensSave,
           argASTSave = astSave, argSurfaceSave = surfacesave,
           argComponents = components, argDistDir = destDirArg,
           argSrcDirs = srcDirsArg }
      | not (null errs) -> Left errs
      | otherwise ->
        let
          thisLast = case lastStageArg of
            Default -> lastStage
            Single stage -> stage
            Multiple -> error "Should not see Multiple at end of arg parsing"

          stageAction Lexer = tokensSave
          stageAction Parser = astSave
          stageAction Collect = surfacesave

          stages = map stageAction (enumFromTo firstStage thisLast)

          destdir = case destDirArg of
            Default -> Nothing
            Single "" -> Nothing
            Single dir
              | isPathSeparator (last dir) -> Just $! Strict.fromString dir
              | otherwise -> Just $! Strict.fromString $! dir ++ [pathSeparator]
            Multiple -> error "Should not see Multiple at end of arg parsing"

          srcdirs = case srcDirsArg of
            [] -> [Strict.fromString ""]
            dirs -> map Strict.fromString dirs
        in
          Right Options { optStages = listArray (firstStage, thisLast) stages,
                          optInputs = map Strict.fromString unmatched,
                          optComponents = components, optSrcDirs = srcdirs,
                          optDistDir = destdir }
