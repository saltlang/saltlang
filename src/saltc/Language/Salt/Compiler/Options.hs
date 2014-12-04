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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module Language.Salt.Compiler.Options(
       Options(..),
       Save(..),
       Stage(..),
       options
       ) where

import Data.Array
import Data.Monoid hiding (All)
import System.Console.GetOpt

version :: String
version = "saltc compiler, development version\n"

-- | Options for the compiler.
data Options =
  Options {
    -- | Input files.
    optInputFiles :: ![FilePath],
    -- | An array of all stages to run, and which structures to save.
    optStages :: !(Array Stage Save)
  }
  deriving Show

-- | What to save at a given stage.
data Save =
  Save {
    -- | Save a textual representation.
    saveText :: !Bool,
    -- | Save an XML representation.
    saveXML :: !Bool
  }
  deriving (Eq, Show)

-- | Datatype representing compiler stages.
data Stage =
    Lexer
  | Parser
    deriving (Eq, Ord, Enum, Ix, Show)

firstStage :: Stage
firstStage = Lexer

lastStage :: Stage
lastStage = Parser

data LastStage =
    Stage !Stage
  | All
  | Conflict
    deriving Eq

-- | Intermediate command-line arguments structure.  This is used by
-- 'getOpt' to record command-line actions.
data Args =
    Args {
      -- | Whether to save tokens.
      argTokensSave :: !Save,
      -- | Whether to save the ASTs.
      argASTSave :: !Save,
      -- | Run every stage up to this one.
      argLastStage :: !LastStage
    }
  | Version
  | Error [String]
    deriving Eq

instance Monoid Save where
  mempty = Save { saveText = False, saveXML = False }
  mappend Save { saveText = text1, saveXML = xml1 }
          Save { saveText = text2, saveXML = xml2 } =
    Save { saveText = text1 || text2, saveXML = xml1 || xml2 }

instance Monoid LastStage where
  mempty = All
  mappend All out = out
  mappend out All = out
  mappend _ _ = Conflict

instance Monoid Args where
  mempty = Args { argTokensSave = mempty, argASTSave = mempty,
                  argLastStage = mempty }

  mappend (Error e1) (Error e2) = Error (e1 <> e2)
  mappend e @ (Error _) _ = e
  mappend _ e @ (Error _) = e
  mappend Args { argTokensSave = toksave1, argASTSave = astsave1,
                 argLastStage = laststage1 }
          Args { argTokensSave = toksave2, argASTSave = astsave2,
                 argLastStage = laststage2 } =
    case laststage1 <> laststage2 of
      Conflict -> Error ["Multiple ending stages\n"]
      laststage -> Args { argTokensSave = toksave1 <> toksave2,
                          argASTSave = astsave1 <> astsave2,
                          argLastStage = laststage }
  mappend Version Version = Version
  mappend Version args
    | args == mempty = Version
    | otherwise = Error ["extra arguments when reporting version\n"]
  mappend args Version
    | args == mempty = Version
    | otherwise = Error ["extra arguments when reporting version\n"]

setSaveText :: Save
setSaveText = mempty { saveText = True }

keepText :: Maybe String -> Args
keepText Nothing = mempty { argTokensSave = setSaveText,
                            argASTSave = setSaveText }
keepText (Just "all") =
  mempty { argTokensSave = setSaveText, argASTSave = setSaveText }
keepText (Just "tokens") =
  mempty { argTokensSave = setSaveText }
keepText (Just "ast") =
  mempty { argASTSave = setSaveText }
keepText (Just txt) = Error ["no compiler structure named " ++ txt ++ "\n"]

setSaveXML :: Save
setSaveXML = mempty { saveXML = True }

keepXML :: Maybe String -> Args
keepXML Nothing = mempty { argTokensSave = setSaveXML,
                            argASTSave = setSaveXML }
keepXML (Just "all") =
  mempty { argTokensSave = setSaveXML, argASTSave = setSaveXML }
keepXML (Just "tokens") =
  mempty { argTokensSave = setSaveXML }
keepXML (Just "ast") =
  mempty { argASTSave = setSaveXML }
keepXML (Just txt) = Error ["no compiler structure named " ++ txt ++ "\n"]

stopAfter :: String -> Args
stopAfter "lexer" = mempty { argLastStage = Stage Lexer }
stopAfter "parser" = mempty { argLastStage = Stage Parser }
stopAfter txt = Error ["no compiler stage named " ++ txt ++ "\n"]

optionsDesc :: [OptDescr Args]
optionsDesc = [
    Option ['V'] ["version"] (NoArg Version)
      "display version number",
    Option ['k'] ["keep"] (OptArg keepText "STAGE")
      "save intermediate structures as text",
    Option ['K'] ["keep-xml"] (OptArg keepXML "STAGE")
      "save intermediate structures as text",
    Option ['s'] ["stop-after"] (ReqArg stopAfter "STAGE")
      "stop after compiler stage"
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
    Error errs' -> Left (errs ++ errs')
    Args { argLastStage = lastStageArg, argTokensSave = tokensSave,
           argASTSave = astSave }
      | not (null errs) -> Left errs
      | otherwise ->
        let
          thisLast = case lastStageArg of
            All -> lastStage
            Stage stage -> stage
            Conflict -> error "Should not see Conflict at end of arg parsing"

          stageAction Lexer = tokensSave
          stageAction Parser = astSave

          stages = map stageAction (enumFromTo firstStage thisLast)
        in
          Right Options { optStages = listArray (firstStage, thisLast) stages,
                          optInputFiles = unmatched }
