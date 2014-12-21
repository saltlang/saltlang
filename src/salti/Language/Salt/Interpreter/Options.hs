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

module Language.Salt.Interpreter.Options(
       Options(..),
       Output(..),
       Stage(..),
       options
       ) where

import Data.Array
import Data.Monoid hiding (All)
import System.Console.GetOpt

version :: String
version = "Salt language interpreter, development version\n"

-- | How to output the results of execution.
data Output =
    Render
    -- | Text
  | Text
    -- | XML
  | XML
    deriving (Eq, Show)

-- | Datatype representing compiler stages.
data Stage =
    Lexer
  | Parser
    deriving (Eq, Ord, Enum, Ix, Show)

-- | Options for the interpreter.
data Options =
  Options {
    -- | Run up to this stage of the interpreter.
    optStage :: !Stage,
    -- | How to output results.
    optOutput :: !Output
  }
  deriving Show

lastStage :: Stage
lastStage = Parser

data ArgOption opt =
    Opt !opt
  | Default
  | Conflict
    deriving Eq

-- | Intermediate command-line arguments structure.  This is used by
-- 'getOpt' to record command-line actions.
data Args =
    Args {
      -- | How to output the results
      argOutput :: !(ArgOption Output),
      -- | Run every stage up to this one.
      argLastStage :: !(ArgOption Stage)
    }
  | Version
  | Error [String]
    deriving Eq

instance Monoid (ArgOption opt) where
  mempty = Default

  mappend Default out = out
  mappend out Default = out
  mappend _ _ = Conflict

instance Monoid Args where
  mempty = Args { argOutput = mempty, argLastStage = mempty }

  mappend (Error e1) (Error e2) = Error (e1 <> e2)
  mappend e @ (Error _) _ = e
  mappend _ e @ (Error _) = e
  mappend Args { argOutput = output1, argLastStage = laststage1 }
          Args { argOutput = output2, argLastStage = laststage2 } =
    case laststage1 <> laststage2 of
      Conflict -> Error ["Multiple ending stages\n"]
      laststage ->
        case output1 <> output2 of
          Conflict -> Error ["Multiple ending stages\n"]
          output -> Args { argOutput = output, argLastStage = laststage }
  mappend Version Version = Version
  mappend Version args
    | args == mempty = Version
    | otherwise = Error ["extra arguments when reporting version\n"]
  mappend args Version
    | args == mempty = Version
    | otherwise = Error ["extra arguments when reporting version\n"]

stopAfter :: String -> Args
stopAfter "lexer" = mempty { argLastStage = Opt Lexer }
stopAfter "parser" = mempty { argLastStage = Opt Parser }
stopAfter txt = Error ["no compiler stage named " ++ txt ++ "\n"]

setOutput :: String -> Args
setOutput "xml" = mempty { argOutput = Opt XML }
setOutput "text" = mempty { argOutput = Opt Text }
setOutput txt = Error ["no output kind named " ++ txt ++ "\n"]

optionsDesc :: [OptDescr Args]
optionsDesc = [
    Option "s" ["stop-after"] (ReqArg stopAfter "STAGE")
      "stop after compiler stage",
    Option "V" ["version"] (NoArg Version)
      "display version number",

    Option [] ["output"] (ReqArg setOutput "STAGE")
      "save intermediate structures as text"
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
    Args { argOutput = output, argLastStage = stage }
      | not (null errs) -> Left errs
      | otherwise ->
        let
          thisStage = case stage of
            Default -> lastStage
            Opt stage' -> stage'
            Conflict -> error "Should not see Conflict at end of arg parsing"

          thisOutput = case output of
            Default -> Render
            Opt stage' -> stage'
            Conflict -> error "Should not see Conflict at end of arg parsing"
        in
          Right Options { optOutput = thisOutput, optStage = thisStage }
