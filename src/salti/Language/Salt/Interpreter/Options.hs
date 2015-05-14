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
