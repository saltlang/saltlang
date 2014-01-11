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

module Test.UnitTests.Control.Monad.ProofRecorder(
       testsuite
       ) where

import Control.Monad.ProofRecorder
import Data.Pos
import Data.Default
import Language.Salt.Core.Proofs.ProofScript
import Language.Salt.Core.Syntax
import Test.HUnit

import qualified Data.Map as Map

testsuite :: Test
testsuite = TestLabel "ProofRecorder" tests

type Symbol = Int

instance Default Int where
  defaultVal = 0

-- | Run a test case
runTestCase :: String
            -- ^ The name of the test case.
            -> (ProofRecorderT Symbol IO (), ProofScript Symbol)
            -- ^ The test case to run.
            -> Test
runTestCase testname (command, expected) =
  testname ~: do
    ((), actual) <- runProofRecorder command
    return (expected @?= actual)

-- | Generate an apply test case from parameters
assumptionTestCase :: Monad m =>
                      Pos
                   -- ^ The position.
                   -> Symbol
                   -- ^ The name.
                   -> (ProofRecorderT Symbol m (), ProofScript Symbol)
                   -- ^ A monad to execute, and the proof script that
                   -- should result from it.
assumptionTestCase pos name =
  let
    doAssumption :: Monad m => ProofRecorderT Symbol m ()
    doAssumption =
      do
        assumption pos name
        return ()
  in
    (doAssumption, [Assumption { assumptionName = name, assumptionPos = pos }])

-- | Generate an apply test from parameters
assumptionTest :: Pos
               -- ^ The position.
               -> Symbol
               -- ^ The name.
               -> Test
               -- ^ A test that "assumption pos name" generates a proof
               -- script containing an Assumption element with the right
               -- data.
assumptionTest pos name =
  runTestCase ("assumption " ++ show pos ++ " " ++ show name)
              (assumptionTestCase pos name)

introTestCase :: Monad m =>
                 Pos
              -- ^ The position.
              -> Symbol
              -- ^ The name.
              -> (ProofRecorderT Symbol m (), ProofScript Symbol)
              -- ^ A monad to execute, and the proof script that
              -- should result from it.
introTestCase pos name =
  let
    doIntro :: Monad m => ProofRecorderT Symbol m ()
    doIntro =
      do
        intro pos name
        return ()
  in
    (doIntro, [Intro { introName = name, introPos = pos }])

introTest :: Pos
          -- ^ The position.
          -> Symbol
          -- ^ The name.
          -> Test
          -- ^ A test that "intro pos name" generates a proof script
          -- containing an Intro element with the right data.
introTest pos name = runTestCase ("intro " ++ show pos ++ " " ++ show name)
                                 (introTestCase pos name)

cutTestCase :: Monad m =>
               Pos
            -- ^ The position.
            -> Term Symbol Symbol
            -- ^ The cut proposition.
            -> (ProofRecorderT Symbol m (), ProofScript Symbol)
            -- ^ A monad to execute, and the proof script that
            -- should result from it.
cutTestCase pos prop =
  let
    doCut :: Monad m => ProofRecorderT Symbol m ()
    doCut =
      do
        cut pos prop
        return ()
  in
    (doCut, [Cut { cutProp = prop, cutPos = pos }])

cutTest :: Pos
        -- ^ The position.
        -> Term Symbol Symbol
        -- ^ The cut proposition.
        -> Test
        -- ^ A test that "cut pos prop" generates a proof script
        -- containing a Cut element with the right data.
cutTest pos prop = runTestCase ("cut " ++ show pos ++ " " ++ show prop)
                               (cutTestCase pos prop)

-- | Generate an apply test case from parameters
applyTestCase :: Monad m =>
                 Pos
              -- ^ The position.
              -> Term Symbol Symbol
              -- ^ The proposition to apply.
              -> [Term Symbol Symbol]
              -- ^ The arguments to the proposition.
              -> (ProofRecorderT Symbol m (), ProofScript Symbol)
              -- ^ A monad to execute, and the proof script that
              -- should result from it.
applyTestCase pos prop args =
  let
    doApply :: Monad m => ProofRecorderT Symbol m ()
    doApply =
      do
        apply pos prop args
        return ()
  in
    (doApply, [Apply { applyProp = prop, applyArgs = args, applyPos = pos }])

-- | Generate an apply test from parameters
applyTest :: Pos
          -- ^ The position.
          -> Term Symbol Symbol
          -- ^ The proposition to apply.
          -> [Term Symbol Symbol]
          -- ^ The arguments to the proposition.
          -> Test
          -- ^ A test that "applyWith pos name" generates a proof
          -- script containing an ApplyWith element with the right
          -- data.
applyTest pos prop args =
  runTestCase ("apply " ++ show pos ++ " " ++ show prop ++ " " ++ show args)
              (applyTestCase pos prop args)

introVarsTestCase :: Monad m =>
                     Pos
                  -- ^ The position.
                  -> [Map.Map Symbol Symbol]
                  -- ^ The renaming maps.
                  -> (ProofRecorderT Symbol m (), ProofScript Symbol)
                  -- ^ A monad to execute, and the proof script that
                  -- should result from it.
introVarsTestCase pos maps =
  let
    doIntro :: Monad m => ProofRecorderT Symbol m ()
    doIntro =
      do
        introVars pos maps
        return ()
  in
    (doIntro, [IntroVars { introVarsMaps = maps, introVarsPos = pos }])

introVarsTest :: Pos
              -- ^ The position.
              -> [Map.Map Symbol Symbol]
              -- ^ The renaming maps.
              -> Test
              -- ^ A test that "introVars pos maps" generates a proof
              -- script containing an IntroVars element with the right
              -- data.
introVarsTest pos maps =
  runTestCase ("introvars " ++ show pos ++ " " ++ show maps)
              (introVarsTestCase pos maps)

{-
genMulti :: Monad m => ProofRecorderT Symbol m ()
genMulti =
  do
    apply applyPosVal applyNameVal
    intro introPosVal introNameVal
    cut cutPosVal cutPropVal
    applyWith applyWithPosVal applyWithPropVal applyWithArgsVals
    introVars introVarsPosVal introVarsMapsVal
    return ()

multiEntries =
  [ applyEntry, introEntry, cutEntry, applyWithEntry, introVarsEntry ]
-}
tests =
  test [
    assumptionTest (point "Assumption" 1 2 3) 1,
    introTest (point "Intro" 4 5 6) 2,
    cutTest (point "Cut" 4 5 6) Var { varSym = 2, varPos = point "Cut" 1 2 3 },
    applyTest (point "Apply" 1 2 3)
              Var { varSym = 2, varPos = point "ApplyWith" 1 2 3 }
              [ Var { varSym = 2, varPos = point "ApplyWith" 4 5 6 },
                Var { varSym = 7, varPos = point "ApplyWith" 7 8 9 } ],
    introVarsTest (point "IntroVars" 2 3 4)
                  [ Map.fromList [ (1, 2), (3, 4) ],
                    Map.fromList [ (5, 6), (7, 8), (9, 0) ],
                    Map.empty ]
{-
    "multi" ~: do
      (_, entries) <- runProofRecorder genMulti
      return (multiEntries == entries)
-}
  ]
