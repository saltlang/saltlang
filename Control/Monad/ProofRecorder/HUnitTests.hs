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

module Control.Monad.ProofRecorder.HUnitTests(
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

applyNameVal = 1
applyPosVal = point "Apply" 2 3 4
applyEntry = Apply { applyName = applyNameVal, applyPos = applyPosVal }

genApply :: ProofRecorder Symbol ()
genApply =
  do
    apply applyPosVal applyNameVal
    return ()

introNameVal = 1
introPosVal = point "Intro" 2 3 4
introEntry = Intro { introName = introNameVal, introPos = introPosVal }

genIntro :: ProofRecorder Symbol ()
genIntro =
  do
    intro introPosVal introNameVal
    return ()

cutPropVal = Var { varSym = 2, varPos = point "Cut" 1 2 3 }
cutPosVal = point "Cut" 4 5 6
cutEntry = Cut { cutProp = cutPropVal, cutPos = cutPosVal }

genCut :: ProofRecorder Symbol ()
genCut =
  do
    cut cutPosVal cutPropVal
    return ()

applyWithPropVal = Var { varSym = 2, varPos = point "ApplyWith" 1 2 3 }
applyWithArgsVals =
  [ Var { varSym = 2, varPos = point "ApplyWith" 4 5 6 },
    Var { varSym = 7, varPos = point "ApplyWith" 7 8 9 } ]
applyWithPosVal = point "ApplyWith" 4 5 6
applyWithEntry = ApplyWith { applyWithProp = applyWithPropVal,
                             applyWithArgs = applyWithArgsVals,
                             applyWithPos = applyWithPosVal }

genApplyWith :: ProofRecorder Symbol ()
genApplyWith =
  do
    applyWith applyWithPosVal applyWithPropVal applyWithArgsVals
    return ()

introVarsMapsVal =
  [ Map.fromList [ (1, 2), (3, 4) ],
    Map.fromList [ (5, 6), (7, 8), (9, 0) ],
    Map.empty ]
introVarsPosVal = point "IntroVars" 2 3 4
introVarsEntry = IntroVars { introVarsMaps = introVarsMapsVal,
                             introVarsPos = introVarsPosVal }

genIntroVars :: ProofRecorder Symbol ()
genIntroVars =
  do
    introVars introVarsPosVal introVarsMapsVal
    return ()

genMulti :: ProofRecorder Symbol ()
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

tests =
  test [
    "apply" ~: do
      (_, entry) <- runProofRecorder genApply
      return ([applyEntry] == entry),
    "intro" ~: do
      (_, entry) <- runProofRecorder genIntro
      return ([introEntry] == entry),
    "cut" ~: do
      (_, entry) <- runProofRecorder genCut
      return ([cutEntry] == entry),
    "apply with" ~: do
      (_, entry) <- runProofRecorder genApplyWith
      return ([applyWithEntry] == entry),
    "intro vars" ~: do
      (_, entry) <- runProofRecorder genIntroVars
      return ([introVarsEntry] == entry),
    "intro vars" ~: do
      (_, entries) <- runProofRecorder genMulti
      return (multiEntries == entries)
  ]
