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

module Distribution.TestSuite.HUnit(
       convertTests,
       ) where

import Distribution.TestSuite
import qualified Test.HUnit as HUnit

runTest :: HUnit.Test -> IO Progress
runTest test =
  do
    (counts, shows) <- HUnit.runTestText HUnit.putTextToShowS test
    if HUnit.errors counts /= 0
      then if HUnit.failures counts /= 0
        then return (Finished Pass)
        else return (Finished (Fail (shows "")))
      else return (Finished (Error (shows "")))

convertTests :: HUnit.Test -> Test
convertTests =
  let
    convertTests' :: String -> HUnit.Test -> Test
    convertTests' path (HUnit.TestLabel name (HUnit.TestList tests)) =
      Group { groupTests = map (convertTests' (path ++ "." ++ name)) tests,
              groupName = path, concurrently = True }
    convertTests' path (HUnit.TestLabel name test) =
      Group { groupTests = [convertTests' (path ++ "." ++ name) test],
              groupName = path, concurrently = True }
    convertTests' path (HUnit.TestList tests) =
      Group { groupTests = map (convertTests' path) tests,
              groupName = path, concurrently = True }
    convertTests' path test @ (HUnit.TestCase _) =
      Test TestInstance { run = runTest test, name = path, tags = [],
                          options = [], setOption = undefined }
  in
    convertTests' ""
