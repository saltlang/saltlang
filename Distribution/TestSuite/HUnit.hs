-- Copyright (c) 2013, 2014 Eric McCorkle.
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
       test,
       testTags
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

test :: String -> HUnit.Test -> Test
test name = testTags name []

testTags :: String -> [String] -> HUnit.Test -> Test
testTags name tags prop =
  Test TestInstance {
      run = runTest prop, name = name, tags = tags,
      options = [], setOption = undefined
    }
