{-# LANGUAGE ExistentialQuantification #-}

module Distribution.TestSuite.QuickCheck(
       test,
       testTags
       ) where

import Distribution.TestSuite
import qualified Test.QuickCheck as QC

runTest :: QC.Testable prop => prop -> IO Progress
runTest prop =
  let
    args = QC.stdArgs { QC.maxSize = 30 }
  in do
    result <- QC.quickCheckWithResult args prop
    case result of
      QC.Success {} -> return (Finished Pass)
      _ -> return (Finished (Fail (show result)))

test :: QC.Testable prop => String -> prop -> Test
test name = testTags name []

testTags :: QC.Testable prop => String -> [String] -> prop -> Test
testTags name tags prop =
  Test TestInstance {
      run = runTest prop, name = name, tags = tags,
      options = [], setOption = undefined
    }
