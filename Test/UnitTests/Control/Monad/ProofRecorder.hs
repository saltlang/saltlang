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
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Pos
import Data.Default
import Language.Salt.Core.Proofs.ProofScript
import Language.Salt.Core.Syntax
import Test.HUnit
import Test.Utils.ComboTests

import qualified Data.Map as Map

testsuite :: Test
testsuite = TestLabel "ProofRecorder" (TestList tests)

type Symbol = Int

instance Default Int where
  defaultVal = 0

-- | Run a test case
runTestCase :: (String, ProofRecorderT Symbol IO (), ProofScript Symbol)
            -- ^ The test case to run.
            -> Test
runTestCase (testname, command, expected) =
  testname ~: do
    ((), actual) <- runProofRecorder command
    return (expected @?= actual)

-- | Run a test case with a proof recorder instade a StateT
runRecorderInStateTestCase :: (String,
                               StateT Int (ProofRecorderT Symbol IO) (Int, Int),
                               Int, ProofScript Symbol,
                               Int -> Int, Int -> Int) ->
                              Test
runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                            beforefunc, afterfunc) =
  let
    beforeexpected = beforefunc initial
    afterexpected = afterfunc beforeexpected
  in
    testname ~: do
      (((beforeactual, afteractual), resultactual), scriptactual) <-
        runProofRecorderT (runStateT command initial)
      return (scriptexpected == scriptactual &&
              beforeexpected == beforeactual &&
              afterexpected == afteractual &&
              afterexpected == resultactual)

-- | Run a test case with a proof recorder instade a StateT
runRecorderInReaderTestCase :: (String,
                                ReaderT Int (ProofRecorderT Symbol IO)
                                        (Int, Int),
                                Int, ProofScript Symbol,
                                Int -> Int, Int -> Int) ->
                              Test
runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                             beforefunc, afterfunc) =
  let
    beforeexpected = beforefunc initial
    afterexpected = afterfunc initial
  in
    testname ~: do
      ((beforeactual, afteractual), scriptactual) <-
        runProofRecorderT (runReaderT command initial)
      return (scriptexpected == scriptactual &&
              beforeexpected == beforeactual &&
              afterexpected == afteractual)

-- | Run a test case with a proof recorder instade a StateT
runRecorderInWriterTestCase :: (String,
                                WriterT [Int] (ProofRecorderT Symbol IO) (),
                                ProofScript Symbol, [Int]) ->
                               Test
runRecorderInWriterTestCase (testname, command, scriptexpected,
                             resultexpected) =
  testname ~: do
    (((), resultactual), scriptactual) <-
      runProofRecorderT (runWriterT command)
    return (scriptexpected == scriptactual && resultexpected == resultactual)

assumptionTestCase :: Monad m =>
                      Pos
                   -- ^ The position.
                   -> Symbol
                   -- ^ The name.
                   -> (String, ProofRecorderT Symbol m (), ProofScript Symbol)
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
    ("assumption " ++ show pos ++ " " ++ show name, doAssumption,
     [Assumption { assumptionName = name, assumptionPos = pos }])


assumptionInStateTest :: Int
                      -- ^ The initial state value.
                      -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                          Int -> Int)
                      -- ^ The state monad to execute before the proof recorder.
                      -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                          Int -> Int)
                      -- ^ The state monad to execute after the proof recorder.
                      ->  Pos
                      -- ^ The position.
                      -> Symbol
                      -- ^ The name.
                      -> Test
                      -- ^ A monad to execute, and the proof script
                      -- that should result from it.
assumptionInStateTest initial (beforename, before, beforefunc)
                      (aftername, after, afterfunc) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: StateT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        assumption pos name
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Assumption { assumptionName = name, assumptionPos = pos }]
    testname = "assumption " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                                beforefunc, afterfunc)

assumptionInReaderTest :: Int
                       -- ^ The initial state value.
                       -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                           Int -> Int)
                       -- ^ The reader monad to execute before the
                       -- proof recorder.
                       -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                           Int -> Int)
                       -- ^ The reader monad to execute after the
                       -- proof recorder.
                       ->  Pos
                       -- ^ The position.
                       -> Symbol
                       -- ^ The name.
                       -> Test
                       -- ^ A monad to execute, and the proof script
                       -- that should result from it.
assumptionInReaderTest initial (beforename, before, beforefunc)
                       (aftername, after, afterfunc) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: ReaderT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        assumption pos name
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Assumption { assumptionName = name, assumptionPos = pos }]
    testname = "assumption " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                                 beforefunc, afterfunc)

assumptionInWriterTest :: (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                           [Int])
                       -- ^ The reader monad to execute before the
                       -- proof recorder.
                       -> (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                           [Int])
                       -- ^ The reader monad to execute after the
                       -- proof recorder.
                       -> Pos
                       -- ^ The position.
                       -> Symbol
                       -- ^ The name.
                       -> Test
                       -- ^ A monad to execute, and the proof script
                       -- that should result from it.
assumptionInWriterTest (beforename, before, beforeval)
                       (aftername, after, afterval) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: WriterT [Int] (ProofRecorderT Symbol IO) ()
    command =
      do
        before
        assumption pos name
        after

    scriptexpected = [Assumption { assumptionName = name, assumptionPos = pos }]
    testname = "assumption " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInWriterTestCase (testname, command, scriptexpected,
                                 beforeval ++ afterval)

introTestCase :: Monad m =>
                 Pos
              -- ^ The position.
              -> Symbol
              -- ^ The name.
              -> (String, ProofRecorderT Symbol m (), ProofScript Symbol)
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
    ("intro " ++ show pos ++ " " ++ show name, doIntro,
     [Intro { introName = name, introPos = pos }])

introInStateTest :: Int
                 -- ^ The initial state value.
                 -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                     Int -> Int)
                 -- ^ The state monad to execute before the proof recorder.
                 -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                     Int -> Int)
                 -- ^ The state monad to execute after the proof recorder.
                 ->  Pos
                 -- ^ The position.
                 -> Symbol
                 -- ^ The name.
                 -> Test
                 -- ^ A monad to execute, and the proof script
                 -- that should result from it.
introInStateTest initial (beforename, before, beforefunc)
                         (aftername, after, afterfunc) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: StateT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        intro pos name
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Intro { introName = name, introPos = pos }]
    testname = "intro " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                                beforefunc, afterfunc)

introInReaderTest :: Int
                  -- ^ The initial state value.
                  -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                      Int -> Int)
                  -- ^ The reader monad to execute before the proof recorder.
                  -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                      Int -> Int)
                  -- ^ The reader monad to execute after the proof recorder.
                  ->  Pos
                  -- ^ The position.
                  -> Symbol
                  -- ^ The name.
                  -> Test
                  -- ^ A monad to execute, and the proof script
                  -- that should result from it.
introInReaderTest initial (beforename, before, beforefunc)
                          (aftername, after, afterfunc) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: ReaderT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        intro pos name
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Intro { introName = name, introPos = pos }]
    testname = "intro " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                                 beforefunc, afterfunc)

introInWriterTest :: (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                      [Int])
                  -- ^ The reader monad to execute before the proof recorder.
                  -> (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                      [Int])
                  -- ^ The reader monad to execute after the proof recorder.
                  ->  Pos
                  -- ^ The position.
                  -> Symbol
                  -- ^ The name.
                  -> Test
                  -- ^ A monad to execute, and the proof script
                  -- that should result from it.
introInWriterTest (beforename, before, beforeval)
                  (aftername, after, afterval) pos name =
  let
    -- Monad to run the test and collect the values.
    command :: WriterT [Int] (ProofRecorderT Symbol IO) ()
    command =
      do
        before
        intro pos name
        after

    scriptexpected = [Intro { introName = name, introPos = pos }]
    testname = "intro " ++ show pos ++ " " ++ show name ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInWriterTestCase (testname, command, scriptexpected,
                                 beforeval ++ afterval)

cutTestCase :: Monad m =>
               Pos
            -- ^ The position.
            -> Term Symbol Symbol
            -- ^ The cut proposition.
            -> (String, ProofRecorderT Symbol m (), ProofScript Symbol)
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
    ("cut " ++ show pos ++ " " ++ show prop, doCut,
     [Cut { cutProp = prop, cutPos = pos }])

cutInStateTest :: Int
               -- ^ The initial state value.
               -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                   Int -> Int)
               -- ^ The state monad to execute before the proof recorder.
               -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                   Int -> Int)
               -- ^ The state monad to execute after the proof recorder.
               -> Pos
               -- ^ The position.
               -> Term Symbol Symbol
               -- ^ The cut proposition.
               -> Test
               -- ^ A monad to execute, and the proof script
               -- that should result from it.
cutInStateTest initial (beforename, before, beforefunc)
                       (aftername, after, afterfunc) pos prop =
  let
    -- Monad to run the test and collect the values.
    command :: StateT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        cut pos prop
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Cut { cutProp = prop, cutPos = pos }]
    testname = "cut " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                                beforefunc, afterfunc)

cutInReaderTest :: Int
                -- ^ The initial state value.
                -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                    Int -> Int)
                -- ^ The reader monad to execute before the proof recorder.
                -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                    Int -> Int)
                -- ^ The reader monad to execute after the proof recorder.
                -> Pos
                -- ^ The position.
                -> Term Symbol Symbol
                -- ^ The cut proposition.
                -> Test
                -- ^ A monad to execute, and the proof script
                -- that should result from it.
cutInReaderTest initial (beforename, before, beforefunc)
                        (aftername, after, afterfunc) pos prop =
  let
    -- Monad to run the test and collect the values.
    command :: ReaderT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        cut pos prop
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Cut { cutProp = prop, cutPos = pos }]
    testname = "cut " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                                 beforefunc, afterfunc)

cutInWriterTest :: (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                    [Int])
                -- ^ The reader monad to execute before the proof recorder.
                -> (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                    [Int])
                -- ^ The reader monad to execute after the proof recorder.
                -> Pos
                -- ^ The position.
                -> Term Symbol Symbol
                -- ^ The cut proposition.
                -> Test
                -- ^ A monad to execute, and the proof script
                -- that should result from it.
cutInWriterTest (beforename, before, beforeval)
                (aftername, after, afterval) pos prop =
  let
    -- Monad to run the test and collect the values.
    command :: WriterT [Int] (ProofRecorderT Symbol IO) ()
    command =
      do
        before
        cut pos prop
        after

    scriptexpected = [Cut { cutProp = prop, cutPos = pos }]
    testname = "cut " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInWriterTestCase (testname, command, scriptexpected,
                                 beforeval ++ afterval)

applyTestCase :: Monad m =>
                 Pos
              -- ^ The position.
              -> Term Symbol Symbol
              -- ^ The proposition to apply.
              -> [Term Symbol Symbol]
              -- ^ The arguments to the proposition.
              -> (String, ProofRecorderT Symbol m (), ProofScript Symbol)
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
    ("apply " ++ show pos ++ " " ++ show prop ++ " " ++ show args, doApply,
     [Apply { applyProp = prop, applyArgs = args, applyPos = pos }])

applyInStateTest :: Int
                 -- ^ The initial state value.
                 -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                     Int -> Int)
                 -- ^ The state monad to execute before the proof recorder.
                 -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                     Int -> Int)
                 -- ^ The state monad to execute after the proof recorder.
                 ->  Pos
                 -- ^ The position.
                 -> Term Symbol Symbol
                 -- ^ The proposition to apply.
                 -> [Term Symbol Symbol]
                 -- ^ The arguments.
                 -> Test
                 -- ^ A monad to execute, and the proof script
                 -- that should result from it.
applyInStateTest initial (beforename, before, beforefunc)
                       (aftername, after, afterfunc) pos prop args =
  let
    -- Monad to run the test and collect the values.
    command :: StateT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        apply pos prop args
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Apply { applyProp = prop, applyArgs = args,
                              applyPos = pos }]
    testname = "apply " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                                beforefunc, afterfunc)

applyInReaderTest :: Int
                  -- ^ The initial state value.
                  -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                      Int -> Int)
                  -- ^ The reader monad to execute before the proof recorder.
                  -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                      Int -> Int)
                  -- ^ The reader monad to execute after the proof recorder.
                  ->  Pos
                  -- ^ The position.
                  -> Term Symbol Symbol
                  -- ^ The proposition to apply.
                  -> [Term Symbol Symbol]
                  -- ^ The arguments.
                  -> Test
                  -- ^ A monad to execute, and the proof script
                  -- that should result from it.
applyInReaderTest initial (beforename, before, beforefunc)
                          (aftername, after, afterfunc) pos prop args =
  let
    -- Monad to run the test and collect the values.
    command :: ReaderT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        apply pos prop args
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [Apply { applyProp = prop, applyArgs = args,
                              applyPos = pos }]
    testname = "apply " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                                 beforefunc, afterfunc)

applyInWriterTest :: (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                      [Int])
                  -- ^ The reader monad to execute before the proof recorder.
                  -> (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                      [Int])
                  -- ^ The reader monad to execute after the proof recorder.
                  ->  Pos
                  -- ^ The position.
                  -> Term Symbol Symbol
                  -- ^ The proposition to apply.
                  -> [Term Symbol Symbol]
                  -- ^ The arguments.
                  -> Test
                  -- ^ A monad to execute, and the proof script
                  -- that should result from it.
applyInWriterTest (beforename, before, beforeval)
                  (aftername, after, afterval) pos prop args =
  let
    -- Monad to run the test and collect the values.
    command :: WriterT [Int] (ProofRecorderT Symbol IO) ()
    command =
      do
        before
        apply pos prop args
        after

    scriptexpected = [Apply { applyProp = prop, applyArgs = args,
                              applyPos = pos }]
    testname = "apply " ++ show pos ++ " " ++ show prop ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInWriterTestCase (testname, command, scriptexpected,
                                 beforeval ++ afterval)

introVarsTestCase :: Monad m =>
                     Pos
                  -- ^ The position.
                  -> [Map.Map Symbol Symbol]
                  -- ^ The renaming maps.
                  -> (String, ProofRecorderT Symbol m (), ProofScript Symbol)
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
    ("introvars " ++ show pos ++ " " ++ show maps, doIntro,
     [IntroVars { introVarsMaps = maps, introVarsPos = pos }])

introVarsInStateTest :: Int
                     -- ^ The initial state value.
                     -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                         Int -> Int)
                     -- ^ The state monad to execute before the proof recorder.
                     -> (String, StateT Int (ProofRecorderT Symbol IO) Int,
                         Int -> Int)
                     -- ^ The state monad to execute after the proof recorder.
                     ->  Pos
                     -- ^ The position.
                     -> [Map.Map Symbol Symbol]
                     -- ^ The renaming maps.
                     -> Test
                     -- ^ A monad to execute, and the proof script
                     -- that should result from it.
introVarsInStateTest initial (beforename, before, beforefunc)
                             (aftername, after, afterfunc) pos maps =
  let
    -- Monad to run the test and collect the values.
    command :: StateT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        introVars pos maps
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [IntroVars { introVarsMaps = maps, introVarsPos = pos }]
    testname = "introVars " ++ show pos ++ " " ++ show maps ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInStateTestCase (testname, command, initial, scriptexpected,
                                beforefunc, afterfunc)

introVarsInReaderTest :: Int
                      -- ^ The initial state value.
                      -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                          Int -> Int)
                      -- ^ The reader monad to execute before the
                      -- proof recorder.
                      -> (String, ReaderT Int (ProofRecorderT Symbol IO) Int,
                          Int -> Int)
                      -- ^ The reader monad to execute after the proof recorder.
                      ->  Pos
                      -- ^ The position.
                      -> [Map.Map Symbol Symbol]
                      -- ^ The renaming maps.
                      -> Test
                      -- ^ A monad to execute, and the proof script
                      -- that should result from it.
introVarsInReaderTest initial (beforename, before, beforefunc)
                              (aftername, after, afterfunc) pos maps =
  let
    -- Monad to run the test and collect the values.
    command :: ReaderT Int (ProofRecorderT Symbol IO) (Int, Int)
    command =
      do
        beforeval <- before
        introVars pos maps
        afterval <- after
        return (beforeval, afterval)

    scriptexpected = [IntroVars { introVarsMaps = maps, introVarsPos = pos }]
    testname = "introVars " ++ show pos ++ " " ++ show maps ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInReaderTestCase (testname, command, initial, scriptexpected,
                                 beforefunc, afterfunc)

introVarsInWriterTest :: (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                          [Int])
                      -- ^ The reader monad to execute before the
                      -- proof recorder.
                      -> (String, WriterT [Int] (ProofRecorderT Symbol IO) (),
                          [Int])
                      -- ^ The reader monad to execute after the proof recorder.
                      ->  Pos
                      -- ^ The position.
                      -> [Map.Map Symbol Symbol]
                      -- ^ The renaming maps.
                      -> Test
                      -- ^ A monad to execute, and the proof script
                      -- that should result from it.
introVarsInWriterTest (beforename, before, beforeval)
                      (aftername, after, afterval) pos maps =
  let
    -- Monad to run the test and collect the values.
    command :: WriterT [Int] (ProofRecorderT Symbol IO) ()
    command =
      do
        before
        introVars pos maps
        after

    scriptexpected = [IntroVars { introVarsMaps = maps, introVarsPos = pos }]
    testname = "introVars " ++ show pos ++ " " ++ show maps ++
               " inside " ++ beforename ++ " and " ++ aftername
  in
    runRecorderInWriterTestCase (testname, command, scriptexpected,
                                 beforeval ++ afterval)

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

-- Data for generating combo tests.  Add more cases here to expand coverage.
symbols = [ 1, 2, 3 ]

positions = [ point "point" 1 2 3, internal "internal",
              range "range" 1 2 3 4 5 6 ]

terms = [ Var { varSym = 1, varPos = internal "term" } ]

args = [ [ Var { varSym = 1, varPos = internal "term" },
           Var { varSym = 2, varPos = internal "term" },
           Var { varSym = 3, varPos = internal "term" } ],
         [ Var { varSym = 1, varPos = internal "term" },
           Var { varSym = 2, varPos = internal "term" } ],
         [ Var { varSym = 1, varPos = internal "term" } ],
         [] ]

maps = [ [ Map.fromList [ (1, 2), (3, 4), (5, 5), (4, 3) ],
           Map.fromList [ (1, 2), (3, 4) ],
           Map.fromList [ (1, 2) ],
           Map.empty ],
         [ Map.fromList [ (1, 2), (3, 4) ],
           Map.fromList [ (1, 2) ],
           Map.empty ],
         [ Map.fromList [ (1, 2) ],
           Map.empty ],
         [ Map.empty ],
         [] ]

initials = [ 1, 2, 3 ]

-- | Writer monads, and what we expect to get out of them.
writerMonads :: Monad m => [(String, WriterT [Int] m (), [Int])]
writerMonads = [ ("tell [5]", tell [5], [5]),
                 ("tell [1] >> tell [2]", tell [1] >> tell [2], [1, 2]) ]

-- | Reader monads, and what we expect them to do to the inital item.
readerMonads :: Monad m => [(String, ReaderT Int m Int, Int -> Int)]
readerMonads = [ ("ask", ask, id),
                 ("local (+ 1) ask", local (+ 1) ask, (+ 1)) ]

-- | State monads, and what we expect them to do to the inital item.
stateMonads :: Monad m => [(String, StateT Int m Int, Int -> Int)]
stateMonads = [ ("get", get, id),
                ("put 5 >> get", put 5 >> get, (\_ -> 5)),
                ("get >>= (\n -> put (n + 1)) >> get",
                 get >>= (\n -> put (n + 1)) >> get, (+ 1)) ]

assumptionComboTest = combos2 assumptionTestCase positions symbols
introComboTest = combos2 introTestCase positions symbols
cutComboTest = combos2 cutTestCase positions terms
applyComboTest = combos3 applyTestCase positions terms args
introVarsComboTest = combos2 introVarsTestCase positions maps

proofRecorderMonads =
  assumptionComboTest ++
  introComboTest ++
  cutComboTest ++
  applyComboTest ++
  introVarsComboTest

assumptionInStateComboTest = combos5 assumptionInStateTest initials stateMonads
                                     stateMonads positions symbols

introInStateComboTest = combos5 introInStateTest initials stateMonads
                                stateMonads positions symbols

cutInStateComboTest = combos5 cutInStateTest initials stateMonads
                              stateMonads positions terms

applyInStateComboTest = combos6 applyInStateTest initials stateMonads
                                stateMonads positions terms args

introVarsInStateComboTest = combos5 introVarsInStateTest initials stateMonads
                                    stateMonads positions maps

assumptionInReaderComboTest =
  combos5 assumptionInReaderTest initials readerMonads
          readerMonads positions symbols

introInReaderComboTest = combos5 introInReaderTest initials readerMonads
                                 readerMonads positions symbols

cutInReaderComboTest = combos5 cutInReaderTest initials readerMonads
                               readerMonads positions terms

applyInReaderComboTest = combos6 applyInReaderTest initials readerMonads
                                 readerMonads positions terms args

introVarsInReaderComboTest = combos5 introVarsInReaderTest initials readerMonads
                                     readerMonads positions maps

assumptionInWriterComboTest =
  combos4 assumptionInWriterTest writerMonads writerMonads positions symbols

introInWriterComboTest =
  combos4 introInWriterTest writerMonads writerMonads positions symbols

cutInWriterComboTest =
  combos4 cutInWriterTest writerMonads writerMonads positions terms

applyInWriterComboTest =
  combos5 applyInWriterTest writerMonads writerMonads positions terms args

introVarsInWriterComboTest =
  combos4 introVarsInWriterTest writerMonads writerMonads positions maps

tests = (map runTestCase proofRecorderMonads) ++
        assumptionInStateComboTest ++
        introInStateComboTest ++
        cutInStateComboTest ++
        applyInStateComboTest ++
        introVarsInStateComboTest ++
        assumptionInReaderComboTest ++
        introInReaderComboTest ++
        cutInReaderComboTest ++
        applyInReaderComboTest ++
        introVarsInReaderComboTest ++
        assumptionInWriterComboTest ++
        introInWriterComboTest ++
        cutInWriterComboTest ++
        applyInWriterComboTest ++
        introVarsInWriterComboTest

--  assumptionInStateComboTest
{-
    "multi" ~: do
      (_, entries) <- runProofRecorder genMulti
      return (multiEntries == entries)
-}
