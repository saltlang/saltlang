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

-- | A module with utility code for constructing combinatorial tests.
module Test.Utils.ComboTests(
       comboTest2,
       comboTest3,
       comboTest4,
       comboTest5
       ) where

import Test.HUnit

-- | Build a combo-test with two dimensions.
comboTest2 :: (a -> b -> Test) -> [a] -> [b] -> [Test]
comboTest2 test args1 args2 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests -> test arg1 arg2 : tests)
                 tests args2)
        [] args1

-- | Build a combo-test with three dimensions.
comboTest3 :: (a -> b -> c -> Test) -> [a] -> [b] -> [c] -> [Test]
comboTest3 test args1 args2 args3 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests ->
                    foldr (\arg3 tests -> test arg1 arg2 arg3 : tests)
                          tests args3)
                 tests args2)
        [] args1

-- | Build a combo-test with four dimensions.
comboTest4 :: (a -> b -> c -> d -> Test) -> [a] -> [b] -> [c] -> [d] -> [Test]
comboTest4 test args1 args2 args3 args4 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests ->
                    foldr (\arg3 tests ->
                             foldr (\arg4 tests ->
                                      test arg1 arg2 arg3 arg4 : tests)
                                   tests args4)
                          tests args3)
                 tests args2)
        [] args1

-- | Build a combo-test with five dimensions.
comboTest5 :: (a -> b -> c -> d -> e -> Test) ->
              [a] -> [b] -> [c] -> [d] -> [e] -> [Test]
comboTest5 test args1 args2 args3 args4 args5 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests ->
                    foldr (\arg3 tests ->
                             foldr (\arg4 tests ->
                                      foldr (\arg5 tests ->
                                               test arg1 arg2 arg3 arg4 arg5 :
                                               tests)
                                            tests args5)
                                   tests args4)
                          tests args3)
                 tests args2)
        [] args1
