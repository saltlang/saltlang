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
       combos2,
       combos3,
       combos4,
       combos5,
       combos6,
       allLists
       ) where

import Test.HUnit

-- | Build a combo-test with two dimensions.
combos2 :: (a -> b -> z) -> [a] -> [b] -> [z]
combos2 test args1 args2 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests -> test arg1 arg2 : tests)
                 tests args2)
        [] args1

-- | Build a combo-test with three dimensions.
combos3 :: (a -> b -> c -> z) -> [a] -> [b] -> [c] -> [z]
combos3 test args1 args2 args3 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests ->
                    foldr (\arg3 tests -> test arg1 arg2 arg3 : tests)
                          tests args3)
                 tests args2)
        [] args1

-- | Build a combo-test with four dimensions.
combos4 :: (a -> b -> c -> d -> z) -> [a] -> [b] -> [c] -> [d] -> [z]
combos4 test args1 args2 args3 args4 =
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
combos5 :: (a -> b -> c -> d -> e -> z) ->
           [a] -> [b] -> [c] -> [d] -> [e] -> [z]
combos5 test args1 args2 args3 args4 args5 =
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

-- | Build a combo-test with five dimensions.
combos6 :: (a -> b -> c -> d -> e -> f -> z) ->
           [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [z]
combos6 test args1 args2 args3 args4 args5 args6 =
  foldr (\arg1 tests ->
           foldr (\arg2 tests ->
                    foldr (\arg3 tests ->
                             foldr (\arg4 tests ->
                                      foldr (\arg5 tests ->
                                              foldr (\arg6 tests ->
                                                      test arg1 arg2 arg3
                                                           arg4 arg5 arg6:
                                                      tests)
                                                    tests args6)
                                            tests args5)
                                   tests args4)
                          tests args3)
                 tests args2)
        [] args1

-- | Given a list of possibilities, build all possible lists up to
-- length n.  The empty list is not included.
allLists :: Int -> [a] -> [[a]]
allLists n items
  | n <= 1 =
    map (\a -> [a]) items
  | otherwise =
    let
      lists = allLists (n - 1) items
    in
      combos2 (:) items lists
