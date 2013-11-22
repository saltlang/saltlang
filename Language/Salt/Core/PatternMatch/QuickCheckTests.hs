-- Copyright (c) 2013 Eric McCorkle.
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
{-# LANGUAGE FlexibleInstances #-}

module Language.Salt.Core.PatternMatch.QuickCheckTests where

import Control.Monad
import Data.Default
import Data.List(sort)
import Data.Map
import Data.Pos
import Data.Word
import Debug.Trace
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import Language.Salt.Core.Syntax
import Language.Salt.Core.PatternMatch
import Test.QuickCheck
import Text.Format

data MatchPair =
  MatchPair { matchPattern :: Pattern Sym (Term Sym) Sym,
              matchTerm :: Term Sym Sym,
              matchMatches :: [(Sym, Term Sym Sym)] }
  deriving Show

data MismatchPair =
  MismatchPair { mismatchPattern :: Pattern Sym (Term Sym) Sym,
                 mismatchTerm :: Term Sym Sym }
  deriving Show

data Sym = Sym (Maybe Word)
  deriving (Ord, Eq)

instance Arbitrary Sym where
  arbitrary = arbitrary >>= return . Sym . Just

instance Default Sym where
  defaultVal = Sym Nothing

instance Format Sym where
  format (Sym Nothing) = format "<unused>"
  format (Sym (Just n)) = format n

instance Show Sym where
  show = show . format

arbitraryPos :: Pos
arbitraryPos = internal "arbitrary"

genAnyTerm :: Int -> Gen (Term Sym Sym)
genAnyTerm size =
  let
    genAnySymbol :: Gen (Term Sym Sym)
    genAnySymbol =
      do
        sym <- arbitrary
        return Var { varSym = Sym (Just sym), varPos = arbitraryPos }

    genAnyField :: Int -> Gen (Sym, Term Sym Sym)
    genAnyField size =
      do
        sym <- arbitrary
        term <- genAnyTerm size
        return (sym, term)

    genFieldSizes :: Int -> Gen [Int]
    genFieldSizes size
      | size > 0 =
        do
          out <- choose (0, size)
          outs <- genFieldSizes (size - out - 1)
          return (out : outs)
      | otherwise = return []

    genAnyFields :: Int -> Gen (Map Sym (Term Sym Sym))
    genAnyFields size =
      do
        fieldsizes <- genFieldSizes size
        fields <- mapM genAnyField fieldsizes
        return (fromList fields)

    genAnyRecord :: Int -> Gen (Term Sym Sym)
    genAnyRecord size =
      do
        fields <- genAnyFields (size - 1)
        return Record { recVals = fields, recPos = arbitraryPos }

    genAnyConstruction :: Int -> Gen (Term Sym Sym)
    genAnyConstruction size =
      do
        sym <- genAnySymbol
        fields <- genAnyFields (size - 1)
        return Call { callFunc = sym, callArgs = fields,
                      callPos = arbitraryPos }
  in if size > 0
    then oneof [ genAnySymbol, genAnyRecord size, genAnyConstruction size ]
    else genAnySymbol

genMatchingFields :: Bool -> Map Sym (Pattern Sym (Term Sym) Sym) ->
                     Gen (Map Sym (Term Sym Sym), [(Sym, Term Sym Sym)])
genMatchingFields strict fieldmap =
  let
    foldfun :: ([(Sym, Term Sym Sym)], [(Sym, Term Sym Sym)]) ->
               (Sym, Pattern Sym (Term Sym) Sym) ->
               Gen ([(Sym, Term Sym Sym)], [(Sym, Term Sym Sym)])
    foldfun (fields, matches) (fname, pat) =
      do
        (field, match) <- genMatchingExp pat
        return ((fname, field) : fields, match ++ matches)

    -- Convert back to a map, and possibly add extra fields
    addExtras :: Map Sym (Term Sym Sym) -> Gen (Map Sym (Term Sym Sym))
    addExtras fieldmap =
      let
        addExtras' :: Word -> Map Sym (Term Sym Sym) ->
                    Gen (Map Sym (Term Sym Sym))
        addExtras' 0 m = return m
        addExtras' n fieldmap =
          do
            sym <- suchThat arbitrary (\x -> notMember (Sym (Just x)) fieldmap)
            term <- sized genAnyTerm
            addExtras' (n - 1) (insert (Sym (Just sym)) term fieldmap)
      in
        if strict
          then return fieldmap
          else do
            size <- arbitrary
            addExtras' size fieldmap
  in do
    (fields, matches) <- foldM foldfun ([], []) (toList fieldmap)
    fieldmap' <- addExtras (fromList fields)
    return (fieldmap', matches)

genMatchingExp :: Pattern Sym (Term Sym) Sym ->
                  Gen (Term Sym Sym, [(Sym, Term Sym Sym)])
genMatchingExp Deconstruct { deconstructConstructor = Sym Nothing,
                             deconstructStrict = strict,
                             deconstructBinds = binds } =
  do
    (vals, matches) <- genMatchingFields strict binds
    return (Record { recPos = arbitraryPos, recVals = vals }, matches)
genMatchingExp Deconstruct { deconstructConstructor = constructor,
                             deconstructStrict = strict,
                             deconstructBinds = binds } =
  do
    (vals, matches) <- genMatchingFields strict binds
    return (Call { callFunc = return constructor, callArgs = vals,
                   callPos = arbitraryPos }, matches)
genMatchingExp As { asBind = pat, asName = sym } =
  do
    (term, matches) <- genMatchingExp pat
    return (term, (sym, term) : matches)
genMatchingExp Name { nameSym = sym } =
  do
    term <- sized genAnyTerm
    return (term, [(sym, term)])
genMatchingExp (Constant const) = return (const, [])

instance Arbitrary MatchPair where
  arbitrary =
    do
      pattern <- arbitrary
      (term, matches) <- genMatchingExp pattern
      return MatchPair { matchPattern = pattern,
                         matchTerm = term,
                         matchMatches = matches }

match :: MatchPair -> Bool
match MatchPair { matchPattern = pattern,
                  matchTerm = term,
                  matchMatches = matches } =
  case patternMatch pattern term of
    Just matches' ->
      if (sort matches) == (sort matches')
        then True
        else trace ("expected matches " ++ show matches ++
                    "\nbut got\n" ++ show matches') False
    _ -> trace "match failed unexpectedly!" False
--genMatch :: Gen MatchPair

testlist = [
    testTags "matched pairs match correctly"
             ["evaluation","pattern match","Core.Syntax"] match
  ]


tests :: [Test]
tests = [testGroup "Core.PatternMatch" testlist]
