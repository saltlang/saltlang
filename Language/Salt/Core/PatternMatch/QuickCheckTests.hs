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

module Language.Salt.Core.PatternMatch.QuickCheckTests(tests) where

import Control.Monad
import Data.Default
import Data.Map
import Data.Pos
import Data.Word
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

type Sym = Maybe Word

instance Format (Maybe Word) where
  format Nothing = format "<unused>"
  format (Just n) = format n

instance Default (Maybe m) where
  defaultVal = Nothing

arbitraryPos :: Pos
arbitraryPos = internal "arbitrary"

genAnyTerm :: Gen (Term Sym Sym)
genAnyTerm =
  let
    genAnySymbol :: Gen (Term Sym Sym)
    genAnySymbol =
      do
        sym <- arbitrary
        return Var { varSym = Just sym, varPos = arbitraryPos }

    genAnyField :: Gen (Sym, Term Sym Sym)
    genAnyField =
      do
        sym <- arbitrary
        term <- genAnyTerm
        return (sym, term)

    genAnyFields :: Gen (Map Sym (Term Sym Sym))
    genAnyFields =
      do
        fields <- listOf1 genAnyField
        return (fromList fields)

    genAnyRecord :: Gen (Term Sym Sym)
    genAnyRecord =
      do
        fields <- genAnyFields
        return Record { recVals = fields, recPos = arbitraryPos }

    genAnyConstruction :: Gen (Term Sym Sym)
    genAnyConstruction = 
      do
        sym <- genAnySymbol
        fields <- genAnyFields
        return Call { callFunc = sym, callArgs = fields,
                      callPos = arbitraryPos }
  in
    oneof [ genAnySymbol, genAnyRecord, genAnyConstruction ]

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
        addExtras' :: Int -> Map Sym (Term Sym Sym) ->
                    Gen (Map Sym (Term Sym Sym))
        addExtras' 0 m = return m
        addExtras' n fieldmap =
          do
            sym <- suchThat arbitrary (\x -> notMember (Just x) fieldmap)
            term <- genAnyTerm
            addExtras' (n - 1) (insert (Just sym) term fieldmap)
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
genMatchingExp Deconstruct { deconstructConstructor = Nothing,
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
    term <- genAnyTerm
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
    Just matches' -> (fromList matches) == (fromList matches')
    _ -> False
--genMatch :: Gen MatchPair

testlist = [
    testTags "matched pairs match correctly"
             ["evaluation","pattern match","Core.Syntax"] match
  ]


tests :: [Test]
tests = [testGroup "Core.PatternMatch" testlist]
