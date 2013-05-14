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

module Language.Salt.Core.Syntax.QuickCheckTests(tests) where

import Data.Default
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import Language.Salt.Core.Syntax

instance Default Int where defaultVal = 0

instance Show (Int -> Int) where
  show f = "Function"

instance Show (Int -> Term Int Int) where
  show f = "Function"

instance Show (Int -> Comp Int Int) where
  show f = "Function"

patternEqOrNeq :: (Pattern Int (Term Int) Int,
                   Pattern Int (Term Int) Int) -> Bool
patternEqOrNeq (a, b) = a == b || a /= b

patternEqOrd :: (Pattern Int (Term Int) Int, Pattern Int (Term Int) Int) -> Bool
patternEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

patternOrdRev :: (Pattern Int (Term Int) Int,
                  Pattern Int (Term Int) Int) -> Bool
patternOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

patternFunctorID :: Pattern Int (Term Int) Int -> Bool
patternFunctorID pattern = pattern == fmap id pattern

patternFunctorCompose :: (Int -> Int, Int -> Int,
                          Pattern Int (Term Int) Int) -> Bool
patternFunctorCompose (f, g, pattern) =
  (fmap (f . g) pattern) == (((fmap f) . (fmap g)) pattern)

caseEqOrNeq :: (Case Int Int, Case Int Int) -> Bool
caseEqOrNeq (a, b) = a == b || a /= b

caseEqOrd :: (Case Int Int, Case Int Int) -> Bool
caseEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

caseOrdRev :: (Case Int Int, Case Int Int) -> Bool
caseOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

caseFunctorID :: Case Int Int -> Bool
caseFunctorID c = c == fmap id c

caseFunctorCompose :: (Int -> Int, Int -> Int, Case Int Int) -> Bool
caseFunctorCompose (f, g, c) =
  (fmap (f . g) c) == (((fmap f) . (fmap g)) c)

elementEqOrNeq :: (Element Int Int, Element Int Int) -> Bool
elementEqOrNeq (a, b) = a == b || a /= b

elementEqOrd :: (Element Int Int, Element Int Int) -> Bool
elementEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

elementOrdRev :: (Element Int Int, Element Int Int) -> Bool
elementOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

elementFunctorID :: Element Int Int -> Bool
elementFunctorID c = c == fmap id c

elementFunctorCompose :: (Int -> Int, Int -> Int, Element Int Int) -> Bool
elementFunctorCompose (f, g, c) =
  (fmap (f . g) c) == (((fmap f) . (fmap g)) c)

termEqOrNeq :: (Term Int Int, Term Int Int) -> Bool
termEqOrNeq (a, b) = a == b || a /= b

termEqOrd :: (Term Int Int, Term Int Int) -> Bool
termEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

termOrdRev :: (Term Int Int, Term Int Int) -> Bool
termOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

termFunctorID :: Term Int Int -> Bool
termFunctorID term = term == fmap id term

termFunctorCompose :: (Int -> Int, Int -> Int, Term Int Int) -> Bool
termFunctorCompose (f, g, term) =
  (fmap (f . g) term) == (((fmap f) . (fmap g)) term)

termMonadLeftID :: (Int -> Term Int Int, Int) -> Bool
termMonadLeftID (f, x) = (return x >>= f) == (f x)

termMonadRightID :: Term Int Int -> Bool
termMonadRightID x = (x >>= return) == x

termMonadAssoc :: (Int -> Term Int Int, Int -> Term Int Int, Term Int Int) ->
                   Bool
termMonadAssoc (f, g, x) = ((x >>= f) >>= g) == (x >>= (\m -> f m >>= g))

cmdEqOrNeq :: (Cmd Int Int, Cmd Int Int) -> Bool
cmdEqOrNeq (a, b) = a == b || a /= b

cmdEqOrd :: (Cmd Int Int, Cmd Int Int) -> Bool
cmdEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

cmdOrdRev :: (Cmd Int Int, Cmd Int Int) -> Bool
cmdOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

cmdFunctorID :: Cmd Int Int -> Bool
cmdFunctorID cmd = cmd == fmap id cmd

cmdFunctorCompose :: (Int -> Int, Int -> Int, Cmd Int Int) -> Bool
cmdFunctorCompose (f, g, cmd) =
  (fmap (f . g) cmd) == (((fmap f) . (fmap g)) cmd)

compEqOrNeq :: (Comp Int Int, Comp Int Int) -> Bool
compEqOrNeq (a, b) = a == b || a /= b

compEqOrd :: (Comp Int Int, Comp Int Int) -> Bool
compEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

compOrdRev :: (Comp Int Int, Comp Int Int) -> Bool
compOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

compFunctorID :: Comp Int Int -> Bool
compFunctorID comp = comp == fmap id comp

compFunctorCompose :: (Int -> Int, Int -> Int, Comp Int Int) -> Bool
compFunctorCompose (f, g, comp) =
  (fmap (f . g) comp) == (((fmap f) . (fmap g)) comp)

compMonadLeftID :: (Int -> Comp Int Int, Int) -> Bool
compMonadLeftID (f, x) = (return x >>= f) == (f x)

compMonadRightID :: Comp Int Int -> Bool
compMonadRightID x = (x >>= return) == x

compMonadAssoc :: (Int -> Comp Int Int, Int -> Comp Int Int, Comp Int Int) ->
                   Bool
compMonadAssoc (f, g, x) = ((x >>= f) >>= g) == (x >>= (\m -> f m >>= g))

testlist = [
    testTags "Pattern t1 == t2 or t1 /= t2"
             ["Eq","instances","Core.Syntax"] patternEqOrNeq,
    testTags "Pattern t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] patternEqOrd,
    testTags "Pattern t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] patternOrdRev,
    testTags "Pattern t1 == t2 or t1 /= t2"
             ["Functor","instances","Core.Syntax"] patternFunctorID,
    testTags "Pattern fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] patternFunctorCompose,
{-
    testTags "Case t1 == t2 or t1 /= t2"
             ["Eq","instances","Core.Syntax"] caseEqOrNeq,
    testTags "Case t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] caseEqOrd,
    testTags "Case t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] caseOrdRev,
    testTags "Case t1 == t2 or t1 /= t2"
             ["Functor","instances","Core.Syntax"] caseFunctorID,
    testTags "Case fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] caseFunctorCompose,
    testTags "Element t1 == t2 or t1 /= t2"
             ["Eq","instances","Core.Syntax"] elementEqOrNeq,
    testTags "Element t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] elementEqOrd,
    testTags "Element t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] elementOrdRev,
    testTags "Element t1 == t2 or t1 /= t2"
             ["Functor","instances","Core.Syntax"] elementFunctorID,
    testTags "Element fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] elementFunctorCompose,
-}
    testTags "Term (return x >>= f) == (f x)"
             ["Eq","instances","Core.Syntax"] termEqOrNeq,
    testTags "Term t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] termEqOrd,
    testTags "Term t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] termOrdRev,
    testTags "Term t == fmap id t"
             ["Functor","instances","Core.Syntax"] termFunctorID,
    testTags "Term fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] termFunctorCompose,
    testTags "Term (return x >>= f) == (f x)"
             ["Monad","instances","Core.Syntax"] termMonadLeftID,
    testTags "Term (x >>= return) == x"
             ["Monad","instances","Core.Syntax"] termMonadRightID,
    testTags "Term (x >>= f) >>= g == m >>= (\\x -> f x >>= g)"
             ["Monad","instances","Core.Syntax"] termMonadAssoc,
    testTags "Cmd (return x >>= f) == (f x)"
             ["Eq","instances","Core.Syntax"] cmdEqOrNeq,
    testTags "Cmd t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] cmdEqOrd,
    testTags "Cmd t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] cmdOrdRev,
    testTags "Cmd t == fmap id t"
             ["Functor","instances","Core.Syntax"] cmdFunctorID,
    testTags "Cmd fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] cmdFunctorCompose,
    testTags "Comp (return x >>= f) == (f x)"
             ["Eq","instances","Core.Syntax"] compEqOrNeq,
    testTags "Comp t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] compEqOrd,
    testTags "Comp t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] compOrdRev,
    testTags "Comp t == fmap id t"
             ["Functor","instances","Core.Syntax"] compFunctorID,
    testTags "Comp fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] compFunctorCompose,
    testTags "Comp (return x >>= f) == (f x)"
             ["Monad","instances","Core.Syntax"] compMonadLeftID,
    testTags "Comp (x >>= return) == x"
             ["Monad","instances","Core.Syntax"] compMonadRightID,
    testTags "Comp (x >>= f) >>= g == m >>= (\\x -> f x >>= g)"
             ["Monad","instances","Core.Syntax"] compMonadAssoc
  ]

tests :: [Test]
tests = [testGroup "Syntax" testlist]