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
import Data.Word
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import Language.Salt.Core.Syntax
import Text.Format

instance Default Word where defaultVal = 0

instance Format f => Format (Word -> f) where
  format f = format "Function" --bracketList "Function" (map (\n -> n <+> equals <+> f n) [0..8])

instance Format f => Show (Word -> f) where
  show = show . format

patternEqOrNeq :: (Pattern Word (Term Word) Word,
                   Pattern Word (Term Word) Word) -> Bool
patternEqOrNeq (a, b) = a == b || a /= b

patternEqOrd :: (Pattern Word (Term Word) Word, Pattern Word (Term Word) Word) -> Bool
patternEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

patternOrdRev :: (Pattern Word (Term Word) Word,
                  Pattern Word (Term Word) Word) -> Bool
patternOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

patternFunctorID :: Pattern Word (Term Word) Word -> Bool
patternFunctorID pattern = pattern == fmap id pattern

patternFunctorCompose :: (Word -> Word, Word -> Word,
                          Pattern Word (Term Word) Word) -> Bool
patternFunctorCompose (f, g, pattern) =
  (fmap (f . g) pattern) == (((fmap f) . (fmap g)) pattern)

caseEqOrNeq :: (Case Word Word, Case Word Word) -> Bool
caseEqOrNeq (a, b) = a == b || a /= b

caseEqOrd :: (Case Word Word, Case Word Word) -> Bool
caseEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

caseOrdRev :: (Case Word Word, Case Word Word) -> Bool
caseOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

caseFunctorID :: Case Word Word -> Bool
caseFunctorID c = c == fmap id c

caseFunctorCompose :: (Word -> Word, Word -> Word, Case Word Word) -> Bool
caseFunctorCompose (f, g, c) =
  (fmap (f . g) c) == (((fmap f) . (fmap g)) c)

elementEqOrNeq :: (Element Word Word, Element Word Word) -> Bool
elementEqOrNeq (a, b) = a == b || a /= b

elementEqOrd :: (Element Word Word, Element Word Word) -> Bool
elementEqOrd (a, b) = (a == b && compare a b == EQ) ||
                      (a /= b && compare a b /= EQ)

elementOrdRev :: (Element Word Word, Element Word Word) -> Bool
elementOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

elementFunctorID :: Element Word Word -> Bool
elementFunctorID c = c == fmap id c

elementFunctorCompose :: (Word -> Word, Word -> Word, Element Word Word) -> Bool
elementFunctorCompose (f, g, c) =
  (fmap (f . g) c) == (((fmap f) . (fmap g)) c)

termEqOrNeq :: (Term Word Word, Term Word Word) -> Bool
termEqOrNeq (a, b) = a == b || a /= b

termEqOrd :: (Term Word Word, Term Word Word) -> Bool
termEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

termOrdRev :: (Term Word Word, Term Word Word) -> Bool
termOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

termFunctorID :: Term Word Word -> Bool
termFunctorID term = term == fmap id term

termFunctorCompose :: (Word -> Word, Word -> Word, Term Word Word) -> Bool
termFunctorCompose (f, g, term) =
  (fmap (f . g) term) == (((fmap f) . (fmap g)) term)

termMonadLeftID :: (Word -> Term Word Word, Word) -> Bool
termMonadLeftID (f, x) = (return x >>= f) == (f x)

termMonadRightID :: Term Word Word -> Bool
termMonadRightID x = (x >>= return) == x

termMonadAssoc :: (Word -> Term Word Word, Word -> Term Word Word, Term Word Word) ->
                   Bool
termMonadAssoc (f, g, x) = ((x >>= f) >>= g) == (x >>= (\m -> f m >>= g))

cmdEqOrNeq :: (Cmd Word Word, Cmd Word Word) -> Bool
cmdEqOrNeq (a, b) = a == b || a /= b

cmdEqOrd :: (Cmd Word Word, Cmd Word Word) -> Bool
cmdEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

cmdOrdRev :: (Cmd Word Word, Cmd Word Word) -> Bool
cmdOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

cmdFunctorID :: Cmd Word Word -> Bool
cmdFunctorID cmd = cmd == fmap id cmd

cmdFunctorCompose :: (Word -> Word, Word -> Word, Cmd Word Word) -> Bool
cmdFunctorCompose (f, g, cmd) =
  (fmap (f . g) cmd) == (((fmap f) . (fmap g)) cmd)

compEqOrNeq :: (Comp Word Word, Comp Word Word) -> Bool
compEqOrNeq (a, b) = a == b || a /= b

compEqOrd :: (Comp Word Word, Comp Word Word) -> Bool
compEqOrd (a, b) = (a == b && compare a b == EQ) ||
                   (a /= b && compare a b /= EQ)

compOrdRev :: (Comp Word Word, Comp Word Word) -> Bool
compOrdRev (a, b) =
  case compare a b of
    LT -> compare b a == GT
    EQ -> compare b a == EQ
    GT -> compare b a == LT

compFunctorID :: Comp Word Word -> Bool
compFunctorID comp = comp == fmap id comp

compFunctorCompose :: (Word -> Word, Word -> Word, Comp Word Word) -> Bool
compFunctorCompose (f, g, comp) =
  (fmap (f . g) comp) == (((fmap f) . (fmap g)) comp)

compMonadLeftID :: (Word -> Comp Word Word, Word) -> Bool
compMonadLeftID (f, x) = (return x >>= f) == (f x)

compMonadRightID :: Comp Word Word -> Bool
compMonadRightID x = (x >>= return) == x

compMonadAssoc :: (Word -> Comp Word Word,
                   Word -> Comp Word Word,
                   Comp Word Word) -> Bool
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
    testTags "Term t1 == t2 or t1 /= t2"
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
    testTags "Cmd t1 == t2 or t1 /= t2"
             ["Eq","instances","Core.Syntax"] cmdEqOrNeq,
    testTags "Cmd t1 == t2 <-> compare t1 t2 == EQ"
             ["Eq","Ord","instances","Core.Syntax"] cmdEqOrd,
    testTags "Cmd t1 < t2 iff t2 > t1"
             ["Ord","instances","Core.Syntax"] cmdOrdRev,
    testTags "Cmd t == fmap id t"
             ["Functor","instances","Core.Syntax"] cmdFunctorID,
    testTags "Cmd fmap (f . g) t == ((fmap f) . (fmap g)) t"
             ["Functor","instances","Core.Syntax"] cmdFunctorCompose,
    testTags "Comp t1 == t2 or t1 /= t2"
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
