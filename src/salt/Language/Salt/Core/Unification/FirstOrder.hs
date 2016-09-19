-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Language.Salt.Core.Unification.FirstOrder(
       unifyIntro
       ) where

import Data.HashMap.Strict(HashMap)
import Language.Salt.Core.Syntax

import qualified Data.HashMap.Strict as HashMap

-- | Type of a unifier.
data Unifier sym term =
  Unifier {
    -- | Map from variables to terms for the left term.
    unifierLeft :: !(HashMap sym term),
    -- | Map from variables to terms for the right term.
    unifierRight :: !(HashMap sym term)
  }

emptyUnifier :: Unifier sym term
emptyUnifier = Unifier { unifierLeft = HashMap.empty,
                         unifierRight = HashMap.empty }

doUnifyIntro :: Unifier bound (Intro bound free)
             -- ^ The current unifier
             -> Intro bound free
             -- ^ The left term.
             -> Intro bound free
             -- ^ The right term.
             -> Maybe (Unifier free (Intro bound free))
-- The fields must match up exactly, and every field must unify cleanly.
doUnifyIntro accum Record { recFields = fields1 }
                   Record { recFields = fileds2 } = _
doUnifyIntro accum Elim { elimTerm = Var { varSym = sym1 } }
                   Elim { elimTerm = Var { varSym = sym2 } }
    -- If the symbols match, then don't do anything; this is trivial
    -- unification.
  | sym1 == sym2 = Just accum
    -- Otherwise, build an equivalence class
  | otherwise = _
-- Every other term unifies only if it is an exact match.
doUnifyIntro accum term1 term2
  | term1 == term2 = Just accum
  | otherwise = Nothing

unifyIntro :: Intro bound free
           -- ^ The left term.
           -> Intro bound free
           -- ^ The right term.
           -> Maybe (Unifier free (Intro bound free))
unifyIntro = doUnifyIntro emptyUnifier
