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

-- | Pattern matching implementation.  This module contains a single
-- function which implements the pattern matching functionality.
--
-- The single function in this module attempts to create a unifier
-- from a pattern and a term.
module Language.Salt.Core.Patterns(
       patternMatch,
       caseMatch
       ) where

import Bound
import Control.Applicative
import Data.Default
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Language.Salt.Core.Syntax

import qualified Data.HashMap.Strict as HashMap

-- | Tail-recursive work function for pattern matching
patternMatchTail :: (Default bound, Eq bound, Hashable bound) =>
                    HashMap bound (Intro bound free) ->
                    Pattern bound ->
                    Intro bound free ->
                    Maybe (HashMap bound (Intro bound free))
-- For an option, try each one in order, take the first.
patternMatchTail result Option { optionPats = pats } term =
  foldr ((<|>) . flip (patternMatchTail result) term) Nothing pats
patternMatchTail result Deconstruct { deconstructConstructor = constructor,
                                      deconstructBinds = binds } term
  -- The default value is the unused symbol, which means there is no
  -- constructor (meaning this is a pure record pattern).
  | constructor == def =
    case term of
      Record { recFields = fields } ->
        let
          foldfun accum sym Field { fieldVal = bind } =
            do
              justaccum <- accum
              Field { fieldVal = val } <- HashMap.lookup sym fields
              patternMatchTail justaccum bind val
        in
          HashMap.foldlWithKey' foldfun (Just result) binds
      _ -> Nothing
    -- Otherwise, we match calls to a constructor.
  | otherwise =
    case term of
      Elim { elimTerm =
                Call { callFunc =
                          Typed { typedTerm =
                                     Constructor {
                                       constructorSym = constructor'
                                     }
                                },
                       callArg = Record { recFields = args } }
           } ->
        if constructor == constructor'
        then let
            foldfun accum sym Field { fieldVal = bind } =
              do
                justaccum <- accum
                Field { fieldVal = arg } <- HashMap.lookup sym args
                patternMatchTail justaccum bind arg
          in
            HashMap.foldlWithKey' foldfun (Just result) binds
        else Nothing
      _ -> Nothing
-- As bindings bind the current term and then continue
patternMatchTail result As { asName = name, asBind = bind } t =
  patternMatchTail (HashMap.insert name t result) bind t
-- Names grab anything
patternMatchTail result Name { nameSym = sym } t =
  Just $! HashMap.insert sym t result
-- Constants must be equal
patternMatchTail result Exact { exactLiteral = exact }
                        Literal { literalVal = val }
  | exact == val = return result
  | otherwise = Nothing
patternMatchTail _ Exact {} _ = Nothing

-- | Take a pattern and a term and attempt to match the pattern
-- represented by the binding.  If the match succeeds, return a
-- unifier in the form of a map from bound variables to terms.  If the
-- match fails, return nothing.
patternMatch :: (Default bound, Eq bound, Hashable bound) =>
                Pattern bound
             -- ^ The pattern being matched.
             -> Intro bound free
             -- ^ The term attempting to match the pattern.
             -> Maybe (HashMap bound (Intro bound free))
             -- ^ A list of bindings from the pattern, or Nothing.
patternMatch = patternMatchTail HashMap.empty

-- | Attempt to match a case.  If the match is successful, the body
-- will be instantiated with all the matched values.
caseMatch :: (Default bound, Hashable bound, Eq bound) =>
             Intro bound free
          -- ^ The term to match and bind.
          -> Case bound free
          -- ^ The case to match and execute.
          -> Maybe (Intro bound free)
          -- ^ A (non-normalized) term obtained from the resulting
          -- substitution, or @Nothing@.
caseMatch term Case { casePat = pat, caseBody = body } =
  case patternMatch pat term of
    Nothing -> Nothing
    Just binds -> Just $! instantiate (binds HashMap.!) body
