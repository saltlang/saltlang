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
--       patternTypes
       ) where

import Data.Default
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Language.Salt.Core.Syntax

import qualified Data.HashMap.Strict as HashMap

-- | Tail-recursive work function for pattern matching
patternMatchTail :: (Default sym, Eq sym, Hashable sym) =>
                    HashMap sym (Intro sym sym) ->
                    Pattern sym (Intro sym) sym ->
                    Intro sym sym ->
                    Maybe (HashMap sym (Intro sym sym))
patternMatchTail result Deconstruct { deconstructConstructor = constructor,
                                      deconstructBinds = binds } term
  -- The default value is the unused symbol, which means there is no
  -- constructor (meaning this is a pure record pattern).
  | constructor == def =
    case term of
      Record { recFields = fields } ->
        let
          foldfun accum sym bind =
            do
              justaccum <- accum
              field <- HashMap.lookup sym fields
              patternMatchTail justaccum bind field
        in
          HashMap.foldlWithKey' foldfun (Just result) binds
      _ -> Nothing
  | otherwise =
    case term of
      Elim { elimTerm = Call { callFunc = Var { varSym = func },
                               callArgs = args } } ->
        if func == constructor
        then let
            foldfun accum sym bind =
              do
                justaccum <- accum
                arg <- HashMap.lookup sym args
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
patternMatchTail result (Constant t1) t2
  | t1 == t2 = return result
  | otherwise = Nothing

-- | Take a pattern and a term and attempt to match the pattern
-- represented by the binding.  If the match succeeds, return a
-- unifier in the form of a map from bound variables to terms.  If the
-- match fails, return nothing.
patternMatch :: (Default sym, Eq sym, Hashable sym) =>
                Pattern sym (Intro sym) sym
             -- ^ The pattern being matched.
             -> Intro sym sym
             -- ^ The term attempting to match the pattern.
             -> Maybe (HashMap sym (Intro sym sym))
             -- ^ A list of bindings from the pattern, or Nothing.
patternMatch = patternMatchTail HashMap.empty
{-
patternTypesTail :: (Default sym, Eq sym, Ord sym) =>
                    HashMap sym (Intro sym sym) ->
                    Pattern sym (Intro sym) sym ->
                    Intro sym sym -> Maybe (HashMap sym (Intro sym sym))
patternTypesTail result Deconstruct { deconstructConstructor = constructor,
                                      deconstructBinds = binds } term
  | constructor == def =
    case term of
      Record { recFields = fields } ->
        let
          foldfun accum sym bind =
            do
              justaccum <- accum
              field <- HashMap.lookup sym fields
              patternTypesTail accum bind field
        in
          HashMap.foldlWithKey' foldfun result binds
      _ -> Nothing
  | otherwise =
    case term of
      Call { callFunc = Var { varSym = func }, callArgs = args } ->
        if func == constructor
        then let
            foldfun accum sym bind =
              do
                justaccum <- accum
                arg <- HashMap.lookup sym args
                patternTypesTail accum bind arg
          in
            HashMap.foldlWithKey' foldfun result binds
        else Nothing
      _ -> Nothing
-- As bindings bind the current term and then continue
patternTypesTail result As { asName = name, asBind = bind } t =
  patternTypesTail (HashMap.insert name t result) bind t
-- Name patterns bind the type to the pattern
patternTypesTail result Name { nameSym = sym } t =
  Just $! HashMap.insert sym t result
-- Constants don't bind any symbols
patternTypesTail result (Constant _) t2 = Just result

-- | Take a pattern and a type and extract the typings for all the
-- variables bound by this pattern.  Note: this is only guaranteed to
-- work for well-typed patterns; if the pattern does not have the
-- given type, it may fail.
patternTypes :: (Default sym, Eq sym, Ord sym) =>
                Pattern sym (Intro sym) sym
             -- ^ The pattern being matched.
             -> Intro sym sym
             -- ^ The type given to the pattern.
             -> Maybe (HashMap sym (Intro sym sym))
             -- ^ A list of bindings from the pattern, or Nothing.
patternTypes = patternTypesTail HashMap.empty
-}
