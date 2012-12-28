-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
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
module Language.Argent.Core.PatternMatch(
       patternMatch
       ) where

import Language.Argent.Core

-- | Given sorted lists of bindings and terms, attempt to match them up
-- and produce a unifier
zipBinds :: (Eq b, Eq s) =>
            Bool -> [(b, Binding b (Term b) s)] -> [(b, Term b s)] ->
            [(b, Term b s)] -> Maybe [(b, Term b s)]
zipBinds strict ((name, bind) : binds)
         allterms @ ((name', term) : terms) result
-- If the names match, then run pattern match
  | name == name' =
    do
      result' <- patternMatchTail result bind term
      zipBinds strict binds terms result'
-- If the names don't match, and we're strict, discard the binding and
-- continue
  | not strict = zipBinds strict binds allterms result
-- Otherwise, we have an error
  | otherwise = Nothing
-- Termination condition: run out of both lists, regardless of strictness
zipBinds _ [] [] result = return result
-- Termination condition: run out of binders, and we're not strict
zipBinds False [] _ result = return result
-- Everything else is a match error
zipBinds _ _ _ _ = Nothing

-- | Tail-recursive work function for pattern matching
patternMatchTail :: (Eq b, Eq s) =>
                 [(b, (Term b s))] -> Binding b (Term b) s -> Term b s ->
                 Maybe [(b, (Term b s))]
-- Projection pairs up with a record
patternMatchTail result (Project { projectBinds = binds,
                                   projectStrict = strict })
                 (Record { recVals = vals }) =
  zipBinds strict binds vals result
-- Constructions pair up with function
patternMatchTail result (Construct { constructBinds = binds,
                                  constructStrict = strict })
              (Call { callArgs = args }) =
  zipBinds strict binds args result
-- As bindings bind the current term and then continue
patternMatchTail result (As { asName = name, asBind = bind }) t =
  patternMatchTail ((name, t) : result) bind t
-- Names grab anything
patternMatchTail result (Name { nameSym = sym }) t =
  return ((sym, t) : result)
-- Constants must be equal
patternMatchTail result (Constant t1) t2
  | t1 == t2 = return result
  | otherwise = Nothing
-- Anything else is a pattern mismatch
patternMatchTail _ _ _ = Nothing

-- | Take a binding and a term, and attempt to match the pattern
-- represented by the binding.  If the match succeeds, return a
-- unifier in the form of a map from bound variables to terms.  If the
-- match fails, return nothing.
patternMatch :: (Eq b, Eq s) =>
                Binding b (Term b) s -> Term b s -> Maybe [(b, (Term b s))]
patternMatch =
    patternMatchTail []

-- | Given a list of bindings and an accompanying list of terms, find
-- the first successful pattern match.
--firstMatch :: [Binding b (Term b) s] -> [Term b s] -> Maybe (Map b (Term b s))
