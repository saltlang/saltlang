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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | Pattern matching implementation.  This module contains a single
-- function which implements the pattern matching functionality.
-- 
-- The single function in this module attempts to create a unifier
-- from a pattern and a term.
module Language.Salt.Core.PatternMatch(
       patternMatch
       ) where

import Data.Default
import Language.Salt.Core.Syntax

import qualified Data.Map as Map

-- | Given sorted lists of bindings and terms, attempt to match them up
-- and produce a unifier
zipBinds :: (Default sym, Eq sym) =>
            Bool -> [(sym, Pattern sym (Term sym) sym)] ->
            [(sym, Term sym sym)] -> [(sym, Term sym sym)] ->
            Maybe [(sym, Term sym sym)]
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
patternMatchTail :: (Default sym, Eq sym) =>
                    [(sym, (Term sym sym))] ->
                    Pattern sym (Term sym) sym -> Term sym sym ->
                    Maybe [(sym, (Term sym sym))]
patternMatchTail result Deconstruct { deconstructConstructor = constructor,
                                      deconstructStrict = strict,
                                      deconstructBinds = binds } term
  | constructor == defaultVal =
    case term of
      Record { recVals = vals } ->
        zipBinds strict (Map.toAscList binds) (Map.toAscList vals) result
      _ -> Nothing
  | otherwise =
    case term of
      Call { callFunc = Var { varSym = func }, callArgs = args } ->
        if func == constructor
        then zipBinds strict (Map.toAscList binds) (Map.toAscList args) result
        else Nothing
      _ -> Nothing
-- As bindings bind the current term and then continue
patternMatchTail result As { asName = name, asBind = bind } t =
  patternMatchTail ((name, t) : result) bind t
-- Names grab anything
patternMatchTail result Name { nameSym = sym } t =
  return ((sym, t) : result)
-- Constants must be equal
patternMatchTail result (Constant t1) t2
  | t1 == t2 = return result
  | otherwise = Nothing

-- | Take a binding and a term, and attempt to match the pattern
-- represented by the binding.  If the match succeeds, return a
-- unifier in the form of a map from bound variables to terms.  If the
-- match fails, return nothing.
patternMatch :: (Default sym, Eq sym) =>
                Pattern sym (Term sym) sym -> Term sym sym ->
                Maybe [(sym, (Term sym sym))]
patternMatch = patternMatchTail []

-- | Given a list of bindings and an accompanying list of terms, find
-- the first successful pattern match.
--firstMatch :: [Binding b (Term b) s] -> [Term b s] -> Maybe (Map b (Term b s))

