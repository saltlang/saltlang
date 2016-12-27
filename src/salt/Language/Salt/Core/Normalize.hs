-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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
{-# LANGUAGE FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, OverloadedStrings #-}

module Language.Salt.Core.Normalize(
       Normalize(..)
       ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Monad
import Control.Monad.Messages
import Data.Array(Array, (!))
import Data.Default
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.Position.DWARFPosition
import Data.PositionElement
import Data.Traversable
import Language.Salt.Core.Syntax
import Language.Salt.Core.Syntax.Util
import Language.Salt.Core.Patterns
import Language.Salt.Message
import Prelude hiding (mapM)

import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HashMap

class Monad m => Normalize m ty where
  -- | Fully normalize a term.
  normalize :: ty
            -- ^ The term to normalize.
            -> m ty
            -- ^ The normalized term.

  -- | Only normalize a term enough such that the top-level
  -- constructor (head) is in normal form.
  normalizeHead :: ty
                -- ^ The term to normalize.
                -> m ty
                -- ^ The normalized term.

unfoldIntro :: (MonadMessages Message m, Applicative m, Eq bound) =>
                Intro bound free
            -> m (Intro bound free)
unfoldIntro t @ Fix { fixTerm = term } = unfoldIntro (instantiate1 t term)
unfoldIntro Elim { elimTerm = Typed { typedTerm = term } } = unfoldIntro term
unfoldIntro term = normalize term

instance (MonadMessages Message m, Applicative m, Eq bound) =>
         Normalize m (Case bound free) where
  normalize c @ Case { caseBody = body } =
    do
      body' <- transverseScope normalize body
      return c { caseBody = body' }

instance (MonadMessages Message m, Applicative m, Eq bound) =>
         Normalize m (Element bound free) where
  normalize e @ Element { elemType = ty } =
    do
      ty' <- transverseScope normalize ty
      return e { elemType = ty' }

instance (MonadMessages Message m, Applicative m, Eq bound) =>
         Normalize m (Intro bound free) where
  normalize t @ Fix { fixTerm = term } =
    do
      term' <- transverseScope normalize term
      return t { fixTerm = term' }
  -- These cases are entirely compositional
  normalize t @ FuncType { funcTypeArgs = args, funcTypeRetTy = retty } =
    do
      args' <- mapM normalize args
      retty' <- transverseScope normalize retty
      return t { funcTypeArgs = args', funcTypeRetTy = retty' }
  normalize t @ RecordType { recTypeBody = body } =
    do
      body' <- mapM normalize body
      return t { recTypeBody = body' }
  normalize t @ RefineType { refineType = ty, refineCases = cases } =
    do
      ty' <- normalize ty
      cases' <- mapM normalize cases
      return t { refineType = ty', refineCases = cases' }
  normalize t @ CompType { compType = ty, compCases = cases } =
    do
      ty' <- normalize ty
      cases' <- mapM normalize cases
      return t { compType = ty', compCases = cases' }
  normalize t @ Quantified { quantType = ty, quantCases = cases } =
    do
      ty' <- normalize ty
      cases' <- mapM normalize cases
      return t { compType = ty', compCases = cases' }
  normalize t @ Lambda { lambdaCases = cases } =
    do
      cases' <- mapM normalize cases
      return t { lambdaCases = cases' }
  normalize t @ Record { recFields = fields } =
    do
      fields' <- mapM (mapM normalize) fields
      return t { recFields = fields' }
  normalize t @ Elim { elimTerm = term } =
    do
      term' <- normalize term
      return t { elimTerm = term }
  -- Everything else is already normalized
  normalize t = return t

  normalizeHead t @ Elim { elimTerm = term } =
    do
      term' <- normalizeHead term
      return t { elimTerm = term }
  -- Everything else is already normalized
  normalizeHead t = return t

instance (MonadMessages Message m, Applicative m, Eq bound) =>
         Normalize m (Elim bound free) where
  normalize t @ Call { callFunc = func, callArg = arg, callPos = pos } =
    do
      -- Head-normalize to do the actual call.
      res <- normalizeHead t { callFunc = func, callArg = arg }
      -- Then normalize the result
      normalize res
  normalize t @ Typed { typedTerm = term, typedType = ty, typedPos = pos } =
    do
      normterm <- normalize term
      normtype <- normalize ty
      case (normterm, normtype) of
        -- Preserve badness
        (BadIntro {}, _) -> return BadElim { badElimPos = pos }
        (_, BadIntro {}) -> return BadElim { badElimPos = pos }
        -- Rewrite a tuple as a record
        (Tuple { tupleFields = fields, tuplePos = pos },
         RecordType { recTypeBody = fieldtys }) ->
          do
            converted <- convertTuple fieldtys fields pos
            return t { typedTerm = converted }
        -- Eliminate a typed-elim cycle
        (Elim { elimTerm = inner }, _) -> return inner
  -- Anything else is already normal
  normalize t = return t

  normalizeHead t @ Call { callFunc = func, callArg = arg, callPos = pos } =
    do
      normfunc <- normalizeHead func
      normarg <- normalize arg
      case (normfunc, normarg) of
        -- Preserve the badness
        (BadElim {}, _) -> return normfunc
        (_, BadIntro { badIntroPos = badpos }) ->
          return BadElim { badElimPos = badpos }
        -- XXX Placeholder for constructors and intrinsics
        (Var {}, _) -> error "Implement intrinsics"
        (Typed { typedType = FuncType { funcTypeArgs = argtys,
                                        funcTypeRetTy = retty },
                 typedTerm = Constructor {} }, _) ->
          do
            -- Convert the argument to the correct type
            converted <- convertArg argtys normarg
            -- Make sure the converted argument isn't bad
            case converted of
              BadIntro {} -> return BadElim { badElimPos = pos }
              _ -> return t { callFunc = normfunc, callArg = converted }
        -- Evalute calls to lambda terms
        (Typed { typedType = FuncType { funcTypeArgs = argtys,
                                        funcTypeRetTy = retty },
                 typedTerm = Lambda { lambdaCases = cases } }, _) ->
          do
            -- Convert the argument to the correct type
            converted <- convertArg argtys normarg
            -- Use the args to try each case
            case foldl1 (<|>) (map (caseMatch converted) cases) of
              Just res ->
                let
                  retty' = _
                in
                  return Typed { typedTerm = res, typedType = retty',
                                 typedPos = pos }
              Nothing ->
                do
                  noMatch normarg pos
                  return BadElim { badElimPos = pos }
        -- Anything else is an evaluation error.
        _ ->
          do
            callNonFunc func pos
            return BadElim { badElimPos = pos }
  normalizeHead t @ Typed { typedTerm = term, typedType = ty, typedPos = pos } =
    do
      normterm <- normalizeHead term
      normtype <- normalizeHead ty
      case (normterm, normtype) of
        -- Preserve badness
        (BadIntro {}, _) -> return BadElim { badElimPos = pos }
        (_, BadIntro {}) -> return BadElim { badElimPos = pos }
        -- Rewrite a tuple as a record
        (Tuple { tupleFields = fields, tuplePos = pos },
         RecordType { recTypeBody = fieldtys }) ->
          do
            converted <- convertTuple fieldtys fields pos
            return t { typedTerm = converted }
        -- Eliminate a typed-elim cycle
        (Elim { elimTerm = inner }, _) -> return inner
  -- Anything else is already normal
  normalizeHead t = return t
