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
import Control.Monad.Positions
import Control.Monad.Symbols
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
import Text.Format

import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HashMap

class Monad m => Normalize m ty where
  -- | Fully normalize a term.
  normalize :: ty
            -- ^ The term to normalize.
            -> m ty
            -- ^ The normalized term.

unfoldIntro :: (MonadMessages Message m, MonadSymbols m, MonadPositions m,
                Applicative m, FormatM m bound, FormatM m free,
                Default bound, Hashable bound, Eq bound) =>
                Intro bound free
            -> m (Intro bound free)
unfoldIntro t @ Fix { fixTerm = term } = unfoldIntro (instantiate1 t term)
unfoldIntro Elim { elimTerm = Typed { typedTerm = term } } = unfoldIntro term
unfoldIntro term = normalize term

instance (MonadMessages Message m, MonadSymbols m, MonadPositions m,
          Applicative m, FormatM m bound, FormatM m free,
          Default bound, Hashable bound, Eq bound) =>
         Normalize m (Intro bound free) where
  -- These cases are entirely compositional
  normalize t @ RefineType { refineType = ty } =
    do
      ty' <- normalize ty
      return t { refineType = ty' }
  normalize t @ CompType { compType = ty } =
    do
      ty' <- normalize ty
      return t { compType = ty' }
  normalize t @ Quantified { quantType = ty } =
    do
      ty' <- normalize ty
      return t { compType = ty' }
  normalize t @ Tuple { tupleFields = fields } =
    do
      fields' <- mapM normalize fields
      return t { tupleFields = fields' }
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

normalizeCall :: (MonadMessages Message m, MonadSymbols m, MonadPositions m,
                  Applicative m, FormatM m bound, FormatM m free,
                  Default bound, Hashable bound, Eq bound) =>
                 Elim bound free
              -> Intro bound free
              -> Position
              -> m (Elim bound free)
-- Preserve the badness
normalizeCall bad @ BadElim {} _ _ = return bad
normalizeCall _ BadIntro { badIntroPos = pos } _ =
  return BadElim { badElimPos = pos }
-- Normalize constructor calls
-- XXX Expand this to intrinsics generally
normalizeCall func @ Typed { typedType = FuncType { funcTypeArgs = argtys,
                                                    funcTypeRetTy = retty },
                             typedTerm = Constructor {} } arg pos =
  do
    -- Convert the argument to the correct type
    converted <- convertArg argtys arg
    -- Make sure the converted argument isn't bad
    case converted of
      BadIntro {} -> return BadElim { badElimPos = pos }
      _ -> return Call { callFunc = func, callArg = converted, callPos = pos }
-- Evalute calls to lambda terms
normalizeCall Typed { typedType = FuncType { funcTypeArgs = argtys,
                                             funcTypeRetTy = retty },
                      typedTerm = Lambda { lambdaCases = cases } } arg pos =
  do
    -- Convert the argument to the correct type
    converted <- convertArg argtys arg
    -- Use the args to try each case
    case foldl1 (<|>) (map (caseMatch converted) cases) of
      Just res ->
        do
          (instparamtys, instretty) <- instantiateParamRetTypes argtys
          return Typed { typedTerm = res, typedType = instretty,
                         typedPos = pos }
      Nothing ->
        do
          noMatch arg pos
          return BadElim { badElimPos = pos }
-- Anything else is an evaluation error
normalizeCall func _ pos =
  do
    callNonFunc func pos
    return BadElim { badElimPos = pos }

instance (MonadMessages Message m, MonadSymbols m, MonadPositions m,
          Applicative m, FormatM m bound, FormatM m free,
          Default bound, Hashable bound, Eq bound) =>
         Normalize m (Elim bound free) where
  normalize t @ Call { callFunc = func, callArg = arg, callPos = pos } =
    do
      normfunc <- normalize func
      normarg <- normalize arg
      res <- normalizeCall normfunc normarg pos
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
            normalize t { typedTerm = converted }
        (Record { recFields = fields, recPos = pos },
         ty @ RecordType { recTypeBody = fieldtys }) ->
          do
            newfieldtys <- instantiateFieldTypes fieldtys fields pos
            return t { typedType = ty { recTypeBody = newfieldtys } }
        -- Eliminate a typed-elim cycle
        (Elim { elimTerm = inner }, _) -> return inner
  -- Anything else is already normal
  normalize t = return t
