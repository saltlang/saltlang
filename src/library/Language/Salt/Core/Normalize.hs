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
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Language.Salt.Core.Normalize(
       normalizeElim,
       normalizeIntro
       ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Monad.Messages
import Data.Default
import Data.Hashable
import Data.HashMap.Strict((!))
import Data.Position.DWARFPosition
import Data.Traversable
import Language.Salt.Core.Syntax
import Language.Salt.Core.Patterns
import Language.Salt.Message
import Prelude hiding (mapM)

-- | Attempt to match and execute a case.
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
    Just binds ->
      Just $! instantiate (binds !) body

normalizeCase :: (MonadMessages Message m, Applicative m, Eq bound) =>
                 Case bound free
              -> m (Case bound free)
normalizeCase c @ Case { caseBody = body } =
  do
    body' <- transverseScope normalizeIntro body
    return c { caseBody = body' }

normalizeElement :: (MonadMessages Message m, Applicative m, Eq bound) =>
                    Element bound free
                 -> m (Element bound free)
normalizeElement e @ Element { elemType = ty } =
  do
    ty' <- transverseScope normalizeIntro ty
    return e { elemType = ty' }


-- | Normalize an intro term.
normalizeIntro :: (MonadMessages Message m, Applicative m, Eq bound) =>
                  Intro bound free
               -> m (Intro bound free)
normalizeIntro t @ Fix { fixTerm = term } =
  do
    term' <- transverseScope normalizeIntro term
    return t { fixTerm = term' }
-- These cases are entirely compositional
normalizeIntro t @ FuncType { funcTypeArgs = args, funcTypeRetTy = retty } =
  do
    args' <- mapM normalizeElement args
    retty' <- transverseScope normalizeIntro retty
    return t { funcTypeArgs = args', funcTypeRetTy = retty' }
normalizeIntro t @ RecordType { recTypeBody = body } =
  do
    body' <- mapM normalizeElement body
    return t { recTypeBody = body' }
normalizeIntro t @ RefineType { refineType = ty, refineCases = cases } =
  do
    ty' <- normalizeIntro ty
    cases' <- mapM normalizeCase cases
    return t { refineType = ty', refineCases = cases' }
normalizeIntro t @ CompType { compType = ty, compCases = cases } =
  do
    ty' <- normalizeIntro ty
    cases' <- mapM normalizeCase cases
    return t { compType = ty', compCases = cases' }
normalizeIntro t @ Quantified { quantType = ty, quantCases = cases } =
  do
    ty' <- normalizeIntro ty
    cases' <- mapM normalizeCase cases
    return t { compType = ty', compCases = cases' }
normalizeIntro t @ Lambda { lambdaCases = cases } =
  do
    cases' <- mapM normalizeCase cases
    return t { lambdaCases = cases' }
normalizeIntro Eta {} = error "Eta is going away"
normalizeIntro t @ Record { recFields = fields } =
  do
    fields' <- mapM normalizeIntro fields
    return t { recFields = fields' }
normalizeIntro t @ Elim { elimTerm = term } =
  do
    term' <- normalizeElim term
    return t { elimTerm = term }
-- Everything else is already normalized
normalizeIntro t = return t

unfoldIntro :: (MonadMessages Message m, Applicative m, Eq bound) =>
                Intro bound free
            -> m (Intro bound free)
unfoldIntro t @ Fix { fixTerm = term } = unfoldIntro (instantiate1 t term)
unfoldIntro Elim { elimTerm = Typed { typedTerm = term } } = unfoldIntro term
unfoldIntro term = normalizeIntro term

-- | Normalize only the head of an elimination term.
normalizeElimHead :: MonadMessages Message m =>
                     Elim bound free
                  -- ^ Term to normalize
                  -> m (Elim bound free)
normalizeElimHead t @ Call { callFunc = func, callArg = arg, callPos = pos } =
  case func of
    -- Pass bad terms on through
    BadElim {} -> return func
    -- A var here is an intrinsic, so send it there.
    Var {} -> error "Implement intrinsics"
    -- No need to normalize calls to constructors.
    Typed { typedType = FuncType { funcTypeArgs = argtys,
                                   funcTypeRetTy = retty },
            typedTerm = Constructor {} } -> return t
    -- Evalute calls to lambda terms
    Typed { typedType = FuncType { funcTypeArgs = argtys,
                                   funcTypeRetTy = retty },
            typedTerm = Lambda { lambdaCases = cases } } ->
      let
        -- Figure out the right argument value and construct the term
        argval = _
        -- Sub in all the arguments to build up the result type
        retty' = _
      in
        -- Use the args to try each case
        case foldl1 (<|>) (map (caseMatch argval) cases) of
          Just res -> return Typed { typedTerm = res, typedType = retty',
                                     typedPos = pos }
          Nothing ->
            do
              noMatch argval pos
              return BadElim { badElimPos = pos }
    -- Anything else is an evaluation error.
    _ ->
      do
        callNonFunc func pos
        return BadElim { badElimPos = pos }
-- Typed terms can contain Typed-Elim cycles.  Wipe them out.  Note:
-- this potentially eliminates type checks, so it must only be done on
-- well-typed terms.
normalizeElimHead t @ Typed { typedType = ty, typedTerm = term } =
  case term of
    -- Pass bad terms on through.
    BadIntro { badIntroPos = p } -> BadElim { badElimPos = p }
    -- Wipe out Typed-Elim cycles.  This is where type-checks could be lost.
    Elim { elimTerm = inner } -> return inner
    _ -> return t { typedType = ty, typedTerm = term }
-- Anything else is already normal
normalizeElimHead t = return t

-- | Normalize an elimination term.
normalizeElim :: (MonadMessages Message m, Applicative m, Eq bound) =>
                 Elim bound free
              -- ^ The term to normalize.
               -> m (Elim bound free)
normalizeElim t @ Call { callFunc = func, callArg = arg, callPos = pos } =
  do
    -- Deep-normalize the function.
    func' <- normalizeElim func
    -- Normalize the arguments as well
    arg' <- normalizeIntro arg
    -- Then head-normalize to do the actual call.
    res <- normalizeElimHead t { callFunc = func', callArg = arg' }
    -- Then normalize the result
    normalizeElim res
normalizeElim Typed { typedTerm = Elim { elimTerm = term } } =
  normalizeElim term
normalizeElim term @ Typed { typedTerm = intro } =
  do
    -- Do one head normalization.
    term' <- normalizeElimHead term
    -- Then repeat.
    normalizeElim term'
-- Anything else is already normal
normalizeElim t = return t
