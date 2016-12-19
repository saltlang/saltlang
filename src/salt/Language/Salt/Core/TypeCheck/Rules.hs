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
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Salt.Core.TypeCheck.Rules(
       -- * Elimination Term Rules
       synthCallRule,
       synthTypedRule,
       synthVarRule,

       -- * Intro Term Rules
       checkAgainstRefineRule,
       checkElimRule,
       ) where

import Bound
import Control.Monad.Messages
import Control.Monad.TypeCheck.Class
import Data.Default
import Data.Position.DWARFPosition
import Data.PositionElement
import Data.Symbol
import Language.Salt.Core.Syntax
import Language.Salt.Core.TypeCheck.Env(Env)
import Language.Salt.Message

import qualified Data.Array as Array
import qualified Language.Salt.Core.TypeCheck.Env as Env

emptyPattern :: Position
             -> Pattern Symbol
emptyPattern pos = Name { nameSym = def, namePos = pos }

makeFuncType :: Monad m =>
                Intro Symbol Symbol
             -> Intro Symbol Symbol
             -> m (Intro Symbol Symbol)
makeFuncType RecordType { recTypeBody = args, recTypeOrder = order,
                          recTypePos = pos } retty =
  let
    retscope = abstract (const Nothing) retty
  in
    return FuncType { funcTypeArgs = args, funcTypeArgOrder = order,
                      funcTypeRetTy = retscope, funcTypePos = pos }
makeFuncType argty retty =
  let
    argscope = abstract (const Nothing) argty
    retscope = abstract (const Nothing) retty
    pos = debugPosition argty
  in do
    -- XXX Need the argument name
    argname <- _
    -- Construct a synthetic function type
    return FuncType { funcTypeArgs = [Element { elemName = argname,
                                                elemPat = emptyPattern pos,
                                                elemType = argscope,
                                                elemPos = pos }],
                      funcTypeArgOrder = Array.listArray (1, 1) [argname],
                      funcTypeRetTy = retscope, funcTypePos = pos }


-- | Check that the called object is a function, get its type, check
-- the argument, and synthesize the return type with the substitutions
-- applied.
--
-- Corresponds to the following type rule:
--
-- >  E |- f => Pi(argtys, retty)   E |- arg <= Sigma(argtys)
-- > ---------------------------------------------------------
-- >           E |- f arg => subst(arg, argty, retty)
synthCallRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                 Elim Symbol Symbol
              -> m (Intro Symbol Symbol)
synthCallRule Call { callFunc = func, callArg = arg, callPos = callpos } =
  let
    checkSingleCall :: (MonadMessages Message m, MonadTypeCheck m) =>
                       Intro Symbol Symbol
                    -> m (Intro Symbol Symbol)
    -- We can work with a function type
    checkSingleCall FuncType { funcTypeArgs = argtys, funcTypeArgOrder = order,
                               funcTypeRetTy = retty, funcTypePos = pos } =
      let
        argty = RecordType { recTypeBody = argtys, recTypeOrder = order,
                             recTypePos = pos }
      in do
        -- Check that the argument has the right type
        checkIntro arg argty
        -- XXX We need to substitute everything that was captured
        -- while checking the arguments into the result type and
        -- return it.
        _
    -- This shouldn't happen
    checkSingleCall _ =
      do
        internalError "Bad argument to checkSingleCall" [basicPosition callpos]
        return BadIntro { badIntroPos = callpos }
  in do
    rawty <- synthElim func
    -- Compute the join with the set of all function types
    functy <- typeJoinFunc rawty
    -- Decide what to do based on the form of the type
    case functy of
      -- XXX When we have join types (represents multiple dispatch),
      -- speculatively type-check the whole group, make sure we have
      -- at least one viable solution, then combine all the good
      -- solutions and join them together.

      -- Run a single call without speculation
      f @ FuncType {} -> checkSingleCall f
      -- Preserve badness
      bad @ BadIntro {} -> return bad
      -- This shouldn't happen
      _ ->
        do
          internalError "Bad result from funcTypeJoin" [basicPosition callpos]
          return BadIntro { badIntroPos = callpos }
synthCallRule term =
  do
    internalError "Improper use of synthCall rule" [position term]
    return BadIntro { badIntroPos = debugPosition term }

-- | Synthesize a type directly from a typed intro expression.
--
-- Corresponds to the following type rule:
--
-- >  E |- ty <= type   E |- term <= ty
-- > -----------------------------------
-- >       E |- term : ty => ty
synthTypedRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                  Elim Symbol Symbol
               -> m (Intro Symbol Symbol)
synthTypedRule Typed { typedType = ty, typedTerm = term, typedPos = pos } =
  do
    -- Check that the type actually is a type.
    checkType ty
    -- Check that the term has the given type.
    checkIntro term ty
    -- Give back the ascribed type as this term's type.
    return ty
synthTypedRule term =
  do
    internalError "Improper use of synthTyped rule" [position term]
    return BadIntro { badIntroPos = debugPosition term }

-- | Determine a symbol reference's type from the type environment.
--
-- Corresponds to the following type rule:
--
-- >
-- > -----------------
-- >  x : t |- x => t
synthVarRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                Env
             -> Elim Symbol Symbol
             -> m (Intro Symbol Symbol)
synthVarRule env Var { varSym = sym, varPos = pos } = Env.lookupSym env pos sym
synthVarRule _ term =
  do
    internalError "Improper use of synthVar rule" [position term]
    return BadIntro { badIntroPos = debugPosition term }

-- | Check a term against a refinement type.
--
-- Corresponds to the following type rule:
--
-- >  E |- term <= ty0   E |- pred <= Pi(ty0, prop)   E ==> pred term
-- > -----------------------------------------------------------------
-- >                  E |- term <= { ty0 | pred }
checkAgainstRefineRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                          Intro Symbol Symbol
                       -- ^ The term being checked.
                       -> Intro Symbol Symbol
                       -- ^ The type against which it's being checked.
                       -> m ()
checkAgainstRefineRule term RefineType { refineType = innerty,
                                         refineCases = cases,
                                         refinePos = pos } =
  let
    pred = Lambda { lambdaCases = cases, lambdaPos = pos }
  in do
    -- First, make sure the inner type is a well-formed type.
    checkType innerty
    -- Now, check the term against the inner type.
    checkIntro term innerty
    -- Get the type of propositions.
    propty <- _
    -- Type-check a call to the predicate.
    resty <- synthElim _
    -- Assert that the term satisfies the predicate.
    assertion _
checkAgainstRefineRule term _ =
  internalError "Improper use of checkAgainstRefineRule rule" [position term]

-- | Check that a quantified term's cases map from the quantifier type
-- to prop.
--
-- Corresponds to the following type rule:
--
-- >  ty :> prop   E |- qty <= type   E |- case_i <= Pi(qty, ty)
-- > ------------------------------------------------------------
-- >               E |- quant qty. [case_i] <= ty
checkQuantifiedRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                       Intro Symbol Symbol
                    -- ^ The term being checked.
                    -> Intro Symbol Symbol
                    -- ^ The type against which it's being checked.
                    -> m ()
checkQuantifiedRule Quantified { quantType = qty, quantCases = cases,
                                 quantPos = pos } ty =
  do
    -- Check that the result type is a subtype of proposition
    checkProp ty
    -- Check that the quantifier type is a well-formed type
    checkType qty
    -- Check the cases against the expected type
    casety <- makeFuncType qty _
    mapM_ ((flip checkCase) casety) cases

-- | Check a lambda term's cases against the expected type, and check
-- for case completeness.
--
-- Corresponds to the following type rule:
--
-- >   E |- case_i <= ty
-- > ----------------------
-- >  E |- \[case_i] <= ty
checkLambdaRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                   Intro Symbol Symbol
                -- ^ The term being checked.
                -> Intro Symbol Symbol
                -- ^ The type against which it's being checked.
                -> m ()
checkLambdaRule Lambda { lambdaCases = cases, lambdaPos = pos } ty =
  do
    -- Check the cases against the expected type
    mapM_ ((flip checkCase) ty) cases
    -- Assert that the cases are complete
    _

-- | Check that an elimination term's actual type is a subtype of the
-- expected type.
--
-- Corresponds to the following type rule:
--
-- >  E |- term => ty'   (term : ty') :> ty
-- > ---------------------------------------
-- >            E |- term <= ty
checkElimRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                 Intro Symbol Symbol
              -- ^ The term being checked.
              -> Intro Symbol Symbol
              -- ^ The type against which it's being checked.
              -> m ()
checkElimRule term @ Elim { elimTerm = elim } expectedty =
  do
    -- Get the actual type from type synthesis
    actualty <- synthElim elim
    -- Check that the actual term is a subtype of the expected
    checkSubtype term actualty expectedty
checkElimRule term _ =
  internalError "Improper use of checkElim rule" [position term]
