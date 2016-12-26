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
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}

-- | The type checker module.
module Language.Salt.Core.TypeCheck(
       synthElimImpl,
       checkIntroImpl
       ) where

import Control.Monad.TypeCheck.Class
import Data.Symbol
import Language.Salt.Core.Syntax

-- | Check the type of 'Intro' terms.  These terms are checked against
-- a type, as opposed to 'Elim' terms, which synthesize a type.
checkIntroImpl :: (MonadIntroCheck m) =>
                  Intro Symbol Symbol
               -- ^ The term being checked.
               -> Intro Symbol Symbol
               -- ^ The type against which to check the term.
               -> m ()
-- We can check an elimination term against any type
checkIntroImpl Elim {} _ = checkElim
-- Checking refinement types against any non-elimination term
-- generates assertions, and then recurses.
checkIntroImpl _ RefineType {} = checkAgainstRefine
-- If we can directly apply type rules, do so.
checkIntroImpl FuncType {} Type {} = checkFuncType
checkIntroImpl RecordType {} Type {} = checkRecordType
checkIntroImpl RefineType {} Type {} = checkRefine
checkIntroImpl CompType {} Type {} = checkCompType
checkIntroImpl Type {} Type {} = checkType
checkIntroImpl PropType {} Type {} = checkProp
checkIntroImpl Quantified {} PropType {} = checkQuantified
checkIntroImpl Lambda {} FuncType {} = checkLambda
checkIntroImpl Record {} RecordType {} = checkRecord
checkIntroImpl Tuple {} RecordType {} = checkTuple
checkIntroImpl Comp {} CompType {} = checkIntroComp
checkIntroImpl Literal {} Elim { elimTerm = Var {} } = checkLiteral
-- If we see an elim term on the right side, then we try to resolve it
-- to a concrete type.
checkIntroImpl _ Elim {} = _
-- Skip bad terms or types completely.
checkIntroImpl BadIntro {} _ = return ()
checkIntroImpl _ BadIntro {} = return ()
-- Anything else is an error.
checkIntroImpl term ty = _

-- | Synthesize the type of an 'Elim' terms.  These terms produce a
-- type during type checking, as opposed to 'Intro' terms, which
-- require a type against which to check.
synthElimImpl :: (MonadElimSynth m) =>
                 Elim Symbol Symbol
              -- ^ The term whose type to synthesize.
              -> m (Intro Symbol Symbol)
              -- ^ The type of the checked term.
-- This implementation is just deciding which type rule to invoke
-- depending on the head structure of the term.
synthElimImpl Call {} = synthCall
synthElimImpl Typed {} = synthTyped
synthElimImpl Var {} = synthVar
-- Preserve bad terms.
synthElimImpl BadElim { badElimPos = pos } =
  return BadIntro { badIntroPos = pos }
