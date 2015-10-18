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
       checkIntroTerm,
       checkElimTerm
       ) where

import Control.Monad.TypeCheck.Class
import Data.Default
import Data.Position.DWARFPosition
import Language.Salt.Core.Syntax

import qualified Data.Map as Map

type TypeContext sym = Map.Map sym (Term sym sym)

-- Utility function to get the type of a symbol
getSymType :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
              TypeContext sym -> sym -> Pos -> m (Term sym sym)
getSymType ctx sym p =
 case Map.lookup sym ctx of
   -- If the context has a type, return it
   Just ty -> return ty
   -- Otherwise log an undefined symbol error
   Nothing -> undefSym sym p

checkSubType :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
                TypeContext sym
             -- ^ The type check context
             -> Term sym sym
             -- ^ The supposed subtype
             -> Term sym sym
             -- ^ The supposed supertype
             -> m Bool
-- For function types, the return type is covariant, and the arguments
-- are contravariant.
checkSubType ctx FuncType {} FuncType {} = error "XXX not implemented"
-- For record types, the subtype must have all the fields of the
-- supertype, and they must be subtypes of the corresponding fields in
-- the supertype.  However, there can be extra fields in the subtype.
checkSubType ctx RecordType {} RecordType {} = error "XXX not implemented"
-- For refinement types, the base types are covariant, and the
-- predicate of the subtype must imply the predicate of the supertype.
checkSubType ctx RefineType { refineType = subbasety }
                 RefineType { refineType = superbasety } =
  do
    out <- checkSubType ctx subbasety superbasety
    -- Only generate a proof obligation if the subtype check succeeded
    if out
      then error "XXX not implemented"
      else return False
-- For computation types, the result type is covariant, and the
-- specification must be a sub-specification of the supertype's
-- specification
checkSubType ctx CompType { compType = subresty }
                 CompType { compType = superresty } =
  do
    out <- checkSubType ctx subresty superresty
    -- Only generate a proof obligation if the subtype check succeeded
    if out
      then error "XXX not implemented"
      else return False
-- For anything else, log a type error
checkSubType _ _ _ = error "XXX not implemented"

-- | Check pattern match cases.
checkCases :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
              TypeContext sym
           -- ^ The type context
           -> Term sym sym
           -- ^ The term being type checked
           -> [Case bound free]
           -- ^ The term being type checked
           -> m [Case bound free]
checkCases _ _ _ = error "XXX not implemented"

-- | Check that a type is well-formed.  At present, it is impossible to construct an arbitrary-rank type, so it is sufficient to
checkType :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
             TypeContext sym
          -- ^ The type context
          -> Term sym sym
          -- ^ The term being type checked
          -> m (Term sym sym)
checkType ctx term =
  do
    typety <- typeType
    checkIntroTerm ctx typety term

checkIntroTerm :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
                  TypeContext sym
               -- ^ The type context.
               -> Term sym sym
               -- ^ The expected type.
               -> Term sym sym
               -- ^ The term being type checked.
               -> m (Term sym sym)
-- Pass checks against a bad type on through
checkIntroTerm _ (BadTerm _) term = return (BadTerm (pos term))
-- For refinement types, check against the base type and emit a proof
-- obligation that the term satisfies the predicate.
checkIntroTerm ctx RefineType { refineType = basety, refineCases = cases,
                                refinePos = p } term =
  let
    -- XXX need to be able to get the type of propositions to
    -- construct the type
    -- predicate = Lambda { lambdaCases = refineCases, lambdaPos = p }
    -- Construct a call to the predicate, with the term
  in do
    term' <- checkIntroTerm ctx basety term
    case term' of
      -- Don't emit a proof obligation if the type check failed
      bad @ (BadTerm _) -> return bad
      _ ->
        do
          -- XXX emit the proof obligation here
          return term'
-- Also, ignore it if we try to check a bad term
checkIntroTerm _ _ bad @ (BadTerm _) = return bad
-- For quantified terms, it's ok to assert that it's a proposition
checkIntroTerm ctx ty term @ Quantified { quantType = quantty,
                                          quantCases = cases,
                                          quantPos = p } =
  do
    propty <- propType
    -- The expected type must be exactly the prop type
    if propty == ty
      -- If it is, then check the type and then the cases
      then do
        quantty' <- checkType ctx quantty
        cases' <- checkCases ctx quantty' cases
        return term { quantType = quantty', quantCases = cases' }
      -- Otherwise, log a type error
      else formMismatch term ty
-- For lambda terms, it is ok to assert that it's a function type, as
-- long as the arguments match up.
checkIntroTerm ctx FuncType {} Lambda {} = error "XXX not implemented yet"
-- Anything else is a type error
checkIntroTerm ctx ty term @ Lambda {} = formMismatch term ty
-- For records, the expected type needs to be a record type, with the
-- correct fields.
checkIntroTerm ctx RecordType {} Record {} = error "XXX not implemented yet"
-- Anything else is a type error
checkIntroTerm ctx ty term @ Record {} = formMismatch term ty
-- For computation terms, expect them to have a computation type
checkIntroTerm ctx CompType {} Comp {} = error "XXX not implemented yet"
-- Anything else is a type error
checkIntroTerm ctx ty term @ Comp {} = formMismatch term ty
-- The rest of the terms are elimination terms, and get checked by
-- checkElimTerm, and we need to assert that the synthesized type is a
-- subtype of the expected type.
checkIntroTerm ctx ty term =
  do
    (term', actualty) <- checkElimTerm ctx term
    subtype <- checkSubType ctx actualty ty
    if subtype
      then return term'
      else return (BadTerm (pos term))

-- | Type check elimination terms.  These terms produce a type during
-- type checking, as opposed to introduction terms, which require a
-- type to check a term against.
checkElimTerm :: (MonadTypeCheck sym m, Default sym, Ord sym) =>
                 TypeContext sym
              -- ^ The type context.
              -> Elim sym sym
              -- ^ The term being type checked.
              -> m (Intro bound free)
              -- ^ The type of the checked term.
-- For calls, expect to get a function type.  Check all the arguments,
-- then figure out what the return type should be.
checkElimTerm ctx Call { callFunc = func, callPos = pos } =
  do
    functy <- checkElimTerm ctx func
    case functy of
      -- Now, check that the arguments match, and then figure out the
      -- return type.
      FuncType {} -> error "XXX not implemented yet"
      -- Pass bad types on through
      BadElim { badElimPos = bpos } -> return BadIntro { badIntroPos = bpos }
      -- Anything else is an error
      _ ->
        do
          expectedFuncType (elimTermPos func)
          return BadIntro { badIntroPos = pos }
-- For typed terms, use the stated type to check the introduction term
checkElimTerm ctx typed @ Typed { typedTerm = term, typedType = ty } =
  do
    -- First check the type, in case it's bad, and also to resolve any
    -- type symbols.
    ty' <- checkType ctx ty
    term' <- checkIntroTerm ctx ty' term
    return ty'
-- For a symbol, look it up in the type context and return it
checkElimTerm ctx term @ Var { varSym = sym, varPos = p } =
  do
    ty' <- getSymType ctx sym p
    return (term, ty')
-- Pass errors on through
checkElimTerm _ BadElim { badElimPos = pos } =
  return BadIntro { badIntroPos = pos }
