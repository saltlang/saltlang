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

-- | The type checker module.
module Language.Salt.Core.TypeCheck(
       checkIntroTerm,
       checkElimTerm
       ) where

import Control.Monad.TypeCheck.Class
import Data.Pos
import Language.Salt.Core.Syntax

import qualified Data.Map as Map

type TypeContext sym = Map.Map sym (Term sym sym)

isSubType :: (MonadTypeCheck sym m, Ord sym) =>
             TypeContext sym
          -> Term sym sym
          -> Term sym sym
          -> m Bool
isSubType = error "XXX not implemented"

checkType :: (MonadTypeCheck sym m, Ord sym) =>
             TypeContext sym
          -- ^ The type context
          -> Term sym sym
          -- ^ The term being type checked
          -> m (Term sym sym)
checkType _ _ = error "XXX not implemented"

checkIntroTerm :: (MonadTypeCheck sym m, Ord sym) =>
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
checkIntroTerm ctx ty Quantified {} = error "XXX not implemented yet"
-- For lambda terms, it is ok to assert that it's a function type, as
-- long as the arguments match up.
checkIntroTerm ctx ProdType {} Lambda {} = error "XXX not implemented yet"
-- Anything else is a type error
checkIntroTerm ctx ty term @ Lambda {} = formMismatch term ty
-- For records, the expected type needs to be a record type, with the
-- correct fields.
checkIntroTerm ctx SumType {} Record {} = error "XXX not implemented yet"
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
    actualty <- checkElimTerm ctx term
    subtype <- isSubType ctx actualty ty
    if subtype
      then return term
      else return (BadTerm (pos term))

-- | Type check elimination terms.  These terms produce a type during
-- type checking, as opposed to introduction terms, which require a
-- type to check a term against.
checkElimTerm :: (MonadTypeCheck sym m, Ord sym) =>
                 TypeContext sym
              -- ^ The type context.
              -> Term sym sym
              -- ^ The term being type checked.
              -> m (Term sym sym)
-- For calls, expect to get a function type.  Check all the arguments,
-- then figure out what the return type should be.
checkElimTerm ctx Call { callFunc = func } =
  do
    functy <- checkElimTerm ctx func
    case functy of
      -- Now, check that the arguments match, and then figure out the
      -- return type.
      ProdType {} -> error "XXX not implemented yet"
      -- Pass bad types on through
      bad @ (BadTerm _) -> return bad
      -- Anything else is an error
      _ -> expectedFunction func functy
-- For typed terms, use the stated type to check the introduction term
checkElimTerm ctx Typed { typedTerm = term, typedType = ty } =
  do
    -- First check the type, in case it's bad, and also to resolve any
    -- type symbols.
    ty' <- checkType ctx ty
    checkIntroTerm ctx ty' term
    return ty'
-- For a symbol, look it up in the type context and return it
checkElimTerm ctx Var { varSym = sym, varPos = p } =
  case Map.lookup sym ctx of
    -- If the context has a type, return it
    Just ty -> return ty
    -- Otherwise log an undefined symbol error
    Nothing -> undefSym sym p
-- Pass errors on through
checkElimTerm _ (BadTerm pos) = return (BadTerm pos)
-- Attempting to check anything else is a compiler bug
checkElimTerm _ _ =
  error "Compiler error: attempting elimination term type check on a non-elimination term"
