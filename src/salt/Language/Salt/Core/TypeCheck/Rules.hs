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
       checkFuncTypeRule,
       checkRecordTypeRule,
       checkCompTypeRule,
       checkRefineRule,
       checkTypeRule,
       checkPropRule,
       checkQuantifiedRule,
       checkLambdaRule,
       checkRecordRule,
       checkTupleRule,
       checkIntroCompRule,
       checkElimRule,
       checkLiteralRule,

       -- * Subtyping Rules
       checkSubtypeTypesRule,
       checkSubtypePropsRule
       ) where

import Bound
import Control.Monad
import Control.Monad.Messages
import Control.Monad.Refs.Class
import Control.Monad.Symbols
import Control.Monad.TypeCheck.Class
import Control.Monad.TypeRanks.Class
import Data.Array(Array, (!))
import Data.Default
import Data.HashMap.Strict(HashMap)
import Data.Maybe
import Data.Position.DWARFPosition
import Data.PositionElement
import Data.Ratio
import Data.Symbol
import Language.Salt.Core.Patterns
import Language.Salt.Core.Syntax
import Language.Salt.Core.Syntax.Util
import Language.Salt.Core.TypeCheck.Env(Env)
import Language.Salt.Message

import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Salt.Core.TypeCheck.Env as Env

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
    checkSingleCall FuncType { funcTypeArgs = argtys, funcTypeRetTy = retty,
                               funcTypePos = pos } =
      let
        argty = RecordType { recTypeBody = argtys, recTypePos = pos }
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
synthTypedRule :: (MonadMessages Message m, MonadTypeCheck m,
                   MonadTypeRanks m) =>
                  Elim Symbol Symbol
               -> m (Intro Symbol Symbol)
synthTypedRule Typed { typedType = ty, typedTerm = term, typedPos = pos } =
  do
    rank <- rankvar
    -- Check that the type actually is a type.
    checkIntro ty Type { typeRank = rank, typePos = pos }
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

checkElementRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                    Intro Symbol Symbol
                 -> Intro Symbol Symbol
                 -> Pattern Symbol
                 -> m (HashMap Symbol (Intro Symbol Symbol))
checkElementRule term ty pat =
  do
    -- Check the term against the field type
    checkIntro term ty
    -- Figure out the substitutions to perform for the rest of the
    -- elements
    case patternMatch pat term of
      Just out -> return out
      Nothing ->
        do
          internalError "Pattern match failed in field check" [position term]
          return HashMap.empty

-- | Check a term against a refinement type.
--
-- Corresponds to the following type rule:
--
-- >  E |- term <= ty0   E ==> pred(term)
-- > ------------------------------------
-- >     E |- term <= { ty0 | pred }
checkAgainstRefineRule :: (MonadMessages Message m, MonadFieldNames m,
                           MonadTypeCheck m) =>
                          Intro Symbol Symbol
                       -- ^ The term being checked.
                       -> Intro Symbol Symbol
                       -- ^ The type against which it's being checked.
                       -> m ()
checkAgainstRefineRule term RefineType { refineType = innerty,
                                         refineCases = cases,
                                         refinePos = pos } =
  let
    assertPred cases' =
      let
        pred = Lambda { lambdaCases = cases', lambdaPos = pos }
      in do
        ty <- funcType innerty PropType { propPos = pos }
        assertion Elim { elimTerm = Call { callFunc = Typed { typedTerm = pred,
                                                              typedType = ty,
                                                              typedPos = pos },
                                           callArg = term,
                                           callPos = pos } }
  in do
    -- First, check the term against the inner type.
    checkIntro term innerty
    -- Assert that the term satisfies the predicate.
    assertPred cases
checkAgainstRefineRule term _ =
  internalError "Improper use of checkAgainstRefineRule rule" [position term]

-- | Check that a function type's arguments and return type are all
-- well-formed types of the same rank as the function type.
--
-- Corresponds to the following type rule:
--
-- >       E |- [field_i : ty_i] <= type(n)
-- >    E, [field_i : ty_i] |- retty <= type(n)
-- > ---------------------------------------------
-- >  E |- Pi([field_i : ty_i], retty) <= type(n)
checkFuncTypeRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                     Intro Symbol Symbol
                  -- ^ The term being checked.
                  -> Intro Symbol Symbol
                  -- ^ The type against which it's being checked.
                  -> m ()
checkFuncTypeRule FuncType { funcTypeArgs = args, funcTypeRetTy = retscope,
                             funcTypePos = pos }
                  ty @ Type {} =
  checkElementTypes args (Just retscope) ty
checkFuncTypeRule term _ =
  internalError "Improper use of checkFuncType rule" [position term]

-- | Check that a record type's fields are all well-formed types of
-- the same rank as the record type.
--
-- Corresponds to the following type rule:
--
-- >   E |- [fname_i : ty_i] <= type(n)
-- > ------------------------------------
-- >  E |- ([fname_i : ty_i]) <= type(n)
checkRecordTypeRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                       Intro Symbol Symbol
                    -- ^ The term being checked.
                    -> Intro Symbol Symbol
                    -- ^ The type against which it's being checked.
                    -> m ()
checkRecordTypeRule RecordType { recTypeBody = body }
                    ty @ Type {} =
  checkElementTypes body Nothing ty
checkRecordTypeRule term _ =
  internalError "Improper use of checkRecordType rule" [position term]

-- | Check that a refinement type's inner type is a type of lesser
-- rank, and that the cases all match the inner type and produce a
-- @prop@.
--
-- Corresponds to the following type rule:
--
-- >  E |- innerty <= type(n)   n < m   E |- case_i <= Pi(innerty, prop)
-- > --------------------------------------------------------------------
-- >                E |- { innerty | [case_i] } <= type(m)
checkRefineRule :: (MonadMessages Message m, MonadFieldNames m,
                    MonadTypeCheck m, MonadTypeRanks m) =>
                   Intro Symbol Symbol
                -- ^ The term being checked.
                -> Intro Symbol Symbol
                -- ^ The type against which it's being checked.
                -> m ()
checkRefineRule RefineType { refineType = innerty, refineCases = cases,
                             refinePos = pos }
                Type {} =
  do
    rank <- rankvar
    -- Check that the inner type is a well-formed type
    checkIntro innerty Type { typeRank = rank, typePos = pos }
    -- Check the type of the cases.
    casety <- funcType innerty PropType { propPos = pos }
    mapM_ (`checkCase` casety) cases
checkRefineRule term _ =
  internalError "Improper use of checkRefine rule" [position term]

-- | Check that a computation type's result type is a type of lesser
-- rank, and that the cases all match the result type and produce a
-- @spec@.
--
-- Corresponds to the following type rule:
--
-- >  E |- resty <= type(n)   n < m   E |- case_i <= Pi(resty, spec)
-- > ----------------------------------------------------------------
-- >               E |- Comp(resty, spec) <= type(m)
checkCompTypeRule :: (MonadMessages Message m, MonadTypeCheck m,
                      MonadTypeRanks m,
                      MonadRefs (Intro Symbol Symbol) m) =>
                     Intro Symbol Symbol
                  -- ^ The term being checked.
                  -> Intro Symbol Symbol
                  -- ^ The type against which it's being checked.
                  -> m ()
checkCompTypeRule CompType { compType = resty, compCases = cases,
                             compTypePos = pos }
                  Type {} =
  do
    rank <- rankvar
    -- Check that the result type is a well-formed type
    checkIntro resty Type { typeRank = rank, typePos = pos }
    -- Check the cases against a function from a result to a spec
    specty <- getCompSpecRef
    casety <- funcType resty specty
    mapM_ (`checkCase` casety) cases

checkCompTypeRule term _ =
  internalError "Improper use of checkRefine rule" [position term]

-- | Axiomatic rule asserting that a @type(n)@ is a @type(m)@ where @n
-- < m@.
--
-- Corresponds to the following type rule:
--
-- >         n < m
-- > -----------------------
-- >  |- type(n) <= type(m)
checkTypeRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                 Intro Symbol Symbol
              -- ^ The term being checked.
              -> Intro Symbol Symbol
              -- ^ The type against which it's being checked.
              -> m ()
-- Prop is a type
checkTypeRule Type { typeRank = n } Type { typeRank = m } = rankLess n m
checkTypeRule term _ =
  internalError "Improper use of checkType rule" [position term]

-- | Axiomatic rule asserting that the @prop@ type is a @type@.
--
-- Corresponds to the following type rule:
--
-- >
-- > -----------------
-- >  |- prop <= type(n)
checkPropRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                 Intro Symbol Symbol
              -- ^ The term being checked.
              -> Intro Symbol Symbol
              -- ^ The type against which it's being checked.
              -> m ()
-- Prop is a type
checkPropRule PropType {} Type {} = return ()
checkPropRule term _ =
  internalError "Improper use of checkProp rule" [position term]

-- | Check that a quantified term's cases map from the quantifier type
-- to prop.
--
-- Corresponds to the following type rule:
--
-- >  E |- ty :> prop   E |- qty <= type   E |- case_i <= Pi(qty, ty)
-- > -----------------------------------------------------------------
-- >                  E |- quant qty. [case_i] <= ty
checkQuantifiedRule :: (MonadMessages Message m, MonadFieldNames m,
                        MonadTypeCheck m, MonadTypeRanks m) =>
                       Intro Symbol Symbol
                    -- ^ The term being checked.
                    -> Intro Symbol Symbol
                    -- ^ The type against which it's being checked.
                    -> m ()
checkQuantifiedRule Quantified { quantType = qty, quantCases = cases,
                                 quantPos = pos }
                    PropType {} =
  do
    rank <- rankvar
    -- Check that the quantifier type is a well-formed type
    checkIntro qty Type { typeRank = rank, typePos = pos }
    -- Check the cases against the expected type
    casety <- funcType qty PropType { propPos = pos }
    mapM_ (`checkCase` casety) cases

-- | Check a 'Lambda' term's cases against the expected type, and
-- check for case completeness.
--
-- Corresponds to the following type rule:
--
-- >    E |- case_i <= ty
-- > ----------------------
-- >  E |- \[case_i] <= ty
checkLambdaRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                   Intro Symbol Symbol
                -- ^ The term being checked.
                -> Intro Symbol Symbol
                -- ^ The type against which it's being checked.
                -> m ()
checkLambdaRule Lambda { lambdaCases = cases, lambdaPos = pos }
                ty @ FuncType {} =
  do
    -- Check the cases against the expected type
    mapM_ (`checkCase` ty) cases
    -- Assert that the cases are complete
    _

-- | Check that a record term has the same fields as the record type,
-- and check all the fields' types.
--
-- Corresponds to the following type rule:
--
-- >  {fname_j} = {bname_i}   E |- [ (term_l, ty_k) | fname_k = bname_l ]
-- > ---------------------------------------------------------------------
-- >           E |- ([bname_i = term_i]) <= ([fname_j : ty_j])
checkRecordRule :: (MonadMessages Message m, MonadSymbols m,
                    MonadTypeCheck m) =>
                   Intro Symbol Symbol
                -- ^ The term being checked.
                -> Intro Symbol Symbol
                -- ^ The type against which it's being checked.
                -> m ()
checkRecordRule Record { recPos = pos, recFields = fields }
                RecordType { recTypeBody = fieldtys } =
  do
    pairings <- matchRecordFields pos fieldtys fields
    checkFields pairings
checkRecordRule term _ =
  internalError "Improper use of checkRecord rule" [position term]

-- | Check that a tuple term has the same number of fields as the record type,
-- and check all the fields' types.
--
-- Corresponds to the following type rule:
--
-- >  |{fname_j}| = |{bname_i}|   E |- [(term_k, ty_k)]
-- > ---------------------------------------------------
-- >   E |- ([bname_i = term_i]) <= ([fname_j : ty_j])
checkTupleRule :: (MonadMessages Message m, MonadSymbols m,
                   MonadTypeCheck m) =>
                  Intro Symbol Symbol
               -- ^ The term being checked.
               -> Intro Symbol Symbol
               -- ^ The type against which it's being checked.
               -> m ()
checkTupleRule Tuple { tuplePos = pos, tupleFields = fields }
               RecordType { recTypeBody = fieldtys } =
  do
    pairings <- matchTupleFields pos fieldtys fields
    checkFields pairings
checkTupleRule term _ =
  internalError "Improper use of checkTuple rule" [position term]

-- | Check that a computation term's actual type is a subtype of the
-- expected type.
--
-- Corresponds to the following type rule:
--
-- >  E |- comp => ty'   E |- (do comp : ty') :> ty
-- > -----------------------------------------------
-- >               E |- do comp <= ty
checkIntroCompRule :: (MonadMessages Message m, MonadTypeCheck m) =>
                      Intro Symbol Symbol
                   -- ^ The term being checked.
                   -> Intro Symbol Symbol
                   -- ^ The type against which it's being checked.
                   -> m ()
checkIntroCompRule c @ Comp { compBody = body }
                   expectedty @ CompType {} =
  do
    actualty <- synthComp body
    checkSubtype c actualty expectedty
checkIntroCompRule term _ =
  internalError "Improper use of checkIntroComp rule" [position term]

-- | Check that an elimination term's actual type is a subtype of the
-- expected type.
--
-- Corresponds to the following type rule:
--
-- >  E |- term => ty'   E |- (term : ty') :> ty
-- > --------------------------------------------
-- >               E |- term <= ty
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

-- | Check that a literal's expected type is a subtype of the most
-- general type for this kind of literal.
--
-- Corresponds to the following type rule:
--
-- >  E |- (lit : ty) :> LitTy[lit]
-- > -------------------------------
-- >        E |- lit <= ty
--
-- Where @LitTy[lit]@ is the most general type for that kind of
-- literal.
checkLiteralRule :: (MonadMessages Message m, MonadTypeCheck m,
                     MonadRefs (Intro Symbol Symbol) m) =>
                    Intro Symbol Symbol
                 -- ^ The term being checked.
                 -> Intro Symbol Symbol
                 -- ^ The type against which it's being checked.
                 -> m ()
checkLiteralRule term @ Literal { literalVal = lit } ty =
  do
    -- Choose the supertype based on the literal value.
    litty <- case lit of
      -- For numbers, we need to look at their value to figure out
      -- what type to use.
      Num { numVal = val }
          -- If the denominator isn't 1, then it's a rational
        | denominator val /= 1 -> getRationalSuperRef
          -- If the denominator is 1, and it's negative, it's an integer
        | val < 0 -> getIntegerSuperRef
          -- Otherwise, it's a natural
        | otherwise -> getNaturalSuperRef
      -- The rest of these are straightforward
      Char {} -> getCharSuperRef
      Str {} -> getStrSuperRef

    -- Check that the expected type is a subtype of the most general
    -- type for this literal.
    checkSubtype term ty litty
    -- XXX literals need to have some way of registering their actual
    -- type.  Likely solution: they have a type variable, and we emit
    -- an equality relationship
checkLiteralRule term _ =
  internalError "Improper use of checkLiteral rule" [position term]

-- >  E |- Sigma(ubinds) :> Sigma(sbinds)   E |- sretty :> uretty
-- > -------------------------------------------------------------
-- >     E |- term : Pi(sbinds, sretty) :> Pi(ubinds, uretty)
checkSubtypeFuncTypesRule :: (MonadMessages Message m, MonadTypeCheck m,
                              MonadRefs (Intro Symbol Symbol) m) =>
                             Intro Symbol Symbol
                          -- ^ The subtype term.
                          -> Intro Symbol Symbol
                          -- ^ The supposed subtype.
                          -> Intro Symbol Symbol
                          -- ^ The supposed supertype
                          -> m ()
checkSubtypeFuncTypesRule term FuncType { funcTypeArgs = subargs,
                                          funcTypeRetTy = subretty,
                                          funcTypePos = subpos }
                          FuncType { funcTypeArgs = superargs,
                                     funcTypeRetTy = superretty,
                                     funcTypePos = superpos } =
  let
    subrecty = RecordType { recTypeBody = subargs, recTypePos = subpos }
    superrecty = RecordType { recTypeBody = superargs, recTypePos = superpos }
  in do
    -- XXX We need to somehow get ahold of values for the arguments as
    -- well as for the return types

    -- Contravariant in the arguments
    checkSubtype _ superrecty subrecty
    -- Covariant in the return type
    checkSubtype _ subretty superretty

-- | Check that a record subtype has all the fields in its supertype,
-- and that the subtyping relationship holds for corresponding fields.
--
-- Corresponds to the following type rule:
--
-- >                    {uname_j} subset {sname_i}
-- >      E |- [term.sname_k : sty_k :> uty_l | sname_k = uname_l ]
-- > ----------------------------------------------------------------
-- >  E |- term : Sigma([sname_i : sty_i]) :> Sigma([uname_j : uty_j])
checkSubtypeRecordTypesRule :: (MonadMessages Message m, MonadTypeCheck m,
                                MonadRefs (Intro Symbol Symbol) m) =>
                               Intro Symbol Symbol
                            -- ^ The subtype term.
                            -> Intro Symbol Symbol
                            -- ^ The supposed subtype.
                            -> Intro Symbol Symbol
                            -- ^ The supposed supertype
                            -> m ()
checkSubtypeRecordTypesRule term subrecty @ RecordType { recTypeBody = subbody }
                            RecordType { recTypeBody = superbody } =
  let
    mapfun (fname @ FieldName { fieldSym = sym },
            Element { elemType = superty }) =
      -- Every field in the supertype must be present in the subtype,
      -- and the subtyping relation must hold
      case HashMap.lookup fname subbody of
        Just subty ->
          do
            -- XXX We do need to rip out the pattern bindings for the
            -- fields as well
            fieldterm <- getField term subrecty fname (position term)
            checkSubtype fieldterm subty superty
            return Nothing
        Nothing -> return (Just sym)
  in do
    -- Map over all fields in the supertype
    res <- mapM mapfun (HashMap.toList superbody)
    -- Report any missing fields
    case catMaybes res of
      [] -> return ()
      missing -> missingFields missing [position term]

-- | Check that a suspected subtype's rank is less than or equal to
-- the suspected supertype's
--
-- Corresponds to the following type rule:
--
-- >              m <= n
-- > ----------------------------------
-- >  E |- (term : type(m)) :> type(n)
checkSubtypeTypesRule :: (MonadMessages Message m, MonadTypeCheck m,
                          MonadRefs (Intro Symbol Symbol) m) =>
                         Intro Symbol Symbol
                      -- ^ The supposed subtype.
                      -> Intro Symbol Symbol
                      -- ^ The supposed supertype
                      -> Position
                      -- ^ The position at which the subtype check occurs
                      -> m ()
checkSubtypeTypesRule Type { typeRank = m } Type { typeRank = n } _ =
  rankLessEqual m n
checkSubtypeTypesRule _ _ pos =
  internalError "Improper use of checkSubtypeTypes rule" [pos]

-- XXX This one isn't done
checkSubtypeRefineTypesRule :: (MonadMessages Message m, MonadTypeCheck m,
                                MonadRefs (Intro Symbol Symbol) m) =>
                               Intro Symbol Symbol
                            -- ^ The subtype term.
                            -> Intro Symbol Symbol
                            -- ^ The supposed subtype.
                            -> Intro Symbol Symbol
                            -- ^ The supposed supertype
                            -> m ()
checkSubtypeRefineTypesRule term RefineType { refineType = subinner,
                                              refineCases = subcases }
                            RefineType { refineType = superinner,
                                         refineCases = supercases } =
  do
    -- Check that the inner types have the subtype relation
    checkSubtype term subinner superinner
    -- Assert that the subtype predicate implies the supertype predicate
    _

-- | Assert that prop is a subtype of itself.
--
-- Corresponds to the following type rules:
--
-- >
-- > ----------------------
-- >  |- (term : ty) :> ty
--
-- and:
--
-- >
-- > -----------------------------------
-- >  sty :> uty |- (term : sty) :> uty
checkSubtypePropsRule :: (MonadMessages Message m, MonadTypeCheck m,
                          MonadRefs (Intro Symbol Symbol) m) =>
                         Intro Symbol Symbol
                      -- ^ The supposed subtype.
                      -> Intro Symbol Symbol
                      -- ^ The supposed supertype
                      -> m ()
checkSubtypePropsRule subty superty
  | subty == superty = return ()
  | otherwise =
    internalError "Improper use of checkSubtypeProps rule" [position term]
