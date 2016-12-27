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

module Language.Salt.Core.Syntax.Util(
--       getField,
       emptyPattern,
       funcType,
       matchRecordFields,
       matchTupleFields,
       convertTuple,
       convertArg,
       baseType
       ) where

import Bound
import Control.Monad
import Control.Monad.Messages
import Control.Monad.Refs.Class
import Control.Monad.Symbols
import Control.Monad.TypeCheck.Class
import Data.Array(Array, (!))
import Data.Default
import Data.HashMap.Strict(HashMap)
import Data.Maybe
import Data.Position.DWARFPosition
import Data.PositionElement
import Data.Symbol
import Language.Salt.Core.Syntax
import Language.Salt.Message
import Prelude hiding (elem)

import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HashMap
{-
getField :: MonadMessages Message m =>
            Intro bound free
         -- ^ The base term.
         -> Intro bound free
         -- ^ The term's type.
         -> FieldName
         -- ^ The field to get.
         -> Position
         -- ^ The position at which to report errors.
         -> m (Intro bound free)
         -- ^ A term representing the field.
-- Preserve badness
getField bad @ BadIntro {} _ _ _ = return bad
getField _ BadIntro {} _ pos = return BadIntro { badIntroPos = pos }
-- Extract the field from records
getField Record { recFields = fields } RecordType {} field pos =
  -- The field should be here
  case HashMap.lookup field fields of
    Just Field { fieldVal = out } -> return out
    Nothing ->
      do
        internalError "No such field in record" [basicPosition pos]
        return BadIntro { badIntroPos = pos }
-}
-- | Make an empty pattern that matches anything.
emptyPattern :: Default bound =>
                Position
             -- ^ The position for the pattern.
             -> Pattern bound
             -- ^ A pattern that matches anything.
emptyPattern pos = Name { nameSym = def, namePos = pos }

-- | Make a non-dependent function type
funcType :: MonadFieldNames m =>
            Intro Symbol Symbol
         -- ^ The argument type.
         -> Intro Symbol Symbol
         -- ^ The result type.
         -> m (Intro Symbol Symbol)
-- If the argument is a record, turn that directly into the args.
funcType RecordType { recTypeBody = args, recTypePos = pos } retty =
  let
    retscope = abstract (const Nothing) retty
  in
    return FuncType { funcTypeArgs = args, funcTypeRetTy = retscope,
                      funcTypePos = pos }
-- Otherwise, synthesize a single-arg function type.
funcType argty retty =
  let
    argscope = abstract (const Nothing) argty
    retscope = abstract (const Nothing) retty
    pos = debugPosition argty

    makeArgs =
      do
        argname <- getArgField
        return (HashMap.singleton argname Element { elemPat = emptyPattern pos,
                                                    elemTupleIdx = 1,
                                                    elemType = argscope,
                                                    elemPos = pos })
  in do
    args <- makeArgs
    -- Construct a synthetic function type
    return FuncType { funcTypeArgs = args, funcTypeRetTy = retscope,
                      funcTypePos = pos }

-- | Match up record type elements with their fields in the record
-- value.  Missing fields will be reported, and 'BadIntro's will be
-- substituted for their values.
matchRecordFields :: (MonadMessages Message m, MonadSymbols m,
                      MonadTypeCheck m) =>
                     Position
                  -- ^ Position of the record value (used for error reporting).
                  -> HashMap FieldName (Element Symbol Symbol)
                  -- ^ Record type elements.
                  -> HashMap FieldName (Field (Intro Symbol Symbol))
                  -- ^ The fields of the record value.
                  -> m [(Element Symbol Symbol, Intro Symbol Symbol)]
                  -- ^ The matched elements with field values.
matchRecordFields pos elems fieldmap =
  let
    mapfun (fname, elem) =
      case HashMap.lookup fname fieldmap of
        Just Field { fieldVal = field } -> ((elem, field), Nothing)
        Nothing -> ((elem, BadIntro { badIntroPos = pos }), Just fname)

    (binds, maybeerrs) = unzip (map mapfun (HashMap.toList elems))
  in case catMaybes maybeerrs of
    [] -> return binds
    fnames ->
      do
        missingFields (map fieldSym fnames) (basicPosition pos)
        return binds

-- | Match up a record type against tuple fields.
matchTupleFields :: (MonadMessages Message m, MonadSymbols m,
                     MonadTypeCheck m) =>
                    Position
                 -- ^ Position of the record value (used for error reporting).
                 -> [Element Symbol Symbol]

                 -> Array Word (Intro Symbol Symbol)
                 -- ^ The fields of the tuple value.
                 -> m [(Element Symbol Symbol, Intro Symbol Symbol)]
                 -- ^ The matched elements with field values.
matchTupleFields debugpos elems fields =
  let
    pos = basicPosition debugpos
    elemlen = fromIntegral (length elems - 1)
    (fieldstart, fieldlen) = Array.bounds fields

    mapfun elem @ Element { elemTupleIdx = idx }
      | idx + fieldstart <= fieldlen = (elem, fields ! (idx + fieldstart))
      | otherwise = (elem, BadIntro { badIntroPos = debugpos })
  in do
    -- The starting indexes should always be 0
    unless (fieldstart == 0) (internalError "Start index is not 0" [pos])
    -- Check that the tuple length matches the number of fields
    unless (elemlen == fieldlen) (tupleMismatch elemlen fieldlen pos)
    return (map mapfun elems)

-- | Convert a tuple into a record
convertTuple :: MonadMessages Message m =>
                HashMap FieldName (Element bound free)
             -- ^ The fields of the record type to which to convert.
             -> Array Word (Intro bound free)
             -- ^ The tuple fields.
             -> Position
             -- ^ The position at which the conversion takes place.
             -> m (Intro bound free)
             -- ^ The tuple converted into a record.
convertTuple recfields tuplefields pos =
  let
    msgpos = basicPosition pos
    reclen = fromIntegral (HashMap.size recfields)
    (fieldstart, fieldlen) = Array.bounds tuplefields

    mapfun Element { elemTupleIdx = idx }
      | idx + fieldstart <= fieldlen =
        let
          val = tuplefields ! (idx + fieldstart)
        in
          Field { fieldVal = val, fieldPos = debugPosition val }
      | otherwise = Field { fieldVal = BadIntro { badIntroPos = pos },
                            fieldPos = pos }
  in do
    -- The starting indexes should always be 0
    unless (fieldstart == 0) (internalError "Start index is not 0" [msgpos])
    -- Check that the tuple length matches the number of fields
    unless (reclen == fieldlen) (tupleMismatch reclen fieldlen msgpos)
    return Record { recFields = fmap mapfun recfields, recPos = pos }

-- | Convert an argument value into an argument based on the parameter types.
-- This will do different things based on the argument type:
--
-- * Record values are left alone
-- * Tuple values will be converted into records
-- * If there is a single parameter, and the argument is not a record
--   or a tuple, then it will be converted into a record having a single
--   field matching the parameter name.
-- * Otherwise, the argument will be left alone
convertArg :: MonadMessages Message m =>
              HashMap FieldName (Element bound free)
           -- ^ The parameters and types.
           -> Intro bound free
           -- ^ The argument value.
           -> m (Intro bound free)
           -- ^ The converted argument value.
-- If the argument is a record, then use it directly.
convertArg _ arg @ Record {} = return arg
-- If the argument is a tuple, then convert it into a record.
convertArg argtys Tuple { tupleFields = fields, tuplePos = pos } =
  convertTuple argtys fields pos
convertArg argtys arg =
  case HashMap.keys argtys of
    -- If there's a single argument that's not a record, and the
    -- parameter type has a single field, then convert the argument
    -- into a record of that type.
    [argname] ->
      let
        pos = debugPosition arg
        boxedargs = HashMap.singleton argname Field { fieldVal = arg,
                                                      fieldPos = pos }
      in
        return Record { recFields = boxedargs, recPos = pos }
    -- If there are multiple arguments, then don't auto-convert.
    _ -> return arg

baseType :: MonadMessages Message m =>
            Intro bound free
         -> Position
         -> m (Intro bound free)
-- Return raw types
baseType ty @ FuncType {} _ = return ty
baseType ty @ RecordType {} _ = return ty
baseType ty @ CompType {} _ = return ty
baseType ty @ PropType {} _ = return ty
baseType ty @ Type {} _ = return ty
-- Recurse on elim-typed cycles
baseType Elim { elimTerm = Typed { typedTerm = inner } } pos =
  baseType inner pos
-- Recurse on refinement types
baseType RefineType { refineType = inner } pos = baseType inner pos
-- These are error conditions
baseType Elim { elimTerm = Var {} } pos =
  do
    internalError "Shouldn't see Vars in normalized terms" [basicPosition pos]
    return BadIntro { badIntroPos = pos }
baseType _ pos =
  do
    internalError "Getting base type of non-type" [basicPosition pos]
    return BadIntro { badIntroPos = pos }
