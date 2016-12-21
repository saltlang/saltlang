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
       emptyPattern,
       funcType,
       matchRecordFields,
       matchTupleFields
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
  in do
    argname <- getArgField
    -- Construct a synthetic function type
    return FuncType { funcTypeArgs = [Element { elemName = argname,
                                                elemPat = emptyPattern pos,
                                                elemTupleIdx = 1,
                                                elemType = argscope,
                                                elemPos = pos }],
                      funcTypeRetTy = retscope, funcTypePos = pos }

-- | Match up record type elements with their fields in the record
-- value.  Missing fields will be reported, and 'BadIntro's will be
-- substituted for their values.
matchRecordFields :: (MonadMessages Message m, MonadSymbols m,
                      MonadTypeCheck m) =>
                     Position
                  -- ^ Position of the record value (used for error reporting).
                  -> [Element Symbol Symbol]
                  -- ^ Record type elements.
                  -> HashMap FieldName (Field (Intro Symbol Symbol))
                  -- ^ The fields of the record value.
                  -> m [(Element Symbol Symbol, Intro Symbol Symbol)]
                  -- ^ The matched elements with field values.
matchRecordFields pos elems fieldmap =
  let
    mapfun elem @ Element { elemName = fname } =
      case HashMap.lookup fname fieldmap of
        Just Field { fieldVal = field } -> ((elem, field), Nothing)
        Nothing -> ((elem, BadIntro { badIntroPos = pos }), Just fname)

    (binds, maybeerrs) = unzip (map mapfun elems)
  in
    case catMaybes maybeerrs of
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
                 -- ^ Record type elements.
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
