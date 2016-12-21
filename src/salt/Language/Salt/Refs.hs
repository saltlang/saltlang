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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE DeriveTraversable, OverloadedStrings #-}

module Language.Salt.Refs(
       FieldNames(..),
       Components(..),
       Refs(..),
       fieldNames,
       componentStrs,
       componentSyms,
       refStrs,
       refSyms,
       ) where

import Control.Monad.Gensym
import Data.Symbol
import Language.Salt.Common

import qualified Data.ByteString.UTF8 as Strict

data Components ty =
  Components {
    -- | Name of the component "salt".
    componentSalt :: !ty
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- | Structure holding all the field names used by the compiler.
data FieldNames ty =
  FieldNames {
    -- | The field name "arg".
    fieldNameArg :: !ty,
    -- | The field name for receiver parameters (i.e. @this@).
    fieldNameReceiver :: !ty
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- | Structure holding references to definitions that are referenced
-- by the compiler.
data Refs ty =
  Refs {
    -- | The compose function.
    refCompose :: !ty,
    -- | The unit type and value.
    refUnit :: !ty,
    -- | The prop type.
    refProp :: !ty,
    -- | The type type function.
    refType :: !ty,
    -- | The supertype of all chars.
    refCharSuper :: !ty,
    -- | The supertype of all strings.
    refStrSuper :: !ty,
    -- | The supertype of all naturals.
    refNaturalSuper :: !ty,
    -- | The supertype of all integers.
    refIntegerSuper :: !ty,
    -- | The supertype of all rationals.
    refRationalSuper :: !ty,
    -- | The type of behavior specifications for computation types.
    refCompSpec :: !ty
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- | The string names of all components referenced by the compiler.
componentStrs :: Components [Strict.ByteString]
componentStrs = Components { componentSalt = ["salt"] }

-- | The string names of all field names used by the compiler.
fieldNameStrs :: FieldNames Strict.ByteString
fieldNameStrs = FieldNames { fieldNameArg = "arg",
                             fieldNameReceiver = "this" }

-- | The symbol names of all field names used by the compiler.
fieldNames :: (MonadGensym m) =>
              m (FieldNames FieldName)
fieldNames =
  let
    mapfun str =
      do
        sym <- symbol str
        return FieldName { fieldSym = sym }
  in
    mapM mapfun fieldNameStrs

-- | The string names of definitions referenced by the compiler.
refStrs :: Refs Strict.ByteString
refStrs = Refs { refCompose = "compose",
                 refUnit = "unit",
                 refProp = "prop",
                 refType = "type",
                 refCharSuper = "char",
                 refStrSuper = "string",
                 refNaturalSuper = "natural",
                 refIntegerSuper = "integer",
                 refRationalSuper = "rational",
                 refCompSpec = "proc" }

-- | The symbol names of all components referenced by the compiler.
componentSyms :: (MonadGensym m) =>
                 m (Components [Symbol])
componentSyms = mapM (mapM symbol) componentStrs

-- | The symbol names of everything referenced by the compiler.
refSyms :: (MonadGensym m) =>
           m (Refs Symbol)
refSyms = mapM symbol refStrs
