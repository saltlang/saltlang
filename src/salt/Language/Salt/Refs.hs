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

module Language.Salt.Refs(
       Refs(..),
       refStrs,
       refSyms
       ) where

import Control.Monad.Gensym
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Symbol

import qualified Data.ByteString.UTF8 as Strict

-- | Structure holding references to definitions that are referenced
-- by the compiler.
data Refs compty refty =
  Refs {
    -- | Name of the component "salt".
    refSaltComponent :: !compty,
    -- | Name of the compose function.
    refCompose :: !refty,
    -- | Name of the unit type and value.
    refUnit :: !refty
  }

saltComponentName :: [Strict.ByteString]
saltComponentName = [Strict.fromString "salt"]

composeName :: Strict.ByteString
composeName = Strict.fromString "compose"

unitName :: Strict.ByteString
unitName = Strict.fromString "unit"

-- | The string names of everything referenced by the compiler.
refStrs :: Refs [Strict.ByteString] Strict.ByteString
refStrs = Refs { refSaltComponent = saltComponentName,
                 refCompose = composeName,
                 refUnit = unitName }

-- | The symbol names of everything referenced by the compiler.
refSyms :: (MonadGensym m) =>
           m (Refs [Symbol] Symbol)
refSyms = bimapM (mapM symbol) symbol refStrs

instance Bifunctor Refs where
  bimap = bimapDefault

instance Bifoldable Refs where
  bifoldMap = bifoldMapDefault

instance Bitraversable Refs where
  bitraverse mapcomp mapref Refs { refSaltComponent = saltcomp,
                                   refCompose = compose,
                                   refUnit = unit } =
    let
      new newsaltcomp newcompose newunit =
        Refs { refSaltComponent = newsaltcomp,
               refCompose = newcompose,
               refUnit = newunit }
    in
      new <$> mapcomp saltcomp <*> mapref compose <*> mapref unit
