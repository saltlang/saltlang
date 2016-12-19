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

module Language.Salt.Core.TypeCheck.Env(
       Env,
       lookupSym,
       insertSym
       ) where

import Control.Monad.Messages
import Data.HashMap.Strict(HashMap)
import Data.Position.DWARFPosition
import Data.Symbol
import Prelude hiding (lookup)
import Language.Salt.Core.Syntax
import Language.Salt.Message

import qualified Data.HashMap.Strict as HashMap

-- | A type environment.
data Env =
  Env {
    -- | The types of regular symbols
    envTypes :: !(HashMap Symbol (Intro Symbol Symbol))
  }

-- | Lookup a symbol's type in the type environment.
lookupSym :: MonadMessages Message m =>
             Env
          -- ^ Type environment in which to lookup.
          -> Position
          -> Symbol
          -> m (Intro Symbol Symbol)
lookupSym Env { envTypes = tab } pos sym =
  case HashMap.lookup sym tab of
    Just out -> return out
    Nothing ->
      let
        msgpos = basicPosition pos
      in do
        internalError "Symbol not present in type environment" [msgpos]
        return BadIntro { badIntroPos = pos }

insertSym :: Env
          -- ^ The type environment into which to insert.
          -> Symbol
          -> Intro Symbol Symbol
          -> Env
insertSym e @ Env { envTypes = tab } sym ty =
  e { envTypes = HashMap.insert sym ty tab }
