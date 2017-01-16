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

module Language.Salt.Surface.Elaborate(
       elaborate
       ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.HashTable.IO(BasicHashTable)
import Data.ScopeID
import Language.Salt.Surface.Syntax

import qualified Data.Array as Array
import qualified Data.HashTable.IO as HashTable

data ElaborateData =
  ElaborateData {
    elaborateScopes :: !(BasicHashTable ScopeID (Elaborated (Exp Apply Ref)))
  }

data ElaborateState =
  ElaborateState {
    elaborateScopeID :: !ScopeID
  }

type ElaborateT m = ReaderT ElaborateData (StateT ElaborateState m)

elaborate :: Monad m =>
             Surface (Resolved (Exp Apply Ref))
          -> m (Surface (Elaborated (Exp Apply Ref)))
elaborate surface @ Surface { surfaceScopes = scopes } =
  let
    elaborateScope :: Monad m =>
                      Resolved (Exp Apply Ref)
                   -> ElaborateT m (Elaborated (Exp Apply Ref))
    elaborateScope = _

    (arrstart, arrend) = Array.bounds scopes
    sizehint = (fromEnum arrend) - (fromEnum arrstart)

    reader scopetab = runReaderT (mapM_ elaborateScope scopes)
                                 ElaborateData { elaborateScopes = scopetab }
  in do
    scopetab <- HashTable.newSized sizehint
    ((), ElaborateState { elaborateScopeID = newarrend }) <-
      runStateT (reader scopetab) ElaborateState { elaborateScopeID = arrend }
    newscopes <- HashTable.toList scopetab
    return surface { surfaceScopes = Array.array (arrstart, newarrend)
                                                 newscopes }
