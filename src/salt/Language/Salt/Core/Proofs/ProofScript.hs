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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | A module implementing a data type for proof scripts.
module Language.Salt.Core.Proofs.ProofScript(
       ProofScriptElem(..),
       ProofScript,
       runScriptElem,
       runScript
       ) where

import Control.Monad.Proof.Class
import Data.Default
import Data.Hashable
import Data.Map(Map)
import Data.Pos
import Language.Salt.Core.Syntax

-- XXX Maybe encapsulate this more, make it just a monoid on the outside?
type ProofScript sym = [ProofScriptElem sym]

-- | An element in a proof script.  Each of these corresponds to one
-- of the five axioms for Salt's intuitionistic predicate logic.
data ProofScriptElem sym =
  -- |
  --   -------------
  --    Env, P |- P
    Assumption {
      -- | The name of the proposition in the truth environment.
      assumptionName :: !sym,
      -- | The position of this script element.
      assumptionPos :: !Pos
    }
  -- |   Env, P |- Q
  --   ---------------
  --    Env |- P -> Q
  | Intro {
      -- | The name under which to introduce the new proposition.
      introName :: !sym,
      -- | The position of this script element.
      introPos :: !Pos
    }
  -- |  Env, x_1 : T_1 ... x_n : T_n |- P
  --   -----------------------------------
  --     Env |- forall (pattern) : T. P
  | IntroVars {
      -- | A map sufficient to reconstruct the naming function.  Any
      -- name not in the map is mapped to itself.
      introVarsMaps :: [Map sym sym],
      -- | The position of this script element.
      introVarsPos :: !Pos
    }
  -- |  Env |- P -> Q    Env |- P
  --   ---------------------------
  --            Env |- Q
  | Cut {
      -- | The cut proposition (what must imply the current goal, and
      -- what the envirenment must prove).
      cutProp :: Term sym sym,
      -- | The position of this script element.
      cutPos :: !Pos
    }
  -- |  Env |- forall (pattern) : T. P   Env |- V : T
  --   -----------------------------------------------
  --                 Env |- [V/(pattern)]P
  | Apply {
      -- | The proposition to apply.
      applyProp :: Term sym sym,
      -- | The argument to the proposition.
      applyArgs :: [Term sym sym],
      -- | The position of this script element.
      applyPos :: !Pos
    }
  deriving (Show, Ord, Eq)

-- | Perform the action represented by a proof script element inside a
-- proof monad.
runScriptElem :: (Ord sym, MonadProof sym m) => ProofScriptElem sym -> m ()
runScriptElem Assumption { assumptionName = name, assumptionPos = p } =
  assumption p name
runScriptElem Intro { introName = name, introPos = p } = intro p name
runScriptElem IntroVars { introVarsMaps = namemaps, introVarsPos = p } =
    introVars p namemaps
runScriptElem Cut { cutProp = prop, cutPos = p } = cut p prop
runScriptElem Apply { applyProp = prop, applyArgs = args, applyPos = p } =
  apply p prop args

-- | Perform the actions represented by a proof script inside a proof monad.
runScript :: (Ord sym, MonadProof sym m) => ProofScript sym -> m ()
runScript = mapM_ runScriptElem

instance Position (ProofScriptElem sym) where
  pos Assumption { assumptionPos = p } = p
  pos Intro { introPos = p } = p
  pos IntroVars { introVarsPos = p } = p
  pos Cut { cutPos = p } = p
  pos Apply { applyPos = p } = p

instance (Default sym, Hashable sym) => Hashable (ProofScriptElem sym) where
  hashWithSalt s Assumption { assumptionName = name, assumptionPos = p } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name `hashWithSalt` p
  hashWithSalt s Intro { introName = name, introPos = p } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` name `hashWithSalt` p
  hashWithSalt s IntroVars { introVarsPos = p } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` p
  hashWithSalt s Cut { cutProp = prop, cutPos = p } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` prop `hashWithSalt` p
  hashWithSalt s Apply { applyProp = prop, applyArgs = args, applyPos = p } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt`
    prop `hashWithSalt` args `hashWithSalt` p
