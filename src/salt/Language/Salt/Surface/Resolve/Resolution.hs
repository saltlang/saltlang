-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
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

module Language.Salt.Surface.Resolve.Resolution(
       PathElem(..),
       ResolvedScope(..),
       NonLocal(..),
       Resolution(..)
       ) where

import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List(sort)
import Data.Semigroup
import Data.ScopeID
import Data.Symbol

import qualified Data.HashMap.Strict as HashMap

-- | Resolution path element.  A sequence of these describe how an
-- indirect resolution is resolved.
data PathElem =
    -- | A definition imported from another scope.
    Imported {
      -- | The set of scopes from which this definition is imported.
      -- Non-singleton maps denate ambiguity.
      importedScope :: !ScopeID
    }
    -- | An inherited definition.
  | Inherited {
      -- | The set of scopes from which the definition is inherited.
      inheritedScope :: !ScopeID
    }
    -- | A definition in an enclosing scope.
  | Enclosing {
      -- | Enclosing scope from which this was captured.
      enclosingScope :: !ScopeID,
      -- | Depth of enclosing scope capture.
      enclosingDepth :: !Word
    }
    deriving (Eq, Ord)

data ResolvedScope expty =
  ResolvedScope {
    resolvedScopeID :: !ScopeID,
    resolvedScopeDeps :: !(HashMap (ScopeID, expty) ScopeID)
  }
  deriving (Eq)

-- | Non-Local resolution result.  This is separate, because we only
-- store non-local resolutions in the dependence set.
data NonLocal expty =
  NonLocal {
    -- | The scope in which the resolved definition is defined.
    nonLocalScope :: !ScopeID,
    -- | The name of the resolved definition in its defining scope.
    nonLocalSym :: !Symbol,
    -- | The source of the non-local resolution in the local scope
    -- (this is also teh first element of the full resolution path).
    nonLocalSrc :: !PathElem,
    -- | The rest of the resolution path by which this result was
    -- derived.
    nonLocalTail :: ![PathElem],
    -- | All resolution results on which this resolution depends.

    -- XXX This may be duplicated by the deps field in ResolvedScope
    nonLocalDeps :: !(HashMap (ScopeID, expty) ScopeID),
    -- | If this resolution points to a builder and we can resolve
    -- the scope, it is stored here.
    nonLocalResolvedScope :: !(Maybe (ResolvedScope expty))
  }
  deriving (Eq)


data Resolution expty =
    -- | Locally-defined symbol.  This is always valid, so no need for
    -- validity.
    Direct {
      -- | If this resolution points to a builder and we can resolve
      -- the scope, it is stored here.
      directResolvedScope :: !(Maybe (ResolvedScope expty))
    }
    -- | Resolution from a different scope.
  | Indirect {
      -- | Resolution data.
      indirectSrc :: !(NonLocal expty)
    }
    deriving (Eq, Ord)

instance Ord expty => Ord (NonLocal expty) where
  compare NonLocal { nonLocalScope = scopeid1, nonLocalSym = sym1,
                     nonLocalSrc = src1, nonLocalTail = tail1,
                     nonLocalDeps = deps1 }
          NonLocal { nonLocalScope = scopeid2, nonLocalSym = sym2,
                     nonLocalSrc = src2, nonLocalTail = tail2,
                     nonLocalDeps = deps2 } =
    let
      sorteddeps1 = sort (HashMap.toList deps1)
      sorteddeps2 = sort (HashMap.toList deps2)
    in case compare scopeid1 scopeid2 of
      EQ -> case compare sym1 sym2 of
        EQ -> case compare src1 src2 of
          EQ -> case compare tail1 tail2 of
            EQ -> compare sorteddeps1 sorteddeps2
            out -> out
          out -> out
        out -> out
      out -> out

instance Ord expty => Ord (ResolvedScope expty) where
  compare ResolvedScope { resolvedScopeID = scopeid1,
                          resolvedScopeDeps = deps1 }
          ResolvedScope { resolvedScopeID = scopeid2,
                          resolvedScopeDeps = deps2 } =
    let
      sorteddeps1 = sort (HashMap.toList deps1)
      sorteddeps2 = sort (HashMap.toList deps2)
    in
      case compare scopeid1 scopeid2 of
        EQ -> compare sorteddeps1 sorteddeps2
        out -> out

instance Hashable PathElem where
  hashWithSalt s Imported { importedScope = scope } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` scope
  hashWithSalt s Inherited { inheritedScope = scope } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` scope
  hashWithSalt s Enclosing { enclosingScope = scope, enclosingDepth = depth } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` scope `hashWithSalt` depth

instance (Ord expty, Hashable expty) => Hashable (ResolvedScope expty) where
  hashWithSalt s ResolvedScope { resolvedScopeID = scopeid,
                                 resolvedScopeDeps = deps } =
    let
      sorteddeps = sort (HashMap.toList deps)
    in
      s `hashWithSalt` scopeid `hashWithSalt` sorteddeps

instance Hashable expty => Hashable (NonLocal expty) where
  hashWithSalt s NonLocal { nonLocalScope = scopeid, nonLocalSym = sym,
                            nonLocalSrc = src, nonLocalTail = pathtail,
                            nonLocalDeps = deps } =
    s `hashWithSalt` scopeid `hashWithSalt` sym `hashWithSalt`
    src `hashWithSalt` pathtail `hashWithSalt` deps

instance (Ord expty, Hashable expty) => Hashable (Resolution expty) where
  hashWithSalt s Direct { directResolvedScope = scope} =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` scope
  hashWithSalt s Indirect { indirectSrc = res } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` res

instance Semigroup (NonLocal expty) where
  out @ NonLocal { nonLocalSrc = Imported {} } <> _ = out
  _ <> out @ NonLocal { nonLocalSrc = Imported {} } = out
  out @ NonLocal { nonLocalSrc = Inherited {} } <> _ = out
  _ <> out @ NonLocal { nonLocalSrc = Inherited {} } = out
  out <> _ = out

instance Semigroup (Resolution expty) where
  out @ Direct {} <> _ = out
  _ <> out @ Direct {} = out
  Indirect { indirectSrc = res1 } <> Indirect { indirectSrc = res2 } =
    Indirect { indirectSrc = res1 <> res2 }
