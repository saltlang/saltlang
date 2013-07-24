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

-- | A module containing common structures for both AST and Syntax.
module Language.Salt.Surface.Common(
       ScopeClass(..),
       TruthClass(..),
       AliasClass(..),
       Visibility(..)
       ) where

import Data.Hashable
import Data.Word
import Test.QuickCheck

-- | Scope classes.  These define the exact semantics of a scoped
-- entity declaration.
data ScopeClass =
    -- | Modules are similar to SML modules.  Unlike SML, a module may
    -- be used at runtime.  Modules declare a named instance of an
    -- anonymous record type
    -- 
    -- A module declaration is tantamount to declaring a structure
    -- type anonymously, followed by a single instance of the type,
    -- then initializing the instance.
    -- 
    -- Put in terms of classes, modules are like declaring a class
    -- anonymously, then instantiating it.
    Module
    -- | Signatures are similar to SML signatures, or Fortress traits.
    -- A signature declares a type, which may not be directly
    -- instantiated, and whose mutable fields are not automatically
    -- inherited by scopes which declare the class as a supertype.
    -- Multiple inheritence of signatures is allowed.
  | Signature
    -- | Classes are similar to Java classes.  A Class defines a type
    -- which may be directly instantiated, and whose mutable fields
    -- are automatically inherited by scopes which declare the class
    -- as a supertype.  Multiple inheritence of classes is not
    -- allowed.
  | Class
    deriving (Ord, Eq)

-- | Truth classes.  These define the exact semantics of a truth
-- declaration.
data TruthClass =
    -- | Theorems are propositions that are added to the proof
    -- environments of every proof obligation generated from within
    -- this scope.  Abstract declarations of theorems do not need to
    -- be proven; however, concrete theorems will generate a proof
    -- obligation.
    Theorem
    -- | Invariants are propositions that are added automatically to
    -- the pre- and post-conditions of every state transition in the
    -- current scope.  They do not need to be proven.
  | Invariant

-- | Alias classes.  These define the exact semantics of an alias.
data AliasClass =
    -- | Imports add an alias to a declaration in another scope to the
    -- current scope.
    Import
    -- | Open aliases (from "import Module.*") are the same as
    -- importing all entities defined in given scope.  They cannot be
    -- qualified.
  | Open
    -- | Exports alter the visibility and possibly the name of a
    -- declaration.  By default, they make a given declaration public.
  | Export
    deriving (Ord, Eq)

-- | Visibilities.  Controls when accesses to a given element are legal.
data Visibility =
    -- | Private access.  The given element may only be accessed
    -- within the scope in which it is defined.
    Private
    -- | Protected access.  The given element may only be accessed
    -- within the scope in which it is defined, or in any derived
    -- scope derived from that one.
  | Protected
    -- | Public access.  The given element may be accessed anywhere.
  | Public
    deriving (Ord, Eq)

instance Hashable ScopeClass where
  hashWithSalt s Module = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Signature = s `hashWithSalt` (2 :: Word)
  hashWithSalt s Class = s `hashWithSalt` (3 :: Word)

instance Hashable TruthClass where
  hashWithSalt s Theorem = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Invariant = s `hashWithSalt` (2 :: Word)

instance Hashable AliasClass where
  hashWithSalt s Import = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Open = s `hashWithSalt` (2 :: Word)
  hashWithSalt s Export = s `hashWithSalt` (3 :: Word)

instance Hashable Visibility where
  hashWithSalt s Private = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Protected = s `hashWithSalt` (2 :: Word)
  hashWithSalt s Public = s `hashWithSalt` (3 :: Word)

instance Show ScopeClass where
  show Module = "module"
  show Signature = "signature"
  show Class = "class"

instance Show TruthClass where
  show Theorem = "theorem"
  show Invariant = "invariant"

instance Show Visibility where
  show Private = "private"
  show Protected = "protected"
  show Public = "public"

instance Arbitrary ScopeClass where
  arbitrary = elements [ Module, Signature, Class ]

instance Arbitrary TruthClass where
  arbitrary = elements [ Theorem, Invariant ]

instance Arbitrary AliasClass where
  arbitrary = elements [ Import, Open, Export ]

instance Arbitrary Visibility where
  arbitrary = elements [ Private, Protected, Public ]
