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
module Language.Argent.Surface.Common(
       ScopeClass(..)
       ) where

import Data.Hash

-- | Scope classes.  These define the exact semantics of a scoped
-- entity declaration, which defines the common structure for all of
-- these.
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

instance Hashable ScopeClass where
  hash Module = hashInt 1
  hash Signature = hashInt 2
  hash Class = hashInt 3

instance Show ScopeClass where
  show Module = "module"
  show Signature = "signature"
  show Class = "class"