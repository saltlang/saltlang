-- Copyright (c) 2014 Eric McCorkle.
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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | A module containing common structures for both AST and Syntax.
module Language.Salt.Surface.Common(
       BuilderKind(..),
       TruthKind(..),
       AbstractionKind(..),
       AliasClass(..),
       Visibility(..)
       ) where

import Data.Array
import Data.Hashable
import Text.Format
import Text.XML.Expat.Pickle

-- | Scope classes.  These define the exact semantics of a scoped
-- entity declaration.
data BuilderKind =
    -- | Signatures are similar to SML signatures, or Fortress traits.
    -- A signature declares a type, which may not be directly
    -- instantiated, and whose mutable fields are not automatically
    -- inherited by scopes which declare the class as a supertype.
    -- Multiple inheritence of signatures is allowed.
    Signature
    -- | Interfaces are classes that are only allowed to contain
    -- function declarations and/or definitions.  They behave exactly
    -- like Java interfaces (as of JDK 8).
  | Interface
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
  | Module
    -- | Classes are similar to Java classes.  A Class defines a type
    -- which may be directly instantiated, and whose mutable fields
    -- are automatically inherited by scopes which declare the class
    -- as a supertype.  Multiple inheritence of classes is not
    -- allowed.
  | Class
  | Typeclass
    deriving (Ord, Eq, Enum)

-- | Truth classes.  These define the exact semantics of a truth
-- declaration.
data TruthKind =
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
    deriving (Ord, Eq, Enum)

data AbstractionKind =
    -- | A function abstraction.
    Lambda
    -- | A forall proposition.
  | Forall
    -- | An exists proposition.
  | Exists
    deriving (Ord, Eq, Enum)

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
    deriving (Ord, Eq, Enum)

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
    deriving (Ord, Eq, Enum, Ix)

instance Hashable BuilderKind where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable AbstractionKind where
  hashWithSalt s = hashWithSalt s . fromEnum

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) BuilderKind where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Signature, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Signature")),
                   xpWrap (const Interface, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Interface")),
                   xpWrap (const Module, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Module")),
                   xpWrap (const Class, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Class")),
                   xpWrap (const Typeclass, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Typeclass"))]

instance Show BuilderKind where
  show Signature = "signature"
  show Interface = "interface"
  show Module = "module"
  show Class = "class"
  show Typeclass = "typeclass"

instance Format BuilderKind where format = string . show

instance Hashable TruthKind where
  hashWithSalt s = hashWithSalt s . fromEnum

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) TruthKind where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Theorem, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Theorem")),
                   xpWrap (const Invariant, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Invariant"))]

instance Show TruthKind where
  show Theorem = "theorem"
  show Invariant = "invariant"

instance Format TruthKind where format = string . show

instance Hashable AliasClass where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Visibility where
  hashWithSalt s = hashWithSalt s . fromEnum

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Visibility where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Private, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Private")),
                   xpWrap (const Protected, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Protected")),
                   xpWrap (const Public, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Public"))]

instance Show Visibility where
  show Private = "private"
  show Protected = "protected"
  show Public = "public"

instance Format Visibility where format = string . show
