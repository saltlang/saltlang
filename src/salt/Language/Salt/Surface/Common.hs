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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
             DeriveTraversable, DeriveFoldable, DeriveFunctor #-}

-- | A module containing common structures for both AST and Syntax.
module Language.Salt.Surface.Common(
       Assoc(..),
       Fixity(..),
       Level(..),
       Prec(..),
       FieldName(..),
       BuilderKind(..),
       ContextKind(..),
       TruthKind(..),
       AbstractionKind(..),
       Visibility(..),
       Literal(..),
       BasicPosition(..),
       Position,
       literalDot,
       getNodeID,
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Control.Monad.State
import Data.Array
import Data.ByteString(ByteString)
import Data.Hashable
import Data.Hashable.Extras(Hashable1)
import Data.Ratio
import Data.Position.BasicPosition
import Data.PositionElement
import Data.Symbol
import Data.Word
import Language.Salt.Format
import Prelude hiding (concat)
import Prelude.Extras(Eq1, Ord1)
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

type Position = BasicPosition

-- | A newtype to discriminate field names from symbols.
newtype FieldName = FieldName { fieldSym :: Symbol }
  deriving (Ord, Eq)

-- | Associativity for syntax directives.
data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Ord, Eq, Enum, Show)

-- | Fixity for syntax directives.
data Fixity =
    Prefix
  | Infix {
      -- | The associativity of the infix operator.
      infixAssoc :: !Assoc
    }
  | Postfix
  deriving (Ord, Eq, Show)

-- | A precedence level
data Level refty =
    -- | The precedence level of another symbol.
    Level {
      levelRef :: !refty
    }
    -- | The default precedence level for prefix symbols.
  | DefaultPrefix {
      prefixPos :: !Position
    }
    -- | The default precedence level for infix symbols.
  | DefaultInfix {
      infixPos :: !Position
    }
    -- | The default precedence level for postfix symbols.
  | DefaultPostfix {
      postfixPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | A precedence relationship.
data Prec refty =
  Prec {
    -- | The kind of precedence relationship.
    precOrd :: !Ordering,
    -- | An expression denoting the symbol to which this relationship
    -- refers.
    precLevel :: !(Level refty),
    -- | The position in source from which this arises.
    precPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

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
  | Instance
    deriving (Ord, Eq, Enum)

-- | Context kind for definition.  This indicates the context to
-- which a given definition is relative.
data ContextKind =
    -- | Static definition.  These are relative only to the global
    -- context (meaning they can access other statically-defined
    -- definitions by name).
    Static
    -- | Local definitions.  These are relative to a given function
    -- body, and can access other local definitions in that function
    -- by name.
  | Local
    -- | Object definitions.  These are relative to a given object,
    -- and can access other definitions relative to that object by
    -- name.
  | Object
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
  | Axiom
    deriving (Ord, Eq, Enum)

data AbstractionKind =
    -- | A function abstraction.
    Lambda
    -- | A forall proposition.
  | Forall
    -- | An exists proposition.
  | Exists
    deriving (Ord, Eq, Enum)

-- | Visibilities.  Controls when accesses to a given element are legal.
data Visibility =
    -- | Hidden access.  Used internally to add synthetic elements.
    Hidden
    -- | Private access.  The given element may only be accessed
    -- within the scope in which it is defined.
  | Private
    -- | Protected access.  The given element may only be accessed
    -- within the scope in which it is defined, or in any derived
    -- scope derived from that one.
  | Protected
    -- | Public access.  The given element may be accessed anywhere.
  | Public
    deriving (Ord, Eq, Enum, Ix)

-- | A literal value.
data Literal =
    -- | A number literal.
    Num {
      -- | The number value.
      numVal :: !Rational,
      -- | The position in source from which this arises.
      numPos :: !Position
    }
    -- | A string literal.
  | Str {
      -- | The string value.
      strVal :: !ByteString,
      -- | The position in source from which this arises.
      strPos :: !Position
    }
    -- | A Character literal.
  | Char {
      -- | The character value.
      charVal :: !Char,
      -- | The position in source from which this arises.
      charPos :: !Position
    }
    -- | A unit value.
  | Unit {
      -- | The position in source from which this arises.
      unitPos :: !Position
    }

getNodeID :: Monad m => StateT Word m String
getNodeID =
  do
    nodeid <- get
    put $! nodeid + 1
    return ("node" ++ show nodeid)

instance PositionElement refty => PositionElement (Level refty) where
  position Level { levelRef = ref } = position ref
  position DefaultPrefix { prefixPos = pos } = pos
  position DefaultInfix { infixPos = pos } = pos
  position DefaultPostfix { postfixPos = pos } = pos

instance PositionElement (Prec expty) where
  position Prec { precPos = pos } = pos

instance PositionElement Literal where
  position Num { numPos = pos } = pos
  position Str { strPos = pos } = pos
  position Char { charPos = pos } = pos
  position Unit { unitPos = pos } = pos

instance Eq expty => Eq (Level expty) where
  Level { levelRef = ref1 } == Level { levelRef = ref2 } = ref1 == ref2
  DefaultPrefix {} == DefaultPrefix {} = True
  DefaultInfix {} == DefaultInfix {} = True
  DefaultPostfix {} == DefaultPostfix {} = True
  _ == _ = False

instance Eq expty => Eq (Prec expty) where
  Prec { precOrd = ord1, precLevel = ref1 } ==
    Prec { precOrd = ord2, precLevel = ref2 } =
      ord1 == ord2 && ref1 == ref2

instance Eq1 Level
instance Eq1 Prec

instance Ord expty => Ord (Level expty) where
  compare Level { levelRef = ref1 } Level { levelRef = ref2 } =
    compare ref1 ref2
  compare Level {} _ = LT
  compare _ Level {} = GT
  compare DefaultPrefix {} DefaultPrefix {} = EQ
  compare DefaultPrefix {} _ = LT
  compare _ DefaultPrefix {} = GT
  compare DefaultInfix {} DefaultInfix {} = EQ
  compare DefaultInfix {} _ = LT
  compare _ DefaultInfix {} = GT
  compare DefaultPostfix {} DefaultPostfix {} = EQ

instance Ord expty => Ord (Prec expty) where
  compare Prec { precOrd = ord1, precLevel = ref1 }
          Prec { precOrd = ord2, precLevel = ref2 } =
    case compare ord1 ord2 of
      EQ -> compare ref1 ref2
      out -> out

instance Ord1 Level
instance Ord1 Prec

literalDot :: Monad m => Literal -> StateT Word m (Doc, String)
literalDot Num { numVal = num } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Num | " <>
                               string (show num)) <!>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Str { strVal = str } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Str | " <>
                               string "\\\"" <> bytestring str <>
                               string "\\\"") <!>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Char { charVal = chr } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Char | " <>
                               squoted (char chr)) <!>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Unit {} =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <> dquoted (string "Unit") <!>
                      string "shape = \"record\"") <> char ';', nodeid)

instance Hashable Assoc where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Fixity where
  hashWithSalt s Prefix = hashWithSalt s (0 :: Int)
  hashWithSalt s (Infix assoc) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` assoc
  hashWithSalt s Postfix = hashWithSalt s (2 :: Int)

instance Hashable refty => Hashable (Level refty) where
  hashWithSalt s Level { levelRef = ref } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` ref
  hashWithSalt s DefaultPrefix {} = s `hashWithSalt` (1 :: Int)
  hashWithSalt s DefaultInfix {} = s `hashWithSalt` (2 :: Int)
  hashWithSalt s DefaultPostfix {} = s `hashWithSalt` (3 :: Int)

instance Hashable expty => Hashable (Prec expty) where
  hashWithSalt s Prec { precOrd = ord, precLevel = ref } =
    s `hashWithSalt` ord `hashWithSalt` ref

instance Hashable1 Level
instance Hashable1 Prec

instance Hashable FieldName where
  hashWithSalt s FieldName { fieldSym = sym } = s `hashWithSalt` sym

instance MonadSymbols m => FormatM m FieldName where
  formatM = formatM . fieldSym

instance Format Assoc where format = string . show
instance Format Fixity where format = string . show

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] FieldName where
  xpickle = xpWrap (FieldName, fieldSym) xpickle

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [(tag, text)] FieldName where
  xpickle = xpWrap (FieldName, fieldSym) xpickle

instance Eq Literal where
  Num { numVal = num1 } == Num { numVal = num2 } = num1 == num2
  Str { strVal = str1 } == Str { strVal = str2 } = str1 == str2
  Char { charVal = chr1 } == Char { charVal = chr2 } = chr1 == chr2
  Unit {} == Unit {} = True
  _ == _ = False

instance Ord Literal where
  compare Num { numVal = num1 } Num { numVal = num2 } = compare num1 num2
  compare Num {} _ = GT
  compare _ Num {} = LT
  compare Str { strVal = str1 } Str { strVal = str2 } = compare str1 str2
  compare Str {} _ = GT
  compare _ Str {} = LT
  compare Char { charVal = chr1 } Char { charVal = chr2 } = compare chr1 chr2
  compare Char {} _ = GT
  compare _ Char {} = LT
  compare Unit {} Unit {} = EQ

instance Hashable Literal where
  hashWithSalt s Num { numVal = num } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` num
  hashWithSalt s Str { strVal = str } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` str
  hashWithSalt s Char { charVal = chr } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` chr
  hashWithSalt s Unit {} = s `hashWithSalt` (4 :: Int)

instance MonadPositions m => FormatM m Literal where
  formatM Num { numVal = num, numPos = pos } =
    do
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Num")
                             [(string "val", string (show num)),
                              (string "pos", posdoc)])
  formatM Str { strVal = str, strPos = pos } =
    do
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Str")
                             [(string "val", bytestring str),
                              (string "pos", posdoc)])
  formatM Char { charVal = chr, charPos = pos } =
    do
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Char")
                             [(string "val", char chr),
                              (string "pos", posdoc)])
  formatM Unit { unitPos = pos } =
    do
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Unit") [(string "pos", posdoc)])

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Level expty) where
  formatM Level { levelRef = ref } = formatM ref
  formatM DefaultPrefix {} = return $! string "default prefix"
  formatM DefaultInfix {} = return $! string "default infix"
  formatM DefaultPostfix {} = return $! string "default postfix"

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Prec expty) where
  formatM Prec { precOrd = ord, precLevel = ref, precPos = pos } =
    let
      orddoc = case ord of
        LT -> string "<"
        EQ -> string "="
        GT -> string ">"
    in do
      expdoc <- formatM ref
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Prec")
                               [(string "pos", posdoc),
                                (string "ord", orddoc),
                                (string "exp", expdoc)])

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
numPickler =
  let
    revfunc Num { numVal = num, numPos = pos } =
      ((numerator num, denominator num), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((numer, denom), pos) -> Num { numVal = numer % denom,
                                            numPos = pos }, revfunc)
           (xpElem (gxFromString "Num")
                   (xpPair (xpAttr (gxFromString "numerator") xpPrim)
                           (xpAttr (gxFromString "denominator") xpPrim))
                   (xpElemNodes (gxFromString "pos") xpickle))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
strPickler =
  let
    revfunc Str { strVal = str, strPos = pos } = (gxFromByteString str, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(str, pos) -> Str { strVal = gxToByteString str,
                                 strPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Str")
                        (xpPair (xpElemNodes (gxFromString "value")
                                             (xpContent xpText0))
                                (xpElemNodes (gxFromString "pos") xpickle)))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
charPickler =
  let
    revfunc Char { charVal = chr, charPos = pos } = (chr, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(chr, pos) -> Char { charVal = chr, charPos = pos }, revfunc)
           (xpElem (gxFromString "Char")
                   (xpAttr (gxFromString "value") xpPrim)
                   (xpElemNodes (gxFromString "pos") xpickle))

unitPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
unitPickler = xpWrap (Unit, unitPos)
                     (xpElemNodes (gxFromString "Unit")
                                  (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Literal where
  xpickle =
    let
      picker Num {} = 0
      picker Str {} = 1
      picker Char {} = 2
      picker Unit {} = 3
    in
      xpAlt picker [numPickler, strPickler, charPickler, unitPickler]

levelPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] expty) =>
                PU [NodeG [] tag text] (Level expty)
levelPickler = xpWrap (Level, levelRef)
                      (xpElemNodes (gxFromString "Level")
                                   (xpElemNodes (gxFromString "ref") xpickle))

defaultPrefixPickler :: (GenericXMLString tag, Show tag,
                         GenericXMLString text, Show text,
                         XmlPickler [NodeG [] tag text] expty) =>
                        PU [NodeG [] tag text] (Level expty)
defaultPrefixPickler =
  let
    revfunc DefaultPrefix { prefixPos = pos } = ((), pos)
    revfunc _ = error "Can't convert"
  in
    xpWrap (\((), pos) -> DefaultPrefix { prefixPos = pos }, revfunc)
           (xpElem (gxFromString "Default")
                   (xpAttrFixed (gxFromString "fixity") (gxFromString "prefix"))
                   (xpElemNodes (gxFromString "pos") xpickle))

defaultInfixPickler :: (GenericXMLString tag, Show tag,
                        GenericXMLString text, Show text,
                        XmlPickler [NodeG [] tag text] expty) =>
                       PU [NodeG [] tag text] (Level expty)
defaultInfixPickler =
  let
    revfunc DefaultInfix { infixPos = pos } = ((), pos)
    revfunc _ = error "Can't convert"
  in
    xpWrap (\((), pos) -> DefaultInfix { infixPos = pos }, revfunc)
           (xpElem (gxFromString "Default")
                   (xpAttrFixed (gxFromString "fixity") (gxFromString "infix"))
                   (xpElemNodes (gxFromString "pos") xpickle))

defaultPostfixPickler :: (GenericXMLString tag, Show tag,
                          GenericXMLString text, Show text,
                          XmlPickler [NodeG [] tag text] expty) =>
                        PU [NodeG [] tag text] (Level expty)
defaultPostfixPickler =
  let
    revfunc DefaultPostfix { postfixPos = pos } = ((), pos)
    revfunc _ = error "Can't convert"
  in
    xpWrap (\((), pos) -> DefaultPostfix { postfixPos = pos }, revfunc)
           (xpElem (gxFromString "Default")
                   (xpAttrFixed (gxFromString "fixity")
                                (gxFromString "postfix"))
                   (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Level expty) where
  xpickle =
    let
      picker Level {} = 0
      picker DefaultPrefix {} = 1
      picker DefaultInfix {} = 2
      picker DefaultPostfix {} = 3
    in
      xpAlt picker [levelPickler, defaultPrefixPickler, defaultInfixPickler,
                    defaultPostfixPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Prec expty) where
  xpickle = xpWrap (\(ord, (ref, pos)) ->
                     Prec { precOrd = ord, precLevel = ref, precPos = pos },
                    \Prec { precOrd = ord, precLevel = ref, precPos = pos } ->
                    (ord, (ref, pos)))
                   (xpElem (gxFromString "Prec")
                           (xpAttr (gxFromString "order") xpPrim)
                           (xpPair (xpElemNodes (gxFromString "level") xpickle)
                                   (xpElemNodes (gxFromString "pos") xpickle)))

instance Hashable BuilderKind where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable ContextKind where
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
                                       (gxFromString "Typeclass")),
                   xpWrap (const Instance, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Instance"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) ContextKind where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Static, const ())
                          (xpAttrFixed (gxFromString "context")
                                       (gxFromString "static")),
                   xpWrap (const Local, const ())
                          (xpAttrFixed (gxFromString "context")
                                       (gxFromString "local")),
                   xpWrap (const Object, const ())
                          (xpAttrFixed (gxFromString "context")
                                       (gxFromString "object"))]

instance Show BuilderKind where
  show Signature = "signature"
  show Interface = "interface"
  show Module = "module"
  show Class = "class"
  show Typeclass = "typeclass"
  show Instance = "instance"

instance Show ContextKind where
  show Static = "static"
  show Local = "local"
  show Object = "object"

instance Format BuilderKind where format = string . show

instance Format ContextKind where format = string . show

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
                                       (gxFromString "Invariant")),
                   xpWrap (const Axiom, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Axiom"))]

instance Show TruthKind where
  show Theorem = "theorem"
  show Invariant = "invariant"
  show Axiom = "axiom"

instance Format TruthKind where format = string . show

instance Hashable Visibility where
  hashWithSalt s = hashWithSalt s . fromEnum

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Visibility where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Hidden, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Hidden")),
                   xpWrap (const Private, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Private")),
                   xpWrap (const Protected, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Protected")),
                   xpWrap (const Public, const ())
                          (xpAttrFixed (gxFromString "visibility")
                                       (gxFromString "Public"))]

instance Show Visibility where
  show Hidden = "hidden"
  show Private = "private"
  show Protected = "protected"
  show Public = "public"

instance Format Visibility where format = string . show

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) AbstractionKind where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Lambda, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Lambda")),
                   xpWrap (const Forall, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Forall")),
                   xpWrap (const Exists, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Exists"))]

instance Show AbstractionKind where
  show Lambda = "lambda"
  show Forall = "forall"
  show Exists = "exists"

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Assoc where
  xpickle = xpAlt fromEnum
                  [xpWrap (const LeftAssoc, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "Left")),
                   xpWrap (const RightAssoc, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "Right")),
                   xpWrap (const NonAssoc, const ())
                          (xpAttrFixed (gxFromString "assoc")
                                       (gxFromString "NonAssoc"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Fixity where
  xpickle =
    let
      picker Prefix = 0
      picker (Infix _) = 1
      picker Postfix = 2

      unpackInfix (Infix a) = ((), Just a)
      unpackInfix _ = error "Can't unpack"

      packInfix ((), Just a) = Infix a
      packInfix _ = error "Need associativity for infix"
    in
      xpAlt picker [xpWrap (const Prefix, const ((), Nothing))
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Prefix"))
                                   (xpOption xpZero)),
                    xpWrap (packInfix, unpackInfix)
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Infix"))
                                   (xpOption xpickle)),
                    xpWrap (const Postfix, const ((), Nothing))
                           (xpPair (xpAttrFixed (gxFromString "fixity")
                                                (gxFromString "Postfix"))
                                   (xpOption xpZero))]

instance Format AbstractionKind where format = string . show
