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
       FieldName(..),
       BuilderKind(..),
       TruthKind(..),
       AbstractionKind(..),
       Visibility(..),
       Literal(..),
       literalPosition,
       literalDot,
       recordDoc,
       constructorDoc,
       listDoc,
       getNodeID
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Control.Monad.State
import Data.Array
import Data.ByteString(ByteString)
import Data.Hashable
import Data.Ratio
import Text.Format
import Data.Position
import Data.Symbol
import Data.Word
import Text.FormatM
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

-- | A newtype to discriminate field names from symbols.
newtype FieldName = FieldName { fieldSym :: Symbol }
  deriving (Ord, Eq)

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

recordDoc :: [(Doc, Doc)] -> Doc
recordDoc =
  let
    entryDoc :: (Doc, Doc) -> Doc
    entryDoc (fieldname, fieldval) = fieldname <+> equals <+> fieldval
  in
    nest 2 . parens . punctuate (comma <> linebreak) . map entryDoc

listDoc :: [Doc] -> Doc
listDoc = nest 2 . brackets . punctuate (comma <> linebreak)

constructorDoc :: Doc -> [(Doc, Doc)] -> Doc
constructorDoc prefix =
  (prefix <+>) . recordDoc

literalPosition :: Literal -> Position
literalPosition Num { numPos = pos } = pos
literalPosition Str { strPos = pos } = pos
literalPosition Char { charPos = pos } = pos
literalPosition Unit { unitPos = pos } = pos

literalDot :: Monad m => Literal -> StateT Word m (Doc, String)
literalDot Num { numVal = num } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Num | " <>
                               string (show num)) <$>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Str { strVal = str } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Str | " <>
                               string "\\\"" <> bytestring str <>
                               string "\\\"") <$>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Char { charVal = chr } =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Char | " <>
                               squoted (char chr)) <$>
                      string "shape = \"record\"") <> char ';', nodeid)
literalDot Unit {} =
  do
    nodeid <- getNodeID
    return (dquoted (string nodeid) <+>
            brackets (string "label = " <> dquoted (string "Unit") <$>
                      string "shape = \"record\"") <> char ';', nodeid)

instance Hashable FieldName where
  hashWithSalt s FieldName { fieldSym = sym } = s `hashWithSalt` sym

instance MonadSymbols m => FormatM m FieldName where
  formatM = formatM . fieldSym

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
      return (constructorDoc (string "Num")
                             [(string "val", string (show num)),
                              (string "pos", posdoc)])
  formatM Str { strVal = str, strPos = pos } =
    do
      posdoc <- formatM pos
      return (constructorDoc (string "Str")
                             [(string "val", bytestring str),
                              (string "pos", posdoc)])
  formatM Char { charVal = chr, charPos = pos } =
    do
      posdoc <- formatM pos
      return (constructorDoc (string "Char")
                             [(string "val", char chr),
                              (string "pos", posdoc)])
  formatM Unit { unitPos = pos } =
    do
      posdoc <- formatM pos
      return (constructorDoc (string "Unit") [(string "pos", posdoc)])

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
numPickler =
  let
    revfunc Num { numVal = num, numPos = pos } =
      (numerator num, denominator num, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(numer, denom, pos) -> Num { numVal = numer % denom,
                                          numPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "Num")
                        (xpTriple (xpAttr (gxFromString "numerator") xpPrim)
                                  (xpAttr (gxFromString "denominator") xpPrim)
                                  xpickle))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Literal
strPickler =
  let
    revfunc Str { strVal = str, strPos = pos } = (pos, gxFromByteString str)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pos, str) -> Str { strVal = gxToByteString str,
                                 strPos = pos }, revfunc)
           (xpElem (gxFromString "Str") xpickle (xpContent xpText0))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
charPickler =
  let
    revfunc Char { charVal = chr, charPos = pos } = (chr, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(chr, pos) -> Char { charVal = chr, charPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "char")
                        (xpPair (xpAttr (gxFromString "value") xpPrim) xpickle))

unitPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Literal
unitPickler = xpWrap (Unit, unitPos) (xpElemAttrs (gxFromString "Unit") xpickle)

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
                                       (gxFromString "Typeclass")),
                   xpWrap (const Instance, const ())
                          (xpAttrFixed (gxFromString "kind")
                                       (gxFromString "Instance"))]

instance Show BuilderKind where
  show Signature = "signature"
  show Interface = "interface"
  show Module = "module"
  show Class = "class"
  show Typeclass = "typeclass"
  show Instance = "instance"

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

instance Format AbstractionKind where format = string . show
