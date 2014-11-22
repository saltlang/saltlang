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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Defines the type of tokens produced by the lexer.  These
module Language.Salt.Surface.Token(
       Token(..),
       keywords,
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Data.Position
import Data.Ratio
import Data.Symbol
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import Data.ByteString.UTF8 as Strict

-- | A token produced by the lexer.
data Token =
  -- | An identifier
    Id Symbol !Position
  -- | A number literal
  | Num !Rational !Position
  -- | A string literal
  | String !Strict.ByteString !Position
  -- | A character literal
  | Character !Char !Position
  -- | The text '='
  | Equal !Position
  -- | The text '...'
  | Ellipsis !Position
  -- | The text '->'
  | Arrow !Position
  -- | The text '.'
  | Dot !Position
  -- | The text ':'
  | Colon !Position
  -- | The text '::'
  | ColonColon !Position
  -- | The text ','
  | Comma !Position
  -- | The text ';'
  | Semicolon !Position
  -- | The text '('
  | LParen !Position
  -- | The text ')'
  | RParen !Position
  -- | The text '['
  | LBrack !Position
  -- | The text ']'
  | RBrack !Position
  -- | The text '{'
  | LBrace !Position
  -- | The text '}'
  | RBrace !Position
  -- | The text 'forall'
  | Forall !Position
  -- | The text 'exists'
  | Exists !Position
  -- | The text 'module'
  | Module !Position
  -- | The text 'signature'
  | Signature !Position
  -- | The text 'class'
  | Class !Position
  -- | The text 'with'
  | With !Position
  -- | The text 'where'
  | Where !Position
  -- | The end-of-file token
  | EOF
    deriving (Ord, Eq)

keywords :: [(Strict.ByteString, Position -> Token)]
keywords = [
    (Strict.fromString "=", Equal),
    (Strict.fromString "->", Arrow),
    (Strict.fromString "\x2192", Arrow),
    (Strict.fromString ":", Colon),
    (Strict.fromString "::", ColonColon),
    (Strict.fromString "\x2200", Forall),
    (Strict.fromString "forall", Forall),
    (Strict.fromString "\x2203", Exists),
    (Strict.fromString "exists", Exists),
    (Strict.fromString "module", Module),
    (Strict.fromString "signature", Signature),
    (Strict.fromString "class", Class),
    (Strict.fromString "with", With),
    (Strict.fromString "where", Where)
  ]

idPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
idPickler =
  let
    revfunc (Id sym pos) = ((), sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), sym, pos) -> Id sym pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpTriple (xpAttrFixed (gxFromString "kind")
                                               (gxFromString "Id"))
                                  xpickle xpickle))

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
numPickler =
  let
    revfunc (Num num pos) = ((), numerator num, denominator num, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), numer, denom, pos) -> Num (numer % denom) pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xp4Tuple (xpAttrFixed (gxFromString "kind")
                                               (gxFromString "Num"))
                                  (xpAttr (gxFromString "numerator") xpPrim)
                                  (xpAttr (gxFromString "denominator") xpPrim)
                                  xpickle))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
strPickler =
  let
    revfunc (String str pos) = (((), pos), gxFromByteString str)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(((), pos), str) -> String (gxToByteString str) pos, revfunc)
           (xpElem (gxFromString "token")
                   (xpPair (xpAttrFixed (gxFromString "kind")
                                        (gxFromString "String"))
                           xpickle)
                   (xpElemNodes (gxFromString "value")
                                (xpContent xpText0)))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
charPickler =
  let
    revfunc (Character chr pos) = ((), chr, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), chr, pos) -> Character chr pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpTriple (xpAttrFixed (gxFromString "kind")
                                               (gxFromString "Character"))
                                  (xpAttr (gxFromString "value") xpPrim)
                                  xpickle))

equalPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
equalPickler =
  let
    revfunc (Equal pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Equal pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Equal"))
                                xpickle))

ellipsisPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
ellipsisPickler =
  let
    revfunc (Ellipsis pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Ellipsis pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Ellipsis"))
                                xpickle))

arrowPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
arrowPickler =
  let
    revfunc (Arrow pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Arrow pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Arrow"))
                                xpickle))

dotPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
dotPickler =
  let
    revfunc (Dot pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Dot pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Dot"))
                                xpickle))

colonPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
colonPickler =
  let
    revfunc (Colon pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Colon pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Colon"))
                                xpickle))

commaPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
commaPickler =
  let
    revfunc (Comma pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Comma pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Comma"))
                                xpickle))

semicolonPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
semicolonPickler =
  let
    revfunc (Semicolon pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Semicolon pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Semicolon"))
                                xpickle))

colonColonPickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text) =>
                     PU [NodeG [] tag text] Token
colonColonPickler =
  let
    revfunc (ColonColon pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> ColonColon pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "ColonColon"))
                                xpickle))

lparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lparenPickler =
  let
    revfunc (LParen pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> LParen pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "LParen"))
                                xpickle))

rparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rparenPickler =
  let
    revfunc (RParen pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> RParen pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "RParen"))
                                xpickle))

lbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbrackPickler =
  let
    revfunc (LBrack pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> LBrack pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "LBrack"))
                                xpickle))

rbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbrackPickler =
  let
    revfunc (RBrack pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> RBrack pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "RBrack"))
                                xpickle))

lbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbracePickler =
  let
    revfunc (LBrace pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> LBrace pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "LBrace"))
                                xpickle))

rbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbracePickler =
  let
    revfunc (RBrace pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> RBrace pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "RBrace"))
                                xpickle))

forallPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
forallPickler =
  let
    revfunc (Forall pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Forall pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Forall"))
                                xpickle))

existsPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
existsPickler =
  let
    revfunc (Exists pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Exists pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Exists"))
                                xpickle))

modulePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
modulePickler =
  let
    revfunc (Module pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Module pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Module"))
                                xpickle))

signaturePickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
signaturePickler =
  let
    revfunc (Signature pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Signature pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Signature"))
                                xpickle))

classPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
classPickler =
  let
    revfunc (Class pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Class pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Class"))
                                xpickle))

withPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
withPickler =
  let
    revfunc (With pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> With pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "With"))
                                xpickle))

wherePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
wherePickler =
  let
    revfunc (Where pos) = ((), pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\((), pos) -> Where pos, revfunc)
           (xpElemAttrs (gxFromString "token")
                        (xpPair (xpAttrFixed (gxFromString "kind")
                                             (gxFromString "Where"))
                                xpickle))

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
eofPickler =
  xpWrap (const EOF, const ()) (xpElemAttrs (gxFromString "token")
                                            (xpAttrFixed (gxFromString "kind")
                                                         (gxFromString "EOF")))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Token where
  xpickle =
    let
      picker EOF = 0
      picker (Id _ _) = 1
      picker (Num _ _) = 2
      picker (String _ _) = 3
      picker (Character _ _) = 4
      picker (Equal _) = 5
      picker (Ellipsis _) = 6
      picker (Arrow _) = 7
      picker (Dot _) = 8
      picker (Colon _) = 9
      picker (ColonColon _) = 10
      picker (Comma _) = 11
      picker (Semicolon _) = 12
      picker (LParen _) = 13
      picker (RParen _) = 14
      picker (LBrack _) = 15
      picker (RBrack _) = 16
      picker (LBrace _) = 17
      picker (RBrace _) = 18
      picker (Forall _) = 19
      picker (Exists _) = 20
      picker (Module _) = 21
      picker (Signature _) = 22
      picker (Class _) = 23
      picker (With _) = 24
      picker (Where _) = 25
    in
      xpAlt picker [eofPickler, idPickler, numPickler, strPickler,
                    charPickler,equalPickler, ellipsisPickler, arrowPickler,
                    dotPickler, colonPickler, colonColonPickler, commaPickler,
                    semicolonPickler, lparenPickler, rparenPickler,
                    lbrackPickler, rbrackPickler, lbracePickler, rbracePickler,
                    forallPickler, existsPickler, modulePickler,
                    signaturePickler, classPickler, withPickler, wherePickler ]

addPosition :: MonadPositions m => Position -> Doc -> m Doc
addPosition pos doc =
  do
    pinfo <- positionInfo pos
    return (doc <+> string "at" <+> format pinfo)

instance (MonadPositions m, MonadSymbols m) => FormatM m Token where
  formatM EOF = return (string "end of input")
  formatM (Id sym pos) =
    do
      symname <- name sym
      addPosition pos (string "identifier" <+> dquoted (bytestring symname))
  formatM (Num n pos) =
    let
      numdoc =
        if denominator n == 1
          then format (numerator n)
          else format (numerator n) <> char '/' <> format (denominator n)
    in
      addPosition pos (string "number" <+> numdoc)
  formatM (String str pos) =
    addPosition pos (string "string" <+> dquoted (bytestring str))
  formatM (Character chr pos) =
    addPosition pos (string "character" <+> squoted (char chr))
  formatM (Equal pos) = addPosition pos (string "punctuation \'=\'")
  formatM (Ellipsis pos) = addPosition pos (string "punctuation \'...\'")
  formatM (Arrow pos) = addPosition pos (string "operator \'->\'")
  formatM (Dot pos) = addPosition pos (string "punctuation \'.\'")
  formatM (Colon pos) = addPosition pos (string "punctuation \':\'")
  formatM (ColonColon pos) = addPosition pos (string "punctuation \'::\'")
  formatM (Comma pos) = addPosition pos (string "punctuation \',\'")
  formatM (Semicolon pos) = addPosition pos (string "punctuation \';\'")
  formatM (LParen pos) = addPosition pos (string "punctuation \'(\'")
  formatM (RParen pos) = addPosition pos (string "punctuation \')\'")
  formatM (LBrack pos) = addPosition pos (string "punctuation \'[\'")
  formatM (RBrack pos) = addPosition pos (string "punctuation \']\'")
  formatM (LBrace pos) = addPosition pos (string "punctuation \'{\'")
  formatM (RBrace pos) = addPosition pos (string "punctuation \'}\'")
  formatM (Forall pos) = addPosition pos (string "keyword \"forall\"")
  formatM (Exists pos) = addPosition pos (string "keyword \"exists\"")
  formatM (Module pos) = addPosition pos (string "keyword \"module\"")
  formatM (Signature pos) = addPosition pos (string "keyword \"signature\"")
  formatM (Class pos) = addPosition pos (string "keyword \"class\"")
  formatM (With pos) = addPosition pos (string "keyword \"with\"")
  formatM (Where pos) = addPosition pos (string "keyword \"where\"")
