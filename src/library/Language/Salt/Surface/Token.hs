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
       position,
       keywords,
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Data.Position
import Data.Ratio
import Data.Symbol
import Text.FormatM
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
  -- | The text '|'
  | Bar !Position
  -- | The text '.'
  | Dot !Position
  -- | The text ':'
  | Colon !Position
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
  | Lambda !Position
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
  -- | The text 'typeclass'
  | Typeclass !Position
  -- | The text 'instance'
  | Instance !Position
  -- | The text 'theorem'
  | Theorem !Position
  -- | The text 'invariant'
  | Invariant !Position
  -- | The text 'axiom'
  | Axiom !Position
  -- | The text 'proof'
  | Proof !Position
  -- | The text 'with'
  | With !Position
  -- | The text 'where'
  | Where !Position
  -- | The text 'at'
  | As !Position
  -- | The text 'private'
  | Private !Position
  -- | The text 'protected'
  | Protected !Position
  -- | The text 'public'
  | Public !Position
  -- | The text 'match'
  | Match !Position
  -- | The text 'let'
  | Let !Position
  -- | The text 'fun'
  | Fun !Position
  -- | The text 'import'
  | Import !Position
  -- | The text 'use'
  | Use !Position
  -- | The end-of-file token
  | EOF
    deriving (Ord, Eq)

position :: Token -> Position
position (Id _ pos) = pos
position (Num _ pos) = pos
position (String _ pos) = pos
position (Character _ pos) = pos
position (Equal pos) = pos
position (Ellipsis pos) = pos
position (Bar pos) = pos
position (Dot pos) = pos
position (Colon pos) = pos
position (Comma pos) = pos
position (Semicolon pos) = pos
position (LParen pos) = pos
position (RParen pos) = pos
position (LBrack pos) = pos
position (RBrack pos) = pos
position (LBrace pos) = pos
position (RBrace pos) = pos
position (Lambda pos) = pos
position (Forall pos) = pos
position (Exists pos) = pos
position (Module pos) = pos
position (Signature pos) = pos
position (Class pos) = pos
position (Typeclass pos) = pos
position (Instance pos) = pos
position (Theorem pos) = pos
position (Invariant pos) = pos
position (Axiom pos) = pos
position (Proof pos) = pos
position (With pos) = pos
position (Where pos) = pos
position (As pos) = pos
position (Private pos) = pos
position (Protected pos) = pos
position (Public pos) = pos
position (Match pos) = pos
position (Let pos) = pos
position (Fun pos) = pos
position (Import pos) = pos
position (Use pos) = pos
position EOF = error "Can't take position of EOF"

keywords :: [(Strict.ByteString, Position -> Token)]
keywords = [
    (Strict.fromString "=", Equal),
    (Strict.fromString ":", Colon),
    (Strict.fromString "|", Bar),
    (Strict.fromString "\x2200", Forall),
    (Strict.fromString "forall", Forall),
    (Strict.fromString "\x2203", Exists),
    (Strict.fromString "exists", Exists),
    (Strict.fromString "module", Module),
    (Strict.fromString "signature", Signature),
    (Strict.fromString "class", Class),
    (Strict.fromString "typeclass", Typeclass),
    (Strict.fromString "instance", Instance),
    (Strict.fromString "theorem", Theorem),
    (Strict.fromString "invariant", Invariant),
    (Strict.fromString "axiom", Axiom),
    (Strict.fromString "proof", Proof),
    (Strict.fromString "with", With),
    (Strict.fromString "where", Where),
    (Strict.fromString "as", As),
    (Strict.fromString "private", Private),
    (Strict.fromString "protected", Protected),
    (Strict.fromString "public", Public),
    (Strict.fromString "match", Match),
    (Strict.fromString "let", Let),
    (Strict.fromString "fun", Fun),
    (Strict.fromString "import", Import),
    (Strict.fromString "use", Use)
  ]

idPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
idPickler =
  let
    revfunc (Id sym pos) = (sym, pos)
    revfunc _ = error $! "Can't convert to Id"
  in
    xpWrap (uncurry Id, revfunc) (xpElemAttrs (gxFromString "Id")
                                              (xpPair xpickle xpickle))

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
numPickler =
  let
    revfunc (Num num pos) = (numerator num, denominator num, pos)
    revfunc _ = error $! "Can't convert to Num"
  in
    xpWrap (\(numer, denom, pos) -> Num (numer % denom) pos, revfunc)
           (xpElemAttrs (gxFromString "Num")
                        (xpTriple (xpAttr (gxFromString "numerator") xpPrim)
                                  (xpAttr (gxFromString "denominator") xpPrim)
                                  xpickle))

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
strPickler =
  let
    revfunc (String str pos) = (pos, gxFromByteString str)
    revfunc _ = error $! "Can't convert to Str"
  in
    xpWrap (\(pos, str) -> String (gxToByteString str) pos, revfunc)
           (xpElem (gxFromString "String") xpickle
                   (xpElemNodes (gxFromString "value")
                                (xpContent xpText0)))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
charPickler =
  let
    revfunc (Character chr pos) = (gxFromChar chr, pos)
    revfunc _ = error $! "Can't convert to Char"
  in
    xpWrap (\(chr, pos) -> Character (gxHead chr) pos, revfunc)
           (xpElemAttrs (gxFromString "Character")
                        (xpPair (xpAttr (gxFromString "value") xpText) xpickle))

equalPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
equalPickler =
  let
    revfunc (Equal pos) = pos
    revfunc _ = error $! "Can't convert to Equal"
  in
    xpWrap (Equal, revfunc) (xpElemAttrs (gxFromString "Equal") xpickle)

ellipsisPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
ellipsisPickler =
  let
    revfunc (Ellipsis pos) = pos
    revfunc _ = error $! "Can't convert to Ellipsis"
  in
    xpWrap (Ellipsis, revfunc) (xpElemAttrs (gxFromString "Ellipsis") xpickle)

barPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
barPickler =
  let
    revfunc (Bar pos) = pos
    revfunc _ = error $! "Can't convert to Bar"
  in
    xpWrap (Bar, revfunc) (xpElemAttrs (gxFromString "Bar") xpickle)

dotPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
dotPickler =
  let
    revfunc (Dot pos) = pos
    revfunc _ = error $! "Can't convert to Dot"
  in
    xpWrap (Dot, revfunc) (xpElemAttrs (gxFromString "Dot") xpickle)

colonPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
colonPickler =
  let
    revfunc (Colon pos) = pos
    revfunc _ = error $! "Can't convert to Colon"
  in
    xpWrap (Colon, revfunc) (xpElemAttrs (gxFromString "Colon") xpickle)

commaPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
commaPickler =
  let
    revfunc (Comma pos) = pos
    revfunc _ = error $! "Can't convert to Comma"
  in
    xpWrap (Comma, revfunc) (xpElemAttrs (gxFromString "Comma") xpickle)

semicolonPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
semicolonPickler =
  let
    revfunc (Semicolon pos) = pos
    revfunc _ = error $! "Can't convert to Semicolon"
  in
    xpWrap (Semicolon, revfunc) (xpElemAttrs (gxFromString "Semicolon") xpickle)

lparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lparenPickler =
  let
    revfunc (LParen pos) = pos
    revfunc _ = error $! "Can't convert to LParen"
  in
    xpWrap (LParen, revfunc) (xpElemAttrs (gxFromString "LParen") xpickle)

rparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rparenPickler =
  let
    revfunc (RParen pos) = pos
    revfunc _ = error $! "Can't convert to RParen"
  in
    xpWrap (RParen, revfunc) (xpElemAttrs (gxFromString "RParen") xpickle)

lbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbrackPickler =
  let
    revfunc (LBrack pos) = pos
    revfunc _ = error $! "Can't convert to LBrack"
  in
    xpWrap (LBrack, revfunc) (xpElemAttrs (gxFromString "LBrack") xpickle)

rbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbrackPickler =
  let
    revfunc (RBrack pos) = pos
    revfunc _ = error $! "Can't convert to RBrack"
  in
    xpWrap (RBrack, revfunc) (xpElemAttrs (gxFromString "RBrack") xpickle)

lbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbracePickler =
  let
    revfunc (LBrace pos) = pos
    revfunc _ = error $! "Can't convert to LBrace"
  in
    xpWrap (LBrace, revfunc) (xpElemAttrs (gxFromString "LBrace") xpickle)

rbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbracePickler =
  let
    revfunc (RBrace pos) = pos
    revfunc _ = error $! "Can't convert to RBrace"
  in
    xpWrap (RBrace, revfunc) (xpElemAttrs (gxFromString "RBrace") xpickle)

lambdaPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lambdaPickler =
  let
    revfunc (Lambda pos) = pos
    revfunc _ = error $! "Can't convert to Lambda"
  in
    xpWrap (Lambda, revfunc) (xpElemAttrs (gxFromString "Lambda") xpickle)

forallPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
forallPickler =
  let
    revfunc (Forall pos) = pos
    revfunc _ = error $! "Can't convert to Forall"
  in
    xpWrap (Forall, revfunc) (xpElemAttrs (gxFromString "Forall") xpickle)

existsPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
existsPickler =
  let
    revfunc (Exists pos) = pos
    revfunc _ = error $! "Can't convert to Exists"
  in
    xpWrap (Exists, revfunc) (xpElemAttrs (gxFromString "Exists") xpickle)

modulePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
modulePickler =
  let
    revfunc (Module pos) = pos
    revfunc _ = error $! "Can't convert to Module"
  in
    xpWrap (Module, revfunc) (xpElemAttrs (gxFromString "Module") xpickle)

signaturePickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
signaturePickler =
  let
    revfunc (Signature pos) = pos
    revfunc _ = error $! "Can't convert to Signature"
  in
    xpWrap (Signature, revfunc) (xpElemAttrs (gxFromString "Signature") xpickle)

classPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
classPickler =
  let
    revfunc (Class pos) = pos
    revfunc _ = error $! "Can't convert to Class"
  in
    xpWrap (Class, revfunc) (xpElemAttrs (gxFromString "Class") xpickle)

typeclassPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
typeclassPickler =
  let
    revfunc (Typeclass pos) = pos
    revfunc _ = error $! "Can't convert to Typeclass"
  in
    xpWrap (Typeclass, revfunc) (xpElemAttrs (gxFromString "Typeclass") xpickle)

instancePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
instancePickler =
  let
    revfunc (Instance pos) = pos
    revfunc _ = error $! "Can't convert to Instance"
  in
    xpWrap (Instance, revfunc) (xpElemAttrs (gxFromString "Instance") xpickle)

theoremPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
theoremPickler =
  let
    revfunc (Theorem pos) = pos
    revfunc _ = error $! "Can't convert to Theorem"
  in
    xpWrap (Theorem, revfunc) (xpElemAttrs (gxFromString "Theorem") xpickle)

invariantPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
invariantPickler =
  let
    revfunc (Invariant pos) = pos
    revfunc _ = error $! "Can't convert to Invariant"
  in
    xpWrap (Invariant, revfunc) (xpElemAttrs (gxFromString "Invariant") xpickle)

axiomPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
axiomPickler =
  let
    revfunc (Axiom pos) = pos
    revfunc _ = error $! "Can't convert to Axiom"
  in
    xpWrap (Axiom, revfunc) (xpElemAttrs (gxFromString "Axiom") xpickle)

proofPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
proofPickler =
  let
    revfunc (Proof pos) = pos
    revfunc _ = error $! "Can't convert to Proof"
  in
    xpWrap (Proof, revfunc) (xpElemAttrs (gxFromString "Proof") xpickle)

withPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
withPickler =
  let
    revfunc (With pos) = pos
    revfunc _ = error $! "Can't convert to With"
  in
    xpWrap (With, revfunc) (xpElemAttrs (gxFromString "With") xpickle)

wherePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
wherePickler =
  let
    revfunc (Where pos) = pos
    revfunc _ = error $! "Can't convert to Where"
  in
    xpWrap (Where, revfunc) (xpElemAttrs (gxFromString "Where") xpickle)

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
asPickler =
  let
    revfunc (As pos) = pos
    revfunc _ = error $! "Can't convert to As"
  in
    xpWrap (As, revfunc) (xpElemAttrs (gxFromString "As") xpickle)

privatePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
privatePickler =
  let
    revfunc (Private pos) = pos
    revfunc _ = error $! "Can't convert to Private"
  in
    xpWrap (Private, revfunc) (xpElemAttrs (gxFromString "Private") xpickle)

protectedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
protectedPickler =
  let
    revfunc (Protected pos) = pos
    revfunc _ = error $! "Can't convert to Protected"
  in
    xpWrap (Protected, revfunc) (xpElemAttrs (gxFromString "Protected") xpickle)

publicPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
publicPickler =
  let
    revfunc (Public pos) = pos
    revfunc _ = error $! "Can't convert to Public"
  in
    xpWrap (Public, revfunc) (xpElemAttrs (gxFromString "Public") xpickle)

matchPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
matchPickler =
  let
    revfunc (Match pos) = pos
    revfunc _ = error $! "Can't convert to Match"
  in
    xpWrap (Match, revfunc) (xpElemAttrs (gxFromString "Match") xpickle)

letPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
letPickler =
  let
    revfunc (Let pos) = pos
    revfunc _ = error $! "Can't convert to Let"
  in
    xpWrap (Let, revfunc) (xpElemAttrs (gxFromString "Let") xpickle)

funPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
funPickler =
  let
    revfunc (Fun pos) = pos
    revfunc _ = error $! "Can't convert to Fun"
  in
    xpWrap (Fun, revfunc) (xpElemAttrs (gxFromString "Fun") xpickle)

importPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
importPickler =
  let
    revfunc (Import pos) = pos
    revfunc _ = error $! "Can't convert to Import"
  in
    xpWrap (Import, revfunc) (xpElemAttrs (gxFromString "Import") xpickle)

usePickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
usePickler =
  let
    revfunc (Use pos) = pos
    revfunc _ = error $! "Can't convert to Use"
  in
    xpWrap (Use, revfunc) (xpElemAttrs (gxFromString "Use") xpickle)

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
eofPickler =
  xpWrap (const EOF, const ()) (xpElemAttrs (gxFromString "EOF") xpUnit)

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
      picker (Bar _) = 7
      picker (Dot _) = 8
      picker (Colon _) = 9
      picker (Comma _) = 10
      picker (Semicolon _) = 11
      picker (LParen _) = 12
      picker (RParen _) = 13
      picker (LBrack _) = 14
      picker (RBrack _) = 15
      picker (LBrace _) = 16
      picker (RBrace _) = 17
      picker (Lambda _) = 18
      picker (Forall _) = 19
      picker (Exists _) = 20
      picker (Module _) = 21
      picker (Signature _) = 22
      picker (Class _) = 23
      picker (Typeclass _) = 24
      picker (Instance _) = 25
      picker (Theorem _) = 26
      picker (Invariant _) = 27
      picker (Axiom _) = 28
      picker (Proof _) = 29
      picker (With _) = 30
      picker (Where _) = 31
      picker (As _) = 32
      picker (Private _) = 33
      picker (Protected _) = 34
      picker (Public _) = 35
      picker (Match _) = 36
      picker (Let _) = 37
      picker (Fun _) = 38
      picker (Import _) = 39
      picker (Use _) = 40
    in
      xpAlt picker [eofPickler, idPickler, numPickler, strPickler,
                    charPickler,equalPickler, ellipsisPickler, barPickler,
                    dotPickler, colonPickler, commaPickler, semicolonPickler,
                    lparenPickler, rparenPickler, lbrackPickler, rbrackPickler,
                    lbracePickler, rbracePickler, lambdaPickler, forallPickler,
                    existsPickler, modulePickler, signaturePickler,
                    classPickler, typeclassPickler, instancePickler,
                    theoremPickler, invariantPickler, axiomPickler,
                    proofPickler, withPickler, wherePickler, asPickler,
                    privatePickler, protectedPickler, publicPickler,
                    matchPickler, letPickler, funPickler, importPickler,
                    usePickler ]

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
  formatM (Bar pos) = addPosition pos (string "punctuation \'|\'")
  formatM (Dot pos) = addPosition pos (string "punctuation \'.\'")
  formatM (Colon pos) = addPosition pos (string "punctuation \':\'")
  formatM (Comma pos) = addPosition pos (string "punctuation \',\'")
  formatM (Semicolon pos) = addPosition pos (string "punctuation \';\'")
  formatM (LParen pos) = addPosition pos (string "punctuation \'(\'")
  formatM (RParen pos) = addPosition pos (string "punctuation \')\'")
  formatM (LBrack pos) = addPosition pos (string "punctuation \'[\'")
  formatM (RBrack pos) = addPosition pos (string "punctuation \']\'")
  formatM (LBrace pos) = addPosition pos (string "punctuation \'{\'")
  formatM (RBrace pos) = addPosition pos (string "punctuation \'}\'")
  formatM (Lambda pos) = addPosition pos (string "lambda")
  formatM (Forall pos) = addPosition pos (string "keyword \"forall\"")
  formatM (Exists pos) = addPosition pos (string "keyword \"exists\"")
  formatM (Module pos) = addPosition pos (string "keyword \"module\"")
  formatM (Signature pos) = addPosition pos (string "keyword \"signature\"")
  formatM (Class pos) = addPosition pos (string "keyword \"class\"")
  formatM (Typeclass pos) = addPosition pos (string "keyword \"typeclass\"")
  formatM (Instance pos) = addPosition pos (string "keyword \"instance\"")
  formatM (Theorem pos) = addPosition pos (string "keyword \"theorem\"")
  formatM (Invariant pos) = addPosition pos (string "keyword \"invariant\"")
  formatM (Axiom pos) = addPosition pos (string "keyword \"axiom\"")
  formatM (Proof pos) = addPosition pos (string "keyword \"proof\"")
  formatM (With pos) = addPosition pos (string "keyword \"with\"")
  formatM (Where pos) = addPosition pos (string "keyword \"where\"")
  formatM (As pos) = addPosition pos (string "keyword \"as\"")
  formatM (Private pos) = addPosition pos (string "keyword \"private\"")
  formatM (Protected pos) = addPosition pos (string "keyword \"protected\"")
  formatM (Public pos) = addPosition pos (string "keyword \"public\"")
  formatM (Match pos) = addPosition pos (string "keyword \"match\"")
  formatM (Let pos) = addPosition pos (string "keyword \"let\"")
  formatM (Fun pos) = addPosition pos (string "keyword \"fun\"")
  formatM (Import pos) = addPosition pos (string "keyword \"import\"")
  formatM (Use pos) = addPosition pos (string "keyword \"use\"")
