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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Defines the type of tokens produced by the lexer.  These
module Language.Salt.Surface.Token(
       Token(..),
       keywords,
       ) where

import Control.Monad.Positions
import Control.Monad.Symbols
import Data.Position.BasicPosition
import Data.PositionElement
import Data.Ratio
import Data.Symbol
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Prelude hiding (Either(..))

import Data.ByteString.UTF8 as Strict

type Position = BasicPosition

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
  -- | The text 'syntax'
  | Syntax !Position
  -- | The text 'component'
  | Component !Position
  -- | The text 'postfix'
  | Postfix !Position
  -- | The text 'infix'
  | Infix !Position
  -- | The text 'left'
  | Left !Position
  -- | The text 'right'
  | Right !Position
  -- | The text 'nonassoc'
  | NonAssoc !Position
  -- | The text 'prec'
  | Prec !Position
  -- | The text '<'
  | Less !Position
  -- | The text '>'
  | Greater !Position
  -- | The end-of-file token
  | EOF
    deriving (Ord, Eq)

instance PositionElement Token where
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
  position (Syntax pos) = pos
  position (Component pos) = pos
  position (Postfix pos) = pos
  position (Infix pos) = pos
  position (Left pos) = pos
  position (Right pos) = pos
  position (NonAssoc pos) = pos
  position (Prec pos) = pos
  position (Less pos) = pos
  position (Greater pos) = pos
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
    (Strict.fromString "use", Use),
    (Strict.fromString "syntax", Syntax),
    (Strict.fromString "component", Component),
    (Strict.fromString "postfix", Postfix),
    (Strict.fromString "infix", Infix),
    (Strict.fromString "left", Left),
    (Strict.fromString "right", Right),
    (Strict.fromString "nonassoc", NonAssoc),
    (Strict.fromString "prec", Prec),
    (Strict.fromString "<", Less),
    (Strict.fromString ">", Greater)
  ]

idPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
idPickler =
  let
    revfunc (Id sym pos) = (sym, pos)
    revfunc _ = error $! "Can't convert to Id"
  in
    xpWrap (uncurry Id, revfunc) (xpElem (gxFromString "Id") xpickle xpickle)

numPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
numPickler =
  let
    revfunc (Num num pos) = ((numerator num, denominator num), pos)
    revfunc _ = error $! "Can't convert to Num"
  in
    xpWrap (\((numer, denom), pos) -> Num (numer % denom) pos, revfunc)
           (xpElem (gxFromString "Num")
                   (xpPair (xpAttr (gxFromString "numerator") xpPrim)
                           (xpAttr (gxFromString "denominator") xpPrim))
                   xpickle)

strPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
strPickler =
  let
    revfunc (String str pos) = (pos, gxFromByteString str)
    revfunc _ = error $! "Can't convert to Str"
  in
    xpWrap (\(pos, str) -> String (gxToByteString str) pos, revfunc)
           (xpElemNodes (gxFromString "String")
                        (xpPair xpickle
                                (xpElemNodes (gxFromString "value")
                                             (xpContent xpText0))))

charPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
charPickler =
  let
    revfunc (Character chr pos) = (gxFromChar chr, pos)
    revfunc _ = error $! "Can't convert to Char"
  in
    xpWrap (\(chr, pos) -> Character (gxHead chr) pos, revfunc)
           (xpElem (gxFromString "Character")
                   (xpAttr (gxFromString "value") xpText) xpickle)

equalPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
equalPickler =
  let
    revfunc (Equal pos) = pos
    revfunc _ = error $! "Can't convert to Equal"
  in
    xpWrap (Equal, revfunc) (xpElemNodes (gxFromString "Equal") xpickle)

ellipsisPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
ellipsisPickler =
  let
    revfunc (Ellipsis pos) = pos
    revfunc _ = error $! "Can't convert to Ellipsis"
  in
    xpWrap (Ellipsis, revfunc) (xpElemNodes (gxFromString "Ellipsis") xpickle)

barPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
barPickler =
  let
    revfunc (Bar pos) = pos
    revfunc _ = error $! "Can't convert to Bar"
  in
    xpWrap (Bar, revfunc) (xpElemNodes (gxFromString "Bar") xpickle)

dotPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
dotPickler =
  let
    revfunc (Dot pos) = pos
    revfunc _ = error $! "Can't convert to Dot"
  in
    xpWrap (Dot, revfunc) (xpElemNodes (gxFromString "Dot") xpickle)

colonPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
colonPickler =
  let
    revfunc (Colon pos) = pos
    revfunc _ = error $! "Can't convert to Colon"
  in
    xpWrap (Colon, revfunc) (xpElemNodes (gxFromString "Colon") xpickle)

commaPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
commaPickler =
  let
    revfunc (Comma pos) = pos
    revfunc _ = error $! "Can't convert to Comma"
  in
    xpWrap (Comma, revfunc) (xpElemNodes (gxFromString "Comma") xpickle)

semicolonPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
semicolonPickler =
  let
    revfunc (Semicolon pos) = pos
    revfunc _ = error $! "Can't convert to Semicolon"
  in
    xpWrap (Semicolon, revfunc) (xpElemNodes (gxFromString "Semicolon") xpickle)

lparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lparenPickler =
  let
    revfunc (LParen pos) = pos
    revfunc _ = error $! "Can't convert to LParen"
  in
    xpWrap (LParen, revfunc) (xpElemNodes (gxFromString "LParen") xpickle)

rparenPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rparenPickler =
  let
    revfunc (RParen pos) = pos
    revfunc _ = error $! "Can't convert to RParen"
  in
    xpWrap (RParen, revfunc) (xpElemNodes (gxFromString "RParen") xpickle)

lbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbrackPickler =
  let
    revfunc (LBrack pos) = pos
    revfunc _ = error $! "Can't convert to LBrack"
  in
    xpWrap (LBrack, revfunc) (xpElemNodes (gxFromString "LBrack") xpickle)

rbrackPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbrackPickler =
  let
    revfunc (RBrack pos) = pos
    revfunc _ = error $! "Can't convert to RBrack"
  in
    xpWrap (RBrack, revfunc) (xpElemNodes (gxFromString "RBrack") xpickle)

lbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lbracePickler =
  let
    revfunc (LBrace pos) = pos
    revfunc _ = error $! "Can't convert to LBrace"
  in
    xpWrap (LBrace, revfunc) (xpElemNodes (gxFromString "LBrace") xpickle)

rbracePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
rbracePickler =
  let
    revfunc (RBrace pos) = pos
    revfunc _ = error $! "Can't convert to RBrace"
  in
    xpWrap (RBrace, revfunc) (xpElemNodes (gxFromString "RBrace") xpickle)

lambdaPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
lambdaPickler =
  let
    revfunc (Lambda pos) = pos
    revfunc _ = error $! "Can't convert to Lambda"
  in
    xpWrap (Lambda, revfunc) (xpElemNodes (gxFromString "Lambda") xpickle)

forallPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
forallPickler =
  let
    revfunc (Forall pos) = pos
    revfunc _ = error $! "Can't convert to Forall"
  in
    xpWrap (Forall, revfunc) (xpElemNodes (gxFromString "Forall") xpickle)

existsPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
existsPickler =
  let
    revfunc (Exists pos) = pos
    revfunc _ = error $! "Can't convert to Exists"
  in
    xpWrap (Exists, revfunc) (xpElemNodes (gxFromString "Exists") xpickle)

modulePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
modulePickler =
  let
    revfunc (Module pos) = pos
    revfunc _ = error $! "Can't convert to Module"
  in
    xpWrap (Module, revfunc) (xpElemNodes (gxFromString "Module") xpickle)

signaturePickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
signaturePickler =
  let
    revfunc (Signature pos) = pos
    revfunc _ = error $! "Can't convert to Signature"
  in
    xpWrap (Signature, revfunc) (xpElemNodes (gxFromString "Signature") xpickle)

classPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
classPickler =
  let
    revfunc (Class pos) = pos
    revfunc _ = error $! "Can't convert to Class"
  in
    xpWrap (Class, revfunc) (xpElemNodes (gxFromString "Class") xpickle)

typeclassPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
typeclassPickler =
  let
    revfunc (Typeclass pos) = pos
    revfunc _ = error $! "Can't convert to Typeclass"
  in
    xpWrap (Typeclass, revfunc) (xpElemNodes (gxFromString "Typeclass") xpickle)

instancePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
instancePickler =
  let
    revfunc (Instance pos) = pos
    revfunc _ = error $! "Can't convert to Instance"
  in
    xpWrap (Instance, revfunc) (xpElemNodes (gxFromString "Instance") xpickle)

theoremPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
theoremPickler =
  let
    revfunc (Theorem pos) = pos
    revfunc _ = error $! "Can't convert to Theorem"
  in
    xpWrap (Theorem, revfunc) (xpElemNodes (gxFromString "Theorem") xpickle)

invariantPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
invariantPickler =
  let
    revfunc (Invariant pos) = pos
    revfunc _ = error $! "Can't convert to Invariant"
  in
    xpWrap (Invariant, revfunc) (xpElemNodes (gxFromString "Invariant") xpickle)

axiomPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
axiomPickler =
  let
    revfunc (Axiom pos) = pos
    revfunc _ = error $! "Can't convert to Axiom"
  in
    xpWrap (Axiom, revfunc) (xpElemNodes (gxFromString "Axiom") xpickle)

proofPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
proofPickler =
  let
    revfunc (Proof pos) = pos
    revfunc _ = error $! "Can't convert to Proof"
  in
    xpWrap (Proof, revfunc) (xpElemNodes (gxFromString "Proof") xpickle)

withPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
withPickler =
  let
    revfunc (With pos) = pos
    revfunc _ = error $! "Can't convert to With"
  in
    xpWrap (With, revfunc) (xpElemNodes (gxFromString "With") xpickle)

wherePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
wherePickler =
  let
    revfunc (Where pos) = pos
    revfunc _ = error $! "Can't convert to Where"
  in
    xpWrap (Where, revfunc) (xpElemNodes (gxFromString "Where") xpickle)

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text) =>
             PU [NodeG [] tag text] Token
asPickler =
  let
    revfunc (As pos) = pos
    revfunc _ = error $! "Can't convert to As"
  in
    xpWrap (As, revfunc) (xpElemNodes (gxFromString "As") xpickle)

privatePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
privatePickler =
  let
    revfunc (Private pos) = pos
    revfunc _ = error $! "Can't convert to Private"
  in
    xpWrap (Private, revfunc) (xpElemNodes (gxFromString "Private") xpickle)

protectedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
protectedPickler =
  let
    revfunc (Protected pos) = pos
    revfunc _ = error $! "Can't convert to Protected"
  in
    xpWrap (Protected, revfunc) (xpElemNodes (gxFromString "Protected") xpickle)

publicPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
publicPickler =
  let
    revfunc (Public pos) = pos
    revfunc _ = error $! "Can't convert to Public"
  in
    xpWrap (Public, revfunc) (xpElemNodes (gxFromString "Public") xpickle)

matchPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
matchPickler =
  let
    revfunc (Match pos) = pos
    revfunc _ = error $! "Can't convert to Match"
  in
    xpWrap (Match, revfunc) (xpElemNodes (gxFromString "Match") xpickle)

letPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
letPickler =
  let
    revfunc (Let pos) = pos
    revfunc _ = error $! "Can't convert to Let"
  in
    xpWrap (Let, revfunc) (xpElemNodes (gxFromString "Let") xpickle)

funPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
funPickler =
  let
    revfunc (Fun pos) = pos
    revfunc _ = error $! "Can't convert to Fun"
  in
    xpWrap (Fun, revfunc) (xpElemNodes (gxFromString "Fun") xpickle)

importPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
importPickler =
  let
    revfunc (Import pos) = pos
    revfunc _ = error $! "Can't convert to Import"
  in
    xpWrap (Import, revfunc) (xpElemNodes (gxFromString "Import") xpickle)

usePickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
usePickler =
  let
    revfunc (Use pos) = pos
    revfunc _ = error $! "Can't convert to Use"
  in
    xpWrap (Use, revfunc) (xpElemNodes (gxFromString "Use") xpickle)

syntaxPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Token
syntaxPickler =
  let
    revfunc (Syntax pos) = pos
    revfunc _ = error $! "Can't convert to Syntax"
  in
    xpWrap (Syntax, revfunc) (xpElemNodes (gxFromString "Syntax") xpickle)

componentPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Token
componentPickler =
  let
    revfunc (Component pos) = pos
    revfunc _ = error $! "Can't convert to Component"
  in
    xpWrap (Component, revfunc) (xpElemNodes (gxFromString "Component") xpickle)

postfixPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Token
postfixPickler =
  let
    revfunc (Postfix pos) = pos
    revfunc _ = error $! "Can't convert to Postfix"
  in
    xpWrap (Postfix, revfunc) (xpElemNodes (gxFromString "Postfix") xpickle)

infixPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
infixPickler =
  let
    revfunc (Infix pos) = pos
    revfunc _ = error $! "Can't convert to Infix"
  in
    xpWrap (Infix, revfunc) (xpElemNodes (gxFromString "Infix") xpickle)

leftPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
leftPickler =
  let
    revfunc (Left pos) = pos
    revfunc _ = error $! "Can't convert to Left"
  in
    xpWrap (Left, revfunc) (xpElemNodes (gxFromString "Left") xpickle)

rightPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Token
rightPickler =
  let
    revfunc (Right pos) = pos
    revfunc _ = error $! "Can't convert to Right"
  in
    xpWrap (Right, revfunc) (xpElemNodes (gxFromString "Right") xpickle)

nonAssocPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Token
nonAssocPickler =
  let
    revfunc (NonAssoc pos) = pos
    revfunc _ = error $! "Can't convert to NonAssoc"
  in
    xpWrap (NonAssoc, revfunc) (xpElemNodes (gxFromString "NonAssoc") xpickle)

precPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
precPickler =
  let
    revfunc (Prec pos) = pos
    revfunc _ = error $! "Can't convert to Prec"
  in
    xpWrap (Prec, revfunc) (xpElemNodes (gxFromString "Prec") xpickle)

lessPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Token
lessPickler =
  let
    revfunc (Less pos) = pos
    revfunc _ = error $! "Can't convert to Less"
  in
    xpWrap (Less, revfunc) (xpElemNodes (gxFromString "Less") xpickle)

greaterPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Token
greaterPickler =
  let
    revfunc (Greater pos) = pos
    revfunc _ = error $! "Can't convert to Greater"
  in
    xpWrap (Greater, revfunc) (xpElemNodes (gxFromString "Greater") xpickle)

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] Token
eofPickler =
  xpWrap (const EOF, const ()) (xpElemNodes (gxFromString "EOF") xpUnit)

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
      picker (Syntax _) = 41
      picker (Component _) = 42
      picker (Postfix _) = 43
      picker (Infix _) = 44
      picker (Left _) = 45
      picker (Right _) = 46
      picker (NonAssoc _) = 47
      picker (Prec _) = 48
      picker (Less _) = 49
      picker (Greater _) = 50
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
                    usePickler, syntaxPickler, componentPickler,
                    postfixPickler, infixPickler, leftPickler, rightPickler,
                    nonAssocPickler, precPickler, lessPickler, greaterPickler ]

addPosition :: MonadPositions m => Position -> Doc -> m Doc
addPosition pos @ File {} doc =
  do
    posdoc <- formatM pos
    return (doc <+> string "in" <+> posdoc)
addPosition pos @ Synthetic {} doc =
  do
    posdoc <- formatM pos
    return (doc <+> posdoc)
addPosition pos @ CmdLine doc =
  do
    posdoc <- formatM pos
    return (doc <+> posdoc)
addPosition pos doc =
  do
    posdoc <- formatM pos
    return (doc <+> string "at" <+> posdoc)

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
  formatM (Syntax pos) = addPosition pos (string "keyword \"syntax\"")
  formatM (Component pos) = addPosition pos (string "keyword \"component\"")
  formatM (Postfix pos) = addPosition pos (string "keyword \"postfix\"")
  formatM (Infix pos) = addPosition pos (string "keyword \"infix\"")
  formatM (Left pos) = addPosition pos (string "keyword \"left\"")
  formatM (Right pos) = addPosition pos (string "keyword \"right\"")
  formatM (NonAssoc pos) = addPosition pos (string "keyword \"nonassoc\"")
  formatM (Prec pos) = addPosition pos (string "keyword \"prec\"")
  formatM (Less pos) = addPosition pos (string "operator \"<\"")
  formatM (Greater pos) = addPosition pos (string "operator \">\"")
