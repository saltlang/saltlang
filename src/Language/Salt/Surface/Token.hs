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

-- | Defines the type of lexer tokens
module Language.Salt.Surface.Token(
       Token(..)
       ) where

import Data.Pos
import Data.Symbol
import Text.Format

-- | A token produced by the lexer
data Token =
  -- | An identifier
    Id !Symbol !Pos
  -- | A number literal
  | Num !Integer !Pos
  -- | The text '='
  | Equal !Pos
  -- | The text '->'
  | Arrow !Pos
  -- | The text '.'
  | Dot !Pos
  -- | The text ':'
  | Colon !Pos
  -- | The text '::'
  | ColonColon !Pos
  -- | The text ','
  | Comma !Pos
  -- | The text ';'
  | Semicolon !Pos
  -- | The text '('
  | LParen !Pos
  -- | The text ')'
  | RParen !Pos
  -- | The text '{'
  | LBrace !Pos
  -- | The text '}'
  | RBrace !Pos
  -- | The text 'module'
  | Module !Pos
  -- | The text 'signature'
  | Signature !Pos
  -- | The text 'with'
  | With !Pos
  -- | The text 'where'
  | Where !Pos
  -- | The text 'public'
  | Public !Pos
  -- | The text 'private'
  | Private !Pos
  -- | The text 'protected'
  | Protected !Pos
  -- | The text 'import'
  | Import !Pos
  -- | The text 'export'
  | Export !Pos
  | EOF

instance Format Token where
  format (Id sym _) = "identifier" <+> sym
  format (Num n _) = "integer literal" <+> n
  format (Equal _) = text "operator '='"
  format (Arrow _) = text "operator '->'"
  format (Dot _) = text "operator '.'"
  format (Colon _) = text "operator ':'"
  format (ColonColon _) = text "operator '::'"
  format (Comma _) = text "operator ','"
  format (Semicolon _) = text "operator ';'"
  format (LParen _) = text "operator '('"
  format (RParen _) = text "operator ')'"
  format (LBrace _) = text "operator '{'"
  format (RBrace _) = text "operator '}'"
  format (Module _) = text "keyword module"
  format (Signature _) = text "keyword signature"
  format (With _) = text "keyword with"
  format (Where _) = text "keyword where"
  format (Public _) = text "keyword public"
  format (Private _) = text "keyword private"
  format (Protected _) = text "keyword protected"
  format (Import _) = text "keyword import"
  format (Export _) = text "keyword export"
  format EOF = text "end of input"

instance Show Token where
  show = show . format

instance Position Token where
  pos (Id _ p) = p
  pos (Num _ p) = p
  pos (Dot p) = p
  pos (Equal p) = p
  pos (Arrow p) = p
  pos (Colon p) = p
  pos (ColonColon p) = p
  pos (Comma p) = p
  pos (Semicolon p) = p
  pos (LParen p) = p
  pos (RParen p) = p
  pos (LBrace p) = p
  pos (RBrace p) = p
  pos (Module p) = p
  pos (Signature p) = p
  pos (With p) = p
  pos (Where p) = p
  pos (Public p) = p
  pos (Private p) = p
  pos (Protected p) = p
  pos (Import p) = p
  pos (Export p) = p
  pos EOF = error "Cannot take position of EOF"
