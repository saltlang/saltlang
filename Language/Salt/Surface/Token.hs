{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
module Frontend.Full.Token(
       Token(..)
       ) where

import Data.Pos
import Data.Symbol
import Text.Format

data Token =
    Id !Symbol !Pos
  | Num !Integer !Pos
  | Equal !Pos
  | Arrow !Pos
  | Dot !Pos
  | Colon !Pos
  | ColonColon !Pos
  | Comma !Pos
  | Semicolon !Pos
  | LParen !Pos
  | RParen !Pos
  | LBrace !Pos
  | RBrace !Pos
  | Module !Pos
  | Signature !Pos
  | With !Pos
  | Where !Pos
  | Public !Pos
  | Private !Pos
  | Protected !Pos
  | Import !Pos
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