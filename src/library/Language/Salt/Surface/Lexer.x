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
{
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Salt.Surface.Lexer(
       Frontend,
       Lexer,
       lexer,
       lex
       ) where

import Control.Monad.Genpos
import Control.Monad.Keywords
import Control.Monad.Messages
import Control.Monad.SourceBuffer hiding (linebreak)
import Control.Monad.Symbols
import Control.Monad.State
import Control.Monad.Trans
import Data.LexicalError
import Data.Position
import Data.Symbol
import Language.Salt.Surface.Token
import Language.Salt.Message
import Prelude hiding (log, span, lex)
import System.IO
import Text.Escapes.ByteString.Lazy

import qualified Control.Monad.CommentBuffer as CommentBuffer
import qualified Control.Monad.Frontend as Frontend
import qualified Data.ByteString.Lazy.UTF8 as Lazy
import qualified Data.ByteString.Lazy as Lazy hiding (uncons, drop)
import qualified Data.ByteString.UTF8 as Strict
import qualified Text.Numbers.ByteString.Lazy as Numbers

import Text.AlexWrapper

}

@opchars = [\!\#\%\^\&\+\?\<\>\:\|\~\-\=\:]
@escape_chars = [abfnrtv\\'\"\?]

tokens :-

-- Record linebreaks at newlines
<0>       \r?\n
        { linebreak `andThen` skip }

-- Newlines preceeded by whitespace emit a warning
<0>       [\ ]+$
        { report trailingWhitespace `andThen` skip }

-- Hard tabs emit a warning
<0>       \t+
        { report hardTabs `andThen` skip }

-- Skip plain whitespace; we don't allow \f or \v at all
<0>       [\ ]+
        { skip }

-- Text, beginning with an alphabetic character, possibly ending with a quote
<0>       [_a-zA-Z][_a-zA-Z0-9]*[']*
        { report bufferedComments `andThen` produce (keywordOrToken Id) }

-- Operators, cannot contain a /* or a */ anywhere, otherwise all
-- sequences are valid
<0>       @opchars*(\/|\*+|@opchars)(@opchars+(\/|\*+)?)*
        { report bufferedComments `andThen` produce (keywordOrToken Id) }

-- Single-character punctuation
<0>       [\.\(\)\[\]\{\}\,\;\:]
        { report bufferedComments `andThen` produce singleChar }

-- Ellipsis
<0>       \.\.\.
        { report bufferedComments `andThen` token (return . Ellipsis) }

-- Decimal literals
<0>       [\-\+]?[1-9][0-9]*(\.[0-9]+)?([ep][\-\+]?[0-9]+)?
        { report bufferedComments `andThen` produce decLiteral }

-- Octal literals
<0>       [\-\+]?0[0-7]+(\.[0-7]+)?([ep][\-\+]?[0-7]+)?
        { report bufferedComments `andThen` produce octLiteral }

-- Hex literals
<0>       [\-\+]?0[xX][0-9a-fA-F]+(\.[0-9a-fA-F]+)?(p[\-\+]?[0-9a-fA-F]+)?
        { report bufferedComments `andThen` produce hexLiteral }

-- Binary literals
<0>       [\-\+]?0[bB][01]+(\.[01]+)?([ep][\-\+]?[01]+)?
        { report bufferedComments `andThen` produce binLiteral }

-- Line comments, skip the rest of the line, add it to the comment buffer
<0>       \/\/[^\n]*
        { record fullComment `andThen` skip }

-- Empty character literal
<0>       ''
        { report emptyCharLiteral `andThen` skip }

-- Unescaped character literal
<0>       '[^\n\t]'
        { report bufferedComments `andThen` produce unescapedChar }

-- Valid escaped character literal
<0>       '\\[abfnrtv\\\'\"\?]'
        { report bufferedComments `andThen` produce escapedChar }

-- A hex unicode escaped character
<0>  '\\[xX][0-9a-fA-F]+'
        { report bufferedComments `andThen` produce escapedChar }

-- A decimal unicode escaped character
<0>  '\\[1-9][0-9]*'
        { report bufferedComments `andThen` produce escapedChar }

-- An octal unicode escaped character
<0>  '\\0[0-7]+'
        { report bufferedComments `andThen` produce escapedChar }

-- A binary unicode escaped character
<0>  '\\[bB][01]+'
        { report bufferedComments `andThen` produce escapedChar }

-- Invalid escaped character literal
<0>       '\\[^\'\n]+'
        { log badEscape `andThen` skip }

-- Newline in character literal
<0>       '\r?\n'
        { linebreakAtOffset 1 `andThen`
	  report newlineCharLiteral `andThen` skip }

-- Hard tab in character literal
<0>       '\t'
        { report tabCharLiteral `andThen` skip }

-- Unescaped character literal
<0>       '[^\n\']{2,}'
        { log longCharLiteral `andThen` skip }

-- Comment begin
<0>       \/\*
        { log startComment `andThen` begin comment }

-- String literal begin
<0>       \"
        { report bufferedComments `andThen`
	  report startString `andThen` begin string  }

-- Warn about hard tabs, but record them in the string buffer
<string>  \t+
        { report tabInStringLiteral `andThen`
	  record stringContent `andThen` skip }

-- Unescaped newlines in strings are an error, but keep going
<string>  \r?\n
        { linebreak `andThen` report newlineInString `andThen` skip }

-- Newlines can be escaped
<string>  \\\r?\n
        { linebreak `andThen` skip }

-- Unescaped characters in the string
<string>  [^\t\r\n\\\"]+
        { record stringContent `andThen` skip }

-- A valid escape sequence
<string>  \\[abfnrtv\\'\"\?]
        { record escapedStringContent `andThen` skip }

-- A bad escape sequence
<string>  \\[^abfnrtv\\'\"\?]
        { log badEscape `andThen` skip }

-- A hex unicode escape
<string>  \\[xX][0-9a-fA-F]+
        { record escapedStringContent `andThen` skip }

-- A decimal unicode escape
<string>  \\[1-9][0-9]*
        { record escapedStringContent `andThen` skip }

-- An octal unicode escape
<string>  \\0[0-7]+
        { record escapedStringContent `andThen` skip }

-- A binary unicode escpae
<string>  \\[bB][01]+
        { record escapedStringContent `andThen` skip }

-- Close-quote, close out the string, convert it to a token, leave string mode
<string>  \"
	{ token bufferedString `andBegin` 0 }

-- Record linebreaks for comments, append them to the comment buffer
<comment> \r?\n
        { linebreak `andThen` record commentText `andThen` skip }

-- Warn about trailing whitespace, even in comments
<comment> [\ ]+$
        { record commentText `andThen`
	  report trailingWhitespace `andThen` skip }

-- Nest comments one level deeper
<comment> \/\*
        { record enterComment `andThen` skip }

-- Decrement level of comment nesting, possibly close the comment
<comment> \*\/
        { record leaveComment `andThen` skip }

-- Warn about hard tabs, even in comments, but append them anyway
<comment> \t+
        { report hardTabs `andThen` record commentText `andThen` skip }

-- Record and skip everything else
<comment> [^\t\n\/\*]+
        { record commentText `andThen` skip }

-- Allow individual stars and slashes that don't form a /* or a */
<comment> [\*\/]
        { record commentText `andThen` skip }

{

hexLiteral :: Lazy.ByteString -> Position -> Lexer Token
hexLiteral bstr = return . Num (Numbers.hexLiteral (Lazy.drop 2 bstr))

decLiteral :: Lazy.ByteString -> Position -> Lexer Token
decLiteral bstr = return . Num (Numbers.decLiteral bstr)

octLiteral :: Lazy.ByteString -> Position -> Lexer Token
octLiteral bstr = return . Num (Numbers.octLiteral (Lazy.drop 1 bstr))

binLiteral :: Lazy.ByteString -> Position -> Lexer Token
binLiteral bstr = return . Num (Numbers.binLiteral (Lazy.drop 2 bstr))

singleChar :: Lazy.ByteString -> Position -> Lexer Token
singleChar bstr =
  case Lazy.toString bstr of
    "(" -> return . LParen
    ")" -> return . RParen
    "[" -> return . LBrack
    "]" -> return . RBrack
    "{" -> return . LBrace
    "}" -> return . RBrace
    "." -> return . Dot
    "," -> return . Comma
    ";" -> return . Semicolon
    ":" -> return . Colon
    "\2200" -> return . Forall
    "\2203" -> return . Exists
    str -> error $! "Unexpected single character " ++ str

-- | Produce a character literal from an unescaped character
unescapedChar :: Lazy.ByteString -> Position -> Lexer Token
unescapedChar bstr =
  case Lazy.toString bstr of
    ['\'', chr, '\''] -> return . (Character chr)
    _ -> error $! "Extra content in character literal " ++ show bstr

-- | Produce a character literal from an escaped character
escapedChar :: Lazy.ByteString -> Position -> Lexer Token
escapedChar bstr = return . Character (fromEscape (Lazy.tail (Lazy.init bstr)))

-- | Start a string literal
startString :: Position -> Lexer ()
startString pos =
  do
    us <- alexGetUserState
    alexSetUserState us { userStringBuf = [], userStartPos = pos }

-- | Add to the string literal buffer
stringContent :: Lazy.ByteString -> Lexer ()
stringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf = str : buf }

-- | Add an escaped character to the string literal buffer
escapedStringContent :: Lazy.ByteString -> Lexer ()
escapedStringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf =
                            Lazy.fromString [fromEscape str] : buf }

-- | Terminate a string literal and return a token.
bufferedString :: Position -> Lexer Token
bufferedString endpos =
  do
    UserState { userStartPos = startpos,
                userStringBuf = buf } <- alexGetUserState
    pos <- span startpos endpos
    return (String (Lazy.toStrict (Lazy.concat (reverse buf))) pos)

-- | Start a new comment
startComment :: Lazy.ByteString -> Position -> Lexer ()
startComment bstr pos =
  do
    us <- alexGetUserState
    alexSetUserState us { userCommentDepth = 0, userStartPos = pos }
    CommentBuffer.startComment
    CommentBuffer.appendComment bstr

-- | Append comment text to the current comment
commentText :: Lazy.ByteString -> Lexer ()
commentText = CommentBuffer.appendComment

-- | Record an nested opening comment
enterComment :: Lazy.ByteString -> Lexer ()
enterComment bstr =
  do
    us @ UserState { userCommentDepth = depth } <- alexGetUserState
    alexSetUserState us { userCommentDepth = depth + 1 }
    CommentBuffer.appendComment bstr

-- | Record a possibly nested close comment
leaveComment :: Lazy.ByteString -> Lexer ()
leaveComment bstr =
  do
    CommentBuffer.appendComment bstr
    us @ UserState { userCommentDepth = depth } <- alexGetUserState
    if depth == 0
      then do
        CommentBuffer.finishComment
	alexSetStartCode 0
      else
        alexSetUserState us { userCommentDepth = depth - 1 }

-- | Add a full comment to the previous comments buffer
fullComment :: Lazy.ByteString -> Lexer ()
fullComment = CommentBuffer.addComment

-- | Save previous comments at the given position and clear the
-- comment buffer (this is used in conjuction with @report@,
-- ie. @report bufferedComments@)
bufferedComments :: Position -> Lexer ()
bufferedComments pos = CommentBuffer.saveCommentsAsPreceeding pos >>
		       CommentBuffer.clearComments

type Frontend = MessagesT [Message] Message (Frontend.Frontend Token)

type Lexer = AlexT UserState Frontend

data UserState =
  UserState {
    -- | Buffer for accumulating string constants.
    userStringBuf :: ![Lazy.ByteString],
    -- | Position at which comments or string constants began.  Used
    -- for reporting unterminated constants.
    userStartPos :: Position,
    userCommentDepth :: !Word
  }

initUserState :: UserState
initUserState = UserState { userStringBuf = [], userStartPos = undefined,
                            userCommentDepth = 0 }

type AlexAction = AlexMonadAction Lexer Token

type AlexState = AlexInternalState UserState

scanWrapper :: AlexResultHandlers Lexer Token ->
               AlexInput -> Int -> Lexer Token
scanWrapper handlers inp sc =
  case alexScan inp sc of
    AlexEOF -> handleEOF handlers
    AlexError inp' -> handleError handlers inp'
    AlexSkip inp' len -> handleSkip handlers inp' len
    AlexToken inp' len action -> handleToken handlers inp' len action

alexEOF :: Lexer Token
alexEOF =
  do
    startcode <- alexGetStartCode
    if startcode /= 0
      then do
        UserState { userStartPos = pos } <- alexGetUserState
        if startcode == comment
          then untermComment pos
          else if startcode == string
            then untermString pos
            else error $! "Unknown start code " ++ show startcode
      else return ()
    return EOF

alexMonadScan :: Lexer Token

skip :: AlexAction

begin :: Int -> AlexAction

AlexActions { actAlexMonadScan = alexMonadScan, actSkip = skip,
              actBegin = begin } =
  mkAlexActions scanWrapper badChars alexEOF

-- | Lexer function required by Happy threaded lexers.
lexer :: (Token -> Lexer a)
      -- ^ A continuation that will receieve the next token.
      -> Lexer a
lexer = (alexMonadScan >>=)

-- | Run the lexer completely.  Expects to be wrapped in 'startFile'
-- and 'finishFile' appropriately.
lex :: Strict.ByteString -> Lazy.ByteString -> Frontend [Token]
lex name input =
  let
    cont :: [Token] -> Token -> Lexer [Token]
    cont accum EOF = return (reverse (EOF : accum))
    cont accum tok = lexer (cont (tok : accum))

    run =
      do
        startFile name input
        out <- lexer (cont [])
	finishFile
	return out
  in
    runAlexT run input name initUserState
}
