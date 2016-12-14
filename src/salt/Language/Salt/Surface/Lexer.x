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
{
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-tabs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- |
-- = Lexer
--
-- This module defines the lexer.  It is designed primarily for use by
-- the (Happy-based) parser defined by "Language.Salt.Surface.Parser",
-- and thus its interface conforms to what Happy expects.
--
-- The lexer produces tokens defined in "Language.Salt.Surface.Token".
module Language.Salt.Surface.Lexer(
       -- * Types
       MonadFrontend,
       Lexer,

       -- * Lexer Interface
       runLexerWithTokens,
       runLexer,

       -- ** Low-Level Interface
       lex,
       lexRemaining,
       ) where

import Control.Monad.CommentBuffer(MonadCommentBuffer)
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords(MonadKeywords)
import Control.Monad.Messages
import Control.Monad.SourceBuffer hiding (linebreak)
import Control.Monad.Symbols
import Control.Monad.State
import Control.Monad.Trans
import Data.Maybe
import Data.Position.BasicPosition
import Data.Symbol
import Language.Salt.Frontend
import Language.Salt.Message(Message)
import Language.Salt.Surface.Token
import Prelude hiding (log, span, lex)
import System.IO
import Text.Escapes.ByteString.Lazy

import qualified Control.Monad.CommentBuffer as CommentBuffer
import qualified Control.Monad.Keywords as Keywords
import qualified Data.ByteString.Lazy.UTF8 as Lazy
import qualified Data.ByteString.Lazy as Lazy hiding (uncons, drop)
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.Position as Position
import qualified Language.Salt.Message as Message
import qualified Text.Numbers.ByteString.Lazy as Numbers

import Text.AlexWrapper

}

%typeclass "MonadFrontend m"
%action "AlexMonadAction (Lexer m) Token"

-- Operator characters
@opchars = [\!\#\%\^\&\+\?\<\>\:\|\~\-\=\:]

-- Valid escape sequences
@escape_chars = [abfnrtv\\'\"\?]

--
@comment_ignore = ((\**|\/*)[^\t\n\/\*]+(\**|\/*))

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

-- Text, beginning with an alphabetic character, possibly ending with
-- a quote.  This is an idenitfier or a keyword.
<0>       [_a-zA-Z][_a-zA-Z0-9]*[']*
        { report bufferedComments `andThen` produce keywordOrToken }

-- Operators, cannot contain a /*, a //, or a */ anywhere, otherwise all
-- sequences are valid
<0>       @opchars*(\/|\*+|@opchars)(@opchars+(\/|\*+)?)*
        { report bufferedComments `andThen` produce keywordOrToken }

-- Single-character punctuation
<0>       [\.\(\)\[\]\{\}\,\;\:]
        { report bufferedComments `andThen` produce singleChar }

-- Ellipsis
<0>       \.\.\.
        { report bufferedComments `andThen` token ellipsis }

-- Decimal literals
<0>       [\-\+]?[1-9][0-9]*(\.[0-9]+)?([ep][\-\+]?[0-9]+)?
        { report bufferedComments `andThen` produce decLiteral }

-- Octal literals
<0>       [\-\+]?0[0-7]*(\.[0-7]+)?([ep][\-\+]?[0-7]+)?
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
<0>       '\\[xX][0-9a-fA-F]+'
        { report bufferedComments `andThen` produce escapedChar }

-- A decimal unicode escaped character
<0>       '\\[1-9][0-9]*'
        { report bufferedComments `andThen` produce escapedChar }

-- An octal unicode escaped character
<0>       '\\0[0-7]+'
        { report bufferedComments `andThen` produce escapedChar }

-- A binary unicode escaped character
<0>       '\\[bB][01]+'
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
<comment> @comment_ignore?\/\*
        { record enterComment `andThen` skip }

-- Decrement level of comment nesting, possibly close the comment
<comment> @comment_ignore?\*\/
        { record leaveComment `andThen` skip }

-- Warn about hard tabs, even in comments, but append them anyway
<comment> \t+
        { report hardTabs `andThen` record commentText `andThen` skip }

-- Record and skip everything else
<comment> @comment_ignore
        { record commentText `andThen` skip }

{

type Position = BasicPosition

hexLiteral :: Monad m =>
              Lazy.ByteString -> Position.Filename -> Position.Point ->
              Position.Point -> m Token
hexLiteral bstr _ startpos endpos =
  return $! Num (Numbers.hexLiteral (Lazy.drop 2 bstr)) (span startpos endpos)

decLiteral :: Monad m =>
              Lazy.ByteString -> Position.Filename -> Position.Point ->
              Position.Point -> m Token
decLiteral bstr _ startpos endpos =
  return $! Num (Numbers.decLiteral bstr) (span startpos endpos)

octLiteral :: Monad m =>
              Lazy.ByteString -> Position.Filename -> Position.Point ->
              Position.Point -> m Token
octLiteral bstr _ startpos endpos =
  return $! Num (Numbers.octLiteral (Lazy.drop 1 bstr)) (span startpos endpos)

binLiteral :: Monad m =>
              Lazy.ByteString -> Position.Filename -> Position.Point ->
              Position.Point -> m Token
binLiteral bstr _ startpos endpos =
  return $! Num (Numbers.binLiteral (Lazy.drop 2 bstr)) (span startpos endpos)

ellipsis :: Monad m =>
            Position.Filename -> Position.Point -> Position.Point -> m Token
ellipsis _ startpos endpos =
  return $! Ellipsis (span startpos endpos)

singleChar :: Monad m =>
              Lazy.ByteString -> Position.Filename -> Position.Point ->
              Position.Point -> m Token
singleChar bstr _ startpos endpos =
  let
    pos = span startpos endpos
  in case Lazy.toString bstr of
    "(" -> return $! LParen pos
    ")" -> return $! RParen pos
    "[" -> return $! LBrack pos
    "]" -> return $! RBrack pos
    "{" -> return $! LBrace pos
    "}" -> return $! RBrace pos
    "." -> return $! Dot pos
    "," -> return $! Comma pos
    ";" -> return $! Semicolon pos
    ":" -> return $! Colon pos
    "\2200" -> return $! Forall pos
    "\2203" -> return $! Exists pos
    str -> error $! "Unexpected single character " ++ str

keywordOrToken :: (MonadKeywords BasicPosition Token m, MonadGensym m) =>
                  Lazy.ByteString -> Position.Filename -> Position.Point ->
                  Position.Point -> m Token
keywordOrToken str _ startpos endpos =
  Keywords.keywordOrToken Id str (span startpos endpos)

-- | Produce a character literal from an unescaped character
unescapedChar :: Monad m =>
                 Lazy.ByteString -> Position.Filename -> Position.Point ->
                 Position.Point -> m Token
unescapedChar bstr _ startpos endpos =
  let
    pos = span startpos endpos
  in case Lazy.toString bstr of
    ['\'', chr, '\''] -> return $! (Character chr) pos
    _ -> error $! "Extra content in character literal " ++ show bstr

-- | Produce a character literal from an escaped character
escapedChar :: Monad m =>
               Lazy.ByteString -> Position.Filename -> Position.Point ->
               Position.Point -> m Token
escapedChar bstr _ startpos endpos =
  let
    pos = span startpos endpos
  in
    return $! Character (fromEscape (Lazy.tail (Lazy.init bstr))) pos

-- | Start a string literal
startString :: MonadState AlexState m =>
               Position.Filename -> Position.Point -> Position.Point -> m ()
startString _ pos _ =
  do
    us <- alexGetUserState
    alexSetUserState us { userStringBuf = [],
                          userStartPos = pos }

-- | Add to the string literal buffer
stringContent :: MonadState AlexState m =>
                 Lazy.ByteString -> m ()
stringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf = str : buf }

-- | Add an escaped character to the string literal buffer
escapedStringContent :: MonadState AlexState m =>
                        Lazy.ByteString -> m ()
escapedStringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf =
                            Lazy.fromString [fromEscape str] : buf }

-- | Report trailing whitespace.
trailingWhitespace :: MonadMessages Message m =>
                      Position.Filename -> Position.Point ->
                      Position.Point -> m ()
trailingWhitespace _ startpos endpos =
  Message.trailingWhitespace (span startpos endpos)

-- | Report bad characters.
badChars :: MonadMessages Message m =>
            Lazy.ByteString -> Position.Filename ->
            Position.Point -> Position.Point -> m ()
badChars chars _ startpos endpos =
  Message.badChars chars (span startpos endpos)

-- | Report hard tabs.
hardTabs :: MonadMessages Message m =>
            Position.Filename -> Position.Point -> Position.Point -> m ()
hardTabs _ startpos endpos = Message.hardTabs (span startpos endpos)

-- | Report an empty character literal.
emptyCharLiteral :: MonadMessages Message m =>
                    Position.Filename -> Position.Point ->
                    Position.Point -> m ()
emptyCharLiteral _ startpos endpos =
  Message.emptyCharLiteral (span startpos endpos)

-- | Report bad escape sequence.
badEscape :: MonadMessages Message m =>
             Lazy.ByteString -> Position.Filename ->
             Position.Point -> Position.Point -> m ()
badEscape chars _ startpos endpos =
  Message.badEscape chars (span startpos endpos)

-- | Report a newline in a character literal.
newlineCharLiteral :: MonadMessages Message m =>
                      Position.Filename -> Position.Point ->
                      Position.Point -> m ()
newlineCharLiteral _ startpos endpos =
  Message.newlineCharLiteral (span startpos endpos)

-- | Report a tab in a character literal.
tabCharLiteral :: MonadMessages Message m =>
                  Position.Filename -> Position.Point ->
                  Position.Point -> m ()
tabCharLiteral _ startpos endpos = Message.tabCharLiteral (span startpos endpos)

-- | Report a long character literal.
longCharLiteral :: MonadMessages Message m =>
                   Lazy.ByteString -> Position.Filename ->
                   Position.Point -> Position.Point -> m ()
longCharLiteral chars _ startpos endpos =
  Message.longCharLiteral chars (span startpos endpos)

-- | Report a tab in a string literal.
tabInStringLiteral :: MonadMessages Message m =>
                      Position.Filename -> Position.Point ->
                      Position.Point -> m ()
tabInStringLiteral _ startpos endpos =
  Message.tabInStringLiteral (span startpos endpos)

-- | Report a newline in a string literal.
newlineInString :: MonadMessages Message m =>
                   Position.Filename -> Position.Point ->
                   Position.Point -> m ()
newlineInString _ startpos endpos =
  Message.newlineInString (span startpos endpos)

-- | Terminate a string literal and return a token.
bufferedString :: MonadState AlexState m =>
                  Position.Filename -> Position.Point ->
                  Position.Point -> m Token
bufferedString _ _ endpos =
  do
    UserState { userStartPos = startpos,
                userStringBuf = buf } <- alexGetUserState
    return (String (Lazy.toStrict (Lazy.concat (reverse buf)))
                   (span startpos endpos))

-- | Start a new comment
startComment :: (MonadCommentBuffer m, MonadState AlexState m) =>
                Lazy.ByteString -> Position.Filename -> Position.Point ->
                Position.Point -> m ()
startComment bstr _ pos _ =
  do
    us <- alexGetUserState
    alexSetUserState us { userCommentDepth = 0, userStartPos = pos }
    CommentBuffer.startComment
    CommentBuffer.appendComment bstr

-- | Append comment text to the current comment
commentText :: (MonadCommentBuffer m, MonadState AlexState m) =>
                Lazy.ByteString -> m ()
commentText = CommentBuffer.appendComment

-- | Record an nested opening comment
enterComment :: (MonadCommentBuffer m, MonadState AlexState m) =>
                Lazy.ByteString -> m ()
enterComment bstr =
  do
    us @ UserState { userCommentDepth = depth } <- alexGetUserState
    alexSetUserState us { userCommentDepth = depth + 1 }
    CommentBuffer.appendComment bstr

-- | Record a possibly nested close comment
leaveComment :: (MonadCommentBuffer m, MonadState AlexState m) =>
                Lazy.ByteString -> m ()
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
fullComment :: (MonadCommentBuffer m, MonadState AlexState m) =>
               Lazy.ByteString -> m ()
fullComment = CommentBuffer.addComment

-- | Save previous comments at the given position and clear the
-- comment buffer (this is used in conjuction with @report@,
-- ie. @report bufferedComments@)
bufferedComments :: (MonadCommentBuffer m, MonadState AlexState m) =>
                    Position.Filename -> Position.Point ->
                    Position.Point -> m ()
bufferedComments _ pos _ = CommentBuffer.saveCommentsAsPreceeding pos >>
                           CommentBuffer.clearComments

-- | A synonym for the monad used by the lexer.
type Lexer m = AlexT UserState m

data UserState =
  UserState {
    -- | Buffer for accumulating string constants.
    userStringBuf :: ![Lazy.ByteString],
    -- | Position at which comments or string constants began.  Used
    -- for reporting unterminated constants.
    userStartPos :: Position.Point,
    -- | Comment depth.
    userCommentDepth :: !Word,
    -- | Token buffer.
    userTokenBuf :: ![Token],
    -- | Whether or not to save tokens.
    userSaveTokens :: !Bool
  }

initUserState :: Bool -> UserState
initUserState saveTokens =
  UserState { userStringBuf = [], userStartPos = undefined, userTokenBuf = [],
              userCommentDepth = 0, userSaveTokens = saveTokens }

-- | An abbreviation for the (very long) list of type classes used by
-- the lexer and the parser.
class (MonadMessages Message m, MonadGensym m, MonadGenpos m,
       MonadCommentBuffer m, MonadSourceBuffer m,
       MonadKeywords BasicPosition Token m) => MonadFrontend m

instance (MonadMessages Message m, MonadGensym m, MonadGenpos m,
          MonadCommentBuffer m, MonadSourceBuffer m,
          MonadKeywords BasicPosition Token m) => MonadFrontend m

type AlexState = AlexInternalState UserState

scanWrapper :: (MonadMessages Message m, MonadGensym m, MonadGenpos m,
                MonadCommentBuffer m, MonadSourceBuffer m,
                MonadKeywords BasicPosition Token m) =>
               AlexResultHandlers (Lexer m) Token ->
               AlexInput -> Int -> Lexer m Token
scanWrapper handlers inp sc =
  case alexScan inp sc of
    AlexEOF -> handleEOF handlers
    AlexError inp' -> handleError handlers inp'
    AlexSkip inp' len -> handleSkip handlers inp' len
    AlexToken inp' len action -> handleToken handlers inp' len action

alexEOF :: (MonadMessages Message m, MonadState AlexState m) => m Token
alexEOF =
  do
    startcode <- alexGetStartCode
    if startcode /= 0
      then do
        UserState { userStartPos = pos } <- alexGetUserState
        if startcode == comment
          then Message.untermComment Point { pointPos = pos }
          else if startcode == string
            then Message.untermString Point { pointPos = pos }
            else error $! "Unknown start code " ++ show startcode
      else return ()
    return EOF

actions :: MonadFrontend m =>
           AlexActions (Lexer m) Token
actions = mkAlexActions scanWrapper badChars alexEOF

alexMonadScan :: MonadFrontend m =>
                 Lexer m Token
alexMonadScan = actAlexMonadScan actions

skip :: MonadFrontend m =>
        AlexMonadAction (Lexer m) Token
skip = actSkip actions

begin :: MonadFrontend m =>
         Int -> AlexMonadAction (Lexer m) Token
begin = actBegin actions

-- | Lexer function required by Happy threaded lexers.
lex :: MonadFrontend m =>
       Lexer m Token
lex =
  do
    us @ UserState { userSaveTokens = saveToks,
                     userTokenBuf = tokbuf } <- alexGetUserState
    tok <- alexMonadScan
    if saveToks
      then do
        alexSetUserState us { userTokenBuf = tok : tokbuf }
        return tok
      else
        return tok

-- | Lex all remaining tokens.  This is primarily used to consume the
-- rest of input in the event of a parse error, thereby buffering
-- everything and reporting any lexical errors past the parse error.
-- It can also be used in conjunction with 'runLexer' or
-- 'runLexerWithTokens' to lex an entire file.
lexRemaining :: (MonadMessages Message m, MonadGensym m, MonadGenpos m,
                 MonadCommentBuffer m, MonadSourceBuffer m,
                 MonadKeywords BasicPosition Token m) =>
                Lexer m ()
lexRemaining =
  do
    tok <- lex
    case tok of
      EOF -> return ()
      _ -> lexRemaining

-- | Run the lexer on input from a file, producing the tokens in
-- addition to the usual output.
--
-- Use with 'lexRemaining' in order to lex an entire file and produce
-- the token stream.
runLexerWithTokens :: (MonadSourceBuffer m) =>
                      Lexer m a
                   -- ^ The lexer monad to run.
                   -> Position.Filename
                   -- ^ The name of the file.
                   -> Lazy.ByteString
                   -- ^ The contents of the file.
                   -> m (a, [Token])
runLexerWithTokens l name input =
  let
    initState = initUserState True

    run =
      do
        startFile name input
        out <- l
        us @ UserState { userTokenBuf = tokbuf } <- alexGetUserState
        finishFile
        return (out, tokbuf)
  in do
    runAlexT run input name initState

-- | Run the lexer on input from a file.
--

-- Use with 'lexRemaining' in order to lex an entire file, buffering
-- it and reporting any errors.  Note that you won't get any output
-- this way; see 'runLexerWithTokens' to get the token stream.
runLexer :: (MonadSourceBuffer m) =>
            Lexer m a
         -- ^ The lexer monad to run.
         -> Position.Filename
         -- ^ The name of the file.
         -> Lazy.ByteString
         -- ^ The contents of the file.
         -> m a
runLexer l name input =
  let
    initState = initUserState False

    run =
      do
        startFile name input
        out <- l
        finishFile
        return out
  in do
    runAlexT run input name initState

}
