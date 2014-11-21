{
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Salt.Surface.Lexer(
       SaltLexer,
       lexer,
       lex
       ) where

import Control.Monad.Lexer hiding (startComment)
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
import qualified Data.ByteString.Lazy.UTF8 as Lazy
import qualified Data.ByteString.Lazy as Lazy hiding (uncons)
import qualified Data.ByteString.UTF8 as Strict
import qualified Text.Numbers.ByteString.Lazy as Numbers

import Text.AlexWrapper

}

@opchars = [\!\#\%\^\&\+\?\<\>\:\|\~\-\=\:]
@escape_chars = [^0-9]

tokens :-

-- Record linebreaks at newlines
<0>       \r?\n
        { linebreak `andThen` skip }

-- Newlines preceeded by whitespace emit a warning
<0>       [\ ]+\r?\n
        { linebreak `andThen` report trailingWhitespace `andThen` skip }

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
<0>       [\-\+]?[1-9][0-9]*(\.[0-9]+)?(e[\-\+]?[0-9]+)?
        { report bufferedComments `andThen` produce decLiteral }

-- Octal literals
<0>       [\-\+]?0[0-7]+(\.[0-7]+)?(e[\-\+]?[0-7]+)?
        { report bufferedComments `andThen` produce octLiteral }

-- Hex literals
<0>       [\-\+]?0[xX][0-9a-fA-F]+(\.[0-9a-fA-F]+)?(e[\-\+]?[0-9a-fA-F]+)?
        { report bufferedComments `andThen` produce hexLiteral }

-- Binary literals
<0>       [\-\+]?0[bB][01]+(\.[01]+)?(e[\-\+]?[01]+)?
        { report bufferedComments `andThen` produce binLiteral }

-- Line comments, skip the rest of the line, add it to the comment buffer
<0>       \/\/[^\n]*
        { record fullComment `andThen` skip }

-- Unescaped character literal
<0>       '[^\\\n\r\t\b]'
        { report bufferedComments `andThen` produce unescapedChar }

-- Escaped character literal
<0>       '\\(@escape_chars)'
        { report bufferedComments `andThen` produce escapedChar }

-- Comment begin
<0>       \/\*
        { log startComment `andThen` begin comment }

-- String literal begin
<0>       \"
        { report bufferedComments `andThen`
	  report startString `andThen` begin string  }

-- Warn about hard tabs, but record them in the string buffer
<string>  \t+
        { report hardTabs `andThen` record stringContent `andThen` skip }

-- Unescaped newlines in strings are an error, but keep going
<string>  \r?\n
        { linebreak `andThen` report newlineInString `andThen` skip }

-- Newlines can be escaped
<string>  \\\r?\n
        { linebreak `andThen` skip }

-- Unescaped characters in the string
<string>  [^\t\r\n\\\"]+
        { record stringContent `andThen` skip }

-- An escape sequence
<string>  \\@escape_chars
        { record escapedStringContent `andThen` skip }

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
<comment> [\ ]+\r?\n
        { linebreak `andThen` record commentText `andThen`
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
<comment> [^\t\/\*]+
        { record commentText `andThen` skip }

-- Allow individual stars and slashes that don't form a /* or a */
<comment> [\*\/]
        { record commentText `andThen` skip }

{

hexLiteral :: Lazy.ByteString -> Position -> SaltLexer Token
hexLiteral bstr = return . Num (Numbers.hexLiteral bstr)

decLiteral :: Lazy.ByteString -> Position -> SaltLexer Token
decLiteral bstr = return . Num (Numbers.decLiteral bstr)

octLiteral :: Lazy.ByteString -> Position -> SaltLexer Token
octLiteral bstr = return . Num (Numbers.octLiteral bstr)

binLiteral :: Lazy.ByteString -> Position -> SaltLexer Token
binLiteral bstr = return . Num (Numbers.binLiteral bstr)

singleChar :: Lazy.ByteString -> Position -> SaltLexer Token
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
unescapedChar :: Lazy.ByteString -> Position -> SaltLexer Token
unescapedChar bstr =
  case Lazy.uncons bstr of
    Just (chr, rest)
      | Lazy.null rest -> return . (Character chr)
      | otherwise -> error $! "Extra content in string " ++ show rest
    Nothing -> error $! "Couldn't decode string " ++ show bstr

-- | Produce a character literal from an escaped character
escapedChar :: Lazy.ByteString -> Position -> SaltLexer Token
escapedChar bstr = return . Character (fromEscape bstr)

-- | Start a string literal
startString :: Position -> SaltLexer ()
startString pos =
  do
    us <- alexGetUserState
    alexSetUserState us { userStringBuf = [], userStartPos = pos }

-- | Add to the string literal buffer
stringContent :: Lazy.ByteString -> SaltLexer ()
stringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf = str : buf }

-- | Add an escaped character to the string literal buffer
escapedStringContent :: Lazy.ByteString -> SaltLexer ()
escapedStringContent str =
  do
    us @ UserState { userStringBuf = buf } <- alexGetUserState
    alexSetUserState us { userStringBuf =
                            Lazy.fromString [fromEscape str] : buf }

-- | Terminate a string literal and return a token.
bufferedString :: Position -> SaltLexer Token
bufferedString endpos =
  do
    UserState { userStartPos = startpos,
                userStringBuf = buf } <- alexGetUserState
    pos <- span startpos endpos
    return (String (Lazy.toStrict (Lazy.concat (reverse buf))) pos)

-- | Start a new comment
startComment :: Lazy.ByteString -> Position -> SaltLexer ()
startComment bstr pos =
  do
    us <- alexGetUserState
    alexSetUserState us { userCommentDepth = 0, userStartPos = pos }
    CommentBuffer.startComment
    CommentBuffer.appendComment bstr

-- | Append comment text to the current comment
commentText :: Lazy.ByteString -> SaltLexer ()
commentText = CommentBuffer.appendComment

-- | Record an nested opening comment
enterComment :: Lazy.ByteString -> SaltLexer ()
enterComment bstr =
  do
    us @ UserState { userCommentDepth = depth } <- alexGetUserState
    alexSetUserState us { userCommentDepth = depth + 1 }
    CommentBuffer.appendComment bstr

-- | Record a possibly nested close comment
leaveComment :: Lazy.ByteString -> SaltLexer ()
leaveComment bstr =
  do
    CommentBuffer.appendComment bstr
    us @ UserState { userCommentDepth = depth } <- alexGetUserState
    alexSetUserState us { userCommentDepth = depth - 1 }
    if depth == 1
      then do
        CommentBuffer.finishComment
	alexSetStartCode 0
      else
        return ()

-- | Add a full comment to the previous comments buffer
fullComment :: Lazy.ByteString -> SaltLexer ()
fullComment = CommentBuffer.addComment

-- | Save previous comments at the given position and clear the
-- comment buffer (this is used in conjuction with @report@,
-- ie. @report bufferedComments@)
bufferedComments :: Position -> SaltLexer ()
bufferedComments pos = CommentBuffer.saveCommentsAsPreceeding pos >>
		       CommentBuffer.clearComments

type SaltLexer = AlexT UserState (MessagesT [Message] Message (Lexer Token))

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

type AlexAction = AlexMonadAction SaltLexer Token

type AlexState = AlexInternalState UserState

scanWrapper :: AlexResultHandlers SaltLexer Token ->
               AlexInput -> Int -> SaltLexer Token
scanWrapper handlers inp sc =
  case alexScan inp sc of
    AlexEOF -> handleEOF handlers
    AlexError inp' -> handleError handlers inp'
    AlexSkip inp' len -> handleSkip handlers inp' len
    AlexToken inp' len action -> handleToken handlers inp' len action

alexEOF :: SaltLexer Token
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

alexMonadScan :: SaltLexer Token

skip :: AlexAction

begin :: Int -> AlexAction

AlexActions { actAlexMonadScan = alexMonadScan, actSkip = skip,
              actBegin = begin } =
  mkAlexActions scanWrapper badChars alexEOF

-- | Lexer function required by Happy threaded lexers.
lexer :: (Token -> SaltLexer a)
      -- ^ A continuation that will receieve the next token.
      -> SaltLexer a
lexer = (alexMonadScan >>=)

-- | Run the lexer completely.  Expects to be wrapped in 'startFile'
-- and 'finishFile' appropriately.
lex :: SaltLexer [Token]
lex =
  let
    cont :: [Token] -> Token -> SaltLexer [Token]
    cont accum EOF = return (reverse (EOF : accum))
    cont accum tok = lexer (cont (tok : accum))
  in
    lexer (cont [])
}
