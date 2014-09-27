{
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer(lexer) where

import Data.Array
import Data.Bits
import Data.Char

}

%wrapper "monadUserState"

@opchars = [\!\#\%\^\&\+\?\<\>\:\|\~\-\=\:]
@escape_chars = [^0-9]

tokens :-

<0>        $white+
        { skip }
<0>        [_a-zA-Z][_a-zA-Z0-9]*[']*
        { makeToken Ident }
<0>        @opchars*(\/|\*+|@opchars)(@opchars+(\/|\*+)?)*
        { makeToken Operator }
<0>        [\.\(\)\[\]\{\}\,\;\`]
        { singleChar }
<0>       [\-\+]?[1-9][0-9]*(\.[0-9]+)?(e[\-\+]?[0-9]+)?
        { makeToken DecLit }
<0>       [\-\+]?0[0-7]+(\.[0-7]+)?(e[\-\+]?[0-7]+)?
        { makeToken OctLit }
<0>       [\-\+]?0[xX][0-9a-fA-F]+(\.[0-9a-fA-F]+)?(e[\-\+]?[0-9a-fA-F]+)?
        { makeToken HexLit }
<0>       [\-\+]?0[bB][01]+(\.[01]+)?(e[\-\+]?[01]+)?
        { makeToken BinLit }
<0>       \/\/[^\n]*
        { makeToken Comment }
<0>       \.\.\.
        { makeToken (const Ellipsis) }
<0>       '[^\\\n\r\t\b]'
        { unescapedChar }
<0>       '\\(@escape_chars)'
        { escapedChar }
<0>       \/\*
        { appendComment `andBegin` comment }
<0>       \*\/
        { makeToken (const CommentEnd) }
<0>       \"
        { begin string }

<string>  [^\\\"]+
        { appendString }
<string>  \\@escape_chars
        { appendEscapeChar }
<string>  \\[xX][0-9a-fA-F]+
        { appendHexEscape }
<string>  \\[0-9][0-9]*
        { appendDecEscape }
<string>  \\0[0-7]+
        { appendOctEscape }
<string>  \\[bB][01]+
        { appendBinEscape }
<string>  \"
	{ closeString `andBegin` 0 }

<comment> \/\*
        { enterComment }
<comment> \*\/
        { leaveComment }
<comment> [^\/\*]+
        { appendComment }
<comment> [\*\/]
        { appendComment }
{

data AlexUserState =
  AlexUserState {
    lexerCommentDepth :: !Int,
    lexerStringBuffer :: ![String],
    lexerCommentBuffer :: ![String]
  }

alexInitUserState = AlexUserState { lexerCommentDepth = 0,
		    		    lexerStringBuffer = [],
				    lexerCommentBuffer = [] }

enterComment :: AlexInput -> Int -> Alex Token
enterComment input @ (_, _, _, s) len =
  let
    str = take len s
  in do
    state @ AlexUserState { lexerCommentBuffer = combuf,
                            lexerCommentDepth = depth } <- alexGetUserState
    alexSetUserState state { lexerCommentBuffer = str : combuf,
    		     	     lexerCommentDepth = depth + 1 }
    skip input len

closeString :: AlexInput -> Int -> Alex Token
closeString _ _ =
  do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = [] }
    return (StringLit (concat (reverse strbuf)))

appendString :: AlexInput -> Int -> Alex Token
appendString input @ (_, _, _, s) len =
  let
    str = take len s
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input len

leaveComment :: AlexInput -> Int -> Alex Token
leaveComment input @ (_, _, _, s) len =
  let
    str = take len s
  in do
    state @ AlexUserState { lexerCommentBuffer = combuf,
    	    		    lexerCommentDepth = depth } <- alexGetUserState
    if depth == 0
      then do
        alexSetUserState state { lexerCommentBuffer = [] }
        alexSetStartCode 0
        return (Comment (concat (reverse (str : combuf))))
      else do
        alexSetUserState state { lexerCommentBuffer = str : combuf,
                                 lexerCommentDepth = depth - 1 }
        skip input len

appendComment :: AlexInput -> Int -> Alex Token
appendComment input @ (_, _, _, s) len =
  let
    str = take len s
  in do
    state @ AlexUserState { lexerCommentBuffer = combuf } <- alexGetUserState
    alexSetUserState state { lexerCommentBuffer = str : combuf }
    skip input len

data Token =
    Ident String
  | DecLit String
  | HexLit String
  | OctLit String
  | BinLit String
  | StringLit String
  | Comment String
  | Ellipsis
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Dot
  | Comma
  | Semicolon
  | Backquote
  | CharLit Char
  | CommentEnd
  | Operator String
  | EOF
  deriving Show

singleChar (_, _, _, char : _) 1 =
  case char of
    '(' -> return LParen
    ')' -> return RParen
    '[' -> return LBrack
    ']' -> return RBrack
    '{' -> return LBrace
    '}' -> return RBrace
    '.' -> return Dot
    ',' -> return Comma
    ';' -> return Semicolon
    '`' -> return Backquote

parseHexStr :: String -> Integer
parseHexStr =
  let
    hexValue :: Char -> Integer
    hexValue '0' = 0x0
    hexValue '1' = 0x1
    hexValue '2' = 0x2
    hexValue '3' = 0x3
    hexValue '4' = 0x4
    hexValue '5' = 0x5
    hexValue '6' = 0x6
    hexValue '7' = 0x7
    hexValue '8' = 0x8
    hexValue '9' = 0x9
    hexValue 'a' = 0xa
    hexValue 'A' = 0xa
    hexValue 'b' = 0xb
    hexValue 'B' = 0xb
    hexValue 'c' = 0xc
    hexValue 'C' = 0xc
    hexValue 'd' = 0xd
    hexValue 'D' = 0xd
    hexValue 'e' = 0xe
    hexValue 'E' = 0xe
    hexValue 'f' = 0xf
    hexValue 'F' = 0xf
    hexValue c = error $! ("Unexpected hex character " ++ (c : []))

    parseHex :: Integer -> String -> Integer
    parseHex accum [] = accum
    parseHex accum (first : rest) =
      parseHex ((accum `shiftL` 4) .|. hexValue first) rest
  in
    parseHex 0

parseDecStr :: String -> Integer
parseDecStr =
  let
    decValue :: Char -> Integer
    decValue '0' = 0x0
    decValue '1' = 0x1
    decValue '2' = 0x2
    decValue '3' = 0x3
    decValue '4' = 0x4
    decValue '5' = 0x5
    decValue '6' = 0x6
    decValue '7' = 0x7
    decValue '8' = 0x8
    decValue '9' = 0x9
    decValue c = error $! ("Unexpected decimal character " ++ (c : []))

    parseDec :: Integer -> String -> Integer
    parseDec accum [] = accum
    parseDec accum (first : rest) =
      parseDec ((accum * 10) + decValue first) rest
  in
    parseDec 0

parseOctStr :: String -> Integer
parseOctStr =
  let
    octValue :: Char -> Integer
    octValue '0' = 0x0
    octValue '1' = 0x1
    octValue '2' = 0x2
    octValue '3' = 0x3
    octValue '4' = 0x4
    octValue '5' = 0x5
    octValue '6' = 0x6
    octValue '7' = 0x7
    octValue c = error $! ("Unexpected octal character " ++ (c : []))

    parseOct :: Integer -> String -> Integer
    parseOct accum [] = accum
    parseOct accum (first : rest) =
      parseOct ((accum `shiftL` 3) .|. octValue first) rest
  in
    parseOct 0

parseBinStr :: String -> Integer
parseBinStr =
  let
    binValue :: Char -> Integer
    binValue '0' = 0x0
    binValue '1' = 0x1
    binValue c = error $! ("Unexpected binary character " ++ (c : []))

    parseBin :: Integer -> String -> Integer
    parseBin accum [] = accum
    parseBin accum (first : rest) =
      parseBin ((accum `shiftL` 1) .|. binValue first) rest
  in
    parseBin 0

fromEscapeChar :: Char -> Char
fromEscapeChar 'a' = '\a'
fromEscapeChar 'b' = '\b'
fromEscapeChar 'f' = '\f'
fromEscapeChar 'n' = '\n'
fromEscapeChar 'r' = '\r'
fromEscapeChar 't' = '\t'
fromEscapeChar 'v' = '\v'
fromEscapeChar '\\' = '\\'
fromEscapeChar '\'' = '\''
fromEscapeChar '\"' = '\"'
fromEscapeChar '?' = '?'
fromEscapeChar c = error ("Unrecognized escape sequence \\" ++ (c : []))

appendEscapeChar :: AlexInput -> Int -> Alex Token
appendEscapeChar input @ (_, _, _, '\\' : char : _) 2 =
  let
    str = fromEscapeChar char : []
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input 2

appendHexEscape :: AlexInput -> Int -> Alex Token
appendHexEscape input @ (_, _, _, s) len =
  let
    '\\' : 'x' : hexstr = take len s
    str = chr (fromInteger (parseHexStr hexstr)) : []
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input 2

appendDecEscape :: AlexInput -> Int -> Alex Token
appendDecEscape input @ (_, _, _, s) len =
  let
    '\\' : decstr = take len s
    str = chr (fromInteger (parseDecStr decstr)) : []
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input 2

appendOctEscape :: AlexInput -> Int -> Alex Token
appendOctEscape input @ (_, _, _, s) len =
  let
    '\\' : '0' : octstr = take len s
    str = chr (fromInteger (parseOctStr octstr)) : []
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input 2

appendBinEscape :: AlexInput -> Int -> Alex Token
appendBinEscape input @ (_, _, _, s) len =
  let
    '\\' : 'b' : binstr = take len s
    str = chr (fromInteger (parseBinStr binstr)) : []
  in do
    state @ AlexUserState { lexerStringBuffer = strbuf } <- alexGetUserState
    alexSetUserState state { lexerStringBuffer = str : strbuf }
    skip input 2

unescapedChar :: AlexInput -> Int -> Alex Token
unescapedChar (_, _, _, '\'' : char : '\'' : _) 3 = return (CharLit char)

escapedChar :: AlexInput -> Int -> Alex Token
escapedChar (_, _, _, '\'' : '\\' : char : '\'' : _) 4 =
  return (CharLit (fromEscapeChar char))

makeToken f (_, _, _, s) len = return (f (take len s))

alexEOF = return EOF

lexer :: String -> Either String [Token]
lexer str =
  let
    getInput accum =
      do
        input <- alexMonadScan
        case input of
          EOF -> return (reverse (EOF : accum))
          _ -> getInput (input : accum)
  in do
    runAlex str (getInput [])
}
