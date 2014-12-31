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

module Language.Salt.Surface.Parser(
       parser,
       parserNoTokens
       ) where

import Control.Monad.Error
import Control.Monad.Genpos
import Control.Monad.SourceBuffer hiding (linebreak)
import Control.Monad.Trans
import Data.ByteString(ByteString)
import Data.Either
import Data.Symbol(Symbol)
import Data.Typeable
import Language.Salt.Frontend
import Language.Salt.Surface.AST
import Language.Salt.Surface.Common
import Language.Salt.Surface.Lexer hiding (lexer)
import Language.Salt.Surface.Token(Token)
import Prelude hiding (span, lex)
import System.IO
import Text.XML.Expat.Format
import Text.XML.Expat.Pickle

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy
import qualified Data.ByteString.UTF8 as Strict
import qualified Language.Salt.Surface.Token as Token
import qualified Language.Salt.Message as Message

}

%name parse top_level
%name parseStms stm_list
%tokentype { Token }
%error { parseError }
%monad { Parser }
%lexer { lexer } { Token.EOF }
%token ID { Token.Id _ _ }
       NUM { Token.Num _ _ }
       STRING { Token.String _ _ }
       CHAR { Token.Character _ _ }
       EQUAL { Token.Equal _ }
       ELLIPSIS { Token.Ellipsis _ }
       BAR { Token.Bar _ }
       DOT { Token.Dot _ }
       COLON { Token.Colon _ }
       COMMA { Token.Comma _ }
       SEMICOLON { Token.Semicolon _ }
       LPAREN { Token.LParen _ }
       RPAREN { Token.RParen _ }
--       LBRACK { Token.LBrack _ }
--       RBRACK { Token.RBrack _ }
       LBRACE { Token.LBrace _ }
       RBRACE { Token.RBrace _ }
       LAMBDA { Token.Lambda _ }
       FORALL { Token.Forall _ }
       EXISTS { Token.Exists _ }
       MODULE { Token.Module _ }
       SIGNATURE { Token.Signature _ }
       CLASS { Token.Class _ }
       TYPECLASS { Token.Typeclass _ }
       INSTANCE { Token.Instance _ }
       THEOREM { Token.Theorem _ }
       INVARIANT { Token.Invariant _ }
       AXIOM { Token.Axiom _ }
       PROOF { Token.Proof _ }
       WITH { Token.With _ }
       WHERE { Token.Where _ }
       AS { Token.As _ }
       PRIVATE { Token.Private _ }
       PROTECTED { Token.Protected _ }
       PUBLIC { Token.Public _ }
       MATCH { Token.Match _ }
       LET { Token.Let _ }
       FUN { Token.Fun _ }
       IMPORT { Token.Import _ }
       USE { Token.Use _ }

%right ID NUM STRING CHAR LPAREN LBRACE LBRACK MODULE SIGNATURE CLASS TYPECLASS
       INSTANCE
%right LAMBDA FORALL EXISTS MATCH
%right BAR
%nonassoc COLON WITH
%nonassoc AS
%left WHERE

%%

top_level: use_list def_list
             { AST { astUses = reverse $1, astScope = reverse $2 } }

use_list: use_list USE qual_id SEMICOLON
            {% let
                 (qualname, _) = $3
               in do
                 pos <- span (Token.position $2) (Token.position $4)
                 return (Use { useName = reverse qualname, usePos = pos } : $1)
            }
        | use_list USE qual_id
            {% let
                 (qualname, qualpos) = $3
               in do
                 pos <- span (Token.position $2) qualpos
                 return (Use { useName = reverse qualname, usePos = pos } : $1)
            }
        |
            { [] }

qual_id: qual_id DOT ID
           {% let
                (list, headpos) = $1
              in do
                pos <- span headpos (Token.position $3)
                return (name $3 : list, pos)
           }
       | ID
           { ([ name $1 ], Token.position $1) }

def_list: closed_def_list
            { $1 }
        | open_def_list
            { $1 }
        |
            { [] }

closed_def_list: open_def_list SEMICOLON
                   { $1 }
               | closed_def_list SEMICOLON
                   { $1 }
               | closed_def_list closed_def
                   { $2 : $1 }
               | closed_def
                   { [ $1 ] }

open_def_list: closed_def_list open_def
                 { $2 : $1 }
             | closed_def_list value_def
                 { $2 : $1 }
             | open_def
                 { [ $1 ] }
             | value_def
                 { [ $1 ] }

closed_def: type_builder_kind ID args_opt extends
            LBRACE def_list group_list RBRACE
              {% let
                   groups = case $6 of
                     [] -> return (reverse $7)
                     _ ->
                       do
                         grouppos <- span (elementPosition (last $6))
                                          (elementPosition (head $6))
                         return (Group { groupVisibility = Public,
                                         groupElements = reverse $6,
                                         groupPos = grouppos } :
                                         reverse $7)
                 in do
                   content <- groups
                   builderpos <- span (snd $1) (Token.position $8)
                   return Builder { builderKind = fst $1,
                                    builderName = name $2,
                                    builderSuperTypes = reverse $4,
                                    builderParams = reverse $3,
                                    builderContent = Body content,
                                    builderPos = builderpos }
              }
          | FUN ID case_list pattern LBRACE stm_list RBRACE
              {% do
                   compoundpos <- span (Token.position $5) (Token.position $7)
                   casepos <- span (patternPosition $4) (Token.position $7)
                   funpos <- span (Token.position $1) (Token.position $7)
                   return Fun { funName = name $2,
                                funCases =
                                  reverse (Case {
                                             casePat = $4,
                                             caseBody =
                                               Compound {
                                                 compoundBody = reverse $6,
                                                 compoundPos = compoundpos
                                               },
                                             casePos = casepos } : $3),
                                funPos = funpos }
              }
          | PROOF static_exp LBRACE stm_list RBRACE
              {% do
                   compoundpos <- span (Token.position $3) (Token.position $5)
                   pos <- span (Token.position $1) (Token.position $5)
                   return Proof { proofName = $2,
                                  proofBody =
                                    Compound { compoundBody = reverse $4,
                                               compoundPos = compoundpos },
                                  proofPos = pos }
              }

value_def: pattern
             {% return Def { defPattern = $1, defInit = Nothing,
                             defPos = patternPosition $1 } }
         | pattern EQUAL exp
             {% do
                  pos <- span (patternPosition $1) (expPosition $3)
                  return Def { defPattern = $1, defInit = Just $3,
                               defPos = pos }
             }

open_def: FUN ID case_list pattern EQUAL exp
            {% do
                 casepos <- span (casePosition (head $3)) (expPosition $6)
                 funpos <- span (Token.position $1) (expPosition $6)
                 return Fun { funName = name $2,
                              funCases = reverse (Case { casePat = $4,
                                                         caseBody = $6,
                                                         casePos = casepos } :
                                                  $3),
                              funPos = funpos }
            }
        | type_builder_kind ID args_opt extends EQUAL exp
            {% do
                 builderpos <- span (snd $1) (expPosition $6)
                 return Builder { builderKind = fst $1,
                                  builderName = name $2,
                                  builderSuperTypes = reverse $4,
                                  builderParams = reverse $3,
                                  builderContent = Value $6,
                                  builderPos = builderpos }
              }
        | truth_kind ID args_opt EQUAL exp
            {% do
                 pos <- span (snd $1) (expPosition $5)
                 return Truth { truthName = name $2, truthKind = fst $1,
                                truthContent = $5, truthPos = pos }
            }
        | PROOF static_exp EQUAL exp
            {% do
                 pos <- span (Token.position $1) (expPosition $4)
                 return Proof { proofName = $2, proofBody = $4,
                                proofPos = pos }
            }
        | IMPORT static_exp
            {% do
                 pos <- span (Token.position $1) (expPosition $2)
                 return Import { importExp = $2, importPos = pos }
            }

group_list: group_list PRIVATE COLON def_list
              {% do
                   pos <- span (Token.position $2) (elementPosition (head $4))
                   return (Group { groupVisibility = Private,
                                   groupElements = $4,
                                   groupPos = pos } : $1) }
          | group_list PROTECTED COLON def_list
              {% do
                   pos <- span (Token.position $2) (elementPosition (head $4))
                   return (Group { groupVisibility = Protected,
                                   groupElements = $4,
                                   groupPos = pos } : $1) }
          | group_list PUBLIC COLON def_list
              {% do
                   pos <- span (Token.position $2) (elementPosition (head $4))
                   return (Group { groupVisibility = Public,
                                   groupElements = $4,
                                   groupPos = pos } : $1) }
          |
              { [] }

truth_kind: THEOREM
              { (Theorem, Token.position $1) }
          | INVARIANT
              { (Invariant, Token.position $1) }
          | AXIOM
              { (Axiom, Token.position $1) }

type_builder_kind: MODULE
                     { (Module, Token.position $1) }
                 | SIGNATURE
                     { (Signature, Token.position $1) }
                 | CLASS
                     { (Class, Token.position $1) }
                 | TYPECLASS
                     { (Typeclass, Token.position $1) }
                 | INSTANCE
                     { (Instance, Token.position $1) }

args_opt: LPAREN field_list RPAREN
            { $2 }
        |
            { [] }

field_list: field_list COMMA ID COLON exp
              {% do
                   pos <- span (Token.position $3) (expPosition $5)
                   return (Field { fieldName = FieldName { fieldSym = name $3 },
                                   fieldVal = $5, fieldPos = pos } : $1)
              }
          | ID COLON exp
              {% do
                   pos <- span (Token.position $1) (expPosition $3)
                   return [ Field { fieldName = FieldName { fieldSym = name $1 },
                                    fieldVal = $3, fieldPos = pos } ]
              }

bind_list: bind_list COMMA ID EQUAL exp
             {% do
                  pos <- span (Token.position $3) (expPosition $5)
                  return (Field { fieldName = FieldName { fieldSym = name $3 },
                                  fieldVal = $5, fieldPos = pos } : $1)
             }
         | ID EQUAL exp
             {% do
                  pos <- span (Token.position $1) (expPosition $3)
                  return [ Field { fieldName = FieldName { fieldSym = name $1 },
                                   fieldVal = $3, fieldPos = pos } ]
             }

extends: COLON static_exp_list
           { $2 }
       |
           { [] }

static_exp_list: static_exp_list COMMA static_exp
               { $3 : $1 }
           | static_exp
               { [ $1 ] }

static_exp: static_exp WITH LPAREN exp_list RPAREN
              {% do
                   withpos <- span (expPosition $1) (Token.position $5)
                   recpos <- span (Token.position $3) (Token.position $5)
                   return With { withVal = $1,
                                 withArgs = Tuple { tupleFields = reverse $4,
                                                    tuplePos = recpos },
                                 withPos = withpos }
              }
          | static_exp WITH LPAREN bind_list RPAREN
              {% do
                   withpos <- span (expPosition $1) (Token.position $5)
                   recpos <- span (Token.position $3) (Token.position $5)
                   return With { withVal = $1,
                                 withArgs = Record { recordFields = reverse $4,
                                                     recordType = False,
                                                     recordPos = recpos },
                                 withPos = withpos }
              }
          | static_exp WHERE LPAREN bind_list RPAREN
              {% do
                   withpos <- span (expPosition $1) (Token.position $5)
                   recpos <- span (Token.position $3) (Token.position $5)
                   return Where { whereVal = $1,
                                  whereProp = Record {
                                                recordFields = reverse $4,
                                                recordType = False,
                                                recordPos = recpos
                                              },
                                  wherePos = withpos }
              }
          | static_exp LPAREN exp_list RPAREN
              {% do
                   seqpos <- span (expPosition $1) (Token.position $4)
                   recpos <- span (Token.position $2) (Token.position $4)
                   return Seq { seqFirst = $1,
                                seqSecond = Tuple { tupleFields = reverse $3,
                                                    tuplePos = recpos },
                                seqPos = seqpos }
              }
          | static_exp LPAREN bind_list RPAREN
              {% do
                   seqpos <- span (expPosition $1) (Token.position $4)
                   recpos <- span (Token.position $2) (Token.position $4)
                   return Seq { seqFirst = $1,
                                seqSecond = Record { recordFields = reverse $3,
                                                     recordType = False,
                                                     recordPos = recpos },
                                seqPos = seqpos }
              }
          | static_exp DOT ID
              {% do
                   pos <- span (expPosition $1) (Token.position $3)
                   return Project { projectVal = $1,
                                    projectFields =
                                      [ FieldName { fieldSym = name $3 } ],
                                    projectPos = pos }
              }
          | type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
              {% do
                   grouppos <- span (elementPosition (last $5))
                                    (elementPosition (head $5))
                   builderpos <- span (snd $1) (Token.position $7)
                   return Anon { anonKind = fst $1,
                                 anonSuperTypes = reverse $3,
                                 anonParams = reverse $2,
                                 anonContent = Group {
                                                 groupVisibility = Public,
                                                 groupElements = reverse $5,
                                                 groupPos = grouppos
                                               } : reverse $6,
                                 anonPos = builderpos }
              }
          | ID
              { Sym { symName = name $1, symPos = Token.position $1 } }

exp_list: exp_list COMMA exp
            { $3 : $1 }
        | exp
            { [ $1 ] }

exp: exp WITH exp
       {% do
            pos <- span (expPosition $1) (expPosition $3)
            return With { withVal = $1, withArgs = $3, withPos = pos }
       }
   | exp WHERE exp
       {% do
            pos <- span (expPosition $1) (expPosition $3)
            return Where { whereVal = $1, whereProp = $3, wherePos = pos }
       }
   | exp AS exp
       {% do
            pos <- span (expPosition $1) (expPosition $3)
            return Ascribe { ascribeVal = $1, ascribeType = $3,
                             ascribePos = pos }
       }
   | exp inner_exp
       {% do
            pos <- span (expPosition $1) (expPosition $2)
            return Seq { seqFirst = $1, seqSecond = $2, seqPos = pos }
       }
   | compound_exp
       { $1 }

compound_exp: MATCH LPAREN exp RPAREN cases
                {% do
                     pos <- span (Token.position $1) (casePosition (head $5))
                     return Match { matchVal = $3, matchCases = reverse $5,
                                    matchPos = pos }
                }
            | abstraction_kind cases
                {% do
                     pos <- span (snd $1) (casePosition (head $2))
                     return Abs { absKind = fst $1, absCases = reverse $2,
                                  absPos = pos }
                }
            | inner_exp
                { $1 }

inner_exp: type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
            {% do
                 grouppos <- span (elementPosition (last $5))
                                  (elementPosition (head $5))
                 builderpos <- span (snd $1) (Token.position $7)
                 return Anon { anonKind = fst $1,
                               anonSuperTypes = reverse $3,
                               anonParams = reverse $2,
                               anonContent = Group { groupVisibility = Public,
                                                     groupElements = reverse $5,
                                                     groupPos = grouppos } :
                                             reverse $6,
                               anonPos = builderpos }
            }
         | LPAREN field_list RPAREN
            {% do
                 pos <- span (Token.position $1) (Token.position $3)
                 return Record { recordFields = reverse $2, recordType = True,
                                 recordPos = pos }
            }
         | LPAREN bind_list RPAREN
            {% do
                 pos <- span (Token.position $1) (Token.position $3)
                 return Record { recordFields = reverse $2, recordType = False,
                                 recordPos = pos }
            }
         | LPAREN exp_list RPAREN
            {% do
                 pos <- span (Token.position $1) (Token.position $3)
                 return Tuple { tupleFields = reverse $2, tuplePos = pos }
            }
         | LBRACE stm_list RBRACE
            {% do
                 compoundpos <- span (Token.position $1) (Token.position $3)
                 return Compound { compoundBody = reverse $2,
                                   compoundPos = compoundpos }
            }
         | inner_exp DOT ID
            {% do
                 pos <- span (expPosition $1) (Token.position $3)
                 return Project { projectVal = $1,
                                  projectFields =
                                    [ FieldName { fieldSym = name $3 } ],
                                  projectPos = pos }
            }
         | inner_exp DOT LPAREN project_list RPAREN
            {% do
                 pos <- span (expPosition $1) (Token.position $5)
                 return Project { projectVal = $1, projectFields = reverse $4,
                                  projectPos = pos }
            }
--         | inner_exp LBRACK exp_list RBRACK
--            {}
         | ID
            { Sym { symName = name $1, symPos = Token.position $1 } }
         | literal
            { Literal $1 }

project_list: project_list COMMA ID
              { FieldName { fieldSym = name $3 } : $1 }
          | ID
              { [ FieldName { fieldSym = name $1 } ] }

abstraction_kind: LAMBDA
                    { (Lambda, Token.position $1) }
                | FORALL
                    { (Forall, Token.position $1) }
                | EXISTS
                    { (Exists, Token.position $1) }

literal: NUM
          { Num { numVal = num $1, numPos = Token.position $1 } }
       | STRING
          { Str { strVal = str $1, strPos = Token.position $1 } }
       | CHAR
          { Char { charVal = char $1, charPos = Token.position $1 } }
       | LPAREN RPAREN
          {% do
               pos <- span (Token.position $1) (Token.position $2)
               return Unit { unitPos = pos }
          }


stm_list: open_stm_list
            { $1 }
        | closed_stm_list
            { $1 }
        |
            { [] }

closed_stm_list: open_stm_list SEMICOLON
                   { $1 }
               | closed_stm_list SEMICOLON
                   { $1 }
               | closed_stm_list closed_def
                   { Element $2 : $1 }
               | closed_def
                   { [ Element $1 ] }

open_stm: open_def
            { Element $1 }
        | LET value_def
            { Element $2 }
        | exp
            { Exp $1 }

open_stm_list: closed_stm_list open_stm
                 { $2 : $1 }
             | open_stm
                 { [ $1 ] }

cases: case_list pattern EQUAL exp %prec COLON
         {% do
              pos <- span (patternPosition $2) (expPosition $4)
              return (reverse (Case { casePat = $2, caseBody = $4,
                                      casePos = pos } : $1))
         }
     | case_list pattern LBRACE stm_list RBRACE %prec COLON
         {% do
              casepos <- span (patternPosition $2) (Token.position $5)
              compoundpos <- span (Token.position $3) (Token.position $5)
              return (reverse (Case { casePat = $2,
                                      caseBody =
                                        Compound { compoundBody = reverse $4,
                                                   compoundPos = compoundpos },
                                      casePos = casepos } : $1))
         }

case_list: case_list pattern EQUAL exp BAR
             {% do
                  pos <- span (patternPosition $2) (expPosition $4)
                  return (Case { casePat = $2, caseBody = $4,
                                 casePos = pos } : $1)
             }
         | case_list pattern LBRACE stm_list RBRACE BAR
             {% do
                  casepos <- span (patternPosition $2) (Token.position $5)
                  compoundpos <- span (Token.position $3) (Token.position $5)
                  return (Case { casePat = $2,
                                 caseBody =
                                   Compound { compoundBody = reverse $4,
                                              compoundPos = compoundpos },
                                 casePos = casepos } : $1)
             }
         |
             { [] }

pattern: LPAREN option_list RPAREN
           {% do
                pos <- span (Token.position $1) (Token.position $3)
                return Option { optionPats = reverse $2, optionPos = pos }
           }
       | LPAREN match_list COMMA ELLIPSIS RPAREN
           {% do
                pos <- span (Token.position $1) (Token.position $5)
                return Split { splitFields = reverse $2, splitStrict = False,
                               splitPos = pos }
           }
       | LPAREN match_list RPAREN
           {% do
                pos <- span (Token.position $1) (Token.position $3)
                return Split { splitFields = reverse $2, splitStrict = True,
                               splitPos = pos }
           }
       | ID AS pattern
           {% do
                pos <- span (Token.position $1) (patternPosition $3)
                return As { asName = name $1, asPat = $3, asPos = pos }
           }
       | pattern COLON exp
           {% do
                pos <- span (patternPosition $1) (expPosition $3)
                return Typed { typedPat = $1, typedType = $3, typedPos = pos }
           }
       | ID pattern
           {% do
                pos <- span (Token.position $1) (patternPosition $2)
                return Deconstruct { deconstructName = name $1,
                                     deconstructPat = $2,
                                     deconstructPos = pos }
           }
       | ID
           { Name { nameSym = name $1, namePos = Token.position $1 } }
       | literal
           { Exact $1 }

option_list: option_list BAR pattern
               { $3 : $1 }
           | pattern BAR pattern
               { [ $3, $1 ] }

match_list: match_list COMMA ID EQUAL pattern
              {% do
                   pos <- span (Token.position $3) (patternPosition $5)
                   return (Named { namedSym = name $3, namedVal = $5,
                                   namedPos = pos } : $1)
              }
          | match_list COMMA pattern
              { Unnamed $3 : $1 }
          | ID EQUAL pattern
              {% do
                   pos <- span (Token.position $1) (patternPosition $3)
                   return [ Named { namedSym = name $1, namedVal = $3,
                                    namedPos = pos } ]
              }
          | pattern
              { [ Unnamed $1 ] }

{

instance Error () where noMsg = ()

type Parser = ErrorT () Lexer

parseError :: Token-> Parser a
parseError tok =
  do
    Message.parseError tok
    throwError ()

lexer :: (Token -> Parser a) -> Parser a
lexer = (lift lex >>=)

name :: Token -> Symbol
name (Token.Id sym _) = sym
name _ = error "Cannot get name of token"

num :: Token -> Rational
num (Token.Num n _) = n
num _ = error "Cannot get num of token"

char :: Token -> Char
char (Token.Character chr _) = chr
char _ = error "Cannot get char of token"

str :: Token -> Strict.ByteString
str (Token.String str _) = str
str _ = error "Cannot get str of token"

-- | Run the parser, include tokens with the result.
parser :: Strict.ByteString
       -- ^ Name of the file being parsed.
       -> Lazy.ByteString
       -- ^ Content of the file being parsed
       -> Frontend (Maybe AST, [Token])
parser name input =
  let
    run :: Lexer (Maybe AST)
    run =
      do
        res <- runErrorT parse
        case res of
          Left () ->
            do
              lexRemaining
              return Nothing
          Right ast -> return $! Just ast
  in
    runLexer run name input

-- | Run the parser
parserNoTokens :: Strict.ByteString
               -- ^ Name of the file being parsed.
               -> Lazy.ByteString
               -- ^ Content of the file being parsed
               -> Frontend (Maybe AST)
parserNoTokens name input =
  let
    run :: Lexer (Maybe AST)
    run =
      do
        res <- runErrorT parse
        case res of
          Left () ->
            do
              lexRemaining
              return Nothing
          Right ast -> return $! Just ast
  in
    runLexerNoTokens run name input

}
