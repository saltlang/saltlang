-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
import Data.Position.BasicPosition
import Data.Semigroup((<>))
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
import qualified Data.Position as Position
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
       SYNTAX { Token.Syntax _ }
       COMPONENT { Token.Component _ }

%right ID NUM STRING CHAR LPAREN LBRACE LBRACK MODULE SIGNATURE CLASS TYPECLASS
       INSTANCE
%right LAMBDA FORALL EXISTS MATCH
%right BAR
%nonassoc COLON WITH
%nonassoc AS
%left WHERE

%%

top_level: component use_list def_list
             { AST { astComponent = $1, astUses = reverse $2,
                     astScope = reverse $3 } }

component: COMPONENT qual_id SEMICOLON
             { let
                  pos = Token.position $1 <> Token.position $3
                  (qualname, _) = $2
               in
                  Just Component { componentName = reverse qualname,
                                   componentPos = pos }
             }
         | COMPONENT qual_id
             { let
                 pos = Token.position $1 <> qualpos
                 (qualname, qualpos) = $2
               in
                  Just Component { componentName = reverse qualname,
                                   componentPos = pos }
             }
         |
             { Nothing }

use_list: use_list USE qual_id SEMICOLON
            { let
                pos = Token.position $2 <> Token.position $4
                (qualname, _) = $3
              in
                Use { useName = reverse qualname, usePos = pos } : $1
            }
        | use_list USE qual_id
            { let
                pos = Token.position $2 <> qualpos
                (qualname, qualpos) = $3
              in
                Use { useName = reverse qualname, usePos = pos } : $1
            }
        |
            { [] }

qual_id: qual_id DOT ID
           { let
               pos = headpos <> Token.position $3
               (list, headpos) = $1
             in
               (name $3 : list, pos)
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
               | closed_def_list closed_value_def
                   { $2 : $1 }
               | closed_def
                   { [ $1 ] }
               | closed_value_def
                   { [ $1 ] }

open_def_list: closed_def_list open_def
                 { $2 : $1 }
             | closed_def_list open_value_def
                 { $2 : $1 }
             | open_def
                 { [ $1 ] }
             | open_value_def
                 { [ $1 ] }

closed_def: type_builder_kind ID args_opt extends
            LBRACE def_list group_list RBRACE
              { let
                  content = case $6 of
                    [] -> reverse $7
                    _ ->
                      let
                        grouppos = elementPosition (last $6) <>
                                   elementPosition (head $6)
                      in
                        Group { groupVisibility = Public,
                                groupElements = reverse $6,
                                groupPos = grouppos } : reverse $7

                  builderpos = snd $1 <> Token.position $8
                in
                  Builder { builderKind = fst $1,
                            builderName = name $2,
                            builderSuperTypes = reverse $4,
                            builderParams = reverse $3,
                            builderContent = Body content,
                            builderPos = builderpos }
              }
          | PROOF static_exp LBRACE stm_list RBRACE
              { let
                  compoundpos = Token.position $3 <> Token.position $5
                  pos = Token.position $1 <> Token.position $5
                in
                  Proof { proofName = $2,
                          proofBody = Compound { compoundBody = reverse $4,
                                                 compoundPos = compoundpos },
                          proofPos = pos }
              }

open_def: type_builder_kind ID args_opt extends EQUAL exp
            { let
                builderpos = snd $1 <> expPosition content
                content = buildExp $6
              in
                Builder { builderKind = fst $1,
                          builderName = name $2,
                          builderSuperTypes = reverse $4,
                          builderParams = reverse $3,
                          builderContent = Value content,
                          builderPos = builderpos }
            }
        | truth_kind ID args_opt EQUAL exp
            { let
                content = buildExp $5
                pos = snd $1 <> expPosition content
              in
                Truth { truthName = name $2, truthKind = fst $1,
                        truthContent = content, truthPos = pos }
            }
        | PROOF static_exp EQUAL exp
            { let
                body = buildExp $4
                pos = Token.position $1 <> expPosition body
              in
                Proof { proofName = $2, proofBody = body, proofPos = pos }
            }
        | IMPORT static_exp
            { let
                pos = Token.position $1 <> expPosition $2
              in
                Import { importExp = $2, importPos = pos }
            }
        | SYNTAX exp
            { let
                exp = buildExp $2
                pos = Token.position $1 <> expPosition exp
              in
                Syntax { syntaxExp = exp, syntaxPos = pos }
            }

closed_value_def: FUN ID case_list pattern LBRACE stm_list RBRACE
                    { let
                        compoundpos = Token.position $5 <> Token.position $7
                        casepos = patternPosition $4 <> Token.position $7
                        funpos = Token.position $1 <> Token.position $7
                        body = Compound { compoundBody = reverse $6,
                                          compoundPos = compoundpos }
                        cases = Case { casePat = $4, caseBody = body,
                                       casePos = casepos } : $3
                      in
                        Fun { funName = name $2, funCases = reverse cases,
                              funPos = funpos }
                    }


open_value_def: pattern
                  { Def { defPattern = $1, defInit = Nothing,
                          defPos = patternPosition $1 }
                  }
              | pattern EQUAL exp
                  { let
                      init = buildExp $3
                      pos = patternPosition $1 <> expPosition init
                    in
                      Def { defPattern = $1, defInit = Just init, defPos = pos }
                  }
              | FUN ID case_list pattern EQUAL exp
                  { let
                      body = buildExp $6
                      casepos = casePosition (head $3) <> expPosition body
                      funpos = Token.position $1 <> expPosition body
                    in
                      Fun { funName = name $2,
                            funCases = reverse (Case { casePat = $4,
                                                       caseBody = body,
                                                       casePos = casepos } :
                                                $3),
                            funPos = funpos }
                  }

group_list: group_list PRIVATE COLON def_list
              { let
                  pos = Token.position $2 <> elementPosition (head $4)
                in
                  Group { groupVisibility = Private, groupElements = $4,
                          groupPos = pos } : $1
              }
          | group_list PROTECTED COLON def_list
              { let
                  pos = Token.position $2 <> elementPosition (head $4)
                in
                  Group { groupVisibility = Protected, groupElements = $4,
                          groupPos = pos } : $1
              }
          | group_list PUBLIC COLON def_list
              { let
                  pos = Token.position $2 <> elementPosition (head $4)
                in
                  Group { groupVisibility = Public, groupElements = $4,
                         groupPos = pos } : $1
              }
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
              { let
                  val = buildExp $5
                  pos = Token.position $3 <> expPosition val
                in
                  Field { fieldName = FieldName { fieldSym = name $3 },
                          fieldVal = val, fieldPos = pos } : $1
              }
          | ID COLON exp
              { let
                  val = buildExp $3
                  pos = Token.position $1 <> expPosition val
                in
                  [ Field { fieldName = FieldName { fieldSym = name $1 },
                            fieldVal = val, fieldPos = pos } ]
              }

bind_list: bind_list COMMA ID EQUAL exp
             { let
                 val = buildExp $5
                 pos = Token.position $3 <> expPosition val
               in
                 Field { fieldName = FieldName { fieldSym = name $3 },
                         fieldVal = val, fieldPos = pos } : $1
             }
         | ID EQUAL exp
             { let
                 val = buildExp $3
                 pos = Token.position $1 <> expPosition val
               in
                 [ Field { fieldName = FieldName { fieldSym = name $1 },
                           fieldVal = val, fieldPos = pos } ]
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
              { let
                  withpos = expPosition $1 <> Token.position $5
                  recpos = Token.position $3 <> Token.position $5
                in
                  With { withVal = $1,
                         withArgs = Tuple { tupleFields = reverse $4,
                                            tuplePos = recpos },
                         withPos = withpos }
              }
          | static_exp WITH LPAREN bind_list RPAREN
              { let
                  withpos = expPosition $1 <> Token.position $5
                  recpos = Token.position $3 <> Token.position $5
                in
                  With { withVal = $1,
                         withArgs = Record { recordFields = reverse $4,
                                             recordType = False,
                                             recordPos = recpos },
                         withPos = withpos }
              }
          | static_exp WHERE LPAREN bind_list RPAREN
              { let
                  wherepos = expPosition $1 <> Token.position $5
                  recpos = Token.position $3 <> Token.position $5
                in
                  Where { whereVal = $1,
                          whereProp = Record { recordFields = reverse $4,
                                               recordType = False,
                                               recordPos = recpos },
                          wherePos = wherepos }
              }
          | static_exp LPAREN exp_list RPAREN
              { let
                  seqpos = expPosition $1 <> Token.position $4
                  recpos = Token.position $2 <> Token.position $4
                in
                  Seq { seqExps = [$1, Tuple { tupleFields = reverse $3,
                                               tuplePos = recpos } ],
                        seqPos = seqpos }
              }
          | static_exp LPAREN bind_list RPAREN
              { let
                  seqpos = expPosition $1 <> Token.position $4
                  recpos = Token.position $2 <> Token.position $4
                in
                  Seq { seqExps = [$1, Record { recordFields = reverse $3,
                                                recordType = False,
                                                recordPos = recpos } ],
                        seqPos = seqpos }
              }
          | static_exp DOT ID
              { let
                  pos = expPosition $1 <>  Token.position $3
                in
                  Project { projectVal = $1,
                            projectFields = [FieldName { fieldSym = name $3 }],
                            projectPos = pos }
              }
          | type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
              { let
                  grouppos = elementPosition (last $5) <>
                              elementPosition (head $5)
                  builderpos = snd $1 <> Token.position $7
                in
                  Anon { anonKind = fst $1,
                         anonSuperTypes = reverse $3,
                         anonParams = reverse $2,
                         anonContent = Group { groupVisibility = Public,
                                               groupElements = reverse $5,
                                               groupPos = grouppos } :
                                       reverse $6,
                         anonPos = builderpos }
              }
          | ID
              { Sym { symName = name $1, symPos = Token.position $1 } }

exp_list: exp_list COMMA exp
            { buildExp $3 : $1 }
        | exp
            { [ buildExp $1 ] }

exp: exp WITH exp
       { let
           val = buildExp $1
           args = buildExp $3
           pos = expPosition val <> expPosition args
         in
            [ With { withVal = val, withArgs = args, withPos = pos } ]
       }
   | exp WHERE exp
       { let
           val = buildExp $1
           prop = buildExp $3
           pos = expPosition val <> expPosition prop
         in
           [ Where { whereVal = val, whereProp = prop, wherePos = pos } ]
       }
   | exp AS exp
       { let
           val = buildExp $1
           ty = buildExp $3
           pos = expPosition val <> expPosition ty
         in
           [ Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } ]
       }
   | exp inner_exp
       { $2 : $1 }
   | compound_exp
       { [ $1 ] }

compound_exp: MATCH LPAREN exp RPAREN cases
                { let
                    val = buildExp $3
                    pos = Token.position $1 <> casePosition (head $5)
                  in
                    Match { matchVal = val, matchCases = reverse $5,
                            matchPos = pos }
                }
            | abstraction_kind cases
                { let
                     pos = snd $1 <> casePosition (head $2)
                  in
                     Abs { absKind = fst $1, absCases = reverse $2,
                           absPos = pos }
                }
            | inner_exp
                { $1 }

inner_exp: type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
            { let
                grouppos = elementPosition (last $5) <>
                           elementPosition (head $5)
                builderpos = snd $1 <> Token.position $7
              in
                Anon { anonKind = fst $1,
                       anonSuperTypes = reverse $3,
                       anonParams = reverse $2,
                       anonContent = Group { groupVisibility = Public,
                                             groupElements = reverse $5,
                                             groupPos = grouppos } : reverse $6,
                       anonPos = builderpos }
            }
         | LPAREN field_list RPAREN
            { let
                pos = Token.position $1 <> Token.position $3
              in
                Record { recordFields = reverse $2, recordType = True,
                         recordPos = pos }
            }
         | LPAREN bind_list RPAREN
            { let
                pos = Token.position $1 <> Token.position $3
              in
                Record { recordFields = reverse $2, recordType = False,
                         recordPos = pos }
            }
         | LPAREN exp_list RPAREN
            { let
                pos = Token.position $1 <> Token.position $3
              in
                Tuple { tupleFields = reverse $2, tuplePos = pos }
            }
         | LBRACE stm_list RBRACE
            { let
                compoundpos = Token.position $1 <> Token.position $3
              in
                Compound { compoundBody = reverse $2,
                           compoundPos = compoundpos }
            }
         | inner_exp DOT ID
            { let
                pos = expPosition $1 <> Token.position $3
              in
                Project { projectVal = $1,
                          projectFields = [ FieldName { fieldSym = name $3 } ],
                          projectPos = pos }
            }
         | inner_exp DOT LPAREN project_list RPAREN
            { let
                pos = expPosition $1 <> Token.position $5
              in
                Project { projectVal = $1, projectFields = reverse $4,
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

abstraction_kind: FUN
                    { (Lambda, Token.position $1) }
                | LAMBDA
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
          { let
              pos = Token.position $1 <> Token.position $2
            in
              Unit { unitPos = pos }
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
               | LET closed_value_def
                   { [ Element $2 ] }
               | closed_def
                   { [ Element $1 ] }

open_stm: open_def
            { Element $1 }
        | LET open_value_def
            { Element $2 }
        | exp
            { Exp (buildExp $1) }

open_stm_list: closed_stm_list open_stm
                 { $2 : $1 }
             | open_stm
                 { [ $1 ] }

cases: case_list pattern EQUAL exp %prec COLON
         { let
             body = buildExp $4
             pos = patternPosition $2 <> expPosition body
           in
             reverse (Case { casePat = $2, caseBody = body,
                             casePos = pos } : $1)
         }
     | case_list pattern LBRACE stm_list RBRACE %prec COLON
         { let
             casepos = patternPosition $2 <> Token.position $5
             compoundpos = Token.position $3 <> Token.position $5
           in
              reverse (Case { casePat = $2,
                              caseBody = Compound { compoundBody = reverse $4,
                                                    compoundPos = compoundpos },
                              casePos = casepos } : $1)
         }

case_list: case_list pattern EQUAL exp BAR
             { let
                 body = buildExp $4
                 pos = patternPosition $2 <> expPosition body
               in
                 Case { casePat = $2, caseBody = body, casePos = pos } : $1
             }
         | case_list pattern LBRACE stm_list RBRACE BAR
             { let
                 casepos = patternPosition $2 <> Token.position $5
                 compoundpos = Token.position $3 <> Token.position $5
               in
                 Case { casePat = $2,
                        caseBody = Compound { compoundBody = reverse $4,
                                              compoundPos = compoundpos },
                        casePos = casepos } : $1
             }
         |
             { [] }

pattern: LPAREN option_list RPAREN
           { let
               pos = Token.position $1 <> Token.position $3
             in
               Option { optionPats = reverse $2, optionPos = pos }
           }
       | LPAREN match_list COMMA ELLIPSIS RPAREN
           { let
               pos = Token.position $1 <> Token.position $5
             in
               Split { splitFields = reverse $2, splitStrict = False,
                       splitPos = pos }
           }
       | LPAREN match_list RPAREN
           { let
               pos = Token.position $1 <> Token.position $3
             in
               Split { splitFields = reverse $2, splitStrict = True,
                       splitPos = pos }
           }
       | ID AS pattern
           { let
               pos = Token.position $1 <> patternPosition $3
             in
               As { asName = name $1, asPat = $3, asPos = pos }
           }
       | pattern COLON exp
           { let
               ty = buildExp $3
               pos = patternPosition $1 <> expPosition ty
             in
               Typed { typedPat = $1, typedType = ty, typedPos = pos }
           }
       | ID pattern
           { let
               pos = Token.position $1 <> patternPosition $2
             in
               Deconstruct { deconstructName = name $1, deconstructPat = $2,
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
              { let
                  pos = Token.position $3 <> patternPosition $5
                in
                   Named { namedSym = name $3, namedVal = $5,
                           namedPos = pos } : $1
              }
          | match_list COMMA pattern
              { Unnamed $3 : $1 }
          | ID EQUAL pattern
              { let
                  pos = Token.position $1 <> patternPosition $3
                in
                  [ Named { namedSym = name $1, namedVal = $3,
                            namedPos = pos } ]
              }
          | pattern
              { [ Unnamed $1 ] }

{

instance Error () where noMsg = ()

type Parser = ErrorT () Lexer

buildExp :: [Exp] -> Exp
buildExp [] = error "Empty expression sequence, shouldn't happen"
buildExp [e] = e
buildExp revexps =
  let
    exps = reverse revexps
    pos = expPosition (head exps) <> expPosition (last exps)
  in
    Seq { seqExps = exps, seqPos = pos }

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
parser :: Position.Filename
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
parserNoTokens :: Position.Filename
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
