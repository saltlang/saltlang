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

module Language.Salt.Surface.Parser(
       parserWithTokens,
       parser
       ) where

import Control.Monad.Except
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.SourceBuffer hiding (linebreak)
import Control.Monad.Trans
import Data.ByteString(ByteString)
import Data.Either
import Data.Position.BasicPosition
import Data.PositionElement
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
       POSTFIX { Token.Postfix _ }
       INFIX { Token.Infix _ }
       LEFT { Token.Left _ }
       RIGHT { Token.Right _ }
       NONASSOC { Token.NonAssoc _ }
       PREC { Token.Prec _ }
       LESS { Token.Less _ }
       GREATER { Token.Greater _ }

%right ID NUM STRING CHAR LPAREN LBRACE LBRACK MODULE SIGNATURE CLASS TYPECLASS
       INSTANCE LESS GREATER LEFT RIGHT NONASSOC PREC POSTFIX INFIX
%right LAMBDA FORALL EXISTS MATCH
%right COLON
%right BAR
%nonassoc WITH
%nonassoc AS
%left WHERE

%%

top_level :: { AST }
          : component use_list def_list
              { AST { astComponent = $1, astUses = reverse $2,
                      astScope = reverse $3 } }

component :: { Maybe Component }
          : COMPONENT qual_id SEMICOLON
              { let
                  pos = position $1 <> position $3
                  (qualname, _) = $2
                in
                   Just Component { componentName = reverse qualname,
                                    componentPos = pos }
              }
          | COMPONENT qual_id
              { let
                  pos = position $1 <> qualpos
                  (qualname, qualpos) = $2
                in
                   Just Component { componentName = reverse qualname,
                                    componentPos = pos }
              }
          |
              { Nothing }

use_list :: { [Use] }
         : use_list USE qual_id SEMICOLON
             { let
                 pos = position $2 <> position $4
                 (qualname, _) = $3
               in
                 Use { useName = reverse qualname, usePos = pos } : $1
             }
         | use_list USE qual_id
             { let
                 pos = position $2 <> qualpos
                 (qualname, qualpos) = $3
               in
                 Use { useName = reverse qualname, usePos = pos } : $1
             }
         |
             { [] }

ident :: { (Symbol, Position) }
      : ID
          { (name $1, position $1) }
      | POSTFIX
          {% do
               idname <- symbol (Strict.fromString "postfix")
               return (idname, position $1)
          }
      | INFIX
          {% do
               idname <- symbol (Strict.fromString "infix")
               return (idname, position $1)
          }
      | LEFT
          {% do
               idname <- symbol (Strict.fromString "left")
               return (idname, position $1)
          }
      | RIGHT
          {% do
               idname <- symbol (Strict.fromString "right")
               return (idname, position $1)
          }
      | NONASSOC
          {% do
               idname <- symbol (Strict.fromString "nonassoc")
               return (idname, position $1)
          }
      | PREC
          {% do
               idname <- symbol (Strict.fromString "prec")
               return (idname, position $1)
          }
      | LESS
          {% do
               idname <- symbol (Strict.fromString "<")
               return (idname, position $1)
          }
      | GREATER
          {% do
               idname <- symbol (Strict.fromString ">")
               return (idname, position $1)
          }

qual_id :: { ([Symbol], Position) }
        : qual_id DOT ident
            { let
                (list, headpos) = $1
                (qualname, qualpos) = $3
                pos = headpos <> qualpos
              in
                (qualname : list, pos)
            }
        | ident
            { let
                (idname, idpos) = $1
              in
                ([idname], idpos)
            }

def_list :: { [Element] }
         : closed_def_list
             { $1 }
         | open_def_list
             { $1 }
         |
             { [] }

closed_def_list :: { [Element] }
                : open_def_list SEMICOLON
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

open_def_list :: { [Element] }
              : closed_def_list open_def
                  { $2 : $1 }
              | closed_def_list open_value_def
                  { $2 : $1 }
              | open_def
                  { [ $1 ] }
              | open_value_def
                  { [ $1 ] }

closed_def :: { Element }
           : type_builder_kind ident args_opt extends
             LBRACE def_list group_list RBRACE
               { let
                   (idname, _) = $2
                   content = case $6 of
                     [] -> reverse $7
                     _ ->
                       let
                         grouppos = position (last $6) <>
                                    position (head $6)
                       in
                         Group { groupVisibility = Public,
                                 groupElements = reverse $6,
                                 groupPos = grouppos } : reverse $7

                   builderpos = snd $1 <> position $8
                 in
                   Builder { builderKind = fst $1,
                             builderName = idname,
                             builderSuperTypes = reverse $4,
                             builderParams = reverse $3,
                             builderContent = Body content,
                             builderPos = builderpos }
               }
           | truth_kind ident args_opt EQUAL exp PROOF LBRACE stm_list RBRACE
               { let
                   (idname, _) = $2
                   content = buildExp $5
                   compoundpos = position $7 <> position $9
                   pos = snd $1 <> position $9
                 in
                   Truth { truthName = idname, truthKind = fst $1,
                           truthProof =
                             Just Compound { compoundBody = reverse $8,
                                             compoundPos = compoundpos },
                           truthContent = content, truthPos = pos }
             }
           | PROOF static_exp LBRACE stm_list RBRACE
               { let
                   compoundpos = position $3 <> position $5
                   pos = position $1 <> position $5
                 in
                   Proof { proofName = $2,
                           proofBody = Compound { compoundBody = reverse $4,
                                                  compoundPos = compoundpos },
                           proofPos = pos }
               }

fixity :: { (Fixity, Position) }
       : POSTFIX
           { (Postfix, position $1) }
       | INFIX LEFT
           { (Infix LeftAssoc, position $1) }
       | INFIX RIGHT
           { (Infix RightAssoc, position $1) }
       | INFIX NONASSOC
           { (Infix NonAssoc, position $1) }

prec_op :: { Ordering }
        : EQUAL
            { EQ }
        | LESS
            { LT }
        | GREATER
            { GT }

precs :: { [Prec Exp] }
      : precs PREC prec_op exp
          { let
              content = buildExp $4
              pos = position $2 <> position content
            in
              Prec { precOrd = $3, precExp = content, precPos = pos } : $1
          }
      | PREC prec_op exp
          { let
              content = buildExp $3
              pos = position $1 <> position content
            in
              [Prec { precOrd = $2, precExp = content, precPos = pos }]
          }

open_def :: { Element }
         : type_builder_kind ident args_opt extends EQUAL exp
             { let
                 (idname, _) = $2
                 builderpos = snd $1 <> position content
                 content = buildExp $6
               in
                 Builder { builderKind = fst $1,
                           builderName = idname,
                           builderSuperTypes = reverse $4,
                           builderParams = reverse $3,
                           builderContent = Value content,
                           builderPos = builderpos }
             }
         | truth_kind ident args_opt EQUAL exp
             { let
                 (idname, _) = $2
                 content = buildExp $5
                 pos = snd $1 <> position content
               in
                 Truth { truthName = idname, truthKind = fst $1,
                         truthProof = Nothing, truthContent = content,
                         truthPos = pos }
             }
         | truth_kind ident args_opt EQUAL exp PROOF EQUAL exp
             { let
                 (idname, _) = $2
                 content = buildExp $5
                 proof = buildExp $8
                 pos = snd $1 <> position proof
               in
                 Truth { truthName = idname, truthKind = fst $1,
                         truthProof = Just proof, truthContent = content,
                         truthPos = pos }
             }
         | PROOF static_exp EQUAL exp
             { let
                 body = buildExp $4
                 pos = position $1 <> position body
               in
                 Proof { proofName = $2, proofBody = body, proofPos = pos }
             }
         | IMPORT static_exp
             { let
                 pos = position $1 <> position $2
               in
                 Import { importExp = $2, importPos = pos }
             }
         | SYNTAX ident fixity precs
             { let
                 (idname, _) = $2
                 (fixity, _) = $3
                 pos = position $1 <> position (head $4)
               in
                 Syntax { syntaxSym = idname, syntaxFixity = fixity,
                          syntaxPrecs = $4, syntaxPos = pos }
             }
         | SYNTAX ident fixity
             { let
                 (idname, _) = $2
                 (fixity, fixitypos) = $3
                 pos = position $1 <> fixitypos
               in
                 Syntax { syntaxSym = idname, syntaxFixity = fixity,
                          syntaxPrecs = [], syntaxPos = pos }
             }
         | SYNTAX ident precs
             { let
                 (idname, _) = $2
                 pos = position $1 <> position (head $3)
               in
                 Syntax { syntaxSym = idname, syntaxFixity = Prefix,
                          syntaxPrecs = $3, syntaxPos = pos }
             }
         | SYNTAX ident
             { let
                 (idname, idpos) = $2
                 pos = position $1 <> idpos
               in
                 Syntax { syntaxSym = idname, syntaxFixity = Prefix,
                          syntaxPrecs = [], syntaxPos = pos }
             }

closed_value_def :: { Element }
                 : FUN ident case_list pattern LBRACE stm_list RBRACE
                     { let
                         (idname, _) = $2
                         compoundpos = position $5 <> position $7
                         casepos = position $4 <> position $7
                         funpos = position $1 <> position $7
                         body = Compound { compoundBody = reverse $6,
                                           compoundPos = compoundpos }
                         cases = Case { casePat = $4, caseBody = body,
                                        casePos = casepos } : $3
                       in
                         Fun { funName = idname, funCases = reverse cases,
                               funPos = funpos }
                     }

open_value_def :: { Element }
               : pattern
                   { Def { defPattern = $1, defInit = Nothing,
                           defPos = position $1 }
                   }
               | pattern EQUAL exp
                   { let
                       init = buildExp $3
                       pos = position $1 <> position init
                     in
                       Def { defPattern = $1, defInit = Just init,
                             defPos = pos }
                   }
               | FUN ident case_list pattern EQUAL exp
                   { let
                       (idname, _) = $2
                       body = buildExp $6
                       casepos = position (head $3) <> position body
                       funpos = position $1 <> position body
                     in
                       Fun { funName = idname,
                             funCases = reverse (Case { casePat = $4,
                                                        caseBody = body,
                                                        casePos = casepos } :
                                                 $3),
                             funPos = funpos }
                   }

group_list :: { [Group] }
           : group_list PRIVATE COLON def_list
               { let
                   pos = position $2 <> position (head $4)
                 in
                   Group { groupVisibility = Private, groupElements = $4,
                           groupPos = pos } : $1
               }
           | group_list PROTECTED COLON def_list
               { let
                   pos = position $2 <> position (head $4)
                 in
                   Group { groupVisibility = Protected, groupElements = $4,
                           groupPos = pos } : $1
               }
           | group_list PUBLIC COLON def_list
               { let
                   pos = position $2 <> position (head $4)
                 in
                   Group { groupVisibility = Public, groupElements = $4,
                          groupPos = pos } : $1
               }
           |
               { [] }

truth_kind :: { (TruthKind, Position) }
           : THEOREM
               { (Theorem, position $1) }
           | INVARIANT
               { (Invariant, position $1) }
           | AXIOM
               { (Axiom, position $1) }

type_builder_kind :: { (BuilderKind, Position) }
                  : MODULE
                      { (Module, position $1) }
                  | SIGNATURE
                      { (Signature, position $1) }
                  | CLASS
                      { (Class, position $1) }
                  | TYPECLASS
                      { (Typeclass, position $1) }
                  | INSTANCE
                      { (Instance, position $1) }

args_opt :: { [Field] }
         : LPAREN field_list RPAREN
             { $2 }
         |
             { [] }

field_list :: { [Field] }
           : field_list COMMA ident COLON exp
               { let
                   (idname, idpos) = $3
                   val = buildExp $5
                   pos = idpos <> position val
                 in
                   Field { fieldName = FieldName { fieldSym = idname },
                           fieldVal = val, fieldPos = pos } : $1
               }
           | ident COLON exp
               { let
                   (idname, idpos) = $1
                   val = buildExp $3
                   pos = idpos <> position val
                 in
                   [ Field { fieldName = FieldName { fieldSym = idname },
                             fieldVal = val, fieldPos = pos } ]
               }

bind_list :: { [Field] }
          : bind_list COMMA ident EQUAL exp
              { let
                  (idname, idpos) = $3
                  val = buildExp $5
                  pos = idpos <> position val
                in
                  Field { fieldName = FieldName { fieldSym = idname },
                          fieldVal = val, fieldPos = pos } : $1
              }
          | ident EQUAL exp
              { let
                  (idname, idpos) = $1
                  val = buildExp $3
                  pos = idpos <> position val
                in
                  [ Field { fieldName = FieldName { fieldSym = idname },
                            fieldVal = val, fieldPos = pos } ]
              }

extends :: { [Exp] }
        : COLON static_exp_list
            { $2 }
        |
            { [] }

static_exp_list :: { [Exp] }
                : static_exp_list COMMA static_exp
                    { $3 : $1 }
                | static_exp
                    { [ $1 ] }

static_exp :: { Exp }
           : static_exp WITH LPAREN exp_list RPAREN
               { let
                   withpos = position $1 <> position $5
                   recpos = position $3 <> position $5
                 in
                   With { withVal = $1,
                          withArgs = Tuple { tupleFields = reverse $4,
                                             tuplePos = recpos },
                          withPos = withpos }
               }
           | static_exp WITH LPAREN bind_list RPAREN
               { let
                   withpos = position $1 <> position $5
                   recpos = position $3 <> position $5
                 in
                   With { withVal = $1,
                          withArgs = Record { recordFields = reverse $4,
                                              recordType = False,
                                              recordPos = recpos },
                          withPos = withpos }
               }
           | static_exp WHERE LPAREN bind_list RPAREN
               { let
                   wherepos = position $1 <> position $5
                   recpos = position $3 <> position $5
                 in
                   Where { whereVal = $1,
                           whereProp = Record { recordFields = reverse $4,
                                                recordType = False,
                                                recordPos = recpos },
                           wherePos = wherepos }
               }
           | static_exp LPAREN exp_list RPAREN
               { let
                   seqpos = position $1 <> position $4
                   recpos = position $2 <> position $4
                 in
                   Seq { seqExps = [$1, Tuple { tupleFields = reverse $3,
                                                tuplePos = recpos } ],
                         seqPos = seqpos }
               }
           | static_exp LPAREN bind_list RPAREN
               { let
                   seqpos = position $1 <> position $4
                   recpos = position $2 <> position $4
                 in
                   Seq { seqExps = [$1, Record { recordFields = reverse $3,
                                                 recordType = False,
                                                 recordPos = recpos } ],
                         seqPos = seqpos }
               }
           | static_exp DOT ident
               { let
                   (idname, idpos) = $3
                   pos = position $1 <>  idpos
                 in
                   Project { projectVal = $1,
                             projectFields = [FieldName { fieldSym = idname }],
                             projectPos = pos }
               }
           | type_builder_kind args_opt extends
             LBRACE def_list group_list RBRACE
               { let
                   grouppos = position (last $5) <> position (head $5)
                   builderpos = snd $1 <> position $7
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
           | ident
               { let
                   (idname, idpos) = $1
                 in
                   Sym { symName = idname, symPos = idpos }
               }

exp_list :: { [Exp] }
         : exp_list COMMA exp
             { buildExp $3 : $1 }
         | exp
             { [ buildExp $1 ] }

exp :: { [Exp] }
    : exp WITH exp
        { let
            val = buildExp $1
            args = buildExp $3
            pos = position val <> position args
          in
             [ With { withVal = val, withArgs = args, withPos = pos } ]
        }
    | exp WHERE exp
        { let
            val = buildExp $1
            prop = buildExp $3
            pos = position val <> position prop
          in
            [ Where { whereVal = val, whereProp = prop, wherePos = pos } ]
        }
    | exp AS exp
        { let
            val = buildExp $1
            ty = buildExp $3
            pos = position val <> position ty
          in
            [ Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } ]
        }
    | exp inner_exp
        { $2 : $1 }
    | compound_exp
        { [ $1 ] }

compound_exp :: { Exp }
             : MATCH LPAREN exp RPAREN cases
                 { let
                     val = buildExp $3
                     pos = position $1 <> position (head $5)
                   in
                     Match { matchVal = val, matchCases = reverse $5,
                             matchPos = pos }
                 }
             | abstraction_kind cases
                 { let
                      pos = snd $1 <> position (head $2)
                   in
                      Abs { absKind = fst $1, absCases = reverse $2,
                            absPos = pos }
                 }
             | inner_exp
                 { $1 }

inner_exp :: { Exp }
          : type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
             { let
                 grouppos = position (last $5) <> position (head $5)
                 builderpos = snd $1 <> position $7
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
          | LPAREN field_list RPAREN
             { let
                 pos = position $1 <> position $3
               in
                 Record { recordFields = reverse $2, recordType = True,
                          recordPos = pos }
             }
          | LPAREN bind_list RPAREN
             { let
                 pos = position $1 <> position $3
               in
                 Record { recordFields = reverse $2, recordType = False,
                          recordPos = pos }
             }
          | LPAREN exp_list RPAREN
             { let
                 pos = position $1 <> position $3
               in
                 Tuple { tupleFields = reverse $2, tuplePos = pos }
             }
          | LBRACE stm_list RBRACE
             { let
                 compoundpos = position $1 <> position $3
               in
                 Compound { compoundBody = reverse $2,
                            compoundPos = compoundpos }
             }
          | inner_exp DOT ident
             { let
                 (idname, idpos) = $3
                 pos = position $1 <> idpos
               in
                 Project { projectVal = $1,
                           projectFields = [FieldName { fieldSym = idname }],
                           projectPos = pos }
             }
          | inner_exp DOT LPAREN project_list RPAREN
             { let
                 pos = position $1 <> position $5
               in
                 Project { projectVal = $1, projectFields = reverse $4,
                           projectPos = pos }
             }
 --         | inner_exp LBRACK exp_list RBRACK
 --            {}
          | ident
             { let
                 (idname, idpos) = $1
               in
                 Sym { symName = idname, symPos = idpos } }
          | literal
             { Literal $1 }

project_list :: { [FieldName] }
             : project_list COMMA ident
                 { let
                     (idname, _) = $3
                   in
                     FieldName { fieldSym = idname } : $1
                 }
             | ident
                 { let
                     (idname, _) = $1
                   in
                     [ FieldName { fieldSym = idname } ]
                 }

abstraction_kind :: { (AbstractionKind, Position) }
                 : FUN
                     { (Lambda, position $1) }
                 | LAMBDA
                     { (Lambda, position $1) }
                 | FORALL
                     { (Forall, position $1) }
                 | EXISTS
                     { (Exists, position $1) }

literal :: { Literal }
        : NUM
           { Num { numVal = num $1, numPos = position $1 } }
        | STRING
           { Str { strVal = str $1, strPos = position $1 } }
        | CHAR
           { Char { charVal = char $1, charPos = position $1 } }
        | LPAREN RPAREN
           { let
               pos = position $1 <> position $2
             in
               Unit { unitPos = pos }
           }

stm_list :: { [Compound] }
         : open_stm_list
             { $1 }
         | closed_stm_list
             { $1 }
         |
             { [] }

closed_stm_list :: { [Compound] }
                : open_stm_list SEMICOLON
                    { $1 }
                | closed_stm_list SEMICOLON
                    { $1 }
                | closed_stm_list closed_def
                    { Element $2 : $1 }
                | LET closed_value_def
                    { [ Element $2 ] }
                | closed_def
                    { [ Element $1 ] }

open_stm :: { Compound }
         : open_def
             { Element $1 }
         | LET open_value_def
             { Element $2 }
         | exp
             { Exp (buildExp $1) }

open_stm_list :: { [Compound] }
              : closed_stm_list open_stm
                  { $2 : $1 }
              | open_stm
                  { [ $1 ] }

cases :: { [Case] }
      : case_list pattern EQUAL exp %prec COLON
          { let
              body = buildExp $4
              pos = position $2 <> position body
            in
              reverse (Case { casePat = $2, caseBody = body,
                              casePos = pos } : $1)
          }
      | case_list pattern LBRACE stm_list RBRACE %prec COLON
          { let
              casepos = position $2 <> position $5
              compoundpos = position $3 <> position $5
            in
               reverse (Case { casePat = $2,
                               caseBody =
                                 Compound { compoundBody = reverse $4,
                                            compoundPos = compoundpos },
                               casePos = casepos } : $1)
          }

case_list :: { [Case] }
          : case_list pattern EQUAL exp BAR
              { let
                  body = buildExp $4
                  pos = position $2 <> position body
                in
                  Case { casePat = $2, caseBody = body, casePos = pos } : $1
              }
          | case_list pattern LBRACE stm_list RBRACE BAR
              { let
                  casepos = position $2 <> position $5
                  compoundpos = position $3 <> position $5
                in
                  Case { casePat = $2,
                         caseBody = Compound { compoundBody = reverse $4,
                                               compoundPos = compoundpos },
                         casePos = casepos } : $1
              }
          |
              { [] }

pattern :: { Pattern }
        : LPAREN option_list RPAREN
            { let
                pos = position $1 <> position $3
              in
                Option { optionPats = reverse $2, optionPos = pos }
            }
        | LPAREN match_list COMMA ELLIPSIS RPAREN
            { let
                pos = position $1 <> position $5
              in
                Split { splitFields = reverse $2, splitStrict = False,
                        splitPos = pos }
            }
        | LPAREN match_list RPAREN
            { let
                pos = position $1 <> position $3
              in
                Split { splitFields = reverse $2, splitStrict = True,
                        splitPos = pos }
            }
        | pattern COLON exp
            { let
                ty = buildExp $3
                pos = position $1 <> position ty
              in
                Typed { typedPat = $1, typedType = ty, typedPos = pos }
            }
        | ident AS pattern
            { let
                (idname, idpos) = $1
                pos = idpos <> position $3
              in
                As { asName = idname, asPat = $3, asPos = pos }
            }
        | ident pattern %prec ID
            { let
                (idname, idpos) = $1
                pos = idpos <> position $2
              in
                Deconstruct { deconstructName = idname, deconstructPat = $2,
                              deconstructPos = pos }
            }
        | ident
            { let
                (idname, idpos) = $1
              in
                Name { nameSym = idname, namePos = idpos }
            }
        | literal
            { Exact $1 }

option_list :: { [Pattern] }
            : option_list BAR pattern
                { $3 : $1 }
            | pattern BAR pattern
                { [ $3, $1 ] }

match_list :: { [Entry] }
           : match_list COMMA ID EQUAL pattern
               { let
                   pos = position $3 <> position $5
                 in
                    Named { namedSym = name $3, namedVal = $5,
                            namedPos = pos } : $1
               }
           | match_list COMMA pattern
               { Unnamed $3 : $1 }
           | ident EQUAL pattern
               { let
                   (idname, idpos) = $1
                   pos = idpos <> position $3
                 in
                   [ Named { namedSym = idname, namedVal = $3,
                             namedPos = pos } ]
               }
           | pattern
               { [ Unnamed $1 ] }

{

  type Parser = ExceptT () (Lexer Frontend)

buildExp :: [Exp] -> Exp
buildExp [] = error "Empty expression sequence, shouldn't happen"
buildExp [e] = e
buildExp revexps =
  let
    exps = reverse revexps
    pos = position (head exps) <> position (last exps)
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
parserWithTokens :: Position.Filename
                 -- ^ Name of the file being parsed.
                 -> Lazy.ByteString
                 -- ^ Content of the file being parsed
                 -> Frontend (Maybe AST, [Token])
parserWithTokens name input =
  let
    run :: Lexer Frontend (Maybe AST)
    run =
      do
        res <- runExceptT parse
        case res of
          Left () ->
            do
              lexRemaining
              return Nothing
          Right ast -> return $! Just ast
  in
    runLexerWithTokens run name input

-- | Run the parser
parser :: Position.Filename
       -- ^ Name of the file being parsed.
       -> Lazy.ByteString
       -- ^ Content of the file being parsed
       -> Frontend (Maybe AST)
parser name input =
  let
    run :: Lexer Frontend (Maybe AST)
    run =
      do
        res <- runExceptT parse
        case res of
          Left () ->
            do
              lexRemaining
              return Nothing
          Right ast -> return $! Just ast
  in
    runLexer run name input

}
