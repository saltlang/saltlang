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
       parse
       ) where

import Language.Salt.Surface.Token(Token)

import qualified Language.Salt.Surface.Token as Token

}

%name parser
%tokentype { Token }
%error { parseError }
%token ID { Token.Id _ _ }
       NUM { Token.Num _ _ }
       STRING { Token.String _ _ }
       CHAR { Token.Character _ _ }
       EQUAL { Token.Equal _ }
       ELLIPSIS { Token.Ellipsis _ }
       BAR { Token.Bar _ }
       DOT { Token.Dot _ }
       COLON { Token.Colon _ }
--       COLONCOLON { Token.ColonColon _ }
       COMMA { Token.Comma _ }
       SEMICOLON { Token.Semicolon _ }
       LPAREN { Token.LParen _ }
       RPAREN { Token.RParen _ }
       LBRACK { Token.LBrack _ }
       RBRACK { Token.RBrack _ }
       LBRACE { Token.LBrace _ }
       RBRACE { Token.RBrace _ }
       LAMBDA { Token.Lambda _ }
       FORALL { Token.Forall _ }
       EXISTS { Token.Exists _ }
       MODULE { Token.Module _ }
       SIGNATURE { Token.Signature _ }
       CLASS { Token.Class _ }
       TYPECLASS { Token.Typelass _ }
       THEOREM { Token.Theorem _ }
       INVARIANT { Token.Invariant _ }
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


%right ID NUM STRING CHAR LPAREN LBRACE LBRACK MODULE SIGNATURE CLASS TYPECLASS
%right LAMBDA FORALL EXISTS MATCH
%right BAR
%nonassoc COLON WITH
%nonassoc AS
%left WHERE

%%

def_list: closed_def_list
            { $1 }
        | open_def_list
            { $1 }
        |
            { [] }

closed_def_list: open_def_list SEMICOLON
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

closed_def: type_builder_kind ID args_opt extends
            LBRACE def_list group_list RBRACE
              { Builder {} }
          | FUN ID case_list pattern LBRACE stm_list RBRACE
              {}
          | PROOF ID LBRACE stm_list RBRACE
              {}

value_def: ID COLON exp
             {}
         | pattern EQUAL exp
             {}

open_def: FUN ID case_list pattern EQUAL exp
              {}
        | type_builder_kind ID args_opt extends EQUAL exp
              {}
        | truth_kind ID args_opt COLON exp
              {}
        | truth_kind ID args_opt EQUAL exp
              {}

group_list: group_list visibility COLON def_list
              { Group { groupVisibility = $2, groupElements = $4 } : $1 }
          |
              { [] }

visibility: PRIVATE
              { Private }
          | PROTECTED
              { Protected }
	  | PUBLIC
              { Public }

truth_kind: THEOREM
              { Theorem }
          | INVARIANT
              { Invariant }

type_builder_kind: MODULE
                     { Module }
                 | SIGNATURE
		     { Signature }
		 | CLASS
		     { Class }
		 | TYPECLASS
		     { Typeclass }

args_opt: LPAREN field_list RPAREN
            { $2 }
        |
	    { [] }

field_list: field_list COMMA ID COLON exp
              {}
          | ID COLON exp
	      {}

bind_list: bind_list COMMA ID EQUAL exp
             {}
         | ID COLON exp
             {}

extends: COLON extend_list
           { $2 }
       |
           { [] }

extend_list: extend_list COMMA extend
               {}
           | extend
	       {}

extend: extend WITH LPAREN bind_list RPAREN
          {}
      | extend WHERE LPAREN bind_list RPAREN
          {}
      | extend LPAREN bind_list RPAREN
          {}
      | extend DOT ID
          {}
      | type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
          {}
      | ID
          {}

exp_list: exp_list COMMA exp
            {}
        | exp
	    {}

exp: exp WITH exp
       {}
   | exp WHERE exp
       {}
   | exp COLON exp
       {}
   | exp inner_exp
       {}
   | compound_exp
       {}

compound_exp: MATCH LPAREN exp RPAREN cases
                {}
            | abstraction_kind cases
                {}
	    | inner_exp
	        {}

inner_exp: type_builder_kind args_opt extends LBRACE def_list group_list RBRACE
            {}
         | LPAREN bind_list RPAREN
            {}
         | LPAREN exp_list RPAREN
            {}
         | LBRACE stm_list RBRACE
            {}
         | inner_exp DOT ID
            {}
         | inner_exp LBRACK exp_list RBRACK
            {}
         | ID
            { Sym { symName = Token.sym $1, symPos = Token.pos $1 } }
	 | literal
	    { $1 }

--seq_exp: inner_exp seq_exp
--           {}
--       | inner_exp
--           {}

abstraction_kind: LAMBDA
                    {}
		| FORALL
		    {}
		| EXISTS
		    {}

--inner_exp:

literal: NUM
          { Num { numValue = Token.num $1, numPos = Token.pos $1 } }
       | STRING
          {}
       | CHAR
          {}
       | LPAREN RPAREN
          {}


stm_list: open_stm_list
            { $1 }
        | closed_stm_list
            { $1 }
        |
            { [] }

closed_stm_list: open_stm_list SEMICOLON
                   {}
               | closed_stm_list closed_def
                   {}
               | closed_def
                   {}

open_stm: open_def
            {}
        | LET value_def
            {}
        | exp
            {}

open_stm_list: closed_stm_list open_stm
                 {}
             | open_stm
                 {}

cases: case_list pattern EQUAL exp %prec COLON
         {}
     | case_list pattern LBRACE stm_list RBRACE %prec COLON
         {}

case_list: case_list pattern EQUAL exp BAR
             {}
         | case_list pattern LBRACE stm_list RBRACE BAR
	     {}
	 |
             {}

pattern: LPAREN option_list RPAREN
           {}
       | LPAREN match_list COMMA ELLIPSIS RPAREN
           {}
       | LPAREN match_list RPAREN
           {}
       | ID AS pattern
           {}
       | pattern COLON exp
           {}
       | ID pattern
           {}
       | ID
           {}
       | literal
           {}

option_list: option_list BAR pattern
               { $2 : $1 }
	   | pattern BAR pattern
	       { [ $2, $1 ]}

match_list: match_list COMMA ID EQUAL pattern
              {}
	  | match_list COMMA pattern
	      {}
	  | ID EQUAL pattern
	      {}
	  | pattern

{
}
