/* $Id: parser.mly,v 1.2 2019-11-26 14:07:46-08 - - $ */

%{
(******** BEGIN PARSER SEMANTICS ********)

open Absyn
open Etc
open Lexing

let syntax () = lexeprint (symbol_start_pos ()) ["syntax error"]

let linenr () = (symbol_start_pos ()).pos_lnum

(******** END PARSER SEMANTICS ********)
%}

%token <string> RELOP EQUAL ADDOP MULOP POWOP
%token <string> IDENT NUMBER STRING
%token COLON COMMA LPAR RPAR LSUB RSUB EOL EOF
%token DIM LET GOTO IF PRINT INPUT

%type <Absyn.program> program

%start program

%%

program : stmts EOF             { List.rev $1 }

stmts   : stmts stmt EOL        { $2::$1 }
        | stmts error EOL       { syntax (); $1 }
        |                       { [] }

stmt    : label action          { (linenr (), Some $1, Some $2) }
        | action                { (linenr (), None, Some $1) }
        | label                 { (linenr (), Some $1, None) }
        |                       { (linenr (), None, None) }

label   : ident COLON           { $1 }

action  : DIM array             { Dim ($2) }
        | LET memref EQUAL expr { Let ($2, $4) }
        | GOTO ident            { Goto ($2) }
        | IF relexpr GOTO ident { If ($2, $4) }
        | PRINT prints          { Print ($2) }
        | PRINT                 { Print ([]) }
        | INPUT inputs          { Input ($2) }

prints  : print COMMA prints    { $1::$3 }
        | print                 { [$1] }

print   : expr                  { Printexpr ($1) }
        | STRING                { String ($1) }

inputs  : memref COMMA inputs   { $1::$3 }
        | memref                { [$1] }

memref  : ident                 { Variable ($1) }
        | array                 { Array ($1) }

array   : ident LSUB expr RSUB  { ($1, $3) }

relexpr : expr RELOP expr       { Binop ($2, $1, $3) }
        | expr EQUAL expr       { Binop ($2, $1, $3) }

expr    : expr ADDOP term       { Binop ($2, $1, $3) }
        | term                  { $1 }

term    : term MULOP factor     { Binop ($2, $1, $3) }
        | factor                { $1 }

factor  : primary POWOP factor  { Binop ($2, $1, $3) }
        | primary               { $1 }

primary : LPAR expr RPAR        { $2 }
        | ADDOP primary         { Unop ($1, $2) }
        | NUMBER                { Constant (float_of_string ($1)) }
        | memref                { Memref ($1) }
        | ident LPAR expr RPAR  { Fncall ($1, $3) }

ident   : IDENT                 { $1 }
        | DIM                   { "dim" }
        | GOTO                  { "goto" }
        | IF                    { "if" }
        | INPUT                 { "input" }
        | LET                   { "let" }
        | PRINT                 { "print" }

