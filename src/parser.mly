%{
open Ast
(*
 program    P ::= "(" "lambda" "(" id ")" e ")"
 expression e ::= "0" | "1" | id
               | "(" "if0" e e e ")"
               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
               | "(" op1 e ")"
               | "(" op2 e e ")"
          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
          op2 ::= "and" | "or" | "xor" | "plus"
          id  ::= [a-z][a-z_0-9]*
*)
%}

%token <string> IDENT
%token <Ast.unop> OP1
%token <Ast.binop> OP2
%token IF0
%token FOLD
%token <int> INT
%token LPAREN
%token RPAREN
%token LAMBDA


/* Point d'entrée */

%start file
%type <string Ast.program> file

%%

file: program { $1 }
;

program: LPAREN LAMBDA LPAREN IDENT RPAREN expression RPAREN
  { { input = $4; expr = $6; } }
;

expression:
| INT
    { match $1 with
      | 0  -> Const Int64.zero
      | 1 -> Const Int64.one
      | n -> assert false }
| IDENT
    { Var $1 }
| LPAREN IF0 expression expression expression RPAREN
    { If_Zero ($3, $4, $5) }
| LPAREN FOLD expression expression
    LPAREN LAMBDA LPAREN IDENT IDENT RPAREN
           expression RPAREN RPAREN
    { Fold ($3, $4, $8, $9, $11) }
| LPAREN OP1 expression RPAREN
    { Unop ($2, $3) }
| LPAREN OP2 expression expression RPAREN
    { Binop ($2, $3, $4) }
;
