%{
(* parser.mly*)

open Syntax
open Localizing

(*
let lift_array t l = List.fold_left (fun t' () -> Array_type t') t l
*)
%}

%token <bool*Localizing.extent>    BOOLEAN_LITERAL
%token <string*Localizing.extent>  IDENTIFIER INT
%token <Localizing.extent>         LET REC IN FUN ARROW TRUE FALSE
%token <unit*Localizing.extent>    ANDAND OROR PLUS MINUS MULT EQ
%token EOF

%start exp

%type <Syntax.exp> exp
%type <Syntax.oper> operation
%type <Syntax.cste> constant

%%

exp:
| IDENTIFIER {Inst(fst $1)}
| constant {Cste($1)}
| exp operation exp {Oper($2,$1,$3)}
| exp exp {Appl($1,$2)}
| FUN IDENTIFIER ARROW exp {Fun((fst $2),$4)}
| LET REC IDENTIFIER IDENTIFIER EQ exp IN exp {Letrec((fst $3),(fst $4),$6,$8)}
| LET IDENTIFIER EQ exp IN exp {Let((fst $2),$4,$6)}

operation:
| ANDAND {And}
| OROR   {Or}
| PLUS   {Plus}
| MINUS  {Minus}
| MULT   {Times}

constant:
| TRUE   {True}
| FALSE  {False}
| INT    {Int(int_of_string(fst $1))}


