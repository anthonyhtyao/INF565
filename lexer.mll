{
(* lexer.mll *)
open Parser
open Localizing

exception Eof

}

let newline = "\n" | "\r" | "\r\n"
let digit = ['0'-'9']
let digit_sequence = digit+
let nodigit = ['_' 'a'-'z' 'A'-'Z']
let identifier= nodigit (nodigit|digit)*

rule token = parse
| newline        {next_line lexbuf; token lexbuf}
| [' ' '\t']+    {token lexbuf}
| "(*"           {comment lexbuf; token lexbuf}
| digit_sequence {let s = Lexing.lexeme lexbuf and e = extent lexbuf in INT(s,e)}
| "let"          {LET(extent lexbuf)}
| "rec"          {REC(extent lexbuf)}
| "in"           {IN(extent lexbuf)}
| "("            {LEFT(extent lexbuf)}
| ")"            {RIGHT(extent lexbuf)}
| "if"           {IF(extent lexbuf)}
| "then"         {THEN(extent lexbuf)}
| "else"         {ELSE(extent lexbuf)}
| "fun"          {FUN(extent lexbuf)}
| "->"           {ARROW(extent lexbuf)}
| "true"         {TRUE(extent lexbuf)}
| "false"        {FALSE(extent lexbuf)}
| "&&"           {ANDAND((),extent lexbuf)}
| "||"           {OROR((),extent lexbuf)}
| "+"            {PLUS((),extent lexbuf)}
| "-"            {MINUS((),extent lexbuf)}
| "*"            {MULT((),extent lexbuf)}
| "="            {EQ((),extent lexbuf)}
| identifier     {IDENTIFIER(Lexing.lexeme lexbuf,extent lexbuf)}
| eof            {EOF}

and comment = parse
| "*)" {()}
| "(*" {comment lexbuf; comment lexbuf}
| _ {comment lexbuf}
| eof {failwith "commentaire non termine"}
