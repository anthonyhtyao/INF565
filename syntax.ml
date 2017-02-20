open Format

type inst = string*int

type oper =
  | Plus
  | Minus
  | Times
  | And
  | Or

type cste =
  | True
  | False
  | Int of int

type exp = 
  | Let of inst*exp*exp  (* lex x = e in e *)
  | Letrec of inst*inst*exp*exp (* let rec f x = e in e *)
  | Fun of inst*exp (* fun x -> e *)
  | Appl of exp*exp (* e e *)
  | Oper of oper*exp*exp (* e + e*)
  | Inst of inst
  | Cste of cste

let printCste c = match c with
  | True -> printf " True "
  | False -> printf " False "
  | Int i -> print_int i

let printOper o = match o with
  | Plus -> printf " + "
  | Minus -> printf " - "
  | Times -> printf " * "
  | And -> printf " && "
  | Or -> printf " || "

let string_of_inst i = "("^(fst i)^","^(string_of_int (snd i))^")"

let rec printExp s = match s with
  | Cste c -> printCste c
  | Inst i -> print_string (string_of_inst i)
  | Oper (o,e1,e2) -> printExp e1;printOper o;printExp e2;print_newline ()
  | Appl (e1,e2) -> print_string "App ";
                    printExp e1;
                    printExp e2;
                    print_newline ()
  | Fun (i,e) -> print_string ("fun "^(string_of_inst i)^" -> ");
                 printExp e;
                 print_newline ()
  | Letrec (i1,i2,e1,e2) -> print_string ("let rec "^(string_of_inst i1)^" "^(string_of_inst i2)^" = ");
                            printExp e1;
                            printf " in@.";
                            printExp e2;
                            print_newline()
  | Let (i,e1,e2)-> print_string ("let "^(string_of_inst i)^" = ");
                    printExp e1;
                    printf " in@.";
                    printExp e2;
                    print_newline ()
