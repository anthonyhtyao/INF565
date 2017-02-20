open Format

type inst = string*int

type exp = 
  | Let of inst*exp*exp  (* lex x = e in e *)
  | Letrec of inst*inst*exp*exp (* let rec f x = e in e *)
  | Fun of inst*exp (* fun x -> e *)
  | Appl of exp*exp (* e e *)
  | Oper of Syntax.oper*exp*exp (* e + e*)
  | Cond of exp*exp*exp (* if A then B else C *)
  | Inst of inst
  | Cste of Syntax.cste

let rec of_exp = function
  | Syntax.Let(i,e1,e2) -> Let(i,of_exp e1,of_exp e2)
  | Syntax.Letrec(i1,i2,e1,e2) -> Letrec(i1,i1,of_exp e1,of_exp e2)
  | Syntax.Fun(i,e) -> Fun(i,of_exp e)
  | Syntax.Appl(e1,e2) -> Appl(of_exp e1, of_exp e2)
  | Syntax.Oper(Syntax.And,e1,e2) -> Cond(of_exp e1,of_exp e2, Cste(Syntax.False))
  | Syntax.Oper(Syntax.Or,e1,e2) -> Cond(of_exp e1,Cste(Syntax.True),of_exp e2)
  | Syntax.Oper(o,e1,e2) -> Oper(o,of_exp e1,of_exp e2)
  | Syntax.Inst i  -> Inst(i)
  | Syntax.Cste c  -> Cste(c)

let printCste c = Syntax.printCste c

let printOper o = Syntax.printOper o

let string_of_inst i = "("^(fst i)^","^(string_of_int (snd i))^")"

let rec printExp s = match s with
  | Cste c -> printCste c
  | Inst i -> print_string (string_of_inst i)
  | Oper (o,e1,e2) -> printExp e1;printOper o;printExp e2;print_newline ()
  | Cond (e1,e2,e3) -> print_string "if ";
                       printExp e1;
                       print_string " then ";
                       printExp e2;
                       print_string " else ";
                       printExp e3;
                       print_newline ()
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
