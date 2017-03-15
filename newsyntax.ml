open Format
module Smap = Map.Make(String)

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

type valeur =
  | Vcste of Syntax.cste
  | Vfun  of exp*(valeur list)

let rec of_exp = function
  | Syntax.Let(i,e1,e2) -> Let((i,0),of_exp e1,of_exp e2)
  | Syntax.Letrec(i1,i2,e1,e2) -> Letrec((i1,0),(i2,0),of_exp e1,of_exp e2)
  | Syntax.Fun(i,e) -> Fun((i,0),of_exp e)
  | Syntax.Appl(e1,e2) -> Appl(of_exp e1, of_exp e2)
  | Syntax.Oper(Syntax.And,e1,e2) -> Cond(of_exp e1,of_exp e2, Cste(Syntax.False))
  | Syntax.Oper(Syntax.Or,e1,e2) -> Cond(of_exp e1,Cste(Syntax.True),of_exp e2)
  | Syntax.Oper(o,e1,e2) -> Oper(o,of_exp e1,of_exp e2)
  | Syntax.Cond(e1,e2,e3) -> Cond(of_exp e1,of_exp e2,of_exp e3)
  | Syntax.Inst i  -> Inst((i,0))
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

let printValeur = function
  | Vcste c-> printCste c
  | Vfun (e,l) -> printExp e

let cal_index exp =
  let incre m = Smap.map (fun x->x+1) m in
  let rec aux m = function
    | Cste c -> Cste c
    | Cond (e1,e2,e3) -> Cond(aux m e1,aux m e2, aux m e3) 
    | Appl (e1,e2) -> Appl(aux m e1,aux m e2)
    | Oper (o,e1,e2) -> Oper(o,aux m e1,aux m e2)
    | Inst (i,_) -> let n = try Smap.find i m with _ -> -1 in Inst(i,n)
    | Fun (i,e) ->  Fun(i,aux (Smap.add (fst i) 0 (incre m)) e)
    | Let (i,e1,e2) -> let mi = incre m in
                       Let (i,aux m e1,aux (Smap.add (fst i) 0 mi) e2)
    | Letrec (i1,i2,e1,e2) -> let mi = Smap.add (fst i1) 0 (incre m) in
                    Letrec (i1,i2,aux (Smap.add (fst i2) 0 (incre mi)) e1,aux mi e2)
    
  in aux Smap.empty exp

let plus v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1+i2))

let minus v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1-i2))

let times v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1*i2))

let subLst lst n =
  let rec aux lst n = match lst with
    | t::q when n = 0 -> q
    | _::q -> aux q (n-1)
  in aux lst n 

let interpretation exp =
  let rec aux etat = function
    | Cste c -> Vcste c
    | Fun (i,e) -> Vfun(e,etat)
    | Inst (i,n) -> List.nth etat n
    | Cond (e1,e2,e3) -> begin 
      match aux etat e1 with
        | Vcste(Syntax.True) -> aux etat e2
        | Vcste(Syntax.False) -> aux etat e3
      end
    | Oper (o,e1,e2) -> begin
      let v1 = aux etat e1 in let v2 = aux etat e2 in 
      match o with
        | Syntax.Plus ->  plus v1 v2
        | Syntax.Minus -> minus v1 v2
        | Syntax.Times -> times v1 v2
      end
    | Let (i,e1,e2) -> let v = aux etat e1 in aux (v::etat) e2
    | Appl (e1,e2) ->
      let v1 = aux etat e1 in let v2 = aux etat e2 in
      match v1 with
      | Vfun (e,l) -> aux (v2::l) e
  in aux [] exp

