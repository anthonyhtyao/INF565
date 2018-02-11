open Format
open Newsyntax

type valeur =
  | Vcste of Syntax.cste
  | Vfun  of exp*(valeur list)
  | Vrecfun  of exp*(valeur list)

let printValeur = function
  | Vcste c-> printCste c
  | Vfun (e,l) -> printExp e
  | Vrecfun (e,l) -> printExp e

let plus v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1+i2))

let minus v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1-i2))

let times v1 v2 = match v1,v2 with
  | Vcste(Syntax.Int i1), Vcste(Syntax.Int i2) -> Vcste(Syntax.Int(i1*i2))

let equals v1 v2 = match v1,v2 with
  | a,b when a=b -> Vcste(True)
  | _,_ -> Vcste(False)

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
        | Syntax.Equals -> equals v1 v2
      end
    | Let (i,e1,e2) -> let v = aux etat e1 in aux (v::etat) e2
    | Letrec (i1,i2,e1,e2) -> aux (Vrecfun(e1,etat)::etat) e2
    | Appl (e1,e2) ->
      let v1 = aux etat e1 in let v2 = aux etat e2 in
      match v1 with
      | Vfun (e,l) -> aux (v2::l) e
      | Vrecfun (e,l) -> aux (v2::Vrecfun(e,l)::l) e
  in aux [] exp
