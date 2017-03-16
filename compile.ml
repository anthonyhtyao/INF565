open Format

type code =
  | Access of int
  | Apply
  | Cur of (code list)
  | Return
  | Let
  | Endlet
  | Branchneg of int
  | Branch of int
  | Op of Syntax.oper
  | Push of valeur
and 
valeur =
  | Int of int
  | Bool of bool
  | Closure of (code list)*(valeur list)

let printValeur = function
  | Int i -> print_int i
  | Bool b when b -> print_string "true"
  | Bool b -> print_string "false"

let op o v1 v2 = match o,v1,v2 with
  | Syntax.Plus,Int(i1),Int(i2) -> Int(i1+i2)
  | Syntax.Minus,Int(i1),Int(i2) -> Int(i1-i2)
  | Syntax.Times,Int(i1),Int(i2) -> Int(i1*i2)
  | Syntax.Equals,a,b -> Bool(a=b)

let rec delete lst n = match lst,n with
  | lst,0 -> lst
  | t::q,n -> delete q (n-1)

let print = function
  | Access(_) -> print_string "Access "
  | Let -> print_string "Let "
  | Op(_)-> print_string "Op "
  | Push(_) -> print_string "Push "
  | Cur(_) -> print_string "Cur "
  | Apply -> print_string "Apply "
  | Return -> print_string "Return "
  | Branch(_) -> print_string "Branch "
  | Branchneg(_)->print_string "Branchneg "

let trans = function
  | (Access(n)::c,e,s,r) -> (c,e,(List.nth e n)::s,r)
  | (Apply::c,e,Closure(c1,e1)::v::s,r) -> (c1,v::e1,s,Closure(c,e)::r)
  | (Cur(c1)::c,e,s,r) -> (c,e,Closure(c1,e)::s,r)
  | (Return::c,e,s,Closure(c1,e1)::r) -> (c1,e1,s,r)
  | (Let::c,e,v::s,r) -> (c,v::e,s,r)
  | (Endlet::c,v::e,s,r) -> (c,e,s,r)
  | (Branch(n)::c,e,s,r) -> (delete c n,e,s,r)
  | (Branchneg(n)::c,e,Bool(true)::s,r) -> (c,e,s,r)
  | (Branchneg(n)::c,e,Bool(false)::s,r) -> (delete c n,e,s,r)
  | (Op(o)::c,e,v::w::s,r) -> (c,e,(op o v w)::s,r)
  | (Push(v)::c,e,s,r) -> (c,e,v::s,r)

let compile exp =
  let rec aux etat = function
   | Newsyntax.Cste(Syntax.Int i) -> Push(Int(i))::etat
   | Newsyntax.Cste(Syntax.True) -> Push(Bool(true))::etat
   | Newsyntax.Cste(Syntax.False) -> Push(Bool(false))::etat
   | Newsyntax.Oper(o,e1,e2) -> let c1 = aux [] e1 in let c2=aux [] e2 in c2@c1@(Op(o)::etat)
   | Newsyntax.Inst(i,n) -> Access(n)::etat
   | Newsyntax.Let(i,e1,e2) -> let c1 = aux [] e1 in let c2=aux [] e2 in c1@(Let::c2)@etat
   | Newsyntax.Inst(i,n) -> Access(n)::etat
   | Newsyntax.Fun(i,e) -> let l = aux [] e in Cur(l@[Return])::etat 
   | Newsyntax.Appl(e1,e2) -> let c1 = aux [] e1 in let c2 = aux [] e2 in c2@c1@(Apply::etat)
   | Newsyntax.Cond(e1,e2,e3) -> let c1 = aux [] e1 in
                                 let c2 = aux [] e2 in
                                 let c3 = aux [] e3 in
                                 let l2 = List.length c2 in
                                 let l3 = List.length c3 in
                                 c1@[Branchneg(l2+1)]@c2@(Branch(l3)::c3)@etat
  in
  aux [] exp

let exect lst = 
  let rec aux = function
    | [],_,v,_ -> [],[],v,[]
    | etat  -> aux (trans etat)
  in
  let _,_,v,_ = aux (lst,[],[],[])
  in List.hd v
