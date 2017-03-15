open Format

type t = |Int|Bool|Alpha of int|Arrow of t*t

let rec printType = function
  | Int -> print_string "Int"
  | Bool -> print_string "Bool"
  | Alpha(i) -> print_string "Alpha ";print_int i
  | Arrow(t1,t2) -> printType t1; print_string "->"; printType t2
let ufh = Hashtbl.create 42

let ind = ref 0

let rec find v = match v with
  | Alpha i -> begin
    try
      begin match Hashtbl.find ufh i with
      | Alpha(j) when i = j -> Alpha(i)
      | Alpha(j) -> find (Alpha(j))
      | a -> a
      end
    with
      | Not_found -> Hashtbl.add ufh i v;v
    end
  | a -> a

let rec union v1 v2 = match v1,v2 with
  | a,b when a=b -> a
  | Arrow(t1,t2), Arrow(t3,t4) -> Arrow(union t1 t3,union t2 t4)
  | Alpha(i1), Alpha(i2) -> begin
    match (find v1), (find v2) with
      | a,b when a=b -> a
      | a,Alpha(i) -> Hashtbl.add ufh i a;a
      | Arrow(t1,t2), Arrow(t3,t4) -> Arrow(union t1 t3,union t2 t4)
    end
  | a, Alpha(i)| Alpha(i),a -> 
    match find (Alpha(i)) with
      | b when b=a -> a
      | Alpha(j) -> Hashtbl.add ufh j a; a

let typage exp =
  let rec aux env= function
    | Newsyntax.Cste c -> begin
      match c with
        | Syntax.Int _ -> Int
        | _ -> Bool
      end
    | Inst(_,n) when n = -1-> ind:=!ind+1;let a = Alpha(!ind) in find a
    | Inst(_,n) -> find (List.nth env n)
    | Fun(i,e) -> ind:=!ind+1;let a = Alpha(!ind) in let b = aux (a::env) e in
                  Arrow(find a, b)
    | Oper(o,e1,e2) -> begin
      match o with
      | Syntax.Plus | Minus | Times -> union (aux env e1) (aux env e2)
          
      end
    | Cond (e1,e2,e3) ->
        assert (union (aux env e1) Bool = Bool);
        union (aux env e2) (aux env e3)
    | Let (i,e1,e2) -> ind:=!ind+1; let a = Alpha(!ind) in let b = aux env e1 in
                       aux ((union a b)::env) e2
  in
  let t = aux [] exp in
  t
          (*
    | Cond (e1,e2,e3) -> begin
      match aux env e1 with
      | Bool -> let t1 = aux env e1 in
                let t2 = aux env e2 in
                union t1 t2
                *)
