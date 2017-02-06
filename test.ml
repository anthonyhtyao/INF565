open Syntax
(*
let () = 
  printExp (Inst("abc"))
*)
let x = Inst("x")

let e = Letrec("f","y",Fun("y",Cste(True)),Let("x",Cste(Int(5)),Oper(Minus,x,Cste(Int(3)))))

let () =
  printExp e
