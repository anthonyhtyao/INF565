open Format
(*
let () = 
  printExp (Inst("abc"))

let x = Inst("x")

let e = Letrec("f","y",Fun("y",Cste(True)),Let("x",Cste(Int(5)),Oper(Minus,x,Cste(Int(3)))))
*)
let main () =
  let f_name = ref "" in
  Arg.parse [ ] (fun s -> f_name:= s) "CouCou";
  if String.compare !f_name "" = 0 then failwith "no program file give";
  Localizing.current_file_name := !f_name;
  let f_desc = open_in !f_name in
  let lexbuf = Lexing.from_channel f_desc in
  let prog = Parser.exp Lexer.token lexbuf in
  printf "Test expression : @. ";
  Syntax.printExp prog;
  let new_prog = Newsyntax.of_exp prog in
  printf "@. New syntax expression : @. ";
  Newsyntax.printExp new_prog;
  let ind = Newsyntax.cal_index new_prog in
  printf "@. After calculating De Bruijn index : @. ";
  Newsyntax.printExp ind;
  print_newline ();
  printf "@. Intrepretation : @. ";
  Newsyntax.printValeur (Newsyntax.interpretation ind);
  printf "@. Typage : @.";
  Typage.printType (Typage.typage ind);
  print_newline ();
  printf "Compiling ... @.";
  Compile.printValeur (Compile.exect (Compile.compile ind));
  print_newline ()

let () =
  main ()
