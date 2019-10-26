open Eval
open Utils

let eval_test s = 
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.p Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error: (%d,%d) %s\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1)
        s;
      exit 1 
  in 
  let st = Eval.init_state in
  let res, _, _, _ = Eval.eval st ast in
  State.deref st res

let eval_checkval = [
  ("let x = 1 in x;x", V_int(1));
  ("let x = 1 in sleep(1);x", V_int(1));
  ("let x = true in if x {4} else {6}", V_int(4));
  ("let x = false in if x {4} else {6}", V_int(6));
  ("let x = 1 in let y = 5 in x + y", V_int(6));
  ("let x = 1 in let y = 5 in y = x; y+y", V_int(2));
  (* TODO - this aliasing behavior seems... wrong? *)
  ("let x = 0 in let y = 0 in y = x; y = 100; x", V_int(100));
  ("let x = 1 in x = x;x", V_int(1));
  ("let x = {mut a : 1} in x.a = x.a;x.a", V_int(1));
  ("let x = 1 in let y = 5 in y = x + y; y+x", V_int(7));
  ("let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; y.a)", V_int(1));
  ("let x = 1 in let y = 5 in (destroy(x);y)", V_int(5));
  ("let x = true in if x { !x } else { x }", V_bool(false));
  ("let x = 1 in (x = x + 1 ; x)", V_int(2));
  ("let x = 1 in let y = 0 in branch x {x}; y", V_int(0));
  (* TODO while *)
]

let eval_success = [
  "let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; y)";
  "let x = {mut a : 1} in let y = {mut a : 2} in (y = x; y)";
]

let eval_failure = [
  "let x = 1 in (destroy(x);destroy(x))";
  "let x = {mut a : 1} in let y = {a : 2} in (y.a = x.a; y.a)";
  "let x = 1 in (destroy(x);x)";
  "let x = 1 in branch x {x}; x";
]

let run_tests = fun () -> 
  print_endline "check value:";
  List.iter (fun (s,exp) -> 
      try 
        (let res = eval_test s in
         (if exp = res then "." else "F") |> print_string)
      with |Utils.GError _ -> "E" |> print_string
    ) eval_checkval;
  print_endline "\ncheck eval success:";
  List.iter (fun s -> 
      try ignore (eval_test s); print_string "." with
      | Utils.GError _ -> print_string "F"
    ) eval_success;
  print_endline "\ncheck eval failure:";
  List.iter (fun s -> 
      try ignore (eval_test s); print_string "F" with
      | Utils.GError _ -> print_string "."
    ) eval_failure;
  print_endline "\ndone!"

