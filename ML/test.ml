open Eval
open Utils
open Core

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
  res,st

(* check for equality between values (dereferencing pointers) *)
let rec compare_values st a b = 
  match a,b with
  |V_ptr(l_, l, mut, t),V_ptr(l_', l', mut', t') -> l = l'
  |V_ptr(l_, l, mut, t),_ -> compare_values st (State.deref st a) b
  |_,V_ptr(l_, l, mut, t) -> compare_values st a (State.deref st b)
  |V_int i1, V_int i2 -> i1 = i2
  |V_bool b1, V_bool b2 -> b1 = b2
  |V_obj o1, V_obj o2 -> begin 
      List.fold2_exn
        ~f:(fun acc (n,(t,_,_,l)) (n',(t',_,_,l')) -> 
           acc && n = n' && t = t' && l = l') 
        ~init:true o1 o2 
    end
  |V_unit, V_unit -> true
  |_,_ -> false

(* check if result value is correct *)
let eval_checkval = [
  ("let x = 1 in x;x", V_int(1));
  ("let x = 1 in x + x", V_int(2));
  ("let x = true in if x {4} else {6}", V_int(4));
  ("let x = false in if x {4} else {6}", V_int(6));
  ("let x = 1 in let y = 5 in x + y", V_int(6));
  ("let x = 1 in let y = 5 in y = x; y+y", V_int(2));
  ("let x = 0 in let y = 0 in y = x; y = 100; x", V_int(0));
  ("let x = 0 in let y = 0 in y = x; y = 100; y", V_int(100));
  ("let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; x.a = 3; x.a)", V_int(3));
  ("let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; x.a = 3; y.a)", V_int(2));
  ("let x = 1 in x = x;x", V_int(1));
  ("let x = {mut a : 1} in x.a = x.a;x.a", V_int(1));
  ("let x = 1 in let y = 5 in y = x + y; y+x", V_int(7));
  ("let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; y.a)", V_int(1));
  ("let x = 1 in let y = 5 in (destroy(x);y)", V_int(5));
  ("let x = true in if x { !x } else { x }", V_bool(false));
  ("let x = 1 in (x = x + 1 ; x)", V_int(2));
  ("let x = 1 in let y = 0 in branch x {x}; y", V_int(0));
  ("let x = 1 in while (x > 0) {x = x - 1}; x", V_int(0));
  ("let x = 3 in while (x > 0) {x = x - 1}; x", V_int(0));
  ("let x = 3 in let y = 0 in while (x > 0) {y = y + x; x = x - 1}; y", V_int(6));
  ("let x = 3 in let y = 0 in while (x > 1) {y = y + x; x = x - 1}; y", V_int(5));
  ("let x = {mut a : 1, mut b : 2} in destroy(x.a)", V_unit);
  (* TODO functions *)
]

(* check that evaluation succeeded *)
let eval_success = [
  "let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; y)";
  "let x = {mut a : 1} in let y = {mut a : 2} in (y = x; y)";
]

(* check that evaluation failed *)
let eval_failure = [
  "let x = 1 in (destroy(x);destroy(x))";
  "let x = {mut a : 1} in let y = {a : 2} in (y.a = x.a; y.a)";
  "let x = 1 in (destroy(x);x)";
  "let x = 1 in branch x {x}; x";
  "let x = {mut a : 1, mut b : 2} in destroy(x.a);x.b";
  (* TODO should this fail? *)
  "let x = {mut a : 1} in let y = {mut a : 2} in (y.a = x.a; x.a)";
]

let run_tests = fun () -> 
  print_endline "check value:";
  let failed = List.fold_left ~f:(fun acc (s,exp) -> 
      try 
        (let res,st = eval_test s in
         if compare_values st exp res then (print_string "."; acc) else (print_string "F" ; s::acc))
      with |Utils.GError _ -> (print_string "E" ; s::acc)
    ) ~init:[] eval_checkval 
  in
  print_endline "\ncheck eval success:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "."; acc with
      | Utils.GError _ -> print_string "F"; s::acc
    ) ~init:failed eval_success
  in
  print_endline "\ncheck eval failure:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "F"; s::acc with
      | Utils.GError _ -> print_string "."; acc
    ) ~init:failed eval_failure
  in
  print_endline "\nFailed:";
  List.rev failed |> List.iter ~f:(fun f -> print_endline f);
  print_endline "\ndone!"

