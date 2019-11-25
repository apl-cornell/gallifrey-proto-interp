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
  let st = Eval.init_state () in
  (* print_endline (string_of_int !(st.counter)); *)
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
  |V_obj(c1, o1), V_obj(c2, o2) -> begin 
      List.fold2_exn
        ~f:(fun acc (n,(t,_,_,l)) (n',(t',_,_,l')) -> 
            acc && n = n' && t = t' && l = l') 
        ~init:true o1 o2 
    end
  |V_unit, V_unit -> true
  |_,_ -> false

(* check if result value is correct *)
let eval_checkval = [
  ("true & false", V_bool(false));
  ("true | false", V_bool(true));
  ("10 * 2", V_int(20));
  ("10 / 2", V_int(5));
  ("10 % 7", V_int(3));
  ("1 == 2", V_bool(false));
  ("1 != 2", V_bool(true));
  ("1 >= 2", V_bool(false));
  ("1 <= 2", V_bool(true));
  ("1 < 2", V_bool(true));
  ("let x = 1 in x;x", V_int(1));
  ("let x = 1 in x + x", V_int(2));
  ("let x = true in if x {4} else {6}", V_int(4));
  ("let x = false in if x {4} else {6}", V_int(6));
  ("let x = 1 in let y = 5 in x + y", V_int(6));
  ("let x = 1 in let y = 5 in y = x; y+y", V_int(2));
  ("let x = 0 in let y = 0 in y = x; y = 100; x", V_int(0));
  ("let x = 0 in let y = 0 in y = x; y = 100; y", V_int(100));
  ("class C {mut a : int}; let c = 1 in let x = C(capof(c),c) in (x.a = 3; x.a)", V_int(3));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; x.a = 3; x.a)", V_int(3));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; x.a = 3; y.a)", V_int(1));
  ("let x = 1 in x = x;x", V_int(1));
  ("let x = 1 in -x", V_int(-1));
  ("class C {mut a : int}; let c = 1 in let x = C(capof(c),c) in x.a = x.a;x.a", V_int(1));
  ("let x = 1 in let y = 5 in y = x + y; y+x", V_int(7));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; y.a)", V_int(1));
  ("let x = 1 in let y = 5 in (destroy(x);y)", V_int(5));
  ("let x = 1 in let y = x in (destroy(x);y)", V_int(1));
  ("let x = true in if x { !x } else { x }", V_bool(false));
  ("let x = 1 in (x = x + 1 ; x)", V_int(2));
  ("let x = 1 in let y = 0 in branch x {x}; y", V_int(0));
  ("let x = 1 in while (x > 0) {x = x - 1}; x", V_int(0));
  ("let x = 3 in while (x > 0) {x = x - 1}; x", V_int(0));
  ("class C {mut a : int}; let c = 3 in let x = C(capof(c),3) in while (x.a > 0) {x.a = x.a - 1}; x.a", V_int(0));
  ("class C {mut a : bool}; let c = false in let x = C(capof(c),false) in let y = 0 in while (x.a) {y = 1}; y", V_int(0));
  ("let x = 3 in let y = 0 in while (x > 0) {y = y + x; x = x - 1}; y", V_int(6));
  ("let x = 3 in let y = 0 in while (x > 1) {y = y + x; x = x - 1}; y", V_int(5));
  ("class C {mut a : int, mut b : int}; let c = 1 in let x = C(capof(c), c, c) in destroy(x.a)", V_unit);
  ("let a = 1 in let f = fun (a | x : a int)->int { x } in f(capof(a),a)", V_int(1));
  ("let a = 1 in let f = fun (a | x : a int)->int { x } in let f2 = fun (a | x : a int)->int { f(capof(x), x) } in f2(capof(a),a)", V_int(1));
  ("let a = 1 in let b = 2 in let f = fun (a | x : a int) (b | y : b int)->int { x + y } in f(capof(a), a, capof(b), b)", V_int(3));
  ("let a = 1 in let f = fun (a | x : a int, y: a int)->int { x + y } in f(capof(a), a, a)", V_int(2));
  ("let a = 1 in let f = fun (a | x : a int)->int { x = 2; 10 } in f(capof(a),a); a", V_int(1));
  ("let a = 1 in let f = fun (a | x : a int)->int { x+1 } in f(capof(a),a)", V_int(2));
  ("let a = 1 in let f = fun (a | x : a int)->int { x+1 } in let b = f(capof(a),a) in b", V_int(2));
  ("let a = 1 in let f = fun (a | x : a int)->unit { x = 0 } in f(capof(a),a); a", V_int(1));
  ("let a = 1 in let f = fun (a | x : a int)->int { x = 0; x } in f(capof(a),a)", V_int(0));
  ("let a = 1 in let f = fun (a | x : a int)->int { x = 0; x } in f(capof(a),a); a", V_int(1));
  ("let a = 1 in let f = fun (a | x : a int)->int { x = 0; x } in let b = f(capof(a),a) in b", V_int(0));
  (* TODO mutable object == consumes cap? *)
  ("class C {mut a : int}; let c = 1 in let x = C(capof(c), c) in x; x.a", V_int(1));
  ("class C {mut a : int}; let c = 1 in let x = C(capof(c), c) in x.a; x.a", V_int(1));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; x.a)", V_int(1));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; x.a = 0; y.a)", V_int(1));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y = x; x.a = 0; y.a)", V_int(0));
  ("class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y = x; x.a = 0; x.a)", V_int(0));
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { a } in f(capof(x),x);x.a", V_int(2));
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->int { a.a } in f(capof(x),x)", V_int(2));
  ("class C {mut a : int}; let c = 1 in let f = fun (x| a : x int)->C { C(capof(a), a) } in (f(capof(c), c)).a", V_int(1));
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->int { let y = a.a in y } in f(capof(x),x)", V_int(2));
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { let y = a in y } in (f(capof(x),x)).a", V_int(2));
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { a } in (f(capof(x),x)).a", V_int(2));  
  ("class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { let y = a in y } in let z = f(capof(x),x) in z.a", V_int(2));
  ("class C {a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | z : x int)->int { z = z + 1; z } in f(capof(x),x.a)", V_int(3));
  ("class C {a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | z : x int)->int { z = z + 1; z } in f(capof(x),x.a); x.a", V_int(2));
  ("class C {a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | z : x int)->int { z = z + 1; z } in f(capof(x),x.a)", V_int(3));
  (* does cap for x get consumed, since it has a mut field? *)
  ("class C {a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | z : x int)->int { z = z + 1; z } in f(capof(x),x.a); x.a", V_int(2));
  ("class C {a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | z : x int)->int { z = z + 1; z } in f(capof(x),x.a); x.a", V_int(2));
  ("class C {mut a : int}", V_unit);
  ("class C {mut U a : int}", V_unit);
  ("class C {mut a : int}; let c = 1 in let x = C(capof(c), c) in x.a", V_int(1));
  ("class C {mut a : int}; class C2 extends C {mut b : int}; let c = 1 in let x = C2(capof(c),c,2) in x.b", V_int(2));
  ("class C {mut a : int}; class C2 extends C {mut b : int}; let c = 1 in let x = C2(capof(c),c,2) in x.a", V_int(1));
  ("class C {mut a : int}; class C2 extends C {mut b : int}; let c = 1 in let x = C2(capof(c),c,2) in let f = fun (x | a : x C)->int { a.a } in f(capof(x), x)", V_int(1));
  ("class C {mut a : int}; class C2 {o : C};let c = 1 in let x = C(capof(c), c) in let y = C2(capof(x), x) in y.o.a", V_int(1));
  ("class C {mut a : int}; class C2 {o : C};let c = 1 in let x = C(capof(c), c) in let y = C2(capof(x), x) in y.o.a = 0; y.o.a", V_int(1));
  ("let x = 1 in destroy(x); x = 3; x", V_int(3));
  ("class C {mut a : int, mut b : int}; let c = 1 in let x = C(capof(c), c, c) in x.a", V_int(1));
  ("class C {mut a : int, mut b : int}; let c = 1 in let x = C(capof(c), c, c) in x.b", V_int(1));
  ("class C {mut a : int, mut b : int}; let c = 1 in let x = C(capof(c), c, c) in x.b; x.a", V_int(1));
  ("class C {mut U a : int, mut U b : int}; let c = 1 in let d = 2 in let x = C(capof(c), c, capof(d), d) in focus x { destroy(x.a); x.b }", V_int(2));
  ("class C {mut U a : int}; let c = 1 in let x = C(capof(c), c) in focus x { destroy(x) }", V_unit);
]

(* check that evaluation succeeded *)
let eval_success = [
  "class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y.a = x.a; y)";
  "class C {mut a : int}; let c = 1 in let d = 2 in let x = C(capof(c),c) in let y = C(capof(d),d) in (y = x; y)";
  "class C {mut a : int}; let c = 1 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { a } in f(capof(x), x)";
  "class C {mut a : int}; let c = 1 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { let y = a in y } in f(capof(x), x)";
  "sleep(0)";
]

(* check that evaluation failed *)
let eval_failure = [
  "let x = 1 in (destroy(x);destroy(x))";
  "let x = 1 in (destroy(x);x)";
  "let x = 1 in branch x {x}; x";
  "class C {mut a : int, mut b : int}; let c = 1 in let x = C(capof(c), c, c) in destroy(x.a);x.b";
  "let a = 1 in let f = fun (a | x : a int)->int { x } in destroy(a);f(capof(a), a)";
  "let a = 1 in let f = fun (a | x : a int)->unit { destroy(x) } in f(capof(a),a);a";
  "class C {mut a : int}; let c = 2 in let x = C(capof(c), c) in let f = fun (x | a : x C)->C { let y = a in y } in let z = f(capof(x), x) in x";
  "class C {mut a : int}; class C2 {o : C};let c = 1 in let d = 2 in let x = C(capof(c), c) in let x = C(capof(d), d) in let y = C2(capof(x), x) in y.o = z";
  "let x = 3 in sleep(x)";
  "let x = 3 in x.y";
  "while (1) { 0 }";
  "if (3) {2} else {1}";
  "let a = 1 in let f = fun (a | x : a int)-> bool { x+1 } in f(capof(a),a)";
  "let x = 1 in x(1)";
  "3 = 2";
  "class C {mut a : int}; let x = 1 in let y = C2(capof(x), x) in y";
  "let x = 1 in focus x { 1 }";
  "class C {mut U a : int}; let c = 1 in let x = C(capof(c), c) in destroy(x); x";
  "class C {mut U a : int}; let c = 1 in let x = C(capof(c), c) in destroy(x.a); x";
  "class C {mut U a : int}; let c = 1 in let x = C(capof(c), c) in focus x { destroy(x) }; x";
  "class C {mut U a : int}; let c = 1 in let x = C(capof(c), c) in focus x { destroy(x.a) }; x";
]

let run_tests = fun () -> 
  print_endline "check value:";
  let failed = List.fold_left ~f:(fun acc (s,exp) -> 
      try 
        (let res,st = eval_test s in
         if compare_values st exp res then (print_string "."; acc) else (print_string "F" ; (s ^ "\nincorrect value")::acc))
      with |Utils.GError m -> (print_string "E" ; (s^"\n"^m)::acc)
    ) ~init:[] eval_checkval 
  in
  print_endline "\ncheck eval success:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "."; acc with
      | Utils.GError m -> print_string "F"; (s^"\n"^m)::acc
    ) ~init:failed eval_success
  in
  print_endline "\ncheck eval failure:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "F"; (s ^ "\nexpected runtime error")::acc with
      | Utils.GError _ -> print_string "."; acc
    ) ~init:failed eval_failure
  in
  print_endline "\nFailed:";
  List.rev failed |> List.iter ~f:(fun f -> print_endline f);
  print_endline "\ndone!"

