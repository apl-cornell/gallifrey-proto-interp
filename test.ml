open Eval
open Utils
open Core
open Core.Poly

let eval_test s = 
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.p Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.sprintf "Syntax error: (%d,%d) %s\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1)
        s |> print_endline;
      exit 1 
  in 
  let st = Eval.init_state () in
  let res, (r,_), _, _ = Eval.eval st ast in
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
  ("false | false", V_bool(false));
  ("10 * 2", V_int(20));
  ("10 / 2", V_int(5));
  ("10 % 7", V_int(3));
  ("1 == 2", V_bool(false));
  ("let x = 1 in x == 1", V_bool(true));
  ("1 != 2", V_bool(true));
  ("1 >= 2", V_bool(false));
  ("1 <= 2", V_bool(true));
  ("1 < 2", V_bool(true));
  ("let x = 1 in 
    x;x", V_int(1));
  ("let x = 1 in 
    x + x", V_int(2));
  ("let x = true in 
    if x {4} else {6}", V_int(4));
  ("let x = false in 
    if x {4} else {6}", V_int(6));
  ("let x = 1 in 
    let y = 5 in 
    x + y", V_int(6));
  ("let x = 2 in 
    let y = x in 
    y + y", V_int(4));
  ("let x = 2 in 
    let y = x in 
    x + x", V_int(4));
  ("let x = 2 in 
    let y = x + 1 in 
    y + x", V_int(5));
  ("class C {mut a : int}; let c = 1 in 
    let x = C(capof(c),c) in 
    (x.a = 3; x.a)", V_int(3));
  ("class C {mut a : int}; let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    y.a = x.a; x.a = 3; x.a", V_int(3));
  ("class C {mut a : int}; let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    y.a = x.a; x.a = 3; y.a", V_int(1));
  ("class C {mut a : int}; let c = 1 in 
    let x = C(capof(c),c) in 
    x.a = x.a; x.a", V_int(1));
  ("let x = 1 in 
    -x", V_int(-1));
  ("class C {mut a : int}; let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    y.a = x.a; y.a", V_int(1));
  ("let x = 1 in 
    let y = 5 in 
    (destroy(x);y)", V_int(5));
  ("let x = 1 in 
    let y = x in 
    destroy(x); y", V_int(1));
  ("let x = true in 
    if x { !x } else { x }", V_bool(false));
  ("class C {mut a : int}; let c = 1 in 
    let x = C(capof(c),c) in 
    x.a = x.a + 1; x.a", V_int(2));
  ("let x = 1 in 
    let y = 0 in 
    branch x {x}; y", V_int(0));
  ("class C {mut a : int}; let c = 1 in 
    let x = C(capof(c),c) in 
    while (x.a > 0) {x.a = x.a - 1}; x.a", V_int(0));
  ("class C {mut a : int}; let c = 3 in 
    let x = C(capof(c),c) in 
    while (x.a > 0) {x.a = x.a - 1}; x.a", V_int(0));
  ("class C {mut a : int}; let c = 3 in 
    let c2 = 0 in 
    let x = C(capof(c), c) in 
    let x2 = C(capof(c2),c2) in 
    while (x.a > 0) {x.a = x.a - 1; x2.a = x2.a + 2}; x2.a", V_int(6));
  ("class C {mut a : bool}; let c = false in 
    let x = C(capof(c), c) in 
    let c2 = false in 
    let y = C(capof(c2), c2) in 
    while (x.a) {y.a = true}; y.a", V_bool(false));
  ("class C {mut a : int, mut b : int}; let c = 1 in 
    let x = C(capof(c), c, c) in 
    destroy(x.a)", V_unit);
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { x } in 
    f(capof(a),a)", V_int(1));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { x } in 
    let f2 = fun (a | x : a int)->int { f(capof(x), x) } in 
    f2(capof(a),a)", V_int(1));
  ("let a = 1 in 
    let b = 2 in 
    let f = fun (a | x : a int) (b | y : b int)->int { x + y } in 
    f(capof(a), a, capof(b), b)", V_int(3));
  ("let a = 1 in 
    let f = fun (a | x : a int, y: a int)->int { x + y } in 
    f(capof(a), a, a)", V_int(2));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { 10 } in 
    f(capof(a),a); a", V_int(1));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { x+1 } in 
    f(capof(a),a)", V_int(2));
  ("let a = 1 in 
    let f = fun (a | x : a int)->unit { x = 0 } in 
    f(capof(a),a); a", V_int(1));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { 
      let x2 = 0 in x2 
    } in 
    f(capof(a),a)", V_int(0));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { 
      let x2 = 0 in x2 
    } in 
    f(capof(a),a); a", V_int(1));
  ("let a = 1 in 
    let f = fun (a | x : a int)->int { 
      let x2 = 0 in x2 
    } in 
    let b = f(capof(a),a) in 
    b", V_int(0));
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    x; x.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    x.a; x.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    (y.a = x.a; x.a)", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    y.a = x.a; x.a = 0; y.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    y.a = 0; y.a", V_int(0));
  (* immutable object does not consume *)
  ("class C {a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    y.a", V_int(1));
  ("class C {a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    x.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { a } in 
    f(capof(x),x);x.a", V_int(2));
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->int { a.a } in 
    f(capof(x),x)", V_int(2));
  ("class C {mut a : int}; 
    let c = 1 in 
    let f = fun (x| a : x int)->C { C(capof(a), a) } in 
    (f(capof(c), c)).a", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let f = fun (x| a : x int)->C { C(capof(a), a) } in 
    let x = f(capof(c), c) in 
    x.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->int { 
      let y = a.a in y 
    } in 
    f(capof(x),x)", V_int(2));
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { 
      let y = a in y 
    } in 
    (f(capof(x),x)).a", V_int(2));
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { a } in 
    (f(capof(x),x)).a", V_int(2));  
  ("class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { 
      let y = a in y 
    } in 
    let z = f(capof(x),x) in 
    z.a", V_int(2));
  ("class C {a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | z : x int)->int { 
      let z2 = z + 1 in z2 
    } in 
    f(capof(x),x.a)", V_int(3));
  ("class C {a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | z : x int)->int { let z2 = z + 1 in 
    z2 } in 
    f(capof(x),x.a); x.a", V_int(2));
  ("class C {a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | z : x int)->int { 
      let z2 = z + 1 in z2 
    } in 
    f(capof(x),x.a)", V_int(3));
  ("class C {a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | z : x int)->int { 
      let z2 = z + 1 in z2 
    } in 
    f(capof(x),x.a); x.a", V_int(2));
  ("class C {a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | z : x int)->int { 
      let z2 = z + 1 in z2 
    } in 
    f(capof(x),x.a); x.a", V_int(2));
  ("class C {mut a : int}", V_unit);
  ("class C {mut U a : int}", V_unit);
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    let f = fun (c | a : c int)->int { x.a } in 
    x.a = 2; 
    f(capof(c),c)", V_int(2));
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    x.a", V_int(1));
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    x.a = 5; 
    x.a", V_int(5));
  ("class C {mut a : int}; 
    class C2 extends C {mut b : int}; 
    let c = 1 in 
    let x = C2(capof(c),c,2) in 
    x.b", V_int(2));
  ("class C {mut a : int}; 
    class C2 extends C {mut b : int}; 
    let c = 1 in 
    let x = C2(capof(c),c,2) in 
    x.a", V_int(1));
  ("class C {mut a : int}; 
    class C2 extends C {mut b : int}; 
    let c = 1 in 
    let x = C2(capof(c),c,2) in 
    let f = fun (x | a : x C)->int { a.a } in 
    f(capof(x), x)", V_int(1));
  ("class C {mut a : int}; 
    class C2 {o : C};
    let c = 1 in 
    let x = C(capof(c), c) in 
    let y = C2(capof(x), x) in 
    y.o.a", V_int(1));
  ("class C {mut a : int}; 
    class C2 {mut o : C};
    let c = 1 in 
    let x = C(capof(c), c) in 
    let y = C2(capof(x), x) in 
    y.o.a = 0; 
    y.o.a", V_int(0));
  ("class C {mut a : int}; 
    class C2 {o : C};
    let c = 1 in 
    let x = C(capof(c), c) in 
    let y = C2(capof(x), x) in 
    y.o.a = 0; 
    y.o.a", V_int(0));
  (* TODO - destroying x.a and x are the same?, cannot assign to aliasable fields if x or x.a are destroyed *)
  ("class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x); 
    x.a = 3; 
    let y = x.a in y", V_int(3));
  ("class C {mut a : int, mut b : int}; 
    let c = 1 in 
    let x = C(capof(c), c, c) in 
    x.a", V_int(1));
  ("class C {mut a : int, mut b : int}; 
    let c = 1 in 
    let x = C(capof(c), c, c) in 
    x.b", V_int(1));
  ("class C {mut a : int, mut b : int}; 
    let c = 1 in 
    let x = C(capof(c), c, c) in 
    x.b; x.a", V_int(1));
  ("class C {mut U a : int, mut U b : int}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c), capof(d), c, d) in 
    focus x { 
      destroy(x.a); 
      let y = x.b in y 
    }", V_int(2));
  ("class C {mut U a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    focus x { 
      destroy(x.a); x.a = 5 
    }; 
    let y = x.a in y", V_int(5));
  ("class C { 
        mut w : (x : c0 C |) (c1 | a : c1 int) (c2 | b : c2 int)->int 
    }; 
    let iters = 2 in 
    let init = 0 in 
    let f = fun (x : c0 C |) (c1 | a : c1 int) (c2 | b : c2 int)->int { 
        if (a == 0) { 
          b 
        } else { 
            let aa = a - 1 in 
            let bb = b + 2 in 
            x.w(x, capof(aa),aa,capof(bb),bb) 
        }
    } in
    let x = C(capof(f), f) in 
    focus x { 
        x.w(x, capof(iters),iters,capof(init),init) 
    }", V_int(4));
  ("let hof = fun (c1 | f : c1 (c2 | x : c2 int)->int) (c3| n : c3 int)->int { \n"^
   " let r = f(capof(n), n) in f(capof(r), r) } in \n"^
   " let double = fun (c | x : c int)->int { x * 2 } in let init = 3 in \n"^ 
   " hof(capof(double), double, capof(init), init)", V_int(12));
  (* TODO are both of these valid? I think so because x and  *)
  ("class C {a : int}; class C2 {mut a : int}; 
    let c1 = 0 in 
   let x = C(capof(c1), c1) in 
   let c2 = x.a in 
   let y = C2(capof(c2), c2) in 
   y.a", V_int(0));
  ("class C {a : int}; class C2 {mut a : int}; 
   let c1 = 0 in 
   let x = C(capof(c1), c1) in 
   let c2 = x.a in 
   let y = C2(capof(c2), c2) in 
   x.a", V_int(0));
  ("class C {mut a : int, mut b : int}; 
    let c = 1 in 
   let d = 2 in 
   let x = C(capof(c), c, c) in 
   x.a = x.b * 2; x.a", V_int(2));
]

(* check that evaluation succeeded *)
let eval_success = [
  "class C {mut a : int}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    (y.a = x.a; y)";
  "class C {mut a : int}; 
    class C2 {mut o : C}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    let z = C2(capof(x),x) in 
    (z.o = y; z.o)";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { a } in 
    f(capof(x), x)";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { 
      let y = a in y 
    } in 
    f(capof(x), x)";
  "sleep(0)";
  "class C {a : int}; 
    class C2 {mut a : int}; 
    let c1 = 0 in 
    let x = C(capof(c1), c1) in 
    let c2 = x.a in 
    let y = C2(capof(c2), c2) in y";
  "class C {a : int}; 
    class C2 {mut a : int}; 
    let c1 = 0 in 
    let x = C(capof(c1), c1) in 
    let c2 = x.a in 
    let y = C2(capof(c2), c2) in x"
]

(* check that evaluation failed *)
let eval_failure = [
  "let x = 1 in 
    (destroy(x);destroy(x))";
  "let x = 1 in 
    (destroy(x); (x + 1))";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x.a); 
    let y = x.a in y";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x.a); 
    let y = x in y";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x); 
    let y = x.a in y";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x); 
    let y = x in y";
  "let x = 1 in 
    branch x {x}; 
    x + 1";
  "class C {mut a : int, mut b : int}; 
    let c = 1 in 
    let x = C(capof(c), c, c) in 
    destroy(x.a); 
    (x.b + 1)";
  "let a = 1 in 
    let f = fun (a | x : a int)->int { x } in 
    destroy(a);
    f(capof(a), a)";
  "let a = 1 in 
    let f = fun (a | x : a int)->unit { destroy(x) } in 
    f(capof(a),a); (a + 1)";
  "class C {mut a : int}; 
    let c = 2 in 
    let x = C(capof(c), c) in 
    let f = fun (x | a : x C)->C { let y = a in 
    y } in 
    let z = f(capof(x), x) in x";
  "class C {mut a : int}; 
    class C2 {o : C};let c = 1 in 
    let d = 2 in 
    let x = C(capof(c), c) in 
    let x = C(capof(d), d) in 
    let y = C2(capof(x), x) in 
    y.o = z";
  "let x = 3 in sleep(x)";
  "let x = 3 in x.y";
  "3.f";
  "while (1) { 0 }";
  "if (3) {2} else {1}";
  "let a = 1 in 
    let f = fun (a | x : a int)-> bool { x+1 } in 
    f(capof(a),a)";
  "let a = 1 in 
    let f = fun (a | x : a int)-> int { x+1 } in 
    f(capof(a))";
  "class C {a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    x.a = 5; x.a";
  (* TODO do constructors consume args *)
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    let y = x.a in 
    let z = x.a in z";
  "class C {mut a : int}; 
    class C2 {mut o : C}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    let z = C2(capof(x),x) in 
    z.o = y; y";
  "class C {mut a : int}; 
    class C2 {mut o : C}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c),c) in 
    let y = C(capof(d),d) in 
    let z = C2(capof(x),x) in 
    z.o = y; x";
  "let x = 1 in x(1)";
  "3 = 2";
  "class C {mut a : int}; 
    let x = 1 in 
    let y = C2(capof(x), x) in y";
  "let x = 1 in 
    focus x { 1 }";
  "class C {mut U a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x); x + 1";
  "class C {mut U a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x.a); x + 1";
  "class C {mut U a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    focus x { destroy(x) }; 
    x + 1";
  "class C {mut U a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    focus x { destroy(x.a) }; 
    x + 1";
  "class C {mut a : int}; 
    let c = true in 
    let x = C(capof(c), c) in x";
  "class C {mut a : int}; 
    let c1 = 0 in 
    let c2 = true in 
    let x = C(capof(c1), c1) in 
    x.a = c2; x";
  (* mutable object assignment consumes cap *)
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    (x.a = 0; y.a)";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    (x.a = 0; x.a)";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c),c) in 
    let y = x in 
    (y.a = 0; x.a)";
  (* recursion SO *)
  "class C { mut w : (x : c0 C |)->unit }; 
    let f = fun (x : c0 C |)->unit { x.w(x) } in 
    let x = C(capof(f), f) in 
    focus x { x.w(x) }";
  "class C {mut U a : int, mut b : int}; 
    let c = 1 in 
    let d = 2 in 
    let x = C(capof(c), capof(d), c, d) in 
    x.a = x.b; x";
  "class C {mut a : int}; 
    let c = 1 in 
    let x = C(capof(c), c) in 
    destroy(x.a); 
    x.a = 3; 
    let y = x.a in y";
]

let run_tests = fun () -> 
  print_endline "check value:";
  let failed = List.fold_left ~f:(fun acc (s,exp) -> 
      try 
        (let res,st = eval_test s in
         if compare_values st exp res then (print_string "."; acc) else (print_string "F" ; (s ^ "\nincorrect value\n")::acc))
      with |Utils.GError m -> (print_string "E" ; (s^"\n"^m^"\n")::acc)
    ) ~init:[] eval_checkval 
  in
  print_endline "\ncheck eval success:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "."; acc with
      | Utils.GError m -> print_string "F"; (s^"\n"^m^"\n")::acc
    ) ~init:failed eval_success
  in
  print_endline "\ncheck eval failure:";
  let failed = List.fold_left ~f:(fun acc s -> 
      try ignore (eval_test s); print_string "F"; (s ^ "\nexpected runtime error\n")::acc with
      | Utils.GError _ -> print_string "."; acc
    ) ~init:failed eval_failure
  in
  print_endline "\nFailed:";
  List.rev failed |> List.iter ~f:(fun f -> print_endline f);
  print_endline "\ndone!"

