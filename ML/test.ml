open Eval
open Utils

let eval_success = [
  (Let("x",Int(1),Seq(Var("x"),Var("x"))), V_int(1));
  (Let("x",Int(1),Seq(Sleep(Int(1)),Var("x"))), V_int(1));
  (Let("x",Bool(true),If(Var("x"),Int(4),Int(6))), V_int(4));
  (Let("x",Bool(false),If(Var("x"),Int(4),Int(6))), V_int(6));
  (* TODO while *)
  (Let("x",Int(1),Let("y", Int(5), Binary(BinopPlus, Var("x"), Var("y")))), V_int(6));
  (Let("x",Int(1),Let("y", Int(5), Seq(Assign(Var("y"), Var("x")), Binary(BinopPlus, Var("y"), Var("y"))))), V_int(12));
]

let eval_failure = []

let run_tests = fun () -> 
  print_endline "evaluation tests:";
  List.map (fun (ast,exp) -> 
      let st = Eval.init_state in
      try 
        (let res, _, _, _ = Eval.eval st ast in
         let res = State.deref st res in
         if exp = res then "." else "F")
      with |Utils.GError _ -> "E"
    ) eval_success |> String.concat "" |> print_endline;
  List.map (fun (ast,exp) -> 
      let st = Eval.init_state in
      try ignore (Eval.eval st ast); "F" with
      | Utils.GError _ -> "."
    ) eval_failure |> String.concat "" |> print_endline;
  print_endline "done!"

