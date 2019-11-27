open Eval
open Pprint
open Test

let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: gallifrey <file> OR gallifrey -test\n";
       exit 0) in
  let filename = try Sys.argv.(1) with _ -> "-test" in
  if filename = "-test" then Test.run_tests () else
    let lexbuf = Lexing.from_channel (open_in filename) in
    let ast =
      try Parser.p Lexer.token lexbuf
      with Parsing.Parse_error ->
        Printf.printf "Syntax error at line %d character %d\n"
          !Lexer.lineno
          (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
        exit 1 
    in 
    (* print_endline (fmt_ast 0 ast); *)
    let st = Eval.init_state () in
    let res, (r,_), _, _ = Eval.eval st ast in
    g_assert (res = V_unit || r <> c_none) "cannot read evaluation result";
    let res = State.deref st res |> State.deref st in
    print_endline (Pprint.fmt_value res)
