open Eval
open Pprint
open Test

let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: gallifrey <file> OR gallifrey -test\n";
       exit 0) in
  let filename = Sys.argv.(1) in
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
    let st = Eval.init_state in
    let res, _, _, _ = Eval.eval st ast in
    let res = State.deref st res in
    print_endline (Pprint.fmt_value res)
