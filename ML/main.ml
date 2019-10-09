open Eval
open Pprint

let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: gallifrey <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast =
    try Parser.p Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 
      in 
    let res, _, _, _ = Eval.eval Eval.init_state ast in
    print_endline (Pprint.fmt_value res)
