(* CS 4110 Homework 3
   The top-level entry point for running our interpreter. You shouldn't need
   to change this file; instead, change the interpreter itself in eval.ml. *)

(* Main function. *)
let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: imp <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let c =
    try Parser.p Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  (* ignore (Eval.evalc (Eval.make_configuration c)) *) c ; ()
