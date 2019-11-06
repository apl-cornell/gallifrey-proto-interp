{
open Parser
open Printf
exception Eof
exception LexingError of string

let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno

let info lexbuf =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let ((l1,c1),(l2,c2)) = i in
  let s =
    if l2=l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  let err = Printf.sprintf "%s: lexing error %s at %s."
    s
    msg
    t in
  raise (LexingError err)
}

let digit = ['-']?['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let cid = ['A'-'Z'] ['a'-'z' '0'-'9']*
let ws = [' ' '\t']

rule token = parse
| ws      { token lexbuf }
| '\n'    { newline lexbuf; token lexbuf }
| "->"     { ARROW(info lexbuf) }
| "+"     { PLUS(info lexbuf) }
| "-"     { MINUS(info lexbuf) }
| "*"     { TIMES(info lexbuf) }
| "/"     { DIV(info lexbuf) }
| "%"     { MOD(info lexbuf) }
| "("     { LPAREN(info lexbuf) }
| ")"     { RPAREN(info lexbuf) }
| "{"     { LBRACE(info lexbuf) }
| "}"     { RBRACE(info lexbuf) }
| "=="    { EQUALS(info lexbuf) }
| "!="    { NOTEQUALS(info lexbuf) }
| "<"     { LT(info lexbuf) }
| "<="    { LEQ(info lexbuf) }
| ">"     { GT(info lexbuf) }
| ">="    { GEQ(info lexbuf) }
| "="    { ASSIGN(info lexbuf) }
| ";"     { SEMI(info lexbuf) }
| ":"     { COLON(info lexbuf) }
| ","     { COMMA(info lexbuf) }
| "."     { DOT(info lexbuf) }
| "true"  { TRUE(info lexbuf) }
| "false" { FALSE(info lexbuf) }
| "!"   { NOT(info lexbuf) }
| "&"   { AND(info lexbuf) }
| "|"    { OR(info lexbuf) }
| "()"  { UNIT(info lexbuf) }
| "if"    { IF(info lexbuf) }
| "else"  { ELSE(info lexbuf) }
| "while" { WHILE(info lexbuf) }
| "int"    { T_INT(info lexbuf) }
| "bool"    { T_BOOL(info lexbuf) }
| "unit"  { T_UNIT(info lexbuf) }
| "fun"   { LAMBDA(info lexbuf) }
| "let"    { LET(info lexbuf) }
| "in"    { IN(info lexbuf) }
| "branch"    { BRANCH(info lexbuf) }
| "destroy"    { DESTROY(info lexbuf) }
| "sleep"    { SLEEP(info lexbuf) }
| "focus"    { FOCUS(info lexbuf) }
| "U"    { U(info lexbuf) }
| "mut"    { MUT(info lexbuf) }
| "class"    { CLASS(info lexbuf) }
| "this"    { THIS(info lexbuf) }
| id as v { VAR(info lexbuf, v) }
| cid as v { CVAR(info lexbuf, v) }
| digit+ as n { INT(info lexbuf, int_of_string n) }
| eof     { EOF }
| _ as c  { error lexbuf (String.make 1 c) }
