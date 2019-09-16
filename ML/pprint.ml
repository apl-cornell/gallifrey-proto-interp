(* CS 4110 Homework 3
   A pretty-printer for the ASTs defined in ast.ml. This is useful for
   debugging, but you should not need to change it. *)

open Ast

(* let sp = Printf.sprintf

let rec pprintAexp a = print_string(strAexp a)
and pprintBexp b = print_string(strBexp b)
and pprintCom c = print_string(strCom(0, c))
and pprintInfo i = print_string(strInfo i)
and space n = if n = 0 then "" else " " ^ space(n-1)
and strAexp e = match e with
  | Int m -> sp "%d" m
  | Var x -> x
  | Plus(a1, a2) -> sp "(%s + %s)" (strAexp a1) (strAexp a2)
  | Minus(a1, a2) -> sp "(%s - %s)" (strAexp a1) (strAexp a2)
  | Times(a1, a2) -> sp "(%s * %s)" (strAexp a1) (strAexp a2)
  | Input -> "input"
and strBexp e = match e with
  | True -> "true"
  | False -> "false"
  | Equals(a1, a2) -> sp "(%s = %s)" (strAexp a1) (strAexp a2)
  | NotEquals(a1, a2) -> sp "(%s != %s)" (strAexp a1) (strAexp a2)
  | Less(a1, a2) -> sp "(%s < %s)" (strAexp a1) (strAexp a2)
  | LessEq(a1, a2) -> sp "(%s <= %s)" (strAexp a1) (strAexp a2)
  | Greater(a1, a2) -> sp "(%s > %s)" (strAexp a1) (strAexp a2)
  | GreaterEq(a1, a2) -> sp "(%s >= %s)" (strAexp a1) (strAexp a2)
  | Not(b) -> sp "not(%s)" (strBexp b)
  | And(b1, b2) -> sp "(%s and %s)" (strBexp b1) (strBexp b2)
  | Or(b1, b2) -> sp "(%s or %s)" (strBexp b1) (strBexp b2)
and strCom(n, c) =
  match c with
  | Skip -> sp "%sskip" (space n)
  | Assign(x, a) -> sp "%s%s := %s" (space n) x (strAexp a)
  | Seq(c1, c2) -> sp "%s;\n%s" (strCom(n, c1)) (strCom(n, c2))
  | If(b, c1, c2) ->
    sp "%sif %s then {\n%s\n%s} else {\n%s\n%s}"
      (space(n)) (strBexp b)
      (strCom(n+2,c1)) (space n) (strCom(n+2,c2)) (space n)
  | While(b, c) ->
    sp "%swhile %s do {\n%s\n%s}"
      (space n) (strBexp b) (strCom(n+2,c)) (space n)
and strInfo ((l1,c1),(l2,c2)) =
  if l2=l1
  then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
  else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 *)
