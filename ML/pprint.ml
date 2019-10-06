(* CS 4110 Homework 3
   A pretty-printer for the ASTs defined in ast.ml. This is useful for
   debugging, but you should not need to change it. *)

open Ast

let sp = Printf.sprintf
let rec space n = if n = 0 then "" else " " ^ space (n-1)

let print_list f l =
  List.map l f |> String.concat ", "

let rec print_ast n c = 
  match c with
  |Int i -> sp "%d" i
  |Bool b -> if b then "true" else "false"
  |Unit -> "()"
  |Var v -> v
  |Binary(b, e1, e2) -> sp "%s %s %s" (print_ast 0 e1) (print_binop b) (print_ast 0 e2)
  |Fun(p,r,e) -> sp "function"
  |Apply(a,b) -> "apply"   
  |Object o -> "object"
  |Get(e,f) -> sp "%s.%s" (print_ast 0 e) f
  |Seq(e1, e2) -> sp "%s;\n%s" (print_ast n e1) (print_ast n e2)
  |If(c, e1, e2) -> sp "%sif (%s) {\n%s\n} else {\n%s\n}" (space n) (print_ast 0 c) (print_ast (n+2) e1) (print_ast (n+2) e2)
  |While(c, e) -> sp "%swhile (%s) {\n%s\n}" (space n) (print_ast 0 c) (print_ast (n+2) e)
  |Let(v, e1, e2) -> sp "%slet %s := %s in %s" (space n) v (print_ast 0 e1) (print_ast 0 e2)
  |Destroy e -> sp "destroy(%s)" (print_ast 0 e)
  |Sleep e -> sp "sleep(%s)" (print_ast 0 e)
  |Branch(v,e) -> sp "%sbranch" (space n)
  |Focus(e1, e2) -> sp "%sfocus (%s) {\n%s\n}" (space n) (print_ast 0 e1) (print_ast (n+2) e2)
  |Assign(e1, e2) -> sp "%s%s := %s" (space n) (print_ast 0 e1) (print_ast 0 e2)
  |Neg e -> sp "-%s" (print_ast 0 e)
  |Not e -> sp "!%s" (print_ast 0 e)
and print_binop = function
  | BinopAnd -> "&"
  | BinopOr -> "|"
  | BinopPlus -> "+"
  | BinopMinus -> "-"
  | BinopTimes -> "*"
  | BinopDiv -> "/"
  | BinopMod -> "%"
  | BinopLt -> "<"
  | BinopLeq -> "<="
  | BinopGt -> ">"
  | BinopGeq -> ">="
  | BinopNeq -> "!="
  | BinopEq -> "=="
