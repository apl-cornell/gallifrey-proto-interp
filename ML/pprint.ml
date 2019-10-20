open Ast
open Eval

let sp = Printf.sprintf
let rec space n = if n = 0 then "" else " " ^ space (n-1)

let fmt_list f l =
  List.map l f |> String.concat ", "

(* TODO complete pretty-printing *)

let rec fmt_ast n c = 
  match c with
  |Int i -> sp "%d" i
  |Bool b -> if b then "true" else "false"
  |Unit -> "()"
  |Var v -> v
  |Binary(b, e1, e2) -> sp "%s %s %s" (fmt_ast 0 e1) (print_binop b) (fmt_ast 0 e2)
  |Fun(cl,caps,p,r,e) -> sp "function"
  |Apply(a,b) -> "apply"   
  |Object o -> "object"
  |Get(e,f) -> sp "%s.%s" (fmt_ast 0 e) f
  |Seq(e1, e2) -> sp "%s;\n%s" (fmt_ast n e1) (fmt_ast n e2)
  |If(c, e1, e2) -> sp "%sif (%s) {\n%s\n} else {\n%s\n}" (space n) (fmt_ast 0 c) (fmt_ast (n+2) e1) (fmt_ast (n+2) e2)
  |While(c, e) -> sp "%swhile (%s) {\n%s\n}" (space n) (fmt_ast 0 c) (fmt_ast (n+2) e)
  |Let(v, e1, e2) -> sp "%slet %s := %s in %s" (space n) v (fmt_ast 0 e1) (fmt_ast 0 e2)
  |Destroy e -> sp "destroy(%s)" (fmt_ast 0 e)
  |Sleep e -> sp "sleep(%s)" (fmt_ast 0 e)
  |Branch(v,e) -> sp "%sbranch" (space n)
  |Focus(e1, e2) -> sp "%sfocus (%s) {\n%s\n}" (space n) (fmt_ast 0 e1) (fmt_ast (n+2) e2)
  |Assign(e1, e2) -> sp "%s%s := %s" (space n) (fmt_ast 0 e1) (fmt_ast 0 e2)
  |Neg e -> sp "-%s" (fmt_ast 0 e)
  |Not e -> sp "!%s" (fmt_ast 0 e)
  |Class(c,t) -> sp "%sclass %s {%s}" (space n) c (fmt_type t)
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
and fmt_value v = 
  match v with
  |V_int i -> (string_of_int i)
  |V_bool b -> (string_of_bool b)
  |V_unit -> "()"
  |V_obj o -> "object<>"
  |V_fun(cls, caps, p, ret, e, closure) -> "closure<>"
  |V_ptr(l,l',m,t) -> sp "pointer<%s>" (fmt_type t)
and fmt_type t = 
  match t with
  | T_unit -> "unit"
  | T_int -> "int"
  | T_bool -> "bool"
  | T_fun(params, return) -> sp "%s -> %s" (fmt_list params fmt_type) (fmt_type return)
  | T_obj fields -> "<object>"
