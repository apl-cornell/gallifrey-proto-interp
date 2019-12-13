open Ast
open Utils

let sp = Printf.sprintf
let rec space n = if n = 0 then "" else " " ^ space (n-1)

let fmt_list f l =
  List.map f l |> String.concat ", "

(* TODO complete pretty-printing *)

let rec fmt_ast n c = 
  match c with
  |Int i -> sp "%d" i
  |Bool b -> if b then "true" else "false"
  |Unit -> "()"
  |Var v -> v
  |Binary(b, e1, e2) -> sp "%s %s %s" (fmt_ast 0 e1) (print_binop b) (fmt_ast 0 e2)
  |Fun(params, rtype, body) -> sp "%sfun(%s)->%s {\n%s\n}" (space n) (fmt_list fmt_param params) (fmt_type rtype) (fmt_ast (n+2) body)
  |Apply(a,b) -> sp "%sapply(%s %s)" (space n) (fmt_ast 0 a) (fmt_list (fmt_ast 0) b)
  |Object(c,o) -> "object"
  |Get(e,f) -> sp "%s.%s" (fmt_ast 0 e) f
  |Seq(e1, e2) -> sp "%s;\n%s" (fmt_ast n e1) (fmt_ast n e2)
  |If(c, e1, e2) -> sp "%sif (%s) {\n%s\n} else {\n%s\n}" (space n) (fmt_ast 0 c) (fmt_ast (n+2) e1) (fmt_ast (n+2) e2)
  |While(c, e) -> sp "%swhile (%s) {\n%s\n}" (space n) (fmt_ast 0 c) (fmt_ast (n+2) e)
  |Let(v, e1, e2) -> sp "%slet %s = %s in %s" (space n) v (fmt_ast 0 e1) (fmt_ast 0 e2)
  |Destroy e -> sp "destroy(%s)" (fmt_ast 0 e)
  |Sleep e -> sp "sleep(%s)" (fmt_ast 0 e)
  |Capof e -> sp "capof(%s)" (fmt_ast 0 e)
  |Branch(v,e) -> sp "%sbranch" (space n)
  |Focus(v, e2) -> sp "%sfocus %s {\n%s\n}" (space n) (v) (fmt_ast (n+2) e2)
  |Assign(e1, e2) -> sp "%s%s = %s" (space n) (fmt_ast 0 e1) (fmt_ast 0 e2)
  |Neg e -> sp "-%s" (fmt_ast 0 e)
  |Not e -> sp "!%s" (fmt_ast 0 e)
  |Class(c,t,super) -> sp "%sclass %s%s {%s}" (space n) c (match super with Some s -> " extends "^s | None -> "") "<object>"
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
  |V_obj(c,o) -> sp "object<%s|%s>" c (fmt_list fmt_field o)
  |V_fun(params, rtype, body, env) -> sp "fun<(%s)->%s {\n%s\n}>" (fmt_list fmt_param params) (fmt_type rtype) (fmt_ast 0 body)
  |V_ptr(l,l',m,t) -> sp "pointer<%d, %d, %s, %s>" l l' (if m = MUT then "mut" else "immut") (fmt_type t)
  |V_cap c  -> sp "cap<%s>" c
and fmt_type t = 
  match t with
  | T_unit -> "unit"
  | T_int -> "int"
  | T_bool -> "bool"
  | T_fun(params, return) -> sp "%s -> %s" (fmt_list fmt_param params) (fmt_type return)
  | T_cls name -> name
  | T_cap -> "cap"

and fmt_param p =  
  match p with
  |Lambda(v, c, t) -> sp "λ %s : %s %s" v c (fmt_type t)
  |SigmaLambda(v, c, t) -> sp "Λ %s : %s %s" v c (fmt_type t)
  |KappaLambda c -> sp "Λ %s" c

and fmt_field (var, (t,u,mut,loc)) = 
  let u = match u with |U -> "U" |A -> "" in
  let mut = match mut with |MUT -> "mut" |IMMUT -> "" in
  u ^ " " ^ mut ^ " " ^ var ^ " : " ^ (fmt_type t) ^ " " ^ (string_of_int loc)
  
let print_node node = fmt_ast 0 node

let print_val v = fmt_value v |> print_endline

let print_type t = fmt_type t |> print_endline

let expected_got t1 t2 = "expected " ^ (fmt_type t1) ^ " argument, got " ^ (fmt_type t2)
