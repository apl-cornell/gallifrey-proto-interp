open Core

(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

(* A type for symboltables. *)
type gtype = 
  | T_unit 
  | T_int 
  | T_bool 
  | T_func of (gtype list) * gtype
  | T_obj of (var * gtype * bool) list
and cap = string
and var = string

type binop =
  | BinopAnd
  | BinopOr
  | BinopPlus
  | BinopMinus
  | BinopTimes
  | BinopDiv
  | BinopMod
  | BinopLt
  | BinopLeq
  | BinopGt
  | BinopGeq
  | BinopNeq
  | BinopEq

type expr =
  |Int of int
  |Bool of bool
  |Unit
  |Var of var
  |Binary of binop * expr * expr
  |Fun of (var * gtype * bool) list * gtype * expr
  |Apply of var * expr list
  |Object of (var * expr * bool * bool) list
  |Get of expr * string
  |Seq of expr * expr
  |If of expr * expr * expr
  |While of expr * expr
  |Let of var * expr * expr
  |Destroy of expr
  |Sleep of expr
  |Branch of var list * expr
  |Focus of expr * expr
  |Assign of expr * expr
  |Neg of expr
  |Not of expr


