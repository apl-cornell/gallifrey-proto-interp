(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

(* A type for symboltables. *)
type gt = T_unit | T_int | T_bool | T_func of (gt list) * gt * symboltable  | T_obj of (string * gt) list
and symboltable = (string * gt) list



type restriction = string

type qualifier = Shared of restriction | Local | Unique

type var = string

type decl = qualifier * gt * var

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
  |Var of var
  |Binary of binop * expr * expr
  |Fun of decl list * expr
  |Apply of expr * expr list
  |Object of (var * expr) list
  |Get of expr * string
  |Seq of expr * expr
  |If of expr * expr * expr
  |While of expr * expr
  |Let of expr * expr
  |Skip
  |Destroy of expr
  |LetC of expr * expr
