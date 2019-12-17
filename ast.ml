(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

(* A type for symboltables. *)
type gtype = 
  | T_unit 
  | T_int 
  | T_bool 
  | T_fun of param list * gtype
  | T_cls of string
  | T_cap
and t_obj = (var * gtype * unique * mut) list
and cap = string
and var = string
and mut = MUT | IMMUT
and unique = U | A
and param = Lambda of var * var * gtype | SigmaLambda of var * var * gtype | KappaLambda of var

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
  |Fun of param list * gtype * expr
  |Apply of expr * (expr list)
  |Object of var * ((var * expr * unique * mut) list)
  |Get of expr * string
  |Seq of expr * expr
  |If of expr * expr * expr
  |While of expr * expr
  |Let of var * expr * expr
  |Destroy of expr
  |Sleep of expr
  |Capof of expr
  |Branch of var list * expr
  |Focus of var * expr
  |Assign of expr * expr
  |Neg of expr
  |Not of expr
  |Class of var * t_obj * (var option)



