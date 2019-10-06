open Ast
open Core

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string

(* TODO *)
type value = 
  |V_Int of int
  |V_bool of bool
  |V_unit of unit
  |V_obj of (var * loc * cap * bool) list
  |V_fun of (var * gtype * bool) list * gtype * expr * store
and store = (string * gtype) list
and loc = int

module CapSet = Set.Make(String)

type state = {
  caps: Set.S;
  store: store list;
  focus: Set.S list;
  classes: unit;
}

