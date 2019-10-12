open Core
include Ast

module CapSet = Set.Make(String)
let c_any = "ANY"
let c_none = "NONE"

type value = 
  |V_int of int
  |V_bool of bool
  |V_unit
  |V_obj of (var * fieldinfo) list
  (* boolean is uniqueness *)
  |V_fun of (var * gtype * bool) list * gtype * expr * t_store
  |V_ptr of loc * gtype
and t_store = (var, storeinfo) Hashtbl.t
and loc = int
(* type, capability, mutable, location *)
and storeinfo = (gtype * cap * mut * loc)
(* type, unique, mutable, location *)
and fieldinfo = (gtype * unique * mut * loc)
and memory = (loc, value) Hashtbl.t

let get_type = function
  | V_int _ -> T_int
  | V_bool _ -> T_bool
  | V_unit -> T_unit
  | V_obj fields -> begin
    let t_fields = List.map fields 
    (fun f -> 
      let fname = fst f in
      let t, _, mut, _ = snd f in
      (fname, t, mut)
    ) in T_obj(t_fields)
  end
  | V_ptr(l,t) -> t
  | V_fun(params, return, _, _) -> begin
    let param_types = List.map params 
    (fun (_, t, _) -> t)
    in T_fun(param_types, return)
  end

let reconcile_read c1 c2 k1 k2 = 
  match c1, c2 with
  |(a,b) when a = b -> a
  (* top relabel *)
  |("ANY",b) -> b
  |(a, "ANY") -> a
  (* drop read *)
  |("NONE", b) -> c_none
  |(a,"NONE") -> c_none
  (* read relabel *)
  |(a,b) -> if CapSet.mem k1 a then b 
      else if CapSet.mem k2 b then a
      else c_none (* drop read  *)

(* TODO relabeling for writes? *)
let reconcile_write c1 c2 = 
  match c1, c2 with
  |(a,b) when a = b -> a
  (* drop write *)
  |("NONE", b) -> c_none
  |(a,"NONE") -> c_none
  |_ -> c_none

let reconcile_caps (r1, w1) (r2, w2) k1 k2 = 
  (reconcile_read r1 r2 k1 k2, reconcile_write w1 w2)

(* check if read capability (RHS) and write capability (LHS) matches *)
let can_write w r k = 
  match (w,r) with
  |("NONE", _) -> failwith "cannot write"
  |(_, "NONE") -> failwith "cannot read"
  |(a, b) when a = b -> a
  |("ANY", b) -> b
  |(a, "ANY") -> a
  |(a,b) -> if CapSet.mem k r then a 
            else failwith "incompatible caps"