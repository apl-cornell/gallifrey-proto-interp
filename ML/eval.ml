include Ast
open Core

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string

(* TODO *)
module CapSet = Set.Make(String)
let c_any = "ANY"
let c_none = "NONE"

type value = 
  |V_int of int
  |V_bool of bool
  |V_unit
  |V_obj of (var * storeinfo) list
  |V_fun of (var * gtype * bool) list * gtype * expr * t_store
and t_store = (var, storeinfo) Hashtbl.t
and loc = int
(* type, capability, mutable, location *)
and storeinfo = (gtype * cap * bool * loc)
and memory = (loc, value) Hashtbl.t

module State = struct
  type t = {
    k: CapSet.t;
    store: t_store list;
    focus: unit; (* stack of maps? *)
    classes: unit; (* unsure about this one *)
    mem: memory;
    counter: int ref;
  }

  let init = {
    k = CapSet.empty;
    store = [Hashtbl.create (module String)];
    focus = ();
    classes = ();
    mem = Hashtbl.create (module Int);
    counter = ref 0;
  }
  
  let new_count s = 
    s.counter := !(s.counter) + 1; !(s.counter)
  
  let enter_scope s = 
    {s with store = (Hashtbl.create (module String))::s.store}
  
  let exit_scope s = 
    match s.store with
    |[] -> failwith "unable to exit scope"
    |h::[] -> failwith "unable to exit scope"
    |h::t -> {s with store = t}

  (* try to find variable in store *)
  let find_var s x = 
    let rec find_helper st x =
    match st with
    |[] -> failwith "not found"
    |h::t -> begin
      match Hashtbl.find h x with
      |Some v -> v
      |None -> find_helper t x
    end
    in find_helper s.store x
    
  (* get value at memory location *)
  let get_mem s loc = Hashtbl.find_exn s.mem loc

  (* check it capability is in K *)
  let has_cap s c = CapSet.mem s.k c

end




(* value, read, write, K', P *)
type result = value * cap * cap * CapSet.t * CapSet.t

let rec eval (st:State.t) (exp:expr): result = 
  match exp with
  |Int i -> V_int(i), c_any, c_none, CapSet.empty, CapSet.empty
  |Bool b -> V_bool(b), c_any, c_none, CapSet.empty, CapSet.empty
  |Unit -> V_unit, c_any, c_none, CapSet.empty, CapSet.empty
  |Var x -> begin
    let _, c, _, loc = State.find_var st x in
    if not (State.has_cap st c) then failwith "no such capability"
    else 
    let v = State.get_mem st loc in
    let k' = CapSet.singleton c in
    v, c, c, k', CapSet.empty
  end
  |Binary(op, e1, e2) -> failwith "unimplemented"
  |Fun(params, return, e) -> failwith "unimplemented"
  |Apply(fname, args) -> failwith "unimplemented"
  |Object o -> failwith "unimplemented"
  |Get(e,fname) -> failwith "unimplemented"
  |Seq(e1, e2) -> failwith "unimplemented"
  |If(c, e1, e2) -> failwith "unimplemented"
  |While(c, e) -> failwith "unimplemented"
  |Let(v, e1, e2) -> failwith "unimplemented"
  |Destroy e -> failwith "unimplemented"
  |Sleep e -> failwith "unimplemented"
  |Branch(vlist, e) -> failwith "unimplemented"
  |Focus(e1, e2) -> failwith "unimplemented"
  |Assign(e1, e2) -> failwith "unimplemented"
  |Neg e -> begin
    let v, r, w, k', p = eval st e in
    (* TODO: making sure we are just throwing away K' *)
    match v with
    | V_int i -> V_int(-1 * i), r, w, CapSet.empty, p
    | _ -> failwith "expected int for integer negation"
  end
  |Not e -> begin
    let v, r, w, k', p = eval st e in
    match v with
    | V_bool i -> V_bool(not i), r, w, CapSet.empty, p
    | _ -> failwith "expected bool for boolean negation"
  end

(* and eval_binop st bop e1 e2 = 
match bop with
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
| BinopEq *)

