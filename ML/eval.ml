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
  |V_obj of (var * fieldinfo) list
  (* boolean is uniqueness *)
  |V_fun of (var * gtype * bool) list * gtype * expr * t_store
and t_store = (var, storeinfo) Hashtbl.t
and loc = int
(* type, capability, mutable, location *)
and storeinfo = (gtype * cap * bool * loc)
(* type, unique, mutable, location *)
and fieldinfo = (gtype * bool * bool * loc)
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
  | V_fun(params, return, _, _) -> begin
    let param_types = List.map params 
    (fun (_, t, _) -> t)
    in T_fun(param_types, return)
  end

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

  (* unique number generator *)
  let unique s = 
    s.counter := !(s.counter) + 1; !(s.counter)

  let enter_scope s = 
    {s with store = (Hashtbl.create (module String))::s.store}

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

  let find_var_opt s x = 
    let rec find_helper st x =
      match st with
      |[] -> None
      |h::t -> begin
          match Hashtbl.find h x with
          |Some v -> Some v
          |None -> find_helper t x
        end
    in find_helper s.store x
  
  let var_exists s x = 
    match find_var_opt s x with
    | Some v -> true
    | None -> false

  (* get value at memory location *)
  let get_mem s loc = Hashtbl.find_exn s.mem loc

  (* check it capability is in K *)
  let has_cap s c = CapSet.mem s.k c

end

let init_state = State.init

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

(* value, read, write, K', P *)
type result = value * (cap * cap) * CapSet.t * CapSet.t

let rec eval (st:State.t) (exp:expr): result = 
  (* do we transfer K to P in all of these *)
  match exp with
  |Int i -> V_int(i), (c_any, c_none), CapSet.empty, st.k
  |Bool b -> V_bool(b), (c_any, c_none), CapSet.empty, st.k
  |Unit -> V_unit, (c_any, c_none), CapSet.empty, st.k
  |Var x -> begin
      let _, c, _, loc = State.find_var st x in
      if not (State.has_cap st c) then failwith "no capability"
      else 
        let v = State.get_mem st loc in
        let k' = CapSet.singleton c in
        (* P is K\c *)
        v, (c, c), k', CapSet.remove st.k c
    end
  |Binary(op, e1, e2) -> eval_binop st op e1 e2
  |Fun(params, return, e) -> failwith "unimplemented"
  |Apply(fname, args) -> failwith "unimplemented"
  |Object o -> failwith "unimplemented"
  |Get(e,fname) -> failwith "unimplemented"
  |Seq(e1, e2) -> begin
    let v1, (r1, w1), k', pl = eval st e1 in
    let st' = {st with k = pl} in
    eval st' e2
  end
  |If(c, e1, e2) -> begin
    (* TODO merge K' and P *)
    let v, (r, w), k', p = eval st c in
    match v with
    |V_bool b -> begin
      let e = if b then e1 else e2 in
      let st' = State.enter_scope {st with k = p} in
      eval st' e
    end
    |_ -> failwith "condition needs to be boolean"
  end
  |While(c, e) -> begin
    (* TODO merge K' and P *)
    let v, (r, w), k', p = eval st c in
    match v with
    |V_bool b -> begin
      let st' = State.enter_scope {st with k = p} in
      eval st' e
    end
    |_ -> failwith "condition needs to be boolean"
  end
  |Let(x, e1, e2) -> begin
    let v, (r, w), k', p = eval st e1 in
    let c = "c."^(string_of_int (State.unique st)) in
    let t = get_type v in
    match State.find_var_opt st x with
    | Some _ -> failwith "no shadowing allowed"
    | None -> begin
      let loc = State.unique st in
      let store = List.hd_exn st.store in
      Hashtbl.add_exn store x (t,c,true,loc);
      Hashtbl.add_exn st.mem loc v;
      (* again, transferring k' or k? *)
      let st' = {st with k = CapSet.add p c} in
      eval st' e2
    end
  end
  |Destroy e -> failwith "unimplemented"
  |Sleep e -> failwith "unimplemented"
  |Branch(vlist, e) -> failwith "unimplemented"
  |Focus(e1, e2) -> failwith "unimplemented"
  |Assign(e1, e2) -> failwith "unimplemented"
  |Neg e -> begin
      let v, (r, w), k', p = eval st e in
      (* TODO: right now we are just throwing away K' *)
      match v with
      | V_int i -> V_int(-1 * i), (r, w), CapSet.empty, p
      | _ -> failwith "expected int for integer negation"
    end
  |Not e -> begin
      let v, (r, w), k', p = eval st e in
      match v with
      | V_bool i -> V_bool(not i), (r, w), CapSet.empty, p
      | _ -> failwith "expected bool for boolean negation"
    end

and eval_binop st bop e1 e2 = 
  (* throw away K' and K'', no caps check atm *)
  let v1, (r1, w1), k', pl = eval st e1 in
  let st' = {st with k = pl} in
  let v2, (r2, w2), k'', pr = eval st' e2 in
  let v' = match (bop, v1, v2) with
    | (BinopAnd, V_bool b1, V_bool b2) -> V_bool(b1 && b2)
    | (BinopOr, V_bool b1, V_bool b2) -> V_bool(b1 || b2)
    | (BinopPlus, V_int i1, V_int i2) -> V_int(i1 + i2)
    | (BinopMinus, V_int i1, V_int i2) -> V_int(i1 - i2)
    | (BinopTimes, V_int i1, V_int i2) -> V_int(i1 * i2)
    | (BinopDiv, V_int i1, V_int i2) -> V_int(i1 / i2)
    | (BinopMod, V_int i1, V_int i2) -> V_int(i1 mod i2)
    | (BinopLt, V_int i1, V_int i2) -> V_bool(i1 < i2)
    | (BinopLeq, V_int i1, V_int i2) -> V_bool(i1 <= i2)
    | (BinopGt, V_int i1, V_int i2) -> V_bool(i1 > i2)
    | (BinopGeq, V_int i1, V_int i2) -> V_bool(i1 >= i2)
    | (BinopNeq, _, _) -> V_bool(v1 <> v2)
    | (BinopEq, _, _) -> V_bool(v1 = v2)
    |_ -> failwith "invalid binop"
  in
  let cr, cw = reconcile_caps (r1, w1) (r2, w2) k' k'' in
  (* TODO k'' or empty? *)
  v', (cr, cw), CapSet.empty, pr