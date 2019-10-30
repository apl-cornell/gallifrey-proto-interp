open Core
include Ast

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string
exception GError of string

module CapSet = Set.Make(String)
let c_any = "ANY"
let c_none = "NONE"

type value = 
  |V_int of int
  |V_bool of bool
  |V_unit
  |V_obj of (var * fieldinfo) list
  (* string option is class cap, boolean is uniqueness *)
  |V_fun of string option * string list * (var * gtype * unique) list * gtype * expr * t_store list
  (* own location + value's location *)
  |V_ptr of loc * loc * mut * gtype
and t_store = (var, storeinfo) Hashtbl.t
and loc = int
(* type, capability, location *)
and storeinfo = (gtype * cap * loc)
(* type, unique, mutable, location *)
and fieldinfo = (gtype * unique * mut * loc)
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

  (* unique number generator *)
  let unique s = 
    s.counter := !(s.counter) + 1; !(s.counter)

  let enter_scope s = 
    {s with store = (Hashtbl.create (module String))::s.store}

  (* try to find variable in store *)
  let find_var s x = 
    let rec find_helper st x =
      match st with
      |[] -> raise (GError ("var " ^ x ^ " not found"))
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

  let deref s v = 
    match v with
    |V_ptr(loc, loc', m, t) -> get_mem s loc'
    |_ -> v

  (* equivalent to hashtbl.replace but for our special stack *)
  let update_store s k newloc =
    let rec update_helper st k v =
      match st with
      |[] -> ()
      |h::t -> begin
          match Hashtbl.find h k with
          |Some((t,c,loc)) -> Hashtbl.set h k (t,c,newloc)
          |None -> update_helper t k newloc
        end
    in update_helper s.store k newloc

  (* this might have infinite loop *)
  let rec is_mutable st v = 
    match v with
    |V_obj fields -> true
      (* List.fold_left 
        ~f:(fun a (v,(_,_,m,_)) -> a && m = IMMUT) 
        ~init:true 
        fields *)
    (* problem TODO: vars are MUT pointers *)
    | V_ptr(l, l', m, t) -> deref st v |> is_mutable st 
    |_ -> false

  let rec destroy_store store cap = 
    match store with
    |[] -> ()
    |h::t -> Hashtbl.filter_inplace ~f:(fun (t,c,l) -> c <> cap) h; destroy_store t cap

  let destroy st cap = 
    destroy_store st.store cap
end

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
  | V_ptr(l, l', m, t) -> t
  | V_fun(cls, caps, params, return, _, _) -> begin
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
let check_write w r k = 
  match (w,r) with
  |("NONE", _) -> raise (GError "cannot write")
  |(_, "NONE") -> raise (GError "cannot read")
  |(a, b) when a = b -> a
  |("ANY", b) -> b
  |(a, "ANY") -> a
  |(a,b) -> if CapSet.mem k b then a 
            else raise (GError "incompatible caps")

let autoclone (st:State.t) (v, (r,w), k', p) = 
  let k', p = if State.is_mutable st v then k', p 
          else CapSet.empty, CapSet.union k' p in
  v,(r,w),k',p