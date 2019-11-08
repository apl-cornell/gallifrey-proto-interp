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
  |V_obj of var * ((var * fieldinfo) list)
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
and classes = (var, gtype) Hashtbl.t

let get_type = function
  | V_int _ -> T_int
  | V_bool _ -> T_bool
  | V_unit -> T_unit
  | V_obj(cls, fields) -> begin
      (* let t_fields = List.map fields 
          (fun f -> 
             let fname = fst f in
             let t, u, mut, _ = snd f in
             (fname, t, u, mut)
          ) in T_obj(t_fields) *)
      T_cls(cls)
    end
  | V_ptr(l, l', m, t) -> t
  | V_fun(cls, caps, params, return, _, _) -> begin
      let param_types = List.map params 
          (fun (_, t, _) -> t)
      in T_fun(param_types, return)
    end

module State = struct
  type t = {
    k: CapSet.t;
    store: t_store list;
    focus: (cap * gtype * loc) option; 
    classes: classes list; (* unsure about this one *)
    mem: memory;
    counter: int ref;
  }

  let init = fun () -> 
    {
      k = CapSet.singleton c_any;
      store = [Hashtbl.create (module String)];
      focus = None;
      classes = [Hashtbl.create (module String)];
      mem = Hashtbl.create (module Int);
      counter = ref 0;
    }

  (* unique number generator *)
  let unique s = 
    s.counter := !(s.counter) + 1; !(s.counter)

  let enter_scope s = 
    let ht_s = Hashtbl.create (module String) in
    let ht_c = Hashtbl.create (module String) in
    {s with store = ht_s::s.store;
            classes = ht_c::s.classes}

  (* try to find variable in store *)

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

  let find_var s x = 
    match find_var_opt s x with
    | Some v -> v
    | None -> raise (GError ("var " ^ x ^ " not found"))

  let find_cls_opt s x = 
    let rec find_helper classes x =
      match classes with
      |[] -> None
      |h::t -> begin
          match Hashtbl.find h x with
          |Some(T_obj(f)) as o -> o
          |None -> find_helper t x
          |_ -> raise (GError ("expected object type"))
        end
    in find_helper s.classes x

  let find_cls s x = 
    match find_cls_opt s x with
    | Some v -> v
    | None -> raise (GError ("class " ^ x ^ " not found"))

  let cls_exists s x = 
    match find_cls_opt s x with
    | Some v -> true
    | None -> false

  (* get value at memory location *)
  let get_mem s loc = Hashtbl.find_exn s.mem loc

  let double_deref st loc = 
    let p = get_mem st loc in
    match p with
    |V_ptr(_,loc',m,_) -> get_mem st loc'
    |_ -> raise (GError "expected pointer")

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
    |V_obj(cls, fields) -> 
      List.fold_left 
        ~f:(fun a (v,(_,_,m,loc)) -> 
            if m = MUT then true
            else 
              let v' = double_deref st loc in
              a || is_mutable st v'
          ) 
        ~init:false 
        fields
    | V_ptr(l, l', m, t) -> begin
        (* TODO do we want to check for pointer mut? i feel like no *)
        let v' = get_mem st l' in
        is_mutable st v'
      end
    | V_fun(_) -> true
    |_ -> false

  let rec destroy_store store cap = 
    match store with
    |[] -> ()
    |h::t -> Hashtbl.filter_inplace ~f:(fun (t,c,l) -> c <> cap) h; destroy_store t cap

  let destroy st cap = 
    destroy_store st.store cap

  let rec eq_types st t1 t2 = 
    match t1, t2 with
    |T_unit, T_unit -> true
    |T_int, T_int -> true
    |T_bool, T_bool -> true
    |T_fun(i,o), T_fun(i2, o2) when i = i2 && o = o2 -> true
    |T_obj f1, T_obj f2 when f1 = f2 -> true
    |T_obj _, T_cls(cname) -> eq_types st t1 (find_cls st cname)
    |T_cls(cname), T_obj _ -> eq_types st t2 (find_cls st cname)
    (* do we check the fields of the classes? *)
    |T_cls(cname1), T_cls(cname2) -> cname1 = cname2
    |_ -> false

  let add_var st name c value = 
    let t = get_type value in
    let loc1 = unique st in
    let loc2 = unique st in
    Hashtbl.add_exn (List.hd_exn st.store) name (t, c, loc1);
    Hashtbl.add_exn st.mem loc1 (V_ptr(loc1, loc2, MUT, t));
    Hashtbl.add_exn st.mem loc2 value
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

let stringify_hashtbl_stack s = 
  let rec helper s acc = 
    match s with 
    |h::t -> helper t (acc @ Hashtbl.keys h)
    |[] -> acc
  in
  "[" ^ (String.concat ~sep:"," (helper s [])) ^ "]"

let stringify_capset s = 
  "[" ^ (String.concat ~sep:"," (CapSet.to_list s)) ^ "]"

