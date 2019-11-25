open Core
include Ast

(* Interpreter exceptions. *)
(* exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string *)
exception GError of string

(* module StringSet = Set.Make(String) *)
let c_any = "ANY"
let c_none = "NONE"

(* module CapSet = struct 
   type t = StringSet.t

   let empty = StringSet.empty

   let add set elt = 
    if elt <> c_any && elt <> c_none then
      StringSet.add set elt
    else set

   let remove = StringSet.remove

   let inter = StringSet.inter

   let union = StringSet.union

   let diff = StringSet.diff

   let to_list = StringSet.to_list

   let of_list = StringSet.of_list

   let singleton elt = 
    if elt <> c_any && elt <> c_none then StringSet.singleton elt
    else StringSet.empty

   let mem = StringSet.mem

   let length = StringSet.length
   end *)

module CapSet = struct 
  type t = cap list

  let empty = []

  let mem set elt = List.mem set elt (=)

  let add set elt = 
    if elt <> c_any && elt <> c_none && (mem set elt |> not) then
      elt::set
    else set

  let remove set elt = List.filter set (fun e -> e <> elt)

  let inter s1 s2 = List.filter s1 (fun e -> mem s2 e)

  let union s1 s2 = List.dedup_and_sort ~compare:(Pervasives.compare) (s1 @ s2)

  let diff s1 s2 = List.filter s1 (fun e -> mem s2 e |> not)

  let to_list x = x

  let of_list x = x

  let singleton elt = 
    if elt <> c_any && elt <> c_none then [elt]
    else []

  let length = List.length
end

type value = 
  |V_int of int
  |V_bool of bool
  |V_unit
  |V_obj of var * ((var * fieldinfo) list)
  (* string option is class cap, boolean is uniqueness *)
  |V_fun of param list * gtype * expr * (t_store list)
  (* own location + value's location *)
  |V_ptr of loc * loc * mut * gtype
  |V_cap of cap
and t_store = (var, storeinfo) Hashtbl.t
and loc = int
(* type, capability, location *)
and storeinfo = (gtype * cap * loc)
(* type, unique, mutable, location *)
and fieldinfo = (gtype * unique * mut * loc)
and memory = (loc, value) Hashtbl.t
and classes = (var, (t_obj * var option)) Hashtbl.t

let hashtbl_stack_tolist s = 
  let rec helper s acc = 
    match s with 
    |h::t -> helper t (acc @ Hashtbl.to_alist h)
    |[] -> acc
  in
  helper s []

let hashtbl_stack_keys s = 
  let rec helper s acc = 
    match s with 
    |h::t -> helper t (acc @ Hashtbl.keys h)
    |[] -> acc
  in
  helper s []

let hashtbl_stack_vals func s = 
  let rec helper s acc = 
    match s with 
    |h::t -> helper t (acc @ (List.map (Hashtbl.data h) func))
    |[] -> acc
  in
  helper s []

let g_assert b msg = 
  if not b then raise (GError msg) else ()

let rec substitute_cap metacap cap lambdalist = 
  match lambdalist with
  |[] -> []
  |hd::tl -> begin
      match hd with
      |Lambda(var, meta, t) when meta = metacap -> Lambda(var, cap, t)::(substitute_cap metacap cap tl)
      |Lambda(var, meta, t) -> hd::(substitute_cap metacap cap tl)
      |SigmaLambda(var, meta, t) when meta = metacap -> lambdalist
      |SigmaLambda(var, meta, t) -> hd::(substitute_cap metacap cap tl)
      |KappaLambda meta when meta = metacap -> lambdalist
      |KappaLambda meta -> hd::(substitute_cap metacap cap tl)
    end

let normalize biglambdalist = 
  let rec helper lst metacaps cap_num =
    match lst with
    |[] -> []
    |hd::tl -> begin
        match hd with
        |Lambda(var, meta, t) as l -> 
          if List.mem metacaps meta (=) then l::(helper tl metacaps cap_num) 
          else raise (GError ("unknown meta-cap "^meta))
        |SigmaLambda(var, meta, t) as l -> 
          if List.mem metacaps meta (=) then 
            l::(helper tl metacaps cap_num) 
          else
            let newmeta = "_mc"^(string_of_int cap_num) in
            let subs = substitute_cap meta newmeta tl in
            (SigmaLambda(var, newmeta, t))::(helper subs (newmeta::metacaps) (cap_num + 1)) 
        |KappaLambda meta as l -> 
          if List.mem metacaps meta (=) then 
            l::(helper tl metacaps cap_num) 
          else
            let newmeta = "_mc"^(string_of_int cap_num) in
            let subs = substitute_cap meta newmeta tl in
            (KappaLambda(newmeta))::(helper subs (newmeta::metacaps) (cap_num + 1)) 
      end
  in
  helper biglambdalist [] 1

let stringify_hashtbl_stack s = 
  let keys = hashtbl_stack_keys s in
  "[" ^ (String.concat ~sep:"," keys) ^ "]"

let stringify_capset s = 
  "[" ^ (String.concat ~sep:"," (CapSet.to_list s)) ^ "]"

let stringify_list l = 
  "[" ^ String.concat ~sep:"," l ^ "]"

let get_type = function
  | V_int _ -> T_int
  | V_bool _ -> T_bool
  | V_unit -> T_unit
  | V_obj(cls, fields) -> T_cls(cls)
  | V_ptr(l, l', m, t) -> t
  | V_fun(params, rtype, _, _) -> T_fun(params, rtype)
  | V_cap _ -> T_cap

module State = struct
  type t = {
    k: CapSet.t;
    store: t_store list;
    focus: (cap * gtype * loc) list; 
    classes: classes list; (* unsure about this one *)
    mem: memory;
    counter: int ref;
  }

  let init = fun () -> 
    {
      k = CapSet.empty;
      store = [Hashtbl.create (module String)];
      focus = [];
      classes = [Hashtbl.create (module String)];
      mem = Hashtbl.create (module Int);
      counter = ref 0;
    }

  (* simple runtime validations to aid debugging *)
  let validate_result st (value, (r,w), k', p) =
    g_assert (not (CapSet.mem k' c_any)) "c_any in k'";
    g_assert (not (CapSet.mem k' c_none)) "c_none in k'";
    g_assert (not (CapSet.mem p c_any)) "c_any in p";
    g_assert (not (CapSet.mem p c_none)) "c_none in p";
    g_assert ((CapSet.inter k' p |> CapSet.length) = 0) "k' and p intersect";
    let store_types = hashtbl_stack_vals (fun (t,_,_) -> t) st.store in
    List.iter store_types (fun t -> g_assert (t <> T_cap) "not allowed to store result of capof");
    (value, (r,w), k', p)

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
          |(Some ((o, super))) as obj -> obj
          |None -> find_helper t x
        end
    in find_helper s.classes x

  let find_cls s x = 
    match find_cls_opt s x with
    | Some (o, super) -> (o, super)
    | None -> raise (GError ("class " ^ x ^ " not found"))

  let cls_exists s x = 
    match find_cls_opt s x with
    | Some (o, super) -> true
    | None -> false

  (* get value at memory location *)
  let get_mem s loc = Hashtbl.find_exn s.mem loc

  let double_deref st loc = 
    let p = get_mem st loc in
    match p with
    |V_ptr(_,loc',m,_) -> get_mem st loc'
    |_ -> raise (GError "expected pointer")

  (* check it capability is in K *)
  let has_cap s c = c = c_any || CapSet.mem s.k c

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
          |Some((t,c,loc)) -> begin 
              let newval = get_mem s loc in
              if get_type newval <> t then raise (GError "old and new types don't match")
              else Hashtbl.set h k (t,c,newloc)
            end
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
    (* functions are considered immutable *)
    | V_fun(_) -> false
    |_ -> false

  let rec destroy_store store cap = 
    match store with
    |[] -> ()
    |h::t -> Hashtbl.filter_inplace ~f:(fun (t,c,l) -> c <> cap) h; destroy_store t cap

  let destroy st cap = 
    destroy_store st.store cap

  let getall_supercls st c = 
    let rec path_to_parent c = match find_cls st c with
      |(_, Some s) -> c::(path_to_parent s)
      |_ -> [c]
    in
    match path_to_parent c with
    |h::t -> t
    |_ -> []

  let rec eq_types st t1 t2 = 
    match t1, t2 with
    |T_cap, T_cap -> true
    |T_unit, T_unit -> true
    |T_int, T_int -> true
    |T_bool, T_bool -> true
    |T_fun(i,o), T_fun(i2, o2) -> begin
        let eq_inputs = List.fold2_exn 
            i i2 ~init:true 
            ~f:(fun acc param1 param2 -> match param1, param2 with
                |KappaLambda c1, KappaLambda c2 -> if c1 <> c2 then false else true && acc
                |SigmaLambda(_,c1,t1), SigmaLambda(_,c2,t2) -> if (not (eq_types st t2 t1)) || c1 <> c2 then false else true && acc
                |Lambda(_,c1,t1), Lambda(_,c2,t2) -> if (not (eq_types st t2 t1)) || c1 <> c2 then false else true && acc
                |_ -> false
              ) in
        eq_inputs && (eq_types st o o2)
      end
    |T_cls(cname1), T_cls(cname2) -> cname1 = cname2
    |_ -> false

  (* is L a subtype of R *)
  let rec is_subtype st l r = 
    match l, r with
    |_, T_unit -> true
    |_, _ when eq_types st l r -> true
    |T_fun(i,o), T_fun(i2, o2) -> begin
        (* contravariant in input, covariant in output *)
        let eq_inputs = List.fold2_exn 
            i i2 ~init:true 
            ~f:(fun acc param1 param2 -> match param1, param2 with
                |KappaLambda c1, KappaLambda c2 -> if c1 <> c2 then false else true && acc
                |SigmaLambda(_,c1,t1), SigmaLambda(_,c2,t2) -> if (not (is_subtype st t2 t1)) || c1 <> c2 then false else true && acc
                |Lambda(_,c1,t1), Lambda(_,c2,t2) -> if (not (is_subtype st t2 t1)) || c1 <> c2 then false else true && acc
                |_ -> false
              ) in
        eq_inputs && (is_subtype st o o2)
      end
    |T_cls(cname1), T_cls(cname2) -> 
      (* purely for formatting/readability *)
      if cname1 = cname2 then true 
      else List.mem (getall_supercls st cname1) cname2 (=)
    |_ -> false

  let add_var st name c value = 
    let t = get_type value in
    let loc1 = unique st in
    let loc2 = unique st in
    Hashtbl.add_exn (List.hd_exn st.store) name (t, c, loc1);
    Hashtbl.add_exn st.mem loc1 (V_ptr(loc1, loc2, MUT, t));
    Hashtbl.add_exn st.mem loc2 value
end

let reconcile_read (c1:cap) (c2:cap) (k1:CapSet.t) (k2:CapSet.t) = 
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

let reconcile_write (c1:cap) (c2:cap) = 
  match c1, c2 with
  |(a,b) when a = b -> a
  (* drop write *)
  |("NONE", b) -> c_none
  |(a,"NONE") -> c_none
  |_ -> c_none

let reconcile_caps (r1, w1) (r2, w2) (k1:CapSet.t) (k2:CapSet.t) = 
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

let unit_coerce (st:State.t) (v, (r,w), k', p) = 
  if State.is_subtype st (get_type v) T_unit then V_unit, (r, w), k', p
  else v, (r, w), k', p

