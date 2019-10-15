include Utils
open Core

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string

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

  let rec deref s v = 
    match v with
    |V_ptr(loc, t) -> get_mem s loc |> deref s
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

end

let init_state = State.init

(* value, read, write, K', P *)
type result = value * (cap * cap) * CapSet.t * CapSet.t

let rec eval (st:State.t) (exp:expr): result = 
  (* do we transfer K to P in all of these *)
  match exp with
  |Int i -> V_int(i), (c_any, c_none), CapSet.empty, st.k
  |Bool b -> V_bool(b), (c_any, c_none), CapSet.empty, st.k
  |Unit -> V_unit, (c_any, c_none), CapSet.empty, st.k
  |Var x -> begin
      let t, c, loc = State.find_var st x in
      if not (State.has_cap st c) then failwith "no capability"
      else 
        (* one step of dereferencing here *)
        let loc' = match State.get_mem st loc with
          |V_ptr(l,t) -> l
          |_ -> loc in
        let k' = CapSet.singleton c in
        let p = CapSet.remove st.k c in
        (* P is K\c *)
        V_ptr(loc',t), (c, c), k', p
    end
  |Binary(op, e1, e2) -> eval_binop st op e1 e2
  |Fun(caps, params, return, e) -> begin
      let store = match st.store with
        |[] -> failwith "empty store"
        |h::t -> (Hashtbl.copy h):: t
      in
      (* remove from K first *)
      let k = CapSet.diff st.k (CapSet.of_list caps) in
      (* TODO read cap for closure? also, no k' because we kill every cap the function wants  *)
      V_fun(caps, params, return, e, store), (c_any, c_none), CapSet.empty, k
    end
  |Apply(fname, args) -> begin
      let t, c, loc = State.find_var st fname in
      let v = State.deref st (V_ptr(loc, t)) in
      match v with
      |V_fun(caps, params, ret, expr, store) -> begin
          (* TODO *)
          failwith "unimplemented"
        end
      |_ -> failwith "expected a function"
    end
  |Object o -> begin
      let eval_field (state, locs, k's, p_) (var, e, u, m) =
        let v, (r, w), k', p = eval state e in
        let t = get_type v in
        let loc = State.unique state in
        let k = framep state.k k' p in
        let state' = {state with k = k} in
        Hashtbl.add_exn state.mem loc v;
        state', (var, (t, u, m, loc))::locs, k'::k's, p
      in
      let st, locs_rev, k's, p = List.fold_left ~f:eval_field ~init:(st,[],[],CapSet.empty) o in
      let field_info = List.rev locs_rev in
      let c = "c."^(string_of_int (State.unique st)) in
      let k' = List.fold_left 
        ~f:(fun a x -> CapSet.union a x) 
        ~init:(CapSet.empty) 
        ((CapSet.singleton c)::k's) 
      in
      (* k' is union of all k' + c, p is p of last field expr *)
      V_obj(field_info), (c, c), k', p
    end
  |Get(e,fname) -> begin
      let v, (r, w), k', p = eval st e in
      match State.deref st v with
      |V_obj fields -> begin
          let (t,u,mut,loc) = List.Assoc.find_exn fields ~equal:(=) fname in
          (* for now, read cap for the obj is always consumed *)
          let k = CapSet.diff st.k (CapSet.singleton r) in
          let p' = framep st.k (CapSet.singleton r) p in
          (* if immutable, no write is allowed *)
          let w, p'' = match mut with 
            |MUT -> w, p'
            (* if immutable then we can apply autoclone *)
            |IMMUT -> c_none, CapSet.union k p'
          in
          (* I don't think I can autoclone here *)
          V_ptr(loc, t), (r,w), k', p''
        end
      |_ -> failwith "expected object"
    end
  |Seq(e1, e2) -> begin
      let v1, (r1, w1), k', pl = eval st e1 in
      let st' = {st with k = pl} in
      eval st' e2
    end
  |If(c, e1, e2) -> begin
      let v, (r, w), k', p = eval st c in
      match State.deref st v with
      |V_bool b -> begin
          let e = if b then e1 else e2 in
          let st' = State.enter_scope {st with k = framep st.k k' p |> CapSet.union k'} in
          eval st' e
        end
      |_ -> failwith "condition needs to be boolean"
    end
  |While(c, e) -> begin
      let v, (r, w), k', p = eval st c in
      match State.deref st v with
      |V_bool b -> begin
          let p = framep st.k k' p |> CapSet.union k' in
          if b then
            let st' = State.enter_scope {st with k = p} in
            eval st' (Seq(e, While(c, e)))
          else
            V_unit, (c_none, c_none), CapSet.empty, p
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
          Hashtbl.add_exn store x (t,c,loc);
          Hashtbl.add_exn st.mem loc v;
          (* semantics differ from doc - transferring k' or k? *)
          let st' = {st with k = CapSet.add p c} in
          eval st' e2
        end
    end
  |Destroy e -> failwith "unimplemented"
  |Sleep e -> failwith "unimplemented"
  |Branch(vlist, e) -> failwith "unimplemented"
  |Focus(e1, e2) -> failwith "unimplemented"
  |Assign(e1, e2) -> begin
      (* k' and p *)
      let v1, (r1, w1), k', pl = eval st e1 in
      let st' = {st with k = framep st.k k' pl} in
      let v2, (r2, w2), k'', pr = eval st' e2 in
      ignore (write w1 r1 st'.k);
      match (v1, v2) with
      |V_ptr(l1,t1), V_ptr(l2,t2) -> begin
          if t1 <> t2 then failwith "types do not match"
          else Hashtbl.set st.mem ~key:l1 ~data:(V_ptr(l2, t2));
          V_unit, (c_none, c_none), CapSet.empty, CapSet.union k'' pr
        end
      |V_ptr(l,t), v -> begin
          if t <> get_type v then failwith "types do not match"
          else Hashtbl.set st.mem l v;
          V_unit, (c_none, c_none), CapSet.empty, CapSet.union k'' pr
        end
      |_ -> failwith "illegal assignment" 
    end
  |Neg e -> begin
      let v, (r, w), k', p = eval st e in
      match State.deref st v with
      | V_int i -> V_int(-1 * i), (r, w), CapSet.empty, framep st.k k' p |> CapSet.union k'
      | _ -> failwith "expected int for integer negation"
    end
  |Not e -> begin
      let v, (r, w), k', p = eval st e in
      match State.deref st v with
      | V_bool i -> V_bool(not i), (r, w), CapSet.empty, framep st.k k' p |> CapSet.union k'
      | _ -> failwith "expected bool for boolean negation"
    end
  |Class(c,t) -> failwith "unimplemented"

and eval_binop st bop e1 e2 = 
  (* throw away K' and K'', no caps check atm *)
  let v1, (r1, w1), k', pl = eval st e1 in
  let st' = {st with k = pl} in
  let v2, (r2, w2), k'', pr = eval st' e2 in
  let v' = match (bop, State.deref st v1, State.deref st v2) with
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
  v', (cr, cw), CapSet.empty, framep st'.k k'' pr
