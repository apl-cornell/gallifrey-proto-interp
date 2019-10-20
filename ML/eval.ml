include Utils
open Core
open Unix

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string
exception ResourceDestroyedError of string

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
        let k' = CapSet.singleton c in
        let p = CapSet.remove st.k c in
        (* P is K\c *)
        State.get_mem st loc, (c, c), k', p
    end
  |Binary(op, e1, e2) -> eval_binop st op e1 e2
  |Fun(cls, caps, params, return, e) -> begin
      let store = match st.store with
        |[] -> failwith "empty store"
        |h::t -> (Hashtbl.copy h):: t
      in
      (* no k', k is moved to p *)
      V_fun(cls, caps, params, return, e, store), (c_any, c_none), CapSet.empty, st.k
    end
  |Apply(fname, args) -> begin
      let t, c, loc = State.find_var st fname in
      let v = State.get_mem st loc |> State.deref st in
      match v with
      |V_fun(cls, caps, params, ret, expr, store) -> begin
          List.iter caps (fun x -> if CapSet.mem st.k x then () else failwith "capability not found");
          (* TODO eval all the arguments *)
          let st' = State.enter_scope st in
          (* assign them to store disregarding shadowing rules *)
          let v, (r, w), k', p = eval st' expr in
          assert (get_type v = ret); 
          v, (r, w), k', p
        end
      |_ -> failwith "expected a function"
    end
  |Object o -> begin
      let eval_field (state, locs, k's, p_) (var, e, u, m) =
        let v, (r, w), k', p = eval state e |> framep state.k |> autoclone state in
        let t = get_type v in
        let loc = State.unique state in
        let loc' = State.unique state in
        let state' = {state with k = p} in
        Hashtbl.add_exn state.mem loc' v;
        Hashtbl.add_exn state.mem loc (V_ptr(loc, loc', m, t));
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
      let v, (r, w), k', p = eval st e |> framep st.k |> autoclone st in
      match State.deref st v with
      |V_obj fields -> begin
          let (t,u,mut,loc) = List.Assoc.find_exn fields ~equal:(=) fname in
          (* for now, read cap for the obj is always consumed *)
          (* TODO check this behavior *)
          let p = CapSet.diff p (CapSet.singleton r) in
          (* if immutable, no write is allowed *)
          let w = match mut with 
            |MUT -> w 
            |IMMUT -> c_none
          in
          (* I don't think I can autoclone here *)
          State.get_mem st loc, (r,w), k', p
        end
      |_ -> failwith "expected object"
    end
  |Seq(e1, e2) -> begin
      let v1, (r1, w1), k', pl = eval st e1 in
      let st' = {st with k = pl} in
      eval st' e2
    end
  |If(c, e1, e2) -> begin
      let v, (r, w), k', p = eval st c |> framep st.k |> autoclone st in
      let st' = State.enter_scope {st with k = p} in
      match State.deref st v with
      |V_bool b -> begin
          let e = if b then e1 else e2 in
          eval st' e
        end
      |_ -> failwith "condition needs to be boolean"
    end
  |While(c, e) -> begin
      let v, (r, w), k', p = eval st c |> framep st.k |> autoclone st in
      let st' = State.enter_scope {st with k = p} in
      match State.deref st v with
      |V_bool b -> begin
          if b then
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
          let loc1 = State.unique st in
          let loc2 = State.unique st in
          let store = List.hd_exn st.store in
          Hashtbl.add_exn store x (t,c,loc1);
          Hashtbl.add_exn st.mem loc1 (V_ptr(loc1, loc2, MUT, get_type v));
          Hashtbl.add_exn st.mem loc2 v;
          (* TODO semantics differ from doc - transferring k' or k? *)
          let st' = {st with k = CapSet.add p c} in
          eval st' e2
        end
    end
  |Destroy e -> failwith "unimplemented"
  |Sleep e -> begin
    match e with
    |Int i -> (Unix.sleep i); V_unit, (c_none, c_none), CapSet.empty, CapSet.empty
    |_ -> failwith "expected int literal"
  end
  |Branch(vlist, e) -> failwith "unimplemented"
  |Focus(e1, e2) -> failwith "unimplemented"
  |Assign(e1, e2) -> begin
      (* TODO k' and p, autoclone for LHS? *)
      let v1, (r1, w1), k', pl = eval st e1 |> framep st.k in
      let st' = {st with k = pl} in
      let v2, (r2, w2), k'', pr = eval st' e2 |> framep st.k |> autoclone st in
      ignore (check_write w1 r1 st'.k);
      match (v1, v2) with
      (* aliasing *)
      |V_ptr(l1,l1',m1,t1), V_ptr(l2,l2',m2,t2) -> begin
          if t1 <> t2 then failwith "types do not match"
          else if m1 <> MUT then failwith "LHS is not mutable"
          else Hashtbl.set st.mem l1 (V_ptr(l1, l2', m1, t1));
          V_unit, (c_none, c_none), CapSet.empty, CapSet.union k'' pr
        end
      (* assigning a value *)
      |V_ptr(l,l',m,t), v -> begin
          if t <> get_type v then failwith "types do not match"
          else Hashtbl.set st.mem l' v;
          V_unit, (c_none, c_none), CapSet.empty, CapSet.union k'' pr
        end
      |_ -> failwith "illegal assignment" 
    end
  |Neg e -> begin
      let v, (r, w), k', p = eval st e |> framep st.k |> autoclone st in
      match State.deref st v with
      | V_int i -> V_int(-1 * i), (r, w), CapSet.empty, p
      | _ -> failwith "expected int for integer negation"
    end
  |Not e -> begin
      let v, (r, w), k', p = eval st e |> framep st.k |> autoclone st in
      match State.deref st v with
      | V_bool i -> V_bool(not i), (r, w), CapSet.empty, p
      | _ -> failwith "expected bool for boolean negation"
    end
  |Class(c,t) -> failwith "unimplemented"

and eval_binop st bop e1 e2 = 
  (* throw away K' and K'', no caps check atm *)
  let v1, (r1, w1), k', pl = eval st e1 |> framep st.k |> autoclone st in
  let st' = {st with k = pl} in
  let v2, (r2, w2), k'', pr = eval st' e2 |> framep st.k |> autoclone st in
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
  v', (cr, cw), CapSet.empty, pr
