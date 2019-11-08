include Utils
open Core
open Unix
open Pprint
open Thread

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
      if not (State.has_cap st c) then raise (GError ("no capability " ^ c))
      else 
        let k' = CapSet.singleton c in
        let p = CapSet.remove st.k c in
        (* framing; P is K\c *)
        State.get_mem st loc, (c, c), k', p
    end
  |Binary(op, e1, e2) -> eval_binop st op e1 e2
  |Fun(cls, caps, params, return, e) -> begin
      let store = match st.store with
        |[] -> raise (GError "empty store")
        (* only copy head of scoping stack *)
        |h::t -> (Hashtbl.copy h):: t
      in
      (* no k', k is moved to p because nothing is really closed over/consumed here *)
      V_fun(cls, caps, params, return, e, store), (c_any, c_none), CapSet.empty, st.k
    end 
  |Apply(fname, args) -> begin
      (* TODO constructors, unique args, check captured env *)
      let t, c, loc = State.find_var st fname in
      let v = State.get_mem st loc |> State.deref st in
      match v with
      |V_fun(cls, caps, params, ret, expr, store) -> begin
          (* check if "method" can be used *)
          (match cls, st.focus with
           |Some c, Some (_, t, _) -> 
             if not (State.eq_types st (T_cls c) t) 
             then raise (GError "class is not focused") 
             else ()
           |Some c, _ -> raise (GError "class is not focused")
           |None, _ -> ());
          (* TODO bug: mutable arguments (objects) consume their caps when evaluated *)
          (* eval all the arguments from L to R *)
          let eval_arg e (state, vs, k's, p_) =
            let v, (r, w), k', p = eval state e |> autoclone state in
            (* make sure W caps aren't consumed - potential workaroudn? *)
            let state' = {state with k = CapSet.add p w} in
            state', (v,r)::vs, k'::k's, p
          in
          let st, vs, k's, p = List.fold_right ~f:eval_arg ~init:(st,[],[],CapSet.empty) args in
          (* validate caps, for now treat as vars *)
          let caps = List.map caps (fun x ->  let _, c, _ = State.find_var st x in c) in
          List.iter caps (fun x -> if CapSet.mem st.k x then () else raise (GError "capability not found"));
          (* check types for args *)
          let arg_value_list = List.map2_exn 
              ~f:(fun (n,t,u) (v,c) -> 
                  if not (State.eq_types st (get_type v) t) then 
                    raise (GError "argument does not match input type")
                  else (n,v,c)
                ) params vs in
          (* TODO validate env *)
          (* assign them to store in new scope, disregarding shadowing rules *)
          let st = State.enter_scope st in
          List.iter ~f:(fun (arg_n,arg_v,c) -> 
              match arg_v with
              |V_ptr(l1, l2, mut, ptr_t) -> begin
                  (* pass mutable things by reference *)
                  if State.is_mutable st arg_v then
                    let loc' = State.unique st in
                    Hashtbl.add_exn (List.hd_exn st.store) arg_n (ptr_t, c, loc');
                    Hashtbl.add_exn st.mem loc' (V_ptr(loc', l2, mut, ptr_t));
                    (* pass immutable things by value *)
                  else
                    let arg_v = State.deref st arg_v in
                    State.add_var st arg_n c arg_v
                end
              |_ -> State.add_var st arg_n c arg_v
            ) arg_value_list;
          (* eval body *)
          let v, (r, w), k', p = eval st expr |> autoclone st in
          if not (State.eq_types st (get_type v) ret) then 
            raise (GError "invalid return type")
          else 
            v, (r, w), k', p
        end
      |_ -> raise (GError "expected a function")
    end
  |Object(cls, o) -> begin
      (* helper function for processing a field and setting up the correct pointers *)
      let eval_field (var, e, u, m) (state, locs, k's, p_) =
        let v, (r, w), k', p = eval state e |> autoclone state in
        let t = get_type v in
        let loc = State.unique state in
        let loc' = State.unique state in
        (match v with
         |V_ptr(l,l',m,t) -> begin
             let value = State.get_mem st l' in
             if State.is_mutable st value then
               Hashtbl.set st.mem loc (V_ptr(loc, l', m, t))
             else 
               Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
             Hashtbl.set st.mem loc' (State.get_mem st l')
           end
         |v -> begin
             Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
             Hashtbl.set st.mem loc' v
           end);
        let state' = {state with k = p} in
        state', (var, (t, u, m, loc))::locs, k'::k's, p
      in
      let st, field_info, k's, p = List.fold_right ~f:eval_field ~init:(st,[],[],CapSet.empty) o in
      let c = "c."^(string_of_int (State.unique st)) in
      let k' = List.fold_left 
          ~f:(fun a x -> CapSet.union a x) 
          ~init:(CapSet.empty) 
          ((CapSet.singleton c)::k's) 
      in
      (* k' is union of all k' + c, p is p of last field expr *)
      V_obj(cls, field_info), (c, c), k', p
    end
  |Get(e,fname) -> begin
      let v, (r, w), k', p = eval st e |> autoclone st in
      match State.deref st v with
      |V_obj(cls, fields) -> begin
          let (t,u,mut,loc) = List.Assoc.find_exn fields ~equal:(=) fname in
          (* if immutable, no write is allowed *)
          let w = match mut with 
            |MUT -> w 
            |IMMUT -> c_none
          in
          let r = match u with
            |U -> r ^ "." ^ fname (* TODO only while focused! *)
            |A -> r
          in
          let fval = State.get_mem st loc in
          (* TODO check this behavior *)
          let p = 
            if State.is_mutable st fval then 
              CapSet.diff p (CapSet.singleton w)
            else p 
          in
          fval, (r,w), k', p
        end
      |_ -> raise (GError "expected object")
    end
  |Seq(e1, e2) -> begin
      let v1, (r1, w1), k', pl = eval st e1|> autoclone st in
      let st' = {st with k = pl} in
      eval st' e2
    end
  |If(c, e1, e2) -> begin
      let v, (r, w), k', p = eval st c |> autoclone st in
      let st' = State.enter_scope {st with k = p} in
      match State.deref st v with
      |V_bool b -> begin
          let e = if b then e1 else e2 in
          eval st' e
        end
      |_ -> raise (GError "condition needs to be boolean")
    end
  |While(c, e) -> begin
      let v, (r, w), k', p = eval st c |> autoclone st in
      let st' = State.enter_scope {st with k = p} in
      match State.deref st v with
      |V_bool b -> begin
          if b then
            eval st' (Seq(e, While(c, e)))
          else
            V_unit, (c_none, c_none), CapSet.empty, p
        end
      |_ -> raise (GError "condition needs to be boolean")
    end
  |Let(x, e1, e2) -> begin
      (* autoclone to prevent own caps from being consumed *)
      let v, (r, w), k', p = eval st e1 |> autoclone st in
      let c = "c."^(string_of_int (State.unique st)) in
      let st = State.enter_scope st in
      match State.find_var_opt st x with
      | Some _ -> raise (GError "no shadowing allowed")
      | None -> begin
          State.add_var st x c v;
          (* TODO semantics differ from doc - transferring k' or k? *)
          let st' = {st with k = CapSet.add p c} in
          eval st' e2
        end
    end
  |Destroy e -> begin
      (* read cap for whatever was evaluated is destroyed *)
      let v, (r, w), k', p = eval st e |> autoclone st in
      (* remove w from p, frame *)
      let p = CapSet.diff (CapSet.union p st.k) (CapSet.add k' w) in
      V_unit, (c_none, c_none), CapSet.empty, p
    end
  |Sleep e -> begin
      match e with
      |Int i -> (Unix.sleep i); V_unit, (c_none, c_none), CapSet.empty, st.k
      |_ -> raise (GError "expected int literal")
    end
  |Branch(vlist, e) -> begin
      let caps = List.map vlist (fun x ->  let _, c, _ = State.find_var st x in c) |> CapSet.of_list in
      let p_k, c_k = CapSet.diff st.k caps, CapSet.inter st.k caps in
      let c_st = State.enter_scope {st with k = c_k} in
      ignore(Thread.create (fun expr -> eval c_st expr) e);
      V_unit, (c_none, c_none), CapSet.empty, p_k
    end
  |Focus(e1, e2) -> begin
      (* do we NEED to focus a named class? *)
      let v, (r, w), k', p = eval st e1 |> autoclone st in
      let st = State.enter_scope st in
      match v, State.deref st v with
      |(V_ptr(l1, l2, _, T_cls cname), V_obj(cls, fields)) -> begin
          let unique_caps = List.filter ~f:(fun (v, (t,u,m,l)) -> u = U) fields |> 
                            List.map ~f:(fun (v, (t,u,m,l)) -> w ^ "." ^ v) |>
                            CapSet.of_list in
          let st' = {
            st with focus = Some (w, T_cls cname, l2); 
                    (* add unique caps, remove object's cap *)
                    k = CapSet.remove (CapSet.union p unique_caps) w
          } in
          let v2, (r2, w2), k'', p2 = eval st' e2 |> autoclone st' in
          (* if all unique caps remain, add object back, otherwise consume *)
          if CapSet.diff unique_caps p2 = CapSet.empty then
            let p = CapSet.add (CapSet.diff p2 unique_caps) w in
            v2, (r2, w2), k'', p
          else
            let p = CapSet.remove (CapSet.diff p2 unique_caps) w in
            v2, (r2, w2), k'', p
        end
      |_ -> raise (GError "expected object")
    end
  |Assign(e1, e2) -> begin
      (* this needs another looking-at *)
      let v1, (r1, w1), k', pl = eval st e1 |> autoclone st in
      let st' = {st with k = pl} in
      let v2, (r2, w2), k'', pr = eval st' e2 |> autoclone st' in
      let c = check_write w1 r1 st'.k in
      let k' = CapSet.add k'' w2 in
      let k', p = CapSet.remove k' c, CapSet.add pr c in
      (match (v1, v2) with
       (* aliasing *)
       |V_ptr(l1,l1',m1,t1), V_ptr(l2,l2',m2,t2) -> begin
           let v_right = State.get_mem st l2' in
           if not (State.eq_types st t1 t2) then raise (GError "types do not match")
           else if m1 <> MUT then raise (GError "LHS is not mutable")
           else if CapSet.mem k' c then raise (GError "c in k'")
           (* if RHS is mutable *)
           else if State.is_mutable st v_right then
             Hashtbl.set st.mem l1 (V_ptr(l1, l2', m1, t1))
             (* if RHS is immutable *)
           else Hashtbl.set st.mem l1' (State.get_mem st l2')
         end
       (* assigning a value *)
       |V_ptr(l,l',m,t), v -> begin
           if not (State.eq_types st t (get_type v)) then raise (GError "types do not match")
           else if CapSet.mem k' c then raise (GError "c in k'")
           else Hashtbl.set st.mem l' v
         end
       |_ -> raise (GError "illegal assignment"));
      V_unit, (c_none, c_none), k', p
    end
  |Neg e -> begin
      let v, (r, w), k', p = eval st e |> autoclone st in
      match State.deref st v with
      | V_int i -> V_int(-1 * i), (r, w), CapSet.empty, p
      | _ -> raise (GError "expected int for integer negation")
    end
  |Not e -> begin
      let v, (r, w), k', p = eval st e |> autoclone st in
      match State.deref st v with
      | V_bool i -> V_bool(not i), (r, w), CapSet.empty, p
      | _ -> raise (GError "expected bool for boolean negation")
    end
  (* |This -> begin
      (* nothing is consumed, TODO immutable pointer; can we apply framing here? *)
      match st.focus with
      |Some (c, t, loc) -> V_ptr(-1, loc, IMMUT, t), (c, c_none), CapSet.empty, st.k
      |None -> raise (GError "not in focus")
     end *)
  |Class(c,t,super) -> 
    match t with
    |T_obj fields -> begin
        if State.cls_exists st c then raise (GError "class name already defined")
        else
          let args = List.fold_right ~f:(fun (v, t, u, _) acc -> (v, t, u)::acc) ~init:[] fields in
          let oexpr_fields = List.fold_right ~f:(fun (v, _, u, mut) acc -> (v, Var(v), u, mut)::acc) ~init:[] fields in
          let constructor = V_fun(None, [], args, T_cls(c), Object(c, oexpr_fields), []) in
          State.add_var st c c_any constructor;
          Hashtbl.add_exn (List.hd_exn st.classes) c t;
          V_unit, (c_any, c_none), CapSet.empty, st.k
      end
    |_ -> raise (GError "expected object type")

and eval_binop st bop e1 e2 = 
  (* throw away K' and K'', no caps check atm *)
  let v1, (r1, w1), k', pl = eval st e1 |> autoclone st in
  let st' = {st with k = pl} in
  let v2, (r2, w2), k'', pr = eval st' e2 |> autoclone st' in
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
    |_ -> raise (GError "invalid binop")
  in
  let cr, cw = reconcile_caps (r1, w1) (r2, w2) k' k'' in
  v', (cr, cw), CapSet.empty, pr
