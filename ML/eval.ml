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
  let result = 
    match exp with
    |Int i -> V_int(i), (c_any, c_none), CapSet.empty, st.k
    |Bool b -> V_bool(b), (c_any, c_none), CapSet.empty, st.k
    |Unit -> V_unit, (c_any, c_none), CapSet.empty, st.k
    |Var x -> begin
        let t, c, loc = State.find_var st x in
        g_assert (State.has_cap st c) ("no capability " ^ c); 
        let k' = CapSet.singleton c in
        let p = CapSet.remove st.k c in
        (* framing; P is K\c *)
        State.get_mem st loc, (c, c), k', p
      end
    |Binary(op, e1, e2) -> eval_binop st op e1 e2
    |Fun(params, rtype, body) -> begin
        let store = match st.store with
          |[] -> raise (GError "empty store")
          (* only copy head of scoping stack *)
          |h::t -> (Hashtbl.copy h):: t
        in
        (* no k', k is moved to p because nothing is really closed over/consumed here *)
        V_fun(params, rtype, body, store), (c_any, c_none), CapSet.empty, st.k
      end 
    |Apply(func, args) -> eval_apply st func args
    |Object(cls, fields) -> eval_object st cls fields
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
        let v1, (r1, w1), k', pl = eval st e1|> unit_coerce st |> autoclone st in
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
    |Let(x, e1, e2) -> eval_let st x e1 e2
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
    |Capof e -> begin
        match e with
        |Var x -> let _, c, _ = State.find_var st x in V_cap c, (c_any, c_any), CapSet.empty, st.k
        |_ -> raise (GError "expected variable")
      end
    |Branch(vlist, e) -> begin
        let caps = List.map vlist (fun x ->  let _, c, _ = State.find_var st x in c) |> CapSet.of_list in
        let p_k, c_k = CapSet.diff st.k caps, CapSet.inter st.k caps in
        let c_st = State.enter_scope {st with k = c_k} in
        ignore(Thread.create (fun expr -> eval c_st expr) e);
        V_unit, (c_none, c_none), CapSet.empty, p_k
      end
    |Focus(var, e2) -> begin
        (* do we NEED to focus a named class? *)
        let e1 = Var(var) in
        let v, (r, w), k', p = eval st e1 |> autoclone st in
        let st = State.enter_scope st in
        match v, State.deref st v with
        |(V_ptr(l1, l2, _, T_cls cname), V_obj(cls, fields)) -> begin
            let unique_caps = List.filter ~f:(fun (v, (t,u,m,l)) -> u = U) fields |> 
                              List.map ~f:(fun (v, (t,u,m,l)) -> w ^ "." ^ v) |>
                              CapSet.of_list in
            let st' = {
              st with focus = (w, T_cls cname, l2)::(st.focus);
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
    |Assign(e1, e2) -> eval_assign st e1 e2
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
    |Class(c,fields,super) -> eval_cls_decl st c fields super
  in
  State.validate_result st result

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
  let result = v', (cr, cw), CapSet.empty, pr in
  State.validate_result st result

(* FUNCTION APPLICATION *)
and eval_apply st func args = 
  let v, (r, w), k', p = eval st func |> autoclone st in
  match State.deref st v with
  |V_fun(params, rtype, body, store) -> begin
      (* helper function for evaluating a single argument *)
      let eval_arg e (state, vs, k's) =
        let v, (r, w), k', p = eval state e |> autoclone state in
        (* make sure W caps aren't consumed *)
        let state' = {state with k = CapSet.add p w} in
        state', (v,r)::vs, k'::k's
      in
      let st, evaluated_args, k's = List.fold_right ~f:eval_arg ~init:(st,[],[]) args in
      (* validate store *)
      let param_names = List.filter_map ~f:(
          fun p -> match p with
            |Lambda(n,_,_) -> Some n
            |_ -> None
        ) params in
      let closure_entries = hashtbl_stack_tolist store |> 
                            List.filter ~f:(fun (k,v) -> List.mem param_names k ~equal:(=)) in
      List.iter closure_entries ~f:(
        fun (k,(t,c,_)) -> 
          let (t',c',_) = State.find_var st k in
          g_assert (t = t' && c = c')  "calling environment validation failed"
      );
      (* process args, while checking caps/metacaps and types, and substituting metacaps *)
      let rec fold_args params args acc = 
        match params, args with
        |[], [] -> acc
        |param::t1, h2::t2 -> begin
            match param, h2 with 
            |Lambda(n,cap,t),(v,c) -> begin
                g_assert (State.is_subtype st (get_type v) t) "argument does not match param type";
                g_assert (cap = c) ("argument does not match required cap: expected "^cap^" got "^c);
                fold_args t1 t2 ((n,v,c)::acc)
              end
            |KappaLambda meta, (V_cap cap, c) -> 
              let new_params = substitute_cap meta cap t1 in
              fold_args new_params t2 acc
            |KappaLambda meta, _ -> raise (GError "kappa argument does not match param type")
            |SigmaLambda(n,meta,t),(v,c) -> begin
                let (focus_cap, focus_t, focus_loc) = List.hd_exn st.focus in
                g_assert (State.eq_types st (get_type v) t) "sigma argument does not match param type";
                g_assert (State.eq_types st (get_type v) focus_t) "sigma argument does not match focus stack type";
                g_assert (focus_cap = c) "sigma argument does not match focus stack cap";
                let new_params = substitute_cap meta c t1 in
                fold_args new_params t2 ((n,v,c)::acc)
              end
          end
        |_ -> raise (GError "different numbers of params and arguments")
      in
      let processed_args = fold_args params evaluated_args [] in
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
        ) processed_args;
      (* eval body *)
      let v, (r, w), k', p = eval st body |> autoclone st in
      if not (State.is_subtype st (get_type v) rtype) then 
        raise (GError "invalid return type")
      else 
        v, (r, w), k', p
    end
  |_ -> raise (GError "expected a function")

(* CONSTRUCTOR BODY *)
and eval_object st cls fields = 
  (* originally for obj literals, currently this node is only used in constructors *)
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
           (Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
            Hashtbl.set st.mem loc' (State.get_mem st l'))
       end
     |v -> begin
         Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
         Hashtbl.set st.mem loc' v
       end);
    let state' = {state with k = p} in
    state', (var, (t, u, m, loc))::locs, k'::k's, p
  in
  let st, field_info, k's, p = List.fold_right ~f:eval_field ~init:(st,[],[],CapSet.empty) fields in
  let c = "c."^(string_of_int (State.unique st)) in
  let k' = List.fold_left 
      ~f:(fun a x -> CapSet.union a x) 
      ~init:(CapSet.empty) 
      ((CapSet.singleton c)::k's) 
  in
  (* k' is union of all k' + c, p is p of last field expr *)
  (* return pointer to the object instead of actual object, TODO check behavior *)
  let loc = State.unique st in
  let obj = V_obj(cls, field_info) in
  let ptr = (V_ptr(-1, loc, (if State.is_mutable st obj then MUT else IMMUT), get_type obj)) in
  Hashtbl.set st.mem loc obj;
  ptr, (c, c), k', p

(* CLASS DECLARATION *)
and eval_cls_decl st c fields super = 
  let check_dedup t_obj = 
    let sorted = List.dedup_and_sort (fun (x,_,_,_) (y,_,_,_) -> String.compare x y) t_obj in
    if List.length sorted <> List.length t_obj then raise (GError "fields must have unique names")
    else ()
  in
  g_assert (State.cls_exists st c |> not) "class name already defined";
  (* constructor generation *)
  let super_fields = match super with
    |Some sc -> State.find_cls st sc |> fst
    |None -> []
  in
  let fields = super_fields @ fields in
  check_dedup fields;
  let oexpr_fields = List.fold_right ~f:(fun (v, _, u, mut) acc -> (v, Var(v), u, mut)::acc) ~init:[] fields in
  let a_fields = List.filter ~f:(fun (_, _, u, _) -> u = A) fields in
  let u_fields = List.filter ~f:(fun (_, _, u, _) -> u = U) fields in
  (* add K-lambda for A fields, if any *)
  let lambdas = if List.length a_fields = 0 then [] else [KappaLambda "_f0"] in
  (* add unique K-lambda for U fields, keep mapping of name -> cap *)
  let lambdas, _, fcap_map = List.fold_left 
      u_fields ~init:(lambdas, 1, []) 
      ~f:(
        fun (l,c,map) (fname,_,_,_) -> 
          let fcap = "_f"^(string_of_int c) in
          ((KappaLambda fcap::l, c+1, (fname,fcap)::map))
      ) 
  in
  (* add regular lambdas, reverse the whole list *)
  let params = List.fold_left
      fields ~init:lambdas 
      ~f: (
        fun lambdas (v,t,u,_) -> 
          let fcap = if u = A then "_f0" else List.Assoc.find_exn fcap_map ~equal:(=) v in
          (Lambda(v, fcap, t))::lambdas
      ) |> List.rev
  in
  (* no closed over store atm, TODO check this *)
  let constructor = V_fun(params, T_cls(c), Object(c, oexpr_fields), []) in
  State.add_var st c c_any constructor;
  Hashtbl.add_exn (List.hd_exn st.classes) c (fields, super);
  V_unit, (c_any, c_none), CapSet.empty, st.k

and eval_let st x e1 e2 = 
  (* autoclone to prevent own caps from being consumed *)
  let v, (r, w), k', p = eval st e1 |> autoclone st in
  let c = "c."^(string_of_int (State.unique st)) in
  let st = State.enter_scope st in
  match State.find_var_opt st x with
  | Some _ -> raise (GError "no shadowing allowed")
  | None -> begin
      let loc = State.unique st in
      let loc' = State.unique st in
      (match v with
       |V_ptr(l,l',m,t) -> begin
           let value = State.get_mem st l' in
           Hashtbl.set (List.hd_exn st.store) x (t, c, loc);
           if State.is_mutable st value then
             Hashtbl.set st.mem loc (V_ptr(loc, l', m, t))
           else 
             (Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
              Hashtbl.set st.mem loc' (State.get_mem st l'))
         end
       |v -> begin
           let t = get_type v in
           Hashtbl.set (List.hd_exn st.store) x (t, c, loc);
           Hashtbl.set st.mem loc (V_ptr(loc, loc', MUT, t));
           Hashtbl.set st.mem loc' v
         end);
      let st' = {st with k = CapSet.add p c} in
      eval st' e2
    end

and eval_assign st e1 e2 = 
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
       g_assert (State.is_subtype st t2 t1) "types do not match";
       g_assert (m1 = MUT) "LHS is not mutable";
       g_assert (CapSet.mem k' c |> not) "c in k'";
       (* if RHS is mutable *)
       if State.is_mutable st v_right then
         Hashtbl.set st.mem l1 (V_ptr(l1, l2', m1, t1))
         (* if RHS is immutable *)
       else Hashtbl.set st.mem l1' (State.get_mem st l2')
     end
   (* assigning a value *)
   |V_ptr(l,l',m,t), v -> begin
       g_assert (State.is_subtype st (get_type v) t) "types do not match";
       g_assert (CapSet.mem k' c |> not) "c in k'";
       Hashtbl.set st.mem l' v
     end
   |_ -> raise (GError "illegal assignment"));
  V_unit, (c_none, c_none), k', p