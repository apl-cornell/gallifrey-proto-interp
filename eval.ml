include Utils
open Core
open Core.Poly
open Unix
open Pprint
open Thread

let init_state = State.init

(* value, read, write, K', P *)
type result = value * (cap * cap) * CapSet.set_t * CapSet.set_t

let rec eval (st:State.t) (exp:expr): result = 
  (* do we transfer K to P in all of these *)
  let st0 = st in
  let result = 
    match exp with
    |Int i -> V_int(i), (c_any, c_none), CapSet.empty, st.k
    |Bool b -> V_bool(b), (c_any, c_none), CapSet.empty, st.k
    |Unit -> V_unit, (c_any, c_none), CapSet.empty, st.k
    |Var x -> begin
        let t, c, loc = State.find_var st x in
        g_assert (State.has_cap st c || State.is_focused st c) ("no capability " ^ c);
        let valid = if State.is_focused st c then true else State.valid_cap st c in 
        let k' = CapSet.singleton (c, valid) in
        let p = CapSet.remove st.k (c, valid) in
        let readcap = if valid then c else c_none in
        (* framing; P is K\c *)
        State.get_mem st loc, (readcap, c), k', p
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
        g_assert (State.valid_cap st r) "invalid read cap for object";
        g_assert (State.valid_cap st r) "invalid write cap for object";
        match State.deref st v with
        |V_obj(cls, fields) -> begin
            let (t,u,mut,loc) = List.Assoc.find_exn fields ~equal:(=) fname in
            (* get unique field capability only while focused, otherwise treat as aliasable field *)
            let rf = match u with
              |U -> if (State.is_focused st w) then r ^ "." ^ fname else r
              |A -> r
            in
            (* if immutable, no write is allowed, always use unique writecap for unique fields *)
            let wf = match u with
              |U -> w ^ "." ^ fname
              |A -> w
            in
            let fval = State.get_mem st loc in
            (* TODO check this behavior *)
            let k', p = 
              if State.is_mutable st fval then 
                CapSet.move_cap p k' w
              else 
                CapSet.move_cap k' p w
            in
            fval, (rf,wf), k', p
          end
        |_ -> raise (GError "expected object")
      end
    |Seq(e1, e2) -> begin
        let v1, (r1, w1), k', pl = eval st e1|> unit_coerce st |> autoclone st in
        let st' = State.dropk' st k' pl in
        eval st' e2
      end
    |If(c, e1, e2) -> begin
        let v, (r, w), k', p = eval st c |> autoclone st in
        g_assert (r <> c_none) "if: cannot use invalid cap";
        let st' = State.enter_scope (State.dropk' st k' p) in
        match State.deref st v with
        |V_bool b -> begin
            let e = if b then e1 else e2 in
            eval st' e
          end
        |_ -> raise (GError "condition needs to be boolean")
      end
    |While(c, e) -> begin
        let v, (r, w), k', p = eval st c |> autoclone st in
        g_assert (r <> c_none) "while: cannot use invalid cap";
        let st' = State.enter_scope (State.dropk' st k' p) in
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
        (* print_endline @@ CapSet.set_to_string k';
           print_endline @@ CapSet.set_to_string p; *)
        let st = State.dropk' st k' p in
        (* invalidate W in p, frame *)
        g_assert (State.valid_cap st w && not (State.is_focused st w)) "cannot destroy invalid/focused cap";
        let p = CapSet.frame st.k p in
        let p = CapSet.invalidate_cap p w in
        V_unit, (c_none, c_none), CapSet.empty, p
      end
    |Sleep e -> begin
        match e with
        |Int i -> (Unix.sleep i); V_unit, (c_none, c_none), CapSet.empty, st.k
        |_ -> raise (GError "expected int literal")
      end
    |Capof e -> begin
        let v, (r, w), k', p = eval st e |> autoclone st in
        V_cap w, (c_any, c_any), CapSet.empty, st.k
      end
    |Branch(vlist, e) -> begin
        let caps = List.map vlist (fun x ->  let _, c, _ = State.find_var st x in c) 
                   |> List.map ~f:(fun c -> (c, State.valid_cap st c)) 
                   |> CapSet.of_list in
        let p_k, c_k = CapSet.diff st.k caps, CapSet.inter st.k caps in
        let c_st = State.enter_scope (State.dropk' st CapSet.empty c_k) in
        ignore(Thread.create (fun expr -> eval c_st expr) e);
        V_unit, (c_none, c_none), CapSet.empty, p_k
      end
    |Focus(var, e2) -> begin
        let e1 = Var(var) in
        let v, (r, w), k', p = eval st e1 |> autoclone st in
        let st = State.enter_scope st in
        g_assert (State.valid_cap st w) "focus: cannot use invalid cap";
        match v, State.deref st v with
        |(V_ptr(l1, l2, _, T_cls cname), V_obj(cls, fields)) -> begin
            let unique_caps = List.filter ~f:(fun (v, (t,u,m,l)) -> u = U) fields 
                              |> List.map ~f:(fun (v, (t,u,m,l)) -> w ^ "." ^ v) 
                              |> List.map ~f:(fun c -> (c, true)) 
                              |> CapSet.of_list in
            let st' = {
              st with focus = (w, T_cls cname, l2)::(st.focus);
                      (* add unique caps, remove object's cap *)
                      k = CapSet.remove_name (CapSet.union p unique_caps) w
            } in
            let v2, (r2, w2), k'', p2 = eval st' e2 |> autoclone st' in
            (* all unique caps valid in p, add valid orig to p *)
            if CapSet.diff unique_caps p2 = CapSet.empty then
              let p = CapSet.add (CapSet.diff p2 unique_caps) (w, true) in
              v2, (r2, w2), k'', p
              (* all unique caps valid in k' or p, add valid orig to k' *)
            else if (CapSet.diff unique_caps (CapSet.union p2 k'') = CapSet.empty) then
              let p = CapSet.diff p2 unique_caps in
              let k'' = CapSet.add (CapSet.diff k'' unique_caps) (w, true) in
              v2, (r2, w2), k'', p
              (* unique caps not all valid, add invalid orig to p *)
            else
              let invalid_unique = CapSet.invalid_all unique_caps in
              let p = CapSet.add (CapSet.diff (CapSet.diff p2 unique_caps) invalid_unique) (w, false) in
              let k'' = CapSet.diff (CapSet.diff k'' unique_caps) invalid_unique in
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
  State.validate_result st result st0.k

and eval_binop st bop e1 e2 = 
  (* throw away K' and K'', no caps check atm *)
  let v1, (r1, w1), k', pl = eval st e1 |> autoclone st in
  let st = State.dropk' st k' pl in
  let v2, (r2, w2), k'', pr = eval st e2 |> autoclone st in
  let st = State.dropk' st k'' pr in
  g_assert (r1 <> c_none) "binop: LHS invalid cap";
  g_assert (r2 <> c_none) "binop: RHS invalid cap";
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
    | (BinopNeq, v1, v2) -> V_bool(v1 <> v2)
    | (BinopEq, v1, v2) -> V_bool(v1 = v2)
    |_ -> print_endline (fmt_value v1); print_endline (fmt_value v2); raise (GError "invalid binop")
  in
  let cr, cw = reconcile_caps (r1, w1) (r2, w2) pl pr in
  v', (cr, cw), CapSet.empty, pr

(* FUNCTION APPLICATION *)
and eval_apply st func args = 
  let v, (r, w), k', p = eval st func |> autoclone st in
  match State.deref st v with
  |V_fun(params, rtype, body, store) -> begin
      (* helper function for evaluating a single argument *)
      let eval_arg e (state, vs, k's) =
        let v, (r, w), k', p = eval state e |> autoclone state in
        (* TODO - check; make sure W caps aren't consumed *)
        let st' = State.dropk' st k' p in
        let st' = {st' with k = CapSet.restore_cap st'.k w} in
        st', (v,r)::vs, k'::k's
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
                let v_t = get_type v in
                g_assert (State.is_subtype st v_t t) (expected_got t v_t);
                let c = check_write cap c st.k in
                g_assert (cap = c) ("argument does not match required cap: expected "^cap^" got "^c);
                fold_args t1 t2 ((n,v,c)::acc)
              end
            |KappaLambda meta, (V_cap cap, c) -> 
              let new_params = substitute_cap meta cap t1 in
              fold_args new_params t2 acc
            |KappaLambda meta, (v,_) -> raise (GError (expected_got T_cap (get_type v)))
            |SigmaLambda(n,meta,t),(v,c) -> begin
                let v_t = get_type v in
                let (focus_cap, focus_t, focus_loc) = List.hd_exn st.focus in
                g_assert (State.eq_types st v_t t) ("sigma argument does not match param type " ^ (expected_got t v_t));
                g_assert (State.eq_types st v_t focus_t) ("sigma argument does not match focus stack type " ^ (expected_got focus_t v_t));
                g_assert (focus_cap = c) "sigma argument does not match focus stack cap";
                let new_params = substitute_cap meta c t1 in
                fold_args new_params t2 ((n,v,c)::acc)
              end
          end
        |_ -> raise (GError "different numbers of params and arguments")
      in
      let processed_args = fold_args params evaluated_args [] in
      (* TODO this line causes infinite loop for some reason *)
      (* let st = {st with store = store} in *)
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
      let st = {st with in_func = true} in
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
     |V_ptr(l,l',m_,t) -> begin
         let value = State.get_mem st l' in
         if State.is_mutable st value then
           Hashtbl.set st.mem loc (V_ptr(loc, l', m, t))
         else 
           (Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
            Hashtbl.set st.mem loc' (State.get_mem st l'))
       end
     |_ -> begin
         Hashtbl.set st.mem loc (V_ptr(loc, loc', m, t));
         Hashtbl.set st.mem loc' v
       end);
    let state' = State.dropk' state k' p in
    state', (var, (t, u, m, loc))::locs, k'::k's, p
  in
  let st, field_info, k's, p = List.fold_right ~f:eval_field ~init:(st,[],[],CapSet.empty) fields in
  let c = "c."^(string_of_int (State.unique st)) in
  let k' = List.fold_left 
      ~f:(fun a x -> CapSet.union a x) 
      ~init:(CapSet.empty) 
      ((CapSet.singleton (c, true))::k's) 
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
  g_assert (r <> c_none) "let: cannot use invalid cap";
  let c = "c."^(string_of_int (State.unique st)) in
  let st = State.enter_scope st in
  match (st.in_func, State.find_var_opt st x) with
  | false, Some _ -> raise (GError "no shadowing allowed")
  | true, _
  | _, None -> begin
      let loc = State.unique st in
      let loc' = State.unique st in
      (match v with
       |V_ptr(l,l',m,t) -> begin
           let value = State.get_mem st l' in
           Hashtbl.set (List.hd_exn st.store) x (t, c, loc);
           if State.is_mutable st value then
             Hashtbl.set st.mem loc (V_ptr(loc, l', IMMUT, t))
           else 
             (Hashtbl.set st.mem loc (V_ptr(loc, loc', IMMUT, t));
              Hashtbl.set st.mem loc' (State.get_mem st l'))
         end
       |v -> begin
           let t = get_type v in
           Hashtbl.set (List.hd_exn st.store) x (t, c, loc);
           Hashtbl.set st.mem loc (V_ptr(loc, loc', IMMUT, t));
           Hashtbl.set st.mem loc' v
         end);
      let st' = State.dropk' st k' (CapSet.add p (c, true)) in
      eval st' e2
    end

and eval_assign st e1 e2 = 
  let v1, (r1, w1), k', pl = eval st e1 |> autoclone st in
  let st' = State.dropk' st k' pl in
  (* print_endline @@ CapSet.set_to_string st'.k; *)
  let v2, (r2, w2), k'', pr = eval st' e2 |> autoclone st' in
  (* print_endline r2; *)
  (* use original K for cap rewrite *)
  let c = check_write w1 r2 st.k in
  (* print_endline c; *)
  let st = State.dropk' st' k'' pr in
  (* print_endline @@ CapSet.set_to_string st.k; *)
  g_assert (r2 <> c_none) "assign: cannot use invalid cap";
  (* move w2 to k' if mutable, p if not *)
  let k',p = if State.is_mutable st v2 then CapSet.move_cap pr k'' w2 else CapSet.move_cap k'' pr w2 in
  (* restore write cap *)
  let k', p = CapSet.move_cap k' p c in
  let p = CapSet.restore_cap p c in
  (match (v1, v2) with
   (* aliasing *)
   |V_ptr(l1,l1',m1,t1), V_ptr(l2,l2',m2,t2) -> begin
       let v_right = State.get_mem st l2' in
       g_assert (State.is_subtype st t2 t1) ("types do not match: expected " ^ (fmt_type t1) ^ " got " ^ (fmt_type t2));
       g_assert (m1 = MUT) "LHS is not mutable";
       g_assert (CapSet.has_name k' c |> not) "c in k'";
       (* if RHS is mutable *)
       if State.is_mutable st v_right then
         Hashtbl.set st.mem l1 (V_ptr(l1, l2', m1, t1))
         (* if RHS is immutable *)
       else Hashtbl.set st.mem l1' (State.get_mem st l2')
     end
   (* assigning a value *)
   |V_ptr(l,l',m,t), v -> begin
       g_assert (State.is_subtype st (get_type v) t) ("types do not match: expected " ^ (fmt_type t) ^ " got " ^ (get_type v |> fmt_type));
       g_assert (m = MUT) "LHS is not mutable";
       g_assert (CapSet.has_name k' c |> not) "c in k'";
       Hashtbl.set st.mem l' v
     end
   |_ -> raise (GError "illegal assignment"));
  V_unit, (c_none, c_none), k', p