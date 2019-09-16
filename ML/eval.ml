open Ast

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception TypeError of string
exception NameError of string
exception UnboundFieldError of string

let has_dups lst =
  List.length lst <> List.length (List.sort_uniq Pervasives.compare lst)

(* TODO: skip/continue, recursive functions using refs *)
let rec st_get (s:symboltable) (name:var) : gt option = 
  match s with
  | [] -> None
  | h::t -> if fst h = name then Some(snd h) else st_get t name

let rec st_add (s:symboltable) (entry: var * gt): symboltable = 
  match s with
  | [] -> [entry]
  | h::t -> if fst h <> fst entry then h::(st_add t entry) else 
    if snd h <> snd entry then raise (TypeError "invalid type") else entry::t

let rec typecheck_c (s:symboltable) (c:command) : symboltable = 
  match c with
  | Skip -> s
  | Assign(l,r) -> begin
      let t_l = typecheck_e s l in
      let t_r = typecheck_e s r in
      match l with 
      |Get(_, _) -> if t_l = t_r then s else raise (TypeError "invalid type")
      |Var(x) -> st_add s (x, t_r)
      |_ -> raise (TypeError "cannot assign")
    end
  | Seq(c1, c2) -> begin
      let s' = typecheck_c s c1 in
      typecheck_c s' c2
    end
  | If(b, c1, c2) -> begin
      let _ = typecheck_c s c1 in
      let _ = typecheck_c s c2 in
      let c = typecheck_e s b in
      match c with
      |T_bool -> s
      |_ -> raise (TypeError "invalid type")
    end
  | While(b, c) -> begin
      let _ = typecheck_c s c in
      let c = typecheck_e s b in
      match c with
      |T_bool -> s
      |_ -> raise (TypeError "invalid type")
    end
and typecheck_e (s:symboltable) (e:expr) : gt = 
  match e with 
  | Int(i) -> T_int
  | Bool(b) -> T_bool
  | Var(x) -> begin
      match st_get s x with
      | None -> raise (UnboundVariable x)
      | Some(t) -> t
    end
  | Binary(op, e1, e2) -> begin
      match (op, typecheck_e s e1, typecheck_e s e2) with
      |(BinopAnd, T_bool, T_bool) -> T_bool
      |(BinopOr, T_bool, T_bool) -> T_bool
      |(BinopPlus, T_int, T_int) -> T_int
      |(BinopMinus, T_int, T_int) -> T_int
      |(BinopTimes, T_int, T_int) -> T_int
      |(BinopDiv, T_int, T_int) -> T_int
      |(BinopMod, T_int, T_int) -> T_int
      |(BinopLt, T_int, T_int) -> T_bool
      |(BinopLeq, T_int, T_int) -> T_bool
      |(BinopGt, T_int, T_int) -> T_bool
      |(BinopGeq, T_int, T_int) -> T_bool
      |(BinopEq, a, b) when a = b -> T_bool
      |(BinopNeq, a, b) when a = b -> T_bool
      |_ -> raise (TypeError "invalid type for binop")
    end
  | Fun(params,output) -> begin
      if params |> List.map fst |> has_dups then
        raise (NameError "duplicate param name")
      else
        let inputs = params |> List.map snd in
        let s_with_inputs = List.fold_left st_add s params in
        let t_output = typecheck_e s_with_inputs output in
        T_func(inputs, t_output, s)
    end
  | Apply(f,inputs) -> T_int
  | Object(fields) -> begin
      if fields |> List.map fst |> has_dups then
        raise (NameError "duplicate field name")
      else
        let field_compare a b = Pervasives.compare (fst a) (fst b) in
        let ordered_fields = List.sort field_compare fields in
        let ordered_types = List.map (fun x -> (fst x, typecheck_e s (snd x))) ordered_fields in
        T_obj(ordered_types)
    end
  | Get(obj, field) -> begin
      match typecheck_e s obj with
      | T_obj(fields) -> begin
          match List.find_opt (fun f -> fst f = field) fields with
          | Some(f) -> snd f
          | None -> raise (UnboundFieldError field)
        end
      | _ -> raise (TypeError "cannot get field of non-object")
    end

let typecheck  (c:command) : symboltable = 
  let initial_symboltable = [] in
  typecheck_c initial_symboltable c

(* Evaluate arithmetic expression *)
(* let rec evala (s:symboltable) (exp:aexp) : int =
   match exp with
   | Int(i) -> i
   | Var(x) -> begin
      try
        List.assoc x s
      with | Not_found -> raise (UnboundVariable x)
    end
   | Plus(e1, e2) -> evala s e1 + evala s e2
   | Minus(e1, e2) -> evala s e1 - evala s e2
   | Times(e1, e2) -> evala s e1 * evala s e2
   | Input -> print_string ">" ; read_int () *)

(* Evaluate boolean expression *)
(* let rec evalb (s : symboltable) (e : bexp) : bool =
   match e with
   | True -> true
   | False -> false
   | Equals (e1, e2) -> evala s e1 = evala s e2
   | NotEquals (e1, e2) -> evala s e1 != evala s e2
   | Less (e1, e2) -> evala s e1 < evala s e2
   | LessEq (e1, e2) -> evala s e1 <= evala s e2
   | Greater (e1, e2) -> evala s e1 > evala s e2
   | GreaterEq (e1, e2) -> evala s e1 >= evala s e2
   | Not e -> if evalb s e then false else true
   | And (e1, e2) -> evalb s e1 && evalb s e2
   | Or (e1, e2) -> evalb s e1 || evalb s e2 *)

(* Evaluate a command. *)
(* let rec evalc (conf:configuration) : symboltable = [] *)
(* let rec evalc (conf:configuration) : symboltable =
   match conf with
   (* SKIP *)
   | (s, Skip, Skip, _) -> s
   | (s, Skip, c', k) -> evalc (s, c', Skip, k)
   (* ASSIGN *)
   | (s, Assign(v,e), c', k) -> begin
      let temp_s = List.filter (fun x -> fst x != v) s in
      let s' = (v, (evala s e))::temp_s in
      evalc (s', Skip, c', k)
    end
   (* SEQ *)
   | (s, Seq(c1,c2), Skip, k) -> evalc (s, c1, c2, k)
   | (s, Seq(c1,c2), c', k) -> evalc (s, c1, Seq(c2, c'), k)
   (* IF *)
   | (s, If(b, c1, c2), c', k) ->
    begin
      match (evalb s b) with
      | true -> evalc (s, c1, c', k)
      | false ->  evalc (s, c2, c', k)
    end
   (* PRINT *)
   | (s, Print(e), c', k) ->
    Int (evala s e) |> strAexp |> print_endline ; evalc (s, Skip, c', k)
   (* WHILE *)
   |(s, While (b,c), c', k) ->
    begin
      let continuation = Seq(While(b,c), c') in
      let k' = (c', continuation)::k in
      match (evalb s b) with
      | true -> evalc (s, Seq(c, Continue), continuation, k')
      | false -> evalc (s, Skip, c', k)
    end
   (* TEST *)
   |(s, Test (loc,b), c', k) ->
    begin
      match (evalb s b) with
      | true -> evalc (s, Skip, c', k)
      | false ->  raise (TestFailure (strInfo loc))
    end
   (* BREAK *)
   |(s, Break, _, k) -> begin
      match k with
      |[] -> raise IllegalBreak
      |h::t -> evalc (s, Skip, fst h, t)
    end
   (* CONTINUE *)
   |(s, Continue, _, k) -> begin
      match k with
      |[] -> raise IllegalContinue
      |h::t -> evalc (s, Skip, snd h, t)
    end *)

