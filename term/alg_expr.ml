type ('mix,'id) e =
    BIN_ALG_OP of Operator.bin_alg_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mix,'id) e Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t

type t = (Connected_component.id array list, int) e

type ('mix,'id) bool_expr =
  | TRUE
  | FALSE
  | BOOL_OP of
      Operator.bool_op *
      ('mix,'id) bool_expr Location.annot * ('mix,'id) bool_expr Location.annot
  | COMPARE_OP of Operator.compare_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot

let rec to_json f_mix f_id = function
  | BIN_ALG_OP (op,a,b) ->
    `List [Operator.bin_alg_op_to_json op;
           Location.annot_to_json (to_json f_mix f_id) a;
           Location.annot_to_json (to_json f_mix f_id) b]
  | UN_ALG_OP (op,a) ->
    `List [Operator.un_alg_op_to_json op;
           Location.annot_to_json (to_json f_mix f_id) a]
  | STATE_ALG_OP op -> Operator.state_alg_op_to_json op
  | ALG_VAR i -> `List [`String "VAR"; f_id i]
  | KAPPA_INSTANCE cc -> f_mix cc
  | TOKEN_ID i -> `List [`String "TOKEN"; f_id i]
  | CONST n -> Nbr.to_json n

let rec of_json f_mix f_id = function
  | `List [op;a;b] ->
    BIN_ALG_OP
      (Operator.bin_alg_op_of_json op,
       Location.annot_of_json (of_json f_mix f_id) a,
       Location.annot_of_json (of_json f_mix f_id) b)
  | `List [`String "VAR"; i] -> ALG_VAR (f_id i)
  | `List [`String "TOKEN"; i] -> TOKEN_ID (f_id i)
  | `List [op;a] ->
    UN_ALG_OP (Operator.un_alg_op_of_json op,
               Location.annot_of_json (of_json f_mix f_id) a)
  | x ->
    try STATE_ALG_OP (Operator.state_alg_op_of_json x)
    with Yojson.Basic.Util.Type_error _ ->
    try  CONST (Nbr.of_json x)
    with Yojson.Basic.Util.Type_error _ ->
    try KAPPA_INSTANCE (f_mix x)
    with Yojson.Basic.Util.Type_error _ ->
      raise (Yojson.Basic.Util.Type_error ("Invalid Alg_expr",x))

let rec print pr_mix pr_tok pr_var f = function
  | CONST n -> Nbr.print f n
  | ALG_VAR lab -> Format.fprintf f "'%a'" pr_var lab
  | KAPPA_INSTANCE ast -> pr_mix f ast
  | TOKEN_ID tk -> Format.fprintf f "|%a|" pr_tok tk
  | STATE_ALG_OP op -> Operator.print_state_alg_op f op
  | BIN_ALG_OP (op, (a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)"
      (print pr_mix pr_tok pr_var) a
      Operator.print_bin_alg_op op
      (print pr_mix pr_tok pr_var) b
  |UN_ALG_OP (op, (a,_)) ->
    Format.fprintf f "(%a %a)" Operator.print_un_alg_op op
      (print pr_mix pr_tok pr_var) a

let rec print_bool pr_mix pr_tok pr_var f = function
  | TRUE -> Format.fprintf f "[true]"
  | FALSE -> Format.fprintf f "[false]"
  | BOOL_OP (op,(a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)" (print_bool pr_mix pr_tok pr_var) a
      Operator.print_bool_op op (print_bool pr_mix pr_tok pr_var) b
  | COMPARE_OP (op,(a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)"
      (print pr_mix pr_tok pr_var) a
      Operator.print_compare_op op
      (print pr_mix pr_tok pr_var) b

let rec bool_to_json f_mix f_id = function
  | TRUE -> `Bool true
  | FALSE -> `Bool false
  | BOOL_OP (op,a,b) ->
    `List [ Operator.bool_op_to_json op;
            Location.annot_to_json (bool_to_json f_mix f_id) a;
            Location.annot_to_json (bool_to_json f_mix f_id) b ]
  | COMPARE_OP (op,a,b) ->
    `List [ Operator.compare_op_to_json op;
            Location.annot_to_json (to_json f_mix f_id) a;
            Location.annot_to_json (to_json f_mix f_id) b ]

let rec bool_of_json f_mix f_id = function
  | `Bool b -> if b then TRUE else FALSE
  | `List [op; a; b] as x ->
    begin
      try BOOL_OP (Operator.bool_op_of_json op,
                   Location.annot_of_json (bool_of_json f_mix f_id) a,
                   Location.annot_of_json (bool_of_json f_mix f_id) b)
      with Yojson.Basic.Util.Type_error _ ->
      try COMPARE_OP (Operator.compare_op_of_json op,
                      Location.annot_of_json (of_json f_mix f_id) a,
                      Location.annot_of_json (of_json f_mix f_id) b)
      with Yojson.Basic.Util.Type_error _ ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect bool expr",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect bool_expr",x))

let rec add_dep (in_t,in_e,toks_d,out as x) d = function
  | BIN_ALG_OP (_, a, b), _ -> add_dep (add_dep x d a) d b
  | UN_ALG_OP (_, a), _ -> add_dep x d a
  | ALG_VAR j, _ ->
    let () = out.(j) <- Operator.DepSet.add d out.(j) in
    x
  | (KAPPA_INSTANCE _ | CONST _), _ -> x
  | TOKEN_ID i, _ ->
    let () = toks_d.(i) <- Operator.DepSet.add d toks_d.(i) in
    x
  | STATE_ALG_OP op, _ ->
    match op with
    | (Operator.EMAX_VAR | Operator.TMAX_VAR | Operator.PLOTPERIOD) -> x
    | Operator.TIME_VAR -> (Operator.DepSet.add d in_t,in_e,toks_d,out)
    | (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR) ->
      (in_t,Operator.DepSet.add d in_e,toks_d,out)

let rec aux_extract_cc acc = function
  | BIN_ALG_OP (_, a, b), _ -> aux_extract_cc (aux_extract_cc acc a) b
  | UN_ALG_OP (_, a), _ -> aux_extract_cc acc a
  | (ALG_VAR _ | CONST _ | TOKEN_ID _ | STATE_ALG_OP _), _ -> acc
  | KAPPA_INSTANCE i, _ -> i :: acc
let extract_connected_components x = aux_extract_cc [] x

let setup_alg_vars_rev_dep toks vars =
  let in_t = Operator.DepSet.empty in
  let in_e = Operator.DepSet.empty in
  let toks_d = Array.make (NamedDecls.size toks) Operator.DepSet.empty in
  let out = Array.make (Array.length vars) Operator.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Operator.ALG i) y) (in_t,in_e,toks_d,out) vars

let rec propagate_constant updated_vars counter vars = function
  | BIN_ALG_OP (op,a,b),pos as x ->
    (match propagate_constant updated_vars counter vars a,
           propagate_constant updated_vars counter vars b with
    | (CONST c1,_),(CONST c2,_) -> CONST (Nbr.of_bin_alg_op op c1 c2),pos
    | ((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
       | TOKEN_ID _ | ALG_VAR _ | CONST _),_),
      ((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
       | TOKEN_ID _ | ALG_VAR _ | CONST _),_) -> x)
  | UN_ALG_OP (op,a),pos as x ->
    (match propagate_constant updated_vars counter vars a with
     | CONST c,_ -> CONST (Nbr.of_un_alg_op op c),pos
     | (BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
       | TOKEN_ID _ | ALG_VAR _),_ -> x)
  | STATE_ALG_OP (Operator.EMAX_VAR),pos ->
    CONST
      (match Counter.max_events counter with
       | Some n -> Nbr.I n
       | None ->
         let () =
           ExceptionDefn.warning
             ~pos (fun f -> Format.pp_print_string
                      f "[Emax] constant is evaluated to infinity") in
         Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.TMAX_VAR),pos ->
    CONST
       (match Counter.max_time counter with
         | Some t -> Nbr.F t
         | None ->
           let () =
             ExceptionDefn.warning
               ~pos (fun f -> Format.pp_print_string
                        f "[Tmax] constant is evaluated to infinity") in
           Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.PLOTPERIOD),pos ->
    CONST (Nbr.F (Counter.plot_period counter)),pos
  | STATE_ALG_OP (Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
                 | Operator.NULL_EVENT_VAR),_ as x -> x
  | ALG_VAR i,pos as x ->
    (if List.mem i updated_vars then x
     else match vars.(i) with
       | _,((CONST _ | ALG_VAR _ as y),_) -> y,pos
       | _,((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
            | TOKEN_ID _),_) -> x)
  | (KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_ as x -> x

let rec propagate_constant_bool updated_vars counter vars = function
  | (TRUE | FALSE),_ as x -> x
  | BOOL_OP (op,a,b),pos ->
    begin match propagate_constant_bool updated_vars counter vars a, op with
      | (TRUE,_), Operator.OR -> TRUE,pos
      | (FALSE,_), Operator.AND -> FALSE,pos
      | (TRUE,_), Operator.AND
      | (FALSE,_), Operator.OR ->
        propagate_constant_bool updated_vars counter vars b
      | ((BOOL_OP _ | COMPARE_OP _),_ as a'),_ ->
        match propagate_constant_bool updated_vars counter vars b, op with
        | (TRUE,_), Operator.OR -> TRUE,pos
        | (FALSE,_), Operator.AND -> FALSE,pos
        | (TRUE,_), Operator.AND
        | (FALSE,_), Operator.OR -> a'
        | ((BOOL_OP _ | COMPARE_OP _),_ as b'),_ ->
          BOOL_OP (op,a',b'),pos
    end
  | COMPARE_OP (op,a,b),pos ->
    let a' = propagate_constant updated_vars counter vars a in
    let b' = propagate_constant updated_vars counter vars b in
    match a',b' with
    | (CONST n1,_), (CONST n2,_) ->
      (if Nbr.of_compare_op op n1 n2 then TRUE,pos else FALSE,pos)
    | (( BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
       | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_), _ ->
      COMPARE_OP (op,a',b'),pos

let rec has_time_dep (in_t,_,_,deps as vars_deps) = function
  | (BIN_ALG_OP (_, a, b),_) ->
    has_time_dep vars_deps a||has_time_dep vars_deps b
  | (UN_ALG_OP (_, a),_) -> has_time_dep vars_deps a
  | ((KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_) -> false
  | (STATE_ALG_OP Operator.TIME_VAR,_) -> true
  | (STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
                   Operator.NULL_EVENT_VAR | Operator.EMAX_VAR |
                   Operator.TMAX_VAR | Operator.PLOTPERIOD),_) -> false
  | (ALG_VAR i,_) ->
    let rec aux j =
      Operator.DepSet.mem (Operator.ALG j) in_t ||
      Operator.DepSet.exists
        (function Operator.ALG k -> aux k
                | (Operator.RULE _ | Operator.PERT _) -> false) deps.(j) in
    aux i

let rec stops_of_bool_expr vars_deps = function
  | TRUE | FALSE -> []
  | BOOL_OP (op,(a,_),(b,_)) ->
    let st1 = stops_of_bool_expr vars_deps a in
    let st2 = stops_of_bool_expr vars_deps b in
    (match op,st1,st2 with
     | _, [], _ -> st2
     | _, _, [] -> st1
     | Operator.OR, n1, n2 -> n1 @ n2
     | Operator.AND, _, _ -> raise ExceptionDefn.Unsatisfiable
    )
  | COMPARE_OP (op,(a1,_ as a),(b1,_ as b)) ->
    match op with
    | Operator.EQUAL when has_time_dep vars_deps a||has_time_dep vars_deps b ->
      begin match a1,b1 with
        | STATE_ALG_OP (Operator.TIME_VAR), CONST n
        | CONST n, STATE_ALG_OP (Operator.TIME_VAR) -> [n]
        | ( BIN_ALG_OP _ | UN_ALG_OP _ | ALG_VAR _
          | STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
                          Operator.TIME_VAR | Operator.NULL_EVENT_VAR |
                          Operator.EMAX_VAR |Operator.TMAX_VAR |
                          Operator.PLOTPERIOD)
          | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ ->
          raise ExceptionDefn.Unsatisfiable
      end
    | (Operator.EQUAL | Operator.SMALLER | Operator.GREATER | Operator.DIFF) -> []
