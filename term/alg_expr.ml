type t =
    BIN_ALG_OP of Operator.bin_alg_op * t Location.annot * t Location.annot
  | UN_ALG_OP of Operator.un_alg_op * t Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t

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
     | Operator.TIME_VAR -> (Operator.DepSet.add d in_t,in_e,toks_d,out)
     | (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR) ->
	(in_t,Operator.DepSet.add d in_e,toks_d,out)

let setup_alg_vars_rev_dep toks vars =
  let in_t = Operator.DepSet.empty in
  let in_e = Operator.DepSet.empty in
  let toks_d = Array.make (NamedDecls.size toks) Operator.DepSet.empty in
  let out = Array.make (Array.length vars) Operator.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Operator.ALG i) y) (in_t,in_e,toks_d,out) vars
