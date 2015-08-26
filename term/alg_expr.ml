type t =
    BIN_ALG_OP of Operator.bin_alg_op * t Location.annot * t Location.annot
  | UN_ALG_OP of Operator.un_alg_op * t Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of
      (Connected_component.cc array *
	Instantiation.abstract Instantiation.test list) list
  | TOKEN_ID of int
  | CONST of Nbr.t

let rec add_dep (in_t,in_e,out as x) d = function
  | BIN_ALG_OP (_, a, b), _ -> add_dep (add_dep x d a) d b
  | UN_ALG_OP (_, a), _ -> add_dep x d a
  | ALG_VAR j, _ ->
     let () = out.(j) <- Operator.DepSet.add d out.(j) in
     x
  | (KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ -> x
  | STATE_ALG_OP op, _ ->
     match op with
     | Operator.TIME_VAR -> (Operator.DepSet.add d in_t,in_e,out)
     | (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR |
	Operator.PROD_EVENT_VAR) -> (in_t,Operator.DepSet.add d in_e,out)

let setup_alg_vars_rev_dep vars =
  let in_t = Operator.DepSet.empty in
  let in_e = Operator.DepSet.empty in
  let out = Array.make (Array.length vars) Operator.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Operator.ALG i) y) (in_t,in_e,out) vars
