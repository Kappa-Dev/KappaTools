type t =
    BIN_ALG_OP of Term.bin_alg_op * t Term.with_pos * t Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * t Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t

let rec add_dep (in_t,in_e,out as x) d = function
  | BIN_ALG_OP (_, a, b), _ -> add_dep (add_dep x d a) d b
  | UN_ALG_OP (_, a), _ -> add_dep x d a
  | ALG_VAR j, _ ->
     let () = out.(j) <- Term.DepSet.add d out.(j) in
     x
  | (KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ -> x
  | STATE_ALG_OP op, _ ->
     match op with
     | Term.TIME_VAR -> (Term.DepSet.add d in_t,in_e,out)
     | (Term.CPUTIME | Term.EVENT_VAR | Term.NULL_EVENT_VAR |
	Term.PROD_EVENT_VAR) -> (in_t,Term.DepSet.add d in_e,out)

let setup_alg_vars_rev_dep vars =
  let in_t = Term.DepSet.empty in
  let in_e = Term.DepSet.empty in
  let out = Array.make (Array.length vars) Term.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Term.ALG i) y) (in_t,in_e,out) vars
