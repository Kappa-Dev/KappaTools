type t =
    BIN_ALG_OP of Term.bin_alg_op * t Location.annot * t Location.annot
  | UN_ALG_OP of Term.un_alg_op * t Location.annot
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t

(** depend in time, depend in event number, depend in given var *)
val add_dep :
  (Term.DepSet.t * Term.DepSet.t * Term.DepSet.t array) -> Term.rev_dep ->
  t Location.annot -> (Term.DepSet.t * Term.DepSet.t * Term.DepSet.t array)
val setup_alg_vars_rev_dep :
  (string Location.annot * t Location.annot) array ->
  (Term.DepSet.t * Term.DepSet.t * Term.DepSet.t array)
