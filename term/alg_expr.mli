type t =
    BIN_ALG_OP of Operator.bin_alg_op * t Location.annot * t Location.annot
  | UN_ALG_OP of Operator.un_alg_op * t Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t

(** depend in time, depend in event number, depend in given var *)
val add_dep :
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array) ->
  Operator.rev_dep ->
  t Location.annot ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)
val setup_alg_vars_rev_dep :
  unit NamedDecls.t -> (string Location.annot * t Location.annot) array ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)

val propagate_constant :
  int list -> Counter.t -> (string Location.annot * t Location.annot) array ->
  t Location.annot -> t Location.annot
val propagate_constant_bool :
  int list -> Counter.t -> (string Location.annot * t Location.annot) array ->
  t Ast.bool_expr Location.annot -> t Ast.bool_expr Location.annot

val stops_of_bool_expr :
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  t Ast.bool_expr -> Nbr.t list
