type 'a e =
    BIN_ALG_OP of
      Operator.bin_alg_op * 'a e Location.annot * 'a e Location.annot
  | UN_ALG_OP of Operator.un_alg_op * 'a e Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of 'a
  | TOKEN_ID of int
  | CONST of Nbr.t

type t = Connected_component.cc array list e

val to_json : ('a -> Yojson.Basic.json) -> 'a e -> Yojson.Basic.json
val of_json : (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a e

(** depend in time, depend in event number, depend in given var *)
val add_dep :
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array) ->
  Operator.rev_dep ->
  'a e Location.annot ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)
val setup_alg_vars_rev_dep :
  unit NamedDecls.t -> (string Location.annot * 'a e Location.annot) array ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)

val propagate_constant :
  int list -> Counter.t ->
  (string Location.annot * 'a e Location.annot) array ->
  'a e Location.annot -> 'a e Location.annot
val propagate_constant_bool :
  int list -> Counter.t ->
  (string Location.annot * 'a e Location.annot) array ->
  'a e Ast.bool_expr Location.annot -> 'a e Ast.bool_expr Location.annot

val stops_of_bool_expr :
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  'a e Ast.bool_expr -> Nbr.t list
