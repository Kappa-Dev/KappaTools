type ('mix,'id) e =
    BIN_ALG_OP of Operator.bin_alg_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mix,'id) e Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t

type t = (Connected_component.cc array list, int) e

type ('mix,'id) bool_expr =
  | TRUE
  | FALSE
  | BOOL_OP of
      Operator.bool_op *
      ('mix,'id) bool_expr Location.annot * ('mix,'id) bool_expr Location.annot
  | COMPARE_OP of Operator.compare_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot

val to_json :
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a,'b) e -> Yojson.Basic.json
val of_json :
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> ('a,'b) e

val print :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) e -> unit

val bool_to_json :
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a,'b) bool_expr -> Yojson.Basic.json
val bool_of_json :
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> ('a,'b) bool_expr

val print_bool :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a,'b) bool_expr -> unit

(** depend in time, depend in event number, depend in given var *)
val add_dep :
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array) ->
  Operator.rev_dep ->
  ('a,int) e Location.annot ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)
val setup_alg_vars_rev_dep :
  unit NamedDecls.t -> (string Location.annot * ('a,int) e Location.annot) array ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)

val extract_connected_components : ('a,'b) e Location.annot -> 'a list

val propagate_constant :
  int list -> Counter.t ->
  (string Location.annot * ('a,int) e Location.annot) array ->
  ('a,int) e Location.annot -> ('a,int) e Location.annot
val propagate_constant_bool :
  int list -> Counter.t ->
  (string Location.annot * ('a,int) e Location.annot) array ->
  ('a,int) bool_expr Location.annot -> ('a,int) bool_expr Location.annot

val stops_of_bool_expr :
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  ('a,int) bool_expr -> Nbr.t list
