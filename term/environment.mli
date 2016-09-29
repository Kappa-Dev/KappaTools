(** Compiled representation of a full Kappa model *)

type t

val init :
  Signature.s -> unit NamedDecls.t -> Alg_expr.t Location.annot NamedDecls.t ->
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  ((string Location.annot option * LKappa.rule Location.annot) array *
     Primitives.elementary_rule array *
       Connected_component.Set.t) ->
  Alg_expr.t Location.annot array -> Primitives.perturbation array -> t
(** [init sigs tokens algs dependencies (ast_rules,rules) obs perts]
 *)

val nb_tokens : t -> int
val nb_algs : t -> int
val nb_rules : t -> int
val nb_syntactic_rules : t -> int
val nb_perturbations : t -> int
val connected_components_of_unary_rules : t -> Connected_component.Set.t

val signatures : t -> Signature.s
val tokens_finder : t -> int Mods.StringMap.t
val algs_finder : t -> int Mods.StringMap.t

val get_alg : t -> int -> Alg_expr.t
val get_algs :
  t -> (string Location.annot * Alg_expr.t Location.annot) array
val get_perturbation : t -> int -> Primitives.perturbation
val get_rule : t -> int -> Primitives.elementary_rule
val get_ast_rule: t -> int -> LKappa.rule
val map_observables : (Alg_expr.t -> 'a) -> t -> 'a array
val fold_rules :
  (int -> 'a -> Primitives.elementary_rule -> 'a) -> 'a -> t -> 'a
val fold_perturbations :
  (int -> 'a -> Primitives.perturbation -> 'a) -> 'a -> t -> 'a

val get_alg_reverse_dependencies : t -> int -> Operator.DepSet.t
val get_token_reverse_dependencies : t -> int -> Operator.DepSet.t
val get_always_outdated : t -> Operator.DepSet.t
val all_dependencies :
  t -> (Operator.DepSet.t * Operator.DepSet.t *
        Operator.DepSet.t array * Operator.DepSet.t array)

val num_of_agent : string Location.annot -> t -> int
val num_of_alg : string Location.annot -> t -> int
val num_of_token : string Location.annot -> t -> int
val nums_of_rule : string -> t -> int list

val print_ast_rule : ?env:t -> Format.formatter -> int -> unit
val print_rule : ?env:t -> Format.formatter -> int -> unit
val print_agent : ?env:t -> Format.formatter -> int -> unit
val print_alg : ?env:t -> Format.formatter -> int -> unit
val print_token : ?env:t -> Format.formatter -> int -> unit

val print :
  (t -> Format.formatter -> Alg_expr.t -> unit) ->
  (t -> Format.formatter -> Primitives.elementary_rule -> unit) ->
  (t -> Format.formatter -> Primitives.perturbation -> unit) ->
  Format.formatter -> t -> unit

val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t

val check_if_counter_is_filled_enough : Counter.t -> t -> unit

val propagate_constant : int list -> Counter.t -> t -> t
(** [propagate_constant updated_vars counter env] *)
