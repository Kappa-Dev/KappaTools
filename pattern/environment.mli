(** Compiled representation of a full Kappa model *)

type t

val init :
  Signature.s -> unit NamedDecls.t -> Alg_expr.t Term.with_pos NamedDecls.t
  -> (Term.DepSet.t * Term.DepSet.t * Term.DepSet.t array)
  -> Primitives.elementary_rule NamedDecls.t -> Alg_expr.t Term.with_pos array
  -> Primitives.perturbation array -> t
(** [init sigs contact_map tokens algs rules obs perts] *)

val nb_tokens : t -> int
val nb_algs : t -> int
val nb_rules : t -> int
val nb_perturbations : t -> int
val signatures : t -> Signature.s

val get_alg : t -> int -> Alg_expr.t
val get_perturbation : t -> int -> Primitives.perturbation
val get_rule : t -> int -> Primitives.elementary_rule
val map_observables : (Alg_expr.t -> 'a) -> t -> 'a array
val iteri_rules : (int -> Primitives.elementary_rule -> unit) -> t -> unit

val get_reverse_dependencies : t -> int -> Term.DepSet.t
val get_always_outdated : t -> Term.DepSet.t

val num_of_agent : string Term.with_pos -> t -> int
val num_of_rule : string Term.with_pos -> t -> int
val num_of_alg : string Term.with_pos -> t -> int
val num_of_token : string Term.with_pos -> t -> int

val print_agent : ?env:t -> Format.formatter -> int -> unit
val print_rule : ?env:t -> Format.formatter -> int -> unit
val print_alg : ?env:t -> Format.formatter -> int -> unit
val print_token : ?env:t -> Format.formatter -> int -> unit

val get_desc : string -> t -> Format.formatter
val close_desc : t -> unit

val print :
  (t -> Format.formatter -> Alg_expr.t -> unit) ->
  (t -> Format.formatter -> Primitives.elementary_rule -> unit) ->
  (t -> Format.formatter -> Primitives.perturbation -> unit) ->
  Format.formatter -> t -> unit
