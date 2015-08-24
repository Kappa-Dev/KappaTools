(** Kappa pattern compiler *)

val connected_components_sum_of_ambiguous_mixture :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?origin:Term.rev_dep -> Ast.mixture ->
  Connected_component.Env.t *
    (Connected_component.t array *
       Primitives.Instantiation.abstract Primitives.Instantiation.test list)
      list

val connected_components_sum_of_ambiguous_rule :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?origin:Term.rev_dep -> Ast.mixture ->
  Ast.mixture ->
  (Connected_component.Env.t * Term.rev_dep option) *
    (Term.rev_dep option * Connected_component.t array *
       (Primitives.Instantiation.abstract Primitives.Instantiation.event) *
	 (Primitives.Transformation.t list * Primitives.Transformation.t list))
      list
