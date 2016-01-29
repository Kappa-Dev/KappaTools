(** Kappa pattern compiler *)

val connected_components_sum_of_ambiguous_mixture :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?origin:Operator.rev_dep ->
  LKappa.rule_mixture ->
  Connected_component.Env.t *
    (Connected_component.t array *
       Instantiation.abstract Instantiation.test list)
      list

val connected_components_sum_of_ambiguous_rule :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?origin:Operator.rev_dep ->
  LKappa.rule_mixture -> Raw_mixture.t ->
  (Operator.rev_dep option * Connected_component.t array *
     (Instantiation.abstract Instantiation.event) *
       (Instantiation.abstract Primitives.Transformation.t list *
	  Instantiation.abstract Primitives.Transformation.t list))
    list *   (Connected_component.Env.t * Operator.rev_dep option)
