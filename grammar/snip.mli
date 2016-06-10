(** Kappa pattern compiler *)

val connected_components_sum_of_ambiguous_mixture :
  Export_to_KaSim.Export_to_KaSim.contact_map -> Connected_component.PreEnv.t ->
  ?origin:Operator.rev_dep -> LKappa.rule_mixture ->
  Connected_component.PreEnv.t *
    (Connected_component.t array *
       Instantiation.abstract Instantiation.test list)
      list

val connected_components_sum_of_ambiguous_rule :
  Export_to_KaSim.Export_to_KaSim.contact_map -> Connected_component.PreEnv.t ->
  ?origin:Operator.rev_dep -> LKappa.rule_mixture -> Raw_mixture.t ->
  (Operator.rev_dep option * Connected_component.t array *
     (Instantiation.abstract Instantiation.event) *
       (Instantiation.abstract Primitives.Transformation.t list *
	  Instantiation.abstract Primitives.Transformation.t list))
    list *   (Connected_component.PreEnv.t * Operator.rev_dep option)
