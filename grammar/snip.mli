val connected_components_sum_of_ambiguous_mixture :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?rule_id:int -> Ast.mixture ->
  Connected_component.Env.t * Connected_component.t array list

val connected_components_sum_of_ambiguous_rule :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> ?rule_id:int -> Ast.mixture -> Ast.mixture ->
  (Connected_component.Env.t * int option) *
    (Connected_component.t array *
       (Primitives.Instantiation.abstract Primitives.Instantiation.test list *
	  Primitives.Instantiation.abstract Primitives.Instantiation.action list) *
       (Primitives.Transformation.t list * Primitives.Transformation.t list))
      list
