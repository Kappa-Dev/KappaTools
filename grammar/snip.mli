val connected_components_sum_of_ambiguous_mixture :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> Ast.mixture ->
  Connected_component.Env.t * Connected_component.t array list

val connected_components_sum_of_ambiguous_rule :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t -> Ast.mixture -> Ast.mixture ->
  Connected_component.Env.t *
    (Connected_component.t array * Compilation_info.t *
       (Primitives.Transformation.t list * Primitives.Transformation.t list))
      list
