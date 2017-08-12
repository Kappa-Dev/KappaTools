type distance
type blackboard
val init_blackboard: int -> int -> blackboard
val explore_influence_map:
  ?fwd:int -> ?bwd:int-> total:int ->
  Remanent_parameters_sig.parameters -> Exception.method_handler ->
  blackboard ->
  int ->
  Remanent_state.bidirectional_influence_map ->
  Exception.method_handler *
  Remanent_state.internal_influence_map *
  blackboard
