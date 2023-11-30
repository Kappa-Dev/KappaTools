val init_blackboard :
  int -> int -> Remanent_state.local_influence_map_blackboard

val explore_influence_map :
  ?fwd:int ->
  ?bwd:int ->
  total:int ->
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Remanent_state.local_influence_map_blackboard ->
  Ckappa_sig.c_rule_id ->
  Remanent_state.bidirectional_influence_map ->
  Exception.method_handler
  * Remanent_state.internal_influence_map
  * Remanent_state.local_influence_map_blackboard
