val print_internal_pattern :
  ?logger:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Remanent_state.internal_constraints_list ->
  Exception.exceptions_caught_and_uncaught

val site_graph_to_list :
  Exception.exceptions_caught_and_uncaught ->
  ((string
   * (string option
     * Site_graphs.KaSa_site_graph.binding_state option
     * (int option * int option) option)
     Wrapped_modules.LoggedStringMap.t)
  * string option Wrapped_modules.LoggedStringMap.t)
  Ckappa_sig.Agent_id_map_and_set.Map.t ->
  Exception.exceptions_caught_and_uncaught
  * (string
    * (string option
      * Site_graphs.KaSa_site_graph.binding_state option
      * (int option * int option) option)
      Wrapped_modules.LoggedStringMap.t)
    list

val site_graph_list_to_list :
  Exception.exceptions_caught_and_uncaught ->
  Site_graphs.KaSa_site_graph.t list ->
  Exception.exceptions_caught_and_uncaught
  * (string
    * (string option
      * Site_graphs.KaSa_site_graph.binding_state option
      * (int option * int option) option)
      Wrapped_modules.LoggedStringMap.t)
    list
    list

val internal_pair_list_to_list :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Site_graphs.KaSa_site_graph.t ->
  Site_graphs.KaSa_site_graph.agent_id ->
  Ckappa_sig.c_site_name ->
  Site_graphs.KaSa_site_graph.agent_id ->
  Ckappa_sig.c_site_name ->
  (Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state) list list ->
  Exception.exceptions_caught_and_uncaught * Site_graphs.KaSa_site_graph.t list
