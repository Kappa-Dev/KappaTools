val print_internal_pattern :
  ?logger:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  Remanent_state.internal_constraints_list -> Exception.method_handler

val site_graph_to_list :
  Exception.method_handler ->
  (string *
   (string option *
    Ckappa_backend.Ckappa_backend.binding_state option *
    (int option * int option) option)
     Wrapped_modules.LoggedStringMap.t)
    Ckappa_sig.Agent_id_map_and_set.Map.t ->
  Exception.method_handler *
  (string *
   (string option *
    Ckappa_backend.Ckappa_backend.binding_state option *
    (int option * int option) option)
     Wrapped_modules.LoggedStringMap.t) list

val site_graph_list_to_list :
  Exception.method_handler ->
  Ckappa_backend.Ckappa_backend.t list ->
  Exception.method_handler *
  (string *
   (string option * Ckappa_backend.Ckappa_backend.binding_state option * (int option * int option) option)
     Wrapped_modules.LoggedStringMap.t)
    list list

val internal_pair_list_to_list :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  Ckappa_backend.Ckappa_backend.t ->
  Ckappa_backend.Ckappa_backend.agent_id ->
  Ckappa_sig.c_site_name ->
  Ckappa_backend.Ckappa_backend.agent_id ->
  Ckappa_sig.c_site_name ->
  (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list list ->
  Exception.method_handler *
  Ckappa_backend.Ckappa_backend.t list
