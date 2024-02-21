val compute_static :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Exception.method_handler * Counters_domain_type.static

val convert_view :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.Site_map_and_set.Set.t
  Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t
  Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t ->
  Ckappa_sig.c_agent_name ->
  Cckappa_sig.agent option ->
  Exception.method_handler
  * ((Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.key
     * Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.key)
    * (Occu1.trans * int) list)
    list
