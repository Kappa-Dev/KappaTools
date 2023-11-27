val local_trace : bool
val empty_pos : string * int * int

val empty_agent :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler
  * 'a Int_storage.Quick_Nearly_inf_Imperatif.t Cckappa_sig.proper_agent

val empty_mixture :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler * Cckappa_sig.mixture

val empty_rule :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler * Cckappa_sig.rule

val empty_e_rule :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler * Cckappa_sig.enriched_rule

val init_contact_map : 'a Ckappa_sig.Agent_map_and_set.Map.t

val declare_agent :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Quark_type.agent_quark ->
  'a Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler
  * 'a Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_map_and_set.Map.t

val declare_site :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  ('a list * 'b list) Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler
  * ('a list * 'b list) Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_map_and_set.Map.t

val add_internal_state_in_contact_map :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Quark_type.agent_quark * Ckappa_sig.c_site_name ->
  'a ->
  ('a list * 'b list) Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler
  * ('a list * 'b list) Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_map_and_set.Map.t

val add_link_in_contact_map :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Quark_type.agent_quark * Ckappa_sig.c_site_name ->
  'a * 'b ->
  ('c list * ('a * 'b) list) Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler
  * ('c list * ('a * 'b) list) Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_map_and_set.Map.t

val dot_of_contact_map :
  ?logger:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Ckappa_sig.PairAgentSite_map_and_set.elt list list Public_data.AccuracyMap.t
  Public_data.AccuracyMap.t ->
  ('a * (Quark_type.agent_quark * Ckappa_sig.c_site_name) list)
  Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler

val gexf_of_contact_map :
  ?logger:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  'a ->
  ('b * (Quark_type.agent_quark * Ckappa_sig.c_site_name) list)
  Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t ->
  Exception_without_parameter.method_handler

val export_contact_map :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler
  * (Ckappa_sig.c_state list
    * (Quark_type.agent_quark * Ckappa_sig.c_site_name) list)
    Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_map_and_set.Map.t

val translate_c_compil :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  ( Ckappa_sig.agent,
    Ckappa_sig.mixture,
    Ckappa_sig.mixture,
    string,
    Ckappa_sig.mixture Ckappa_sig.rule )
  Ast.compil ->
  Exception_without_parameter.method_handler
  * Cckappa_sig.kappa_handler
  * Cckappa_sig.compil

val translate_pert :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  (Ckappa_sig.mixture, string) Alg_expr.e * Locality.t ->
  Ckappa_sig.mixture * 'a ->
  Exception_without_parameter.method_handler * Cckappa_sig.enriched_init

val rename_rule_lhs :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Ckappa_sig.c_agent_id ->
  Cckappa_sig.rule ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_agent_id

val rename_rule_rhs :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Ckappa_sig.c_agent_id ->
  Cckappa_sig.rule ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_agent_id

val lift_forbidding_question_marks :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler ->
  Ckappa_sig.mixture ->
  Exception_without_parameter.method_handler * Cckappa_sig.mixture
