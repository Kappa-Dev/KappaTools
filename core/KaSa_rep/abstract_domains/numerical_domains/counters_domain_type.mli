type comparison_op = LTEQ | LT | GT | GTEQ | EQ

type restriction = {
  tests: (Occu1.trans * comparison_op * int) list;
  invertible_assignments: (Occu1.trans * int) list;
  non_invertible_assignments: (Occu1.trans * int) list;
}

val empty_restriction : restriction

type static = {
  counters: Ckappa_sig.AgentSite_map_and_set.Set.t;
  packs:
    Ckappa_sig.Site_map_and_set.Set.t
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
  backward_pointers:
    Ckappa_sig.Site_map_and_set.Set.t
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
  rule_restrictions:
    restriction Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.t;
  rule_creation:
    (Occu1.trans * int) list list
    Ckappa_sig
    .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .t
    Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.t;
}

val print_restriction :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  restriction ->
  Exception.exceptions_caught_and_uncaught

val print_agent_restriction :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  restriction Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.t ->
  Exception.exceptions_caught_and_uncaught

val print_rule_restriction :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  restriction Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.t
  Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.t ->
  Exception.exceptions_caught_and_uncaught

val print_restrictions :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  restriction Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif.t
  Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.t
  Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.t ->
  Exception.exceptions_caught_and_uncaught

val print :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  static ->
  Exception.exceptions_caught_and_uncaught
