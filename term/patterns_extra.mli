val raw_mixture_to_species:
  ?parameters:Remanent_parameters_sig.parameters ->
  ?signature:Signature.s ->
  Pattern.PreEnv.t ->
  Raw_mixture.t ->
  (int * int) list ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val species_to_raw_mixture:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s ->
  Pattern.cc -> (Raw_mixture.t * (int * int) list) option

val pattern_to_mixture:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s ->
    Pattern.cc -> LKappa.rule_mixture option

val pattern_id_to_mixture:
  ?parameters:Remanent_parameters_sig.parameters ->
  Model.t ->
  Signature.s ->
  Pattern.id ->
  LKappa.rule_mixture option

val pattern_id_to_cc:
  Model.t ->
  Pattern.id ->
  Pattern.cc option

val raw_mixture_to_lkappa_rule: Raw_mixture.t -> LKappa.rule

val species_to_lkappa_rule:
  Remanent_parameters_sig.parameters ->
  Model.t -> Pattern.t -> LKappa.rule

val pattern_to_lkappa_rule :
  Remanent_parameters_sig.parameters ->
  Model.t -> Pattern.cc -> LKappa.rule

val pattern_id_to_lkappa_rule :
  Remanent_parameters_sig.parameters ->
  Model.t -> Pattern.id -> LKappa.rule

val copy_agent_in_raw_mixture:
  Raw_mixture.agent -> Raw_mixture.agent
val copy_agent_in_lkappa:
  LKappa.rule_agent -> LKappa.rule_agent
