val normalize_species:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  int Symmetries_sig.site_partition array ->
  Pattern.cc ->
  LKappa_auto.cache * Pattern.PreEnv.t * Pattern.cc

val is_pattern_invariant_internal_states_permutation:
  parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool

val is_pattern_invariant_binding_states_permutation:
  parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool

val is_pattern_invariant_full_states_permutation:
  parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool
