

val lkappa_of_raw_mixture : Raw_mixture.t -> LKappa.rule
val lkappa_of_rule_mixture: LKappa.rule_mixture -> LKappa.rule

val normalize:
  LKappa_auto.cache ->
  int Symmetries_sig.site_partition array ->
  Raw_mixture.t ->
  LKappa_auto.cache * Raw_mixture.t

(*val check_symmetries_of_internal_states:
  int Symmetries_sig.site_partition array ->
  Raw_mixture.t ->
  bool*)
