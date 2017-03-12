val normalize:
  LKappa_auto.cache ->
  int Symmetries_sig.site_partition array ->
  Raw_mixture.t ->
  LKappa_auto.cache * Raw_mixture.t

val check_symmetries_of_internal_states:
  int Symmetries_sig.site_partition array ->
  Raw_mixture.t ->
  bool
