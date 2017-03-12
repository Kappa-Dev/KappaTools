val normalize:
  LKappa_auto.cache ->
  ((int list list) * (int list list)) array ->
  Raw_mixture.t ->
  LKappa_auto.cache * Raw_mixture.t

val check_symmetries_of_internal_states:
  ((int list list) * (int list list)) array -> Raw_mixture.t ->
  bool
