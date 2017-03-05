val normalize_internal_states_in_raw_mixture:
  Signature.s ->
  Pattern.PreEnv.t ->
  ((int list list) * (int list list)) array ->
  Pattern.cc ->
  Pattern.PreEnv.t * Pattern.cc
