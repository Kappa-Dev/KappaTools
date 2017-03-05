val raw_mixture_to_pattern:
  Pattern.PreEnv.t ->
  Raw_mixture.t ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val pattern_to_raw_mixture:
  Signature.s -> Pattern.cc -> Raw_mixture.t option

(*val pattern_id_to_raw_mixture:
  Pattern.PreEnv.t ->
  Signature.s ->
  Pattern.id ->
  Raw_mixture.t option*)
