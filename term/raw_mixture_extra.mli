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


(*val pattern_id_to_raw_mixture:
  Pattern.PreEnv.t ->
  Signature.s ->
  Pattern.id ->
  Raw_mixture.t option*)
