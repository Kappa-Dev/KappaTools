val raw_mixture_to_species :
  ?parameters:Remanent_parameters_sig.parameters ->
  ?sigs:Signature.s ->
  Pattern.PreEnv.t ->
  Raw_mixture.t ->
  (int * int) list ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val mixture_to_pattern :
  ?parameters:Remanent_parameters_sig.parameters ->
  ?sigs:Signature.s ->
  Pattern.PreEnv.t ->
  LKappa.rule_mixture ->
  (int * int) list ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val species_to_raw_mixture :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  Pattern.cc ->
  (Raw_mixture.t * (int * int) list) option

val pattern_to_mixture :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  Pattern.cc ->
  (LKappa.rule_mixture * (int * int) list) option

val pattern_id_to_mixture :
  ?parameters:Remanent_parameters_sig.parameters ->
  Model.t ->
  Pattern.id ->
  (LKappa.rule_mixture * (int * int) list) option

val pattern_id_to_cc : Model.t -> Pattern.id -> Pattern.cc option
val raw_mixture_to_lkappa_rule : Raw_mixture.t -> LKappa.rule

val species_to_lkappa_rule :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  Pattern.t ->
  LKappa.rule

val species_to_lkappa_rule_and_unspec :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  Pattern.t ->
  LKappa.rule * (int * int) list

val pattern_to_lkappa_rule :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  Pattern.cc ->
  LKappa.rule

val pattern_id_to_lkappa_rule :
  ?parameters:Remanent_parameters_sig.parameters ->
  Model.t ->
  Pattern.id ->
  LKappa.rule

val pattern_id_to_lkappa_rule_and_unspec :
  ?parameters:Remanent_parameters_sig.parameters ->
  Model.t ->
  Pattern.id ->
  LKappa.rule * (int * int) list
