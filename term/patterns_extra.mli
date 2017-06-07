val raw_mixture_to_species:
  ?fmt:Format.formatter ->
  ?sigs:Signature.s ->
  Pattern.PreEnv.t ->
  Raw_mixture.t ->
  (int * int) list ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val mixture_to_pattern:
  ?fmt:Format.formatter ->
  ?sigs:Signature.s ->
  Pattern.PreEnv.t ->
  LKappa.rule_mixture ->
  (int * int) list ->
  Pattern.PreEnv.t * Pattern.cc * Pattern.id

val species_to_raw_mixture:
  ?fmt:Format.formatter ->
  sigs:Signature.s ->
  Pattern.cc -> (Raw_mixture.t * (int * int) list) option

val pattern_to_mixture:
  ?fmt:Format.formatter ->
  sigs:Signature.s ->
  Pattern.cc -> (LKappa.rule_mixture * (int * int) list) option

val pattern_id_to_mixture:
  ?fmt:Format.formatter ->
  Model.t ->
  Pattern.id ->
  (LKappa.rule_mixture * (int * int) list) option

val pattern_id_to_cc:
  Model.t ->
  Pattern.id ->
  Pattern.cc option

val raw_mixture_to_lkappa_rule: Raw_mixture.t -> LKappa.rule

val species_to_lkappa_rule:
  ?fmt:Format.formatter ->
  sigs:Signature.s -> Pattern.t -> LKappa.rule

val species_to_lkappa_rule_and_unspec:
    ?fmt:Format.formatter ->
    sigs:Signature.s -> Pattern.t -> LKappa.rule * (int * int) list

val pattern_to_lkappa_rule :
  ?fmt:Format.formatter ->
  sigs:Signature.s -> Pattern.cc -> LKappa.rule

val pattern_to_lkappa_rule_and_unspec :
    ?fmt:Format.formatter ->
    sigs:Signature.s -> Pattern.cc -> LKappa.rule * (int * int) list

val pattern_id_to_lkappa_rule :
  ?fmt:Format.formatter ->
  Model.t -> Pattern.id -> LKappa.rule

val pattern_id_to_lkappa_rule_and_unspec :
    ?fmt:Format.formatter ->
    Model.t -> Pattern.id -> LKappa.rule * (int * int) list

val copy_agent_in_raw_mixture:
  Raw_mixture.agent -> Raw_mixture.agent
val copy_agent_in_lkappa:
  LKappa.rule_agent -> LKappa.rule_agent
