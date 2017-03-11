val normalize:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s ->
  Pattern.PreEnv.t ->
  ((int list list) * (int list list)) array ->
  Pattern.cc ->
  Pattern.PreEnv.t * Pattern.cc

(* Please pick an appropriate name and correct the type *)
(*val normalize_internal_states_in_raw_mixture_init:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s ->
  Pattern.PreEnv.t ->
  ((int list list) * (int list list)) array ->
  Pattern.cc ->
  Pattern.PreEnv.t * Pattern.cc
*)
