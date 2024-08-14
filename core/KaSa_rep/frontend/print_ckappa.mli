val local_trace : bool

val print_rule :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.mixture Ckappa_sig.rule ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_bool :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  (Ckappa_sig.mixture, string) Alg_expr.bool Loc.annoted ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_alg :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  (Ckappa_sig.mixture, string) Alg_expr.e ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_binding_state :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.binding_state ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_internal_state :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.internal_state ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_site_name :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.site_name ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_agent_name :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.agent_name ->
  Exception_without_parameter.exceptions_caught_and_uncaught
