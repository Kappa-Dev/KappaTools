val local_trace : bool
val trace : bool

val gexf_of_contact_map :
  ?loggers:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val dot_of_contact_map :
  ?loggers:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val print_handler :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val string_of_site :
  'a -> (string, string, string) Ckappa_sig.site_type -> string
