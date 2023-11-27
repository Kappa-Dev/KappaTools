val local_trace : bool
val trace : bool

val gexf_of_contact_map :
  ?loggers:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler

val dot_of_contact_map :
  ?loggers:Loggers.t ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler

val print_handler :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler

val string_of_site :
  'a -> (string, string, string) Ckappa_sig.site_type -> string
