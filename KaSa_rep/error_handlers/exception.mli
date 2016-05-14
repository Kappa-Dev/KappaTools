type method_handler = Exception_without_parameter.method_handler

val empty_error_handler : method_handler
val warn :
  Remanent_parameters_sig.parameters -> method_handler -> string option ->
  string option -> exn -> (unit -> 'a) -> method_handler * 'a
val print : Remanent_parameters_sig.parameters -> method_handler -> unit
val print_errors_light_for_kasim : Remanent_parameters_sig.parameters -> method_handler -> unit
val print_for_KaSim : Remanent_parameters_sig.parameters -> method_handler -> unit

val wrap : Remanent_parameters_sig.parameters ->
	   method_handler -> string -> string option -> exn -> method_handler
val check :
  (Remanent_parameters_sig.parameters -> method_handler -> string option ->
   exn -> unit -> method_handler * unit) ->
  Remanent_parameters_sig.parameters -> method_handler -> method_handler ->
  string option -> exn -> method_handler

