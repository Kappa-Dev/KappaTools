(** Time-stamp: <Jul 02 2016>*)

type exceptions_caught_and_uncaught = Exception_without_parameter.exceptions_caught_and_uncaught

val empty_exceptions_caught_and_uncaught : exceptions_caught_and_uncaught
val is_empty_exceptions_caught_and_uncaught : exceptions_caught_and_uncaught -> bool

val warn_with_exn :
  Remanent_parameters_sig.parameters ->
  exceptions_caught_and_uncaught ->
  ?to_ui:bool ->
  string * int * int * int ->
  ?message:string ->
  ?pos:Loc.t option ->
  exn ->
  (unit -> 'a) ->
  exceptions_caught_and_uncaught * 'a

val warn :
  Remanent_parameters_sig.parameters ->
  exceptions_caught_and_uncaught ->
  ?to_ui:bool ->
  string * int * int * int ->
  ?message:string ->
  ?pos:Loc.t ->
  exn ->
  'a ->
  exceptions_caught_and_uncaught * 'a

val print : Remanent_parameters_sig.parameters -> exceptions_caught_and_uncaught -> unit

val print_errors_light_for_kasim :
  Remanent_parameters_sig.parameters -> exceptions_caught_and_uncaught -> unit

val print_for_KaSim :
  Remanent_parameters_sig.parameters -> exceptions_caught_and_uncaught -> unit

val wrap :
  Remanent_parameters_sig.parameters ->
  exceptions_caught_and_uncaught ->
  string ->
  string option ->
  exn ->
  exceptions_caught_and_uncaught

val check_point :
  (Remanent_parameters_sig.parameters ->
  exceptions_caught_and_uncaught ->
  ?to_ui:bool ->
  'a ->
  ?message:string ->
  ?pos:Loc.t ->
  exn ->
  unit ->
  exceptions_caught_and_uncaught * unit) ->
  Remanent_parameters_sig.parameters ->
  exceptions_caught_and_uncaught ->
  exceptions_caught_and_uncaught ->
  'a ->
  ?to_ui:bool ->
  ?message:string ->
  ?pos:Loc.t ->
  exn ->
  exceptions_caught_and_uncaught
