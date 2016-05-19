(** Main entry to the story machinery *)

type secret_log_info

(** {6 Build} *)

val init_secret_log_info : unit -> secret_log_info

(** {6 Use} *)

val compress_and_print :
  called_from:Remanent_parameters_sig.called_from ->
  dotFormat:bool ->
  ?js_interface:Cflow_js_interface.cflow_state ref ->
  none:bool -> weak:bool -> strong:bool ->
  Environment.t -> secret_log_info -> Trace.t -> unit
