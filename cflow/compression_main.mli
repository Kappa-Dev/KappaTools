(** Main entry to the story machinery *)

type secret_log_info
type secret_parameter

(** {6 Build} *)

val init_secret_log_info : unit -> secret_log_info
val build_parameter :
  called_from:Remanent_parameters_sig.called_from -> none:bool -> weak:bool ->
  strong:bool -> secret_parameter

(** {6 Use} *)
val get_logger : secret_parameter -> Loggers.t

val compress_and_print :
  secret_parameter -> dotFormat:Causal.formatCflow ->
  ?js_interface:Cflow_js_interface.cflow_state ref ->
  Model.t -> secret_log_info -> Trace.t -> unit
