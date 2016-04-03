(** Main entry to the story machinery *)

type secret_log_info
type secret_step

(** {6 Build} *)

val init_secret_log_info :
  unit -> secret_log_info
val secret_store_event :
  secret_log_info ->
  Causal.event_kind *
    Instantiation.concrete Instantiation.event * unit Mods.simulation_info ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_obs :
  secret_log_info ->
  (Causal.event_kind *
     Instantiation.concrete Instantiation.test list *
     unit Mods.simulation_info) ->
  secret_step list -> secret_log_info * secret_step list

(** {6 Use} *)

val compress_and_print :
  called_from:Remanent_parameters_sig.called_from ->
  Format.formatter -> Environment.t -> secret_log_info ->
  secret_step list -> unit
