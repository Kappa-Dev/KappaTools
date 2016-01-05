type secret_log_info
type secret_step

val init_secret_log_info :
  unit -> secret_log_info
val secret_store_event :
  secret_log_info ->
  Causal.event_kind *
    Instantiation.concrete Instantiation.event ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_obs :
  Format.formatter ->
  secret_log_info ->
  (Causal.event_kind *
     Instantiation.concrete Instantiation.test list *
     unit Mods.simulation_info) ->
  secret_step list -> secret_log_info * secret_step list

val compress_and_print :
  Format.formatter -> Environment.t -> secret_log_info ->
  secret_step list -> unit
