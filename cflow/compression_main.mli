type secret_log_info
type secret_step

val init_secret_log_info :
  unit -> secret_log_info
val secret_store_event :
  secret_log_info ->
  int * Primitives.Instantiation.concrete Primitives.Instantiation.event ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_obs :
  secret_log_info ->
  (int * Primitives.Instantiation.concrete Primitives.Instantiation.test list *
     unit Mods.simulation_info) ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_init :
  secret_log_info ->
  Primitives.Instantiation.concrete Primitives.Instantiation.action list ->
  secret_step list -> secret_log_info * secret_step list

val compress :
  Format.formatter -> Environment.t -> secret_log_info ->
  secret_step list ->
  (Causal.grid * secret_log_info Mods.simulation_info option list) list option *
    (Causal.grid * secret_log_info Mods.simulation_info option list) list option *
      (Causal.grid * secret_log_info Mods.simulation_info option list) list option
