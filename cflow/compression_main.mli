type secret_log_info
type secret_step

val init_secret_log_info :
  unit -> secret_log_info
val secret_store_event :
  secret_log_info ->
  (Primitives.rule * int Mods.IntMap.t * int Mods.IntMap.t) *
    ((int * int Mods.IntMap.t) list * Primitives.rule * int * Mods.Int2Set.t) ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_obs :
  secret_log_info ->
  (int * Mixture.t * int Mods.IntMap.t * unit Mods.simulation_info) ->
  secret_step list -> secret_log_info * secret_step list
val secret_store_init :
  secret_log_info -> State.t -> secret_step list ->
  secret_log_info * secret_step list

val compress :
  Format.formatter -> Environment.t -> State.t -> secret_log_info ->
  secret_step list ->
  (Causal.grid * secret_log_info Mods.simulation_info option list) list option *
    (Causal.grid * secret_log_info Mods.simulation_info option list) list option *
      (Causal.grid * secret_log_info Mods.simulation_info option list) list option
