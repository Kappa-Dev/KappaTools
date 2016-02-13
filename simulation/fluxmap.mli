(** Flux map *)
val create_flux : Environment.t -> Counter.t -> string -> Data.flux_data
val stop_flux :  Environment.t -> Counter.t -> Data.flux_data -> Data.flux_map

val incr_flux_flux : int -> int -> float -> Data.flux_data -> unit
(** [incr_flux_flux of_rule on_rule val flux] *)

val incr_flux_hit : int -> Data.flux_data -> unit

val get_flux_name : Data.flux_data -> string
val flux_has_name : string -> Data.flux_data -> bool
