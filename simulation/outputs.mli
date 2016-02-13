(** Flux map *)
type flux_data

val create_flux : Environment.t -> Counter.t -> string -> flux_data

val incr_flux_flux : int -> int -> float -> flux_data -> unit
(** [incr_flux_flux of_rule on_rule val flux] *)

val incr_flux_hit : int -> flux_data -> unit

val get_flux_name : flux_data -> string
val flux_has_name : string -> flux_data -> bool

val output_flux : Environment.t -> Counter.t -> flux_data -> unit
