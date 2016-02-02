(** Flux map *)
type flux

val create_flux : Environment.t -> Counter.t -> string -> flux

val incr_flux_flux : int -> int -> float -> flux -> unit
(** [incr_flux_flux of_rule on_rule val flux] *)

val incr_flux_hit : int -> flux -> unit

val get_flux_name : flux -> string
val flux_has_name : string -> flux -> bool

val output_flux : Environment.t -> Counter.t -> flux -> unit
