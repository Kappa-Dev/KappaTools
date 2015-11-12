val create : string -> unit
val value : int (*width*) -> string
val close : Format.formatter -> Mods.Counter.t -> unit

val plot_now : Environment.t -> float -> Nbr.t array -> unit

(** Warning: This function is also in charge of the progressBar *)
val fill : Format.formatter -> Mods.Counter.t ->
	   Environment.t -> Nbr.t array -> unit
