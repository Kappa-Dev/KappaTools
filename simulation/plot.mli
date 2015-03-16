val create : string -> unit
val value : int (*width*) -> string
val close : Format.formatter -> Mods.Counter.t -> unit

val plot_now : Environment.t -> Mods.Counter.t -> ?time:float -> State.t -> unit

(** Warning: This function is also in charge of the progressBar *)
val fill : Format.formatter -> State.t -> Mods.Counter.t ->
	   Environment.t -> float -> unit
