type t

val initial : Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
	      (Nbr.t * int) list -> t
