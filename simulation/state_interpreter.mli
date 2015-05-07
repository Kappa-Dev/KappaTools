type t

val initial : Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
	      (Nbr.t * int) list -> t

val loop :
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Mods.Counter.t -> Rule_interpreter.t -> t -> unit
