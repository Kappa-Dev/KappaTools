type t

val initial : Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
	      (Nbr.t * int) list -> t

val observables_values :
  Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
  t -> float * Nbr.t array

val activity : t -> float

val loop :
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Mods.Counter.t -> Rule_interpreter.t -> t -> unit
