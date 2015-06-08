(**Event loop module*)

type t

(** [initial env c graph stopping_times]*)
val initial : Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
	      (Nbr.t * int) list -> t

val observables_values :
  Environment.t -> Mods.Counter.t -> Rule_interpreter.t ->
  t -> float * Nbr.t array

val activity : t -> float

(**Event loop for javascript*)
val loop_cps :
  Format.formatter -> ((unit -> 'a) -> 'a) ->
  (Format.formatter -> Mods.Counter.t -> 'a) ->
  Environment.t -> Connected_component.Env.t ->
  Mods.Counter.t -> Rule_interpreter.t -> t -> 'a

(**Event loop*)
val loop :
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Mods.Counter.t -> Rule_interpreter.t -> t -> unit
