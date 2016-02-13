(**Event loop module*)

type t (** Abstract state *)

val initial : Environment.t -> Counter.t -> Rule_interpreter.t ->
	      (Nbr.t * int) list -> Rule_interpreter.t * t
(** [initial env c graph stopping_times] builds up the initial state *)

val observables_values :
  Environment.t -> Counter.t -> Rule_interpreter.t ->
  t -> Nbr.t array
(** Returns (the current biological time, an array of the current
values of observables) *)

val activity : t -> float
(** Returns the current activity *)

val loop_cps :
  outputs:(Data.t -> unit) ->
  Format.formatter -> ((unit -> 'a) -> 'a) ->
  (Rule_interpreter.t -> t -> 'a) ->
  Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> 'a
(**Event loop for javascript*)

val loop :
  outputs:(Data.t -> unit) ->
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> unit
(** [loop message_formatter env domain counter graph] does one event
loop *)
