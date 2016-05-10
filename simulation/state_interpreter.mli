(**Event loop module*)

type t (** Abstract state *)

val empty : Environment.t -> (Nbr.t * int) list -> bool -> t
(** [empty env stopping_times relative_fluxmaps] *)

val reinit : (int * Alg_expr.t) list -> t -> t

val initialize :
  Environment.t -> Connected_component.Env.t -> Counter.t ->
  Rule_interpreter.t -> t ->
  (Alg_expr.t * Primitives.elementary_rule * Location.t) list ->
  Rule_interpreter.t * t
(** [initial env domain counter graph state] builds up the initial state *)

val observables_values :
  Environment.t -> Counter.t -> Rule_interpreter.t ->
  t -> Nbr.t array
(** Returns (the current biological time, an array of the current
values of observables) *)

val activity : t -> float
(** Returns the current activity *)

val a_loop :
  outputs:(Data.t -> unit) -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> (bool * Rule_interpreter.t * t)
(** One event loop *)

val end_of_simulation :
  outputs:(Data.t -> unit) ->
  called_from:Remanent_parameters_sig.called_from -> Format.formatter ->
  Environment.t -> Counter.t -> Rule_interpreter.t -> t -> unit
(** What to do after stopping simulation *)

val loop :
  outputs:(Data.t -> unit) ->
  called_from:Remanent_parameters_sig.called_from ->
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> unit
(** [loop message_formatter env domain counter graph] does one event
loop *)
