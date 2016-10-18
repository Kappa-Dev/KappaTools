(**Event loop module*)

type t (** Abstract state *)

val empty : Environment.t -> (Nbr.t * int) list -> (int * Alg_expr.t) list -> t
(** [empty env stopping_times variable_overwrite] *)

val initialize :
  bind:('a -> (Rule_interpreter.t * t -> 'a) -> 'a) ->
  return:(Rule_interpreter.t * t -> 'a) ->
  Environment.t -> Connected_component.Env.t -> Counter.t ->
  Rule_interpreter.t -> t ->
  (Alg_expr.t * Primitives.elementary_rule * Location.t) list ->
  'a
(** [initial env domain counter graph state] builds up the initial state *)

val observables_values :
  Environment.t -> Counter.t -> Rule_interpreter.t ->
  t -> Nbr.t array
(** Returns (the current biological time, an array of the current
values of observables) *)

val do_modification :
  outputs:(Data.t -> unit) -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> Primitives.modification ->
  bool * Rule_interpreter.t * t

val activity : t -> float
(** Returns the current activity *)

val a_loop :
  outputs:(Data.t -> unit) -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> (bool * Rule_interpreter.t * t)
(** One event loop *)

val end_of_simulation :
  outputs:(Data.t -> unit) -> Format.formatter ->
  Environment.t -> Counter.t -> Rule_interpreter.t -> t ->
  (((bool*bool*bool)*bool)*Trace.t) option
(** What to do after stopping simulation. Returns maybe a trace *)

val finalize :
  outputs:(Data.t -> unit) -> called_from:Remanent_parameters_sig.called_from ->
  Ast.formatCflow -> Format.formatter ->
  Environment.t -> Counter.t -> Rule_interpreter.t -> t -> unit

val batch_loop :
  outputs:(Data.t -> unit) -> formatCflows:Ast.formatCflow ->
  Format.formatter -> Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> unit
(** [loop message_formatter env domain counter graph]
 does a simulation in the command-line setting *)

val interactive_loop :
  outputs:(Data.t -> unit) -> Format.formatter ->
  (Connected_component.t array list,int) Alg_expr.bool_expr ->
  Environment.t -> Connected_component.Env.t ->
  Counter.t -> Rule_interpreter.t -> t -> (bool * Rule_interpreter.t * t)
(** [interactive_loop message_formatter env domain counter graph]
 does a simulation in the command-line setting up to an interruption *)
