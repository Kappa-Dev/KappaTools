(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**Event loop module*)

type t (** Abstract state *)

val empty :
  with_delta_activities:bool -> Model.t -> (Nbr.t option * Nbr.t * int) list ->
  t
(** [empty ~with_delta_activities env stopping_times] *)

val initialize :
  bind:('a -> (bool * Rule_interpreter.t * t -> 'a) -> 'a) ->
  return:(bool * Rule_interpreter.t * t -> 'a) -> outputs:(Data.t -> unit) ->
  Model.t -> Counter.t -> Rule_interpreter.t -> t ->
  (Alg_expr.t * Primitives.elementary_rule * Locality.t) list ->
  'a
(** [initial env counter graph state] builds up the initial state *)

val observables_values :
  Model.t -> Rule_interpreter.t -> Counter.t -> Nbr.t array
(** Returns (the current biological time, an array of the current
values of observables) *)

val do_modifications :
  outputs:(Data.t -> unit) -> Model.t -> Counter.t ->
  Rule_interpreter.t -> t -> Primitives.modification list ->
  (bool * Rule_interpreter.t * t)

val a_loop :
  outputs:(Data.t -> unit) -> dumpIfDeadlocked:bool ->
  maxConsecutiveClash:int -> Model.t -> Counter.t ->
  Rule_interpreter.t -> t -> (bool * Rule_interpreter.t * t)
(** One event loop *)

val end_of_simulation :
  outputs:(Data.t -> unit) -> Format.formatter ->
  Model.t -> Counter.t -> Rule_interpreter.t -> t -> unit
(** What to do after stopping simulation. *)
