(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Utilities to make mixtures from traces *)

type state = {
  graph: Edges.t;
  time: float;
  event: int;
  connected_components: Agent.SetMap.Set.t Mods.IntMap.t option;
}

type summary = { unary_distances: (int * int) option }

val init_state : with_connected_components:bool -> state

val do_step : Signature.s -> state -> Trace.step -> state * summary
(** @return the new state and, if the step was an unary instance of a
    binary rule, the id of the rule and the distance between its 2
    connected patterns. *)

val is_step_triggerable : state -> Trace.step -> bool
(** determines whether or not a step can be applied
    from a given state. *)

val is_step_triggerable_on_edges : Edges.t -> Trace.step -> bool
(** same function but takes a graph of type Edges.t directly. *)

val tests_pass_on :
  Edges.t -> Instantiation.concrete Instantiation.test list list -> bool
(** exported for convenience. *)

val cc_of_state :
  debugMode:bool ->
  state ->
  Pattern.PreEnv.t ->
  Pattern.PreEnv.t * ((int * int) list * Pattern.cc * Pattern.id) list
