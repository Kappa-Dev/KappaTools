(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Pathes to explore a mixture *)

type abstract = Existing of int | Fresh of Agent.t
type 'a port = 'a * int
type 'a arrow = ToNode of 'a port | ToNothing | ToInternal of int
type 'a step = 'a port * 'a arrow
type 'a t = 'a step list

val print :
  Signature.s -> (int -> int) -> Format.formatter -> abstract t -> unit
(** [print signatures find_existing_type nav] *)

val step_to_yojson : abstract step -> Yojson.Basic.t
val step_of_yojson : Yojson.Basic.t -> abstract step
val to_yojson : abstract t -> Yojson.Basic.t
val of_yojson : Yojson.Basic.t -> abstract t

val rename :
  debugMode:bool -> Renaming.t -> abstract t -> Renaming.t * abstract t

val compatible_fresh_point :
  debugMode:bool ->
  abstract step ->
  Agent.t ->
  int ->
  abstract arrow ->
  Renaming.t option
(** Retuns the extension of the given injections so that the second edge
    is the image of the first *)

val is_subnavigation :
  debugMode:bool ->
  Renaming.t ->
  abstract t ->
  abstract t ->
  (Renaming.t * abstract t) option
(** [is_subnavigation inj_nav2sub nav subpart] *)

val check_edge : Edges.t -> abstract step -> bool

val injection_for_one_more_edge :
  debugMode:bool ->
  ?root:Agent.t ->
  Renaming.t ->
  Edges.t ->
  abstract step ->
  Renaming.t option

val imperative_edge_is_valid :
  debugMode:bool ->
  ?root:Agent.t ->
  Renaming.t ->
  Edges.t ->
  abstract step ->
  bool

val concretize :
  debugMode:bool -> Agent.t -> Edges.t -> abstract t -> int t option
