(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Concrete graph implementation *)

type t

val empty : with_connected_components:bool -> t

val copy : t -> t
(** You'd better NOT use that on the state of a simulation *)

type stats = { nb_agents: int }

val stats : t -> stats

val add_agent : ?id:int -> Signature.s -> int -> t -> int * t
(** [add_agent ?id sigs agent_type graph] *)

val add_free : int -> int -> t -> t
(** [add_free agent_id site graph] *)

val add_internal : int -> int -> int -> t -> t
(** [add_internal agent_id site internal_state graph] *)

val add_link : Agent.t -> int -> Agent.t -> int -> t -> t * (int * int) option
(** [add_link ag1 s1 ag2 s2 t]
 Some (i,j) as second returned element means cc j is now merged into cc i *)

val remove_agent : int -> t -> t
val remove_free : int -> int -> t -> t
val remove_internal : int -> int -> t -> int * t

val remove_link : int -> int -> int -> int -> t -> t * (int * int) option
(** Some (i,j) as second returned element means separate "new" cc j from cc i *)

val is_agent : Agent.t -> t -> bool
(** [is_agent agent graph] *)

val is_agent_id : int -> t -> bool
(** [is_agent_id agent_id graph] *)

val is_free : int -> int -> t -> bool
(** [is_free agent_id site graph] *)

val is_internal : int -> int -> int -> t -> bool
(** [is_internal internal_state agent_id site graph] *)

val link_exists : int -> int -> int -> int -> t -> bool
(** [link_exists ag1_id site1 ag2_id site2 graph] *)

val exists_fresh : int -> int -> int -> int -> t -> int option
(** [exists_fresh ag1 site1 type_of_ag2 site2 graph] *)

val link_destination : int -> int -> t -> (Agent.t * int) option
(** [link_destination ag site graph] *)

val get_internal : int -> int -> t -> int
(** [get_internal ag site graph] *)

val get_sites : int -> t -> int
val get_sort : int -> t -> int
val get_connected_component : int -> t -> int option
val in_same_connected_component : int -> int -> t -> bool

val iter_neighbors : (Agent.t -> unit) -> int -> t -> unit
(** [iter_neighbors f ag graph] calls function [f] on all direct
    neighbors of agent [ag] in [graph]. *)

val all_agents_where : (Agent.t -> bool) -> t -> IntCollection.t

type path = ((Agent.t * int) * (Agent.t * int)) list

val empty_path : path
val singleton_path : Agent.t -> int -> Agent.t -> int -> path
val rev_path : path -> path
val print_path : ?sigs:Signature.s -> Format.formatter -> path -> unit
val is_valid_path : path -> t -> bool

val are_connected :
  ?max_distance:int -> t -> Agent.t list -> Agent.t list -> path option
(** [are_connected ?max_distance graph nodes_x nodes_y] *)

val species :
  debugMode:bool -> Signature.s -> int -> t -> User_graph.connected_component

val build_snapshot : raw:bool -> Signature.s -> t -> Snapshot.t

val build_user_snapshot :
  debugMode:bool ->
  raw:bool ->
  Signature.s ->
  t ->
  (int * User_graph.connected_component) list

val debug_print : Format.formatter -> t -> unit
