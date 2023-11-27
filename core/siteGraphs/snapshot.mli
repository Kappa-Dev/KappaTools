(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cc_site = {
  site_link: (int * int) option;  (** (node_id, site_id) *)
  site_state: int option;
}

type cc_node = {
  node_type: int;
  node_id_in_witness: int;
  node_sites: cc_site array;
}

type connected_component = cc_node array
type t

val cc_to_user_cc :
  debugMode:bool ->
  raw:bool ->
  Signature.s ->
  connected_component ->
  User_graph.connected_component

val empty : t

val increment_in_snapshot :
  raw:bool -> Signature.s -> connected_component -> t -> t

val export :
  debugMode:bool ->
  raw:bool ->
  Signature.s ->
  t ->
  (int * User_graph.connected_component) list

val fold : ('a -> int -> connected_component -> 'a) -> 'a -> t -> 'a
