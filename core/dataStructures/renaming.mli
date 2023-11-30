(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Functions from a subset of nat to a subset of nat *)

exception Undefined
exception NotBijective
exception Clashing

type t

val dummy : t
val empty : unit -> t
val is_identity : t -> bool
val identity : int list -> t
val image : t -> Mods.IntSet.t

val cyclic_permutation_from_list : stop_at:int -> int list -> t
(** very specific use case for Connected_component.remove_ag_cc *)

val imperative_add : debugMode:bool -> int -> int -> t -> bool
(** @raise Clashing in debug mode
@return if the addition preserves injectivity *)

val add : debugMode:bool -> int -> int -> t -> t option
(** @raise Clashing in debug mode
@return [None] if the addition would break injectivity *)

val compose : debugMode:bool -> bool -> t -> t -> t
(** @raise Undefined *)

val apply : debugMode:bool -> t -> int -> int
(** @raise Undefined *)

val mem : int -> t -> bool

val inverse : t -> t
(** @raise NotBijective *)

val compare : t -> t -> int
val equal : t -> t -> bool
val min_elt : t -> (int * int) option
val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
val to_list : t -> (int * int) list

val print : Format.formatter -> t -> unit
(** prints only non identity points *)

val print_full : Format.formatter -> t -> unit
val to_yojson : t -> Yojson.Basic.t
val of_yojson : Yojson.Basic.t -> t
