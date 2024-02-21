(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Imperative int set *)

type t

val create : int -> t
(** [create initial_guess_size] *)

val is_empty : t -> bool
val size : t -> int
val mem : int -> t -> bool
val add : int -> t -> unit
val remove : int -> t -> unit
val random : Random.State.t -> t -> int option
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
val print : Format.formatter -> t -> unit
