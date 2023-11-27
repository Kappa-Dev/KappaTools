(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type key = int
type t

val print : Format.formatter -> t -> unit
val random : Random.State.t -> t -> key
val empty : t
val is_empty : t -> bool
val add : key -> int -> t -> t
val mem : key -> t -> bool
val remove : key -> t -> t
val total : t -> Int64.t
