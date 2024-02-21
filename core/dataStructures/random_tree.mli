(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type tree

val create : int -> tree
val total : tree -> float
val copy : tree -> tree
val copy_in : tree -> tree -> tree
val add : int -> float -> tree -> unit
val random : Random.State.t -> tree -> int * float
val find : int -> tree -> float
val is_infinite : int -> tree -> bool
val debug_print : Format.formatter -> tree -> unit
