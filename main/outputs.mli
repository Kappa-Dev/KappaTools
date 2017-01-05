(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Deal with simulation output *)

val initialize :
  string option -> (string * string * string array) option ->
  Environment.t -> unit
val create_distances : string array -> bool -> unit

val go : Signature.s -> Data.t -> unit
val close : unit -> unit
