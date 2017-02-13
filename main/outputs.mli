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
  Model.t -> unit

val initial_inputs :
  Eval.configuration -> Model.t -> Contact_map.t ->
  (Alg_expr.t * Primitives.elementary_rule * Locality.t) list -> unit

val input_modifications : Model.t -> int -> Primitives.modification list -> unit

val go : Signature.s -> Data.t -> unit
val close : unit -> unit
