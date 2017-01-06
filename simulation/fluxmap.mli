(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Flux map *)
val create_flux :
  Model.t -> Counter.t -> bool -> string -> Data.flux_data
val stop_flux : Model.t -> Counter.t -> Data.flux_data -> Data.flux_map

val incr_flux_flux : int -> int -> float -> Data.flux_data -> unit
(** [incr_flux_flux of_rule on_rule val flux] *)

val incr_flux_hit : int -> Data.flux_data -> unit

val get_flux_name : Data.flux_data -> string
val flux_has_name : string -> Data.flux_data -> bool
