(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val create_flux : Model.t -> Counter.t -> Primitives.din_kind -> Data.din_data
(** Flux map *)

val stop_flux : Model.t -> Counter.t -> Data.din_data -> Data.din

val incr_flux_flux : int -> int -> float -> Data.din_data -> unit
(** [incr_flux_flux of_rule on_rule val flux] *)

val incr_flux_hit : int -> Data.din_data -> unit
