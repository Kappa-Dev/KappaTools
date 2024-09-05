(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val continue_simulation : unit -> unit
val pause_simulation : unit -> unit
val stop_simulation : unit -> unit
val start_simulation : unit -> unit
val intervene_simulation : unit -> unit
val simulation_trace : unit -> unit
val simulation_outputs : unit -> unit
val focus_range : Loc.t -> unit
