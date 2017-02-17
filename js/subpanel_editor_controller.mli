(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val set_manager : string -> unit
val set_content : string -> unit
val create_project : string -> unit
val set_project : string -> unit
val close_project : unit -> unit


val create_file : string -> unit
val set_file : string -> unit
val close_file : unit -> unit
val set_file_compile : string -> bool -> unit
val order_files : string list -> unit

val close_simulation : unit -> unit
val create_simulation : Api_types_j.simulation_id -> unit
val set_simulation : Api_types_j.simulation_id -> unit

val continue_simulation : unit -> unit
val pause_simulation : unit -> unit
val stop_simulation : unit -> unit
val start_simulation : unit -> unit
val perturb_simulation : unit -> unit
