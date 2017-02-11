(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val model_pause_condition : string React.signal
val set_model_pause_condition : ?step:React.step -> string -> unit

val create_parameter : Api_types_j.simulation_id -> Api_types_j.simulation_parameter


val model_seed : int option React.signal
val set_model_seed : ?step:React.step -> int option -> unit


val model_plot_period : float React.signal
val set_model_plot_period : ?step:React.step -> float -> unit

val init : unit -> unit Lwt.t
