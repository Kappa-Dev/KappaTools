(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Simulation progress keeper *)

type period = DE of int | DT of float

type t
val create : ?init_t:float -> ?init_e:int ->
  ?max_time:float -> ?max_event:int ->
  plot_period:period -> t

val reinitialize : t -> unit

val current_simulation_info : t -> unit Trace.Simulation_info.t
val next_story : t -> unit Trace.Simulation_info.t

val fill : outputs:(Data.t -> unit) -> t -> (t -> Nbr.t array) -> unit

val one_constructive_event : t -> float -> bool
val one_clashing_instance_event : t -> float -> bool
val one_no_more_unary_event : t -> float -> bool
val one_no_more_binary_event : t -> float -> bool
val one_time_correction_event : t -> Nbr.t -> bool

val inc_stories : t -> unit

val max_time : t -> float option
val max_events : t -> int option
val set_max_time  : t -> float option -> unit
val set_max_events : t -> int option -> unit
val event_percentage : t -> int option
val event : t -> int
val time_percentage : t -> int option
val time : t -> float
val tracked_events : t -> int option

val plot_period : t -> float
val set_plot_period : t -> period -> unit

val current_time : t -> float
val current_event : t -> int
val current_story : t -> int
val nb_null_event : t -> int
val consecutive_null_event : t -> int

val print_efficiency : Format.formatter -> t -> unit

(** {5 Output on stdout } *)
val tick : t -> unit
val complete_progress_bar : t -> unit
