(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Simulation progress keeper *)

module Efficiency : sig
    type t = {
      mutable consecutive : int;
      mutable no_more_binary : int;
      mutable no_more_unary : int;
      mutable clashing_instance : int;
      mutable time_correction : int
    }

  val write_t : Bi_outbuf.t -> t -> unit
  (** Output a JSON value of type {!t}. *)

  val string_of_t : ?len:int -> t -> string
  (** Serialize a value of type {!t} into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

  val read_t : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!t}. *)

  val t_of_string : string -> t
  (** Deserialize JSON data of type {!t}. *)
end

type progressBar = {
  progressSize : int;
  progressChar : char;
}

val default_progress : progressBar

type period = DE of int | DT of float

type t
val create : ?init_t:float -> ?init_e:int ->
  ?max_time:float -> ?max_event:int ->
  plot_period:period -> t

val reinitialize : t -> unit

val current_simulation_info : t -> unit Trace.Simulation_info.t
val next_story : t -> unit Trace.Simulation_info.t

val fill : outputs:(t -> float -> unit) -> t -> dt:float -> unit
val fake_time : t -> float -> t

val one_time_advance : t -> float -> unit
val one_constructive_event : t -> bool
val one_clashing_instance_event : t -> bool
val one_no_more_unary_event : t -> bool
val one_no_more_binary_event : t -> bool
val one_time_correction_event : t -> Nbr.t -> bool

val inc_stories : t -> unit

val init_time : t -> float
val max_time : t -> float option
val max_events : t -> int option
val set_max_time  : t -> float option -> unit
val set_max_events : t -> int option -> unit
val event_percentage : t -> int option
val time_percentage : t -> int option
val tracked_events : t -> int option

val positive_plot_period : t -> bool
val plot_period : t -> period
val set_plot_period : t -> period -> unit

val current_time : t -> float
val current_event : t -> int
val current_story : t -> int
val nb_null_event : t -> int
val consecutive_null_event : t -> int

val get_efficiency : t -> Efficiency.t
val print_efficiency : Format.formatter -> t -> unit

(** {5 Output on stdout } *)
val tick : efficiency:bool -> progressBar -> t -> unit
val complete_progress_bar : t -> unit
