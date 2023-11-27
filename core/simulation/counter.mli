(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Simulation progress keeper *)

module Efficiency : sig
  type t = {
    consecutive: int array;
    mutable consecutive_blocked: int;
    mutable no_more_binary: int;
    mutable no_more_unary: int;
    mutable clashing_instance: int;
    mutable time_correction: int;
  }

  val write_t : Buffer.t -> t -> unit
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

type t

val create :
  ?init_t:float ->
  ?init_e:int ->
  ?max_time:float ->
  ?max_event:int ->
  plot_period:Configuration.period ->
  nb_rules:int ->
  unit ->
  t

val reinitialize : t -> unit
val current_simulation_info : t -> unit Trace.Simulation_info.t
val next_step_simulation_info : t -> unit Trace.Simulation_info.t
val next_story : t -> unit Trace.Simulation_info.t
val fill : outputs:(t -> float -> unit) -> t -> dt:float -> unit
val fake_time : t -> float -> t
val one_time_advance : t -> float -> bool
val one_blocked_event : t -> bool
val one_constructive_event : rule_id:int -> t -> bool
val one_clashing_instance_event : rule_id:int -> t -> bool
val one_no_more_unary_event : rule_id:int -> t -> bool
val one_no_more_binary_event : rule_id:int -> t -> bool
val one_time_correction_event : ?ti:Nbr.t -> t -> bool
val inc_stories : t -> unit
val init_time : t -> float
val max_time : t -> float option
val max_events : t -> int option
val set_max_time : t -> float option -> unit
val set_max_events : t -> int option -> unit
val event_ratio : t -> float option
val time_ratio : t -> float option
val tracked_events : t -> int option
val positive_plot_period : t -> bool
val plot_period : t -> Configuration.period
val set_plot_period : t -> Configuration.period -> unit
val current_time : t -> float
val current_event : t -> int
val current_story : t -> int
val nb_null_event : t -> int
val consecutive_null_event : rule_id:int -> t -> int
val consecutive_blocked : t -> int
val get_efficiency : t -> Efficiency.t
val print_efficiency : Format.formatter -> t -> unit
