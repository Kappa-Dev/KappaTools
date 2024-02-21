(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Trace of simulation *)

module Simulation_info : sig
  type 'a t = {
    story_id: int;
    story_time: float;
    story_event: int;
    profiling_info: 'a;
  }
  (** type of data to be given with observables for story compression
      (such as date when the obs is triggered*)

  val compare_by_story_id : 'a t -> 'a t -> int
  val update_profiling_info : 'a -> 'b t -> 'a t
  val event : 'a t -> int
  val story_id : 'a t -> int
  val json_dictionnary : string
  val to_json : ('a -> Yojson.Basic.t) -> 'a t -> Yojson.Basic.t
  val of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a t
end

type event_kind =
  | RULE of int
  | INIT of int list  (** the agents *)
  | PERT of string  (** the rule *)

val print_event_kind : ?env:Model.t -> Format.formatter -> event_kind -> unit

val print_event_kind_dot_annot :
  Model.t -> Format.formatter -> event_kind -> unit

type step =
  | Subs of int * int
  | Rule of
      int * Instantiation.concrete Instantiation.event * unit Simulation_info.t
  | Pert of
      string
      * Instantiation.concrete Instantiation.event
      * unit Simulation_info.t
  | Init of Instantiation.concrete Instantiation.action list
  | Obs of
      string
      * Instantiation.concrete Instantiation.test list list
      * unit Simulation_info.t
  | Dummy of string

type t = step list

val dummy_step : string -> step
val subs_step : int -> int -> step
val step_is_obs : step -> bool
val step_is_init : step -> bool
val step_is_subs : step -> bool
val step_is_rule : step -> bool
val step_is_pert : step -> bool
val has_creation_of_step : step -> bool
val tests_of_step : step -> Instantiation.concrete Instantiation.test list

val actions_of_step :
  step ->
  Instantiation.concrete Instantiation.action list
  * (Instantiation.concrete Instantiation.site
    * Instantiation.concrete Instantiation.binding_state)
    list
(** @return (actions, side_effects) *)

val side_effects_of_step :
  step -> Instantiation.concrete Instantiation.site list

val simulation_info_of_step : step -> unit Simulation_info.t option
val creation_of_actions : ('a -> 'b) -> 'a Instantiation.action list -> 'b list
val creation_of_step : step -> int list

val print_step :
  ?compact:bool -> ?env:Model.t -> Format.formatter -> step -> unit

val print_label_of_step : ?env:Model.t -> Format.formatter -> step -> unit
val step_to_yojson : step -> Yojson.Basic.t
val json_dictionnary : string

val write_step : Buffer.t -> step -> unit
(** Output a JSON value of type {!step}. *)

val string_of_step : ?len:int -> step -> string
(** Serialize a value of type {!step} into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_step : Yojson.Safe.lexer_state -> Lexing.lexbuf -> step
(** Input JSON data of type {!step}. *)

val step_of_string : string -> step
(** Deserialize JSON data of type {!step}. *)

val write_json : Buffer.t -> t -> unit
val read_json : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
val init_trace_file : uuid:int -> Model.t -> out_channel -> unit

val fold_trace :
  (Model.t -> 'a -> step -> 'a) ->
  (Model.t -> 'a) ->
  Yojson.Safe.lexer_state ->
  Lexing.lexbuf ->
  Model.t * 'a

val fold_trace_file :
  (Model.t -> 'a -> step -> 'a) -> (Model.t -> 'a) -> string -> Model.t * 'a

val get_headers_from_file : string -> int option * Model.t
