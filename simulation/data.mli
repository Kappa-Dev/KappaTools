(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type snapshot = {
  snapshot_file : string;
  snapshot_event : int;
  snapshot_time : float;
  snapshot_agents : (int * User_graph.connected_component) list;
  snapshot_tokens : (string * Nbr.t) array;
}

type flux_data = {
  flux_name : string;
  flux_kind : Primitives.flux_kind;
  flux_start : float;
  flux_hits : int array;
  flux_fluxs : float array array;
}
type flux_map = {
  flux_rules : string array;
  flux_data : flux_data;
  flux_end : float;
}

type file_line = {
  file_line_name : string option;
  file_line_text : string;
}

type t =
  | Flux of flux_map
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of snapshot
  | Log of string
  | Species of string * float * User_graph.connected_component

val print_snapshot : ?uuid: int -> Format.formatter -> snapshot -> unit

val print_dot_snapshot : ?uuid: int -> Format.formatter -> snapshot -> unit

val write_snapshot :
  Bi_outbuf.t -> snapshot -> unit
  (** Output a JSON value of type {!snapshot}. *)

val string_of_snapshot :
  ?len:int -> snapshot -> string
  (** Serialize a value of type {!snapshot}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_snapshot :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> snapshot
  (** Input JSON data of type {!snapshot}. *)

val snapshot_of_string :
  string -> snapshot
  (** Deserialize JSON data of type {!snapshot}. *)

val write_flux_map :
  Bi_outbuf.t -> flux_map -> unit
  (** Output a JSON value of type {!flux_map}. *)

val string_of_flux_map :
  ?len:int -> flux_map -> string
  (** Serialize a value of type {!flux_map}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_flux_map :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> flux_map
  (** Input JSON data of type {!flux_map}. *)

val flux_map_of_string :
  string -> flux_map
  (** Deserialize JSON data of type {!flux_map}. *)
