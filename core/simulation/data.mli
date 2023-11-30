(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type snapshot = {
  snapshot_event: int;
  snapshot_time: float;
  snapshot_agents: (int * User_graph.connected_component) list;
  snapshot_tokens: (string * Nbr.t) array;
}

type din_data = {
  din_kind: Primitives.din_kind;
  din_start: float;
  din_hits: int array;
  din_fluxs: float array array;
}

type din = { din_rules: string array; din_data: din_data; din_end: float }
type file_line = { file_line_name: string option; file_line_text: string }

type t =
  | DIN of string * din
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array  (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of string * snapshot
  | Log of string
  | Species of string * float * User_graph.connected_component
  | Warning of Loc.t option * (Format.formatter -> unit)

val print_snapshot : ?uuid:int -> Format.formatter -> snapshot -> unit
val print_dot_snapshot : ?uuid:int -> Format.formatter -> snapshot -> unit

val write_snapshot : Buffer.t -> snapshot -> unit
(** Output a JSON value of type {!snapshot}. *)

val string_of_snapshot : ?len:int -> snapshot -> string
(** Serialize a value of type {!snapshot}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_snapshot : Yojson.Safe.lexer_state -> Lexing.lexbuf -> snapshot
(** Input JSON data of type {!snapshot}. *)

val snapshot_of_string : string -> snapshot
(** Deserialize JSON data of type {!snapshot}. *)

val print_dot_din : ?uuid:int -> Format.formatter -> din -> unit
val print_html_din : Format.formatter -> din -> unit

val write_din : Buffer.t -> din -> unit
(** Output a JSON value of type {!din}. *)

val string_of_din : ?len:int -> din -> string
(** Serialize a value of type {!din}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_din : Yojson.Safe.lexer_state -> Lexing.lexbuf -> din
(** Input JSON data of type {!din}. *)

val din_of_string : string -> din
(** Deserialize JSON data of type {!din}. *)

type plot = { plot_legend: string array; plot_series: float option array list }

val add_plot_line : Nbr.t array -> plot -> plot
val init_plot : Model.t -> plot

val write_plot : Buffer.t -> plot -> unit
(** Output a JSON value of type {!plot}. *)

val string_of_plot : ?len:int -> plot -> string
(** Serialize a value of type {!plot}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_plot : Yojson.Safe.lexer_state -> Lexing.lexbuf -> plot
(** Input JSON data of type {!plot}. *)

val plot_of_string : string -> plot
(** Deserialize JSON data of type {!plot}. *)

val print_plot_legend : is_tsv:bool -> Format.formatter -> string array -> unit

val print_plot_line :
  is_tsv:bool ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a array ->
  unit

val export_plot : is_tsv:bool -> plot -> string

val print_initial_inputs :
  ?uuid:int ->
  Configuration.t ->
  Model.t ->
  Format.formatter ->
  (Primitives.alg_expr * Primitives.elementary_rule) list ->
  unit

val print_warning :
  ?pos:Loc.t -> Format.formatter -> (Format.formatter -> unit) -> unit
