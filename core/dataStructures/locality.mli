(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type position = { chr: int; line: int }
type range = { file: string; from_position: position; to_position: position }
type t = range
type 'a annot = 'a * t
type 'a maybe = ?pos:t -> 'a

val of_pos : Lexing.position -> Lexing.position -> t
val dummy : t
val dummy_annot : 'a -> 'a annot
val has_dummy_annot : 'a annot -> bool

val merge : range -> range -> range
(** [merge b e] creates the range from beginning of [b] to the end of [e]
 (filename must match) *)

val is_included_in : string -> position -> range -> bool
val to_string : t -> string
val print : Format.formatter -> t -> unit

val print_annot :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a annot -> unit

val annot_of_yojson :
  ?filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  Yojson.Basic.t ->
  'a annot

val annot_to_yojson :
  ?filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  'a annot ->
  Yojson.Basic.t

val write_position : Buffer.t -> position -> unit
val read_position : Yojson.Safe.lexer_state -> Lexing.lexbuf -> position

val write_range : Buffer.t -> t -> unit
(** Output a JSON value of type {!t}. *)

val string_of_range : ?len:int -> t -> string
(** Serialize a value of type {!t} into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_range : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
(** Input JSON data of type {!t}. *)

val range_of_string : string -> t
(** Deserialize JSON data of type {!t}. *)
