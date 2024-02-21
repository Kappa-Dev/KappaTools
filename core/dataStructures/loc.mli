(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Module for type Loc.t annotating structured data with the line range
 * in a file which was used to define it *)

type position = { chr: int; line: int }
type t = { file: string; from_position: position; to_position: position }
type 'a annoted = 'a * t

val v : 'a annoted -> 'a
(** Extract value from Loc.annoted *)

val get_annot : 'a annoted -> t
(** Extract annotation from Loc.annoted *)

val copy_annot : 'b annoted -> 'a -> 'a annoted
(** Create annoted variable with same annotation as existing variable *)

val map_annot : ('a -> 'b) -> 'a annoted -> 'b annoted
(** Apply operation on variable and keep annotation *)

val of_pos : Lexing.position -> Lexing.position -> t
val dummy : t
val annot_with_dummy : 'a -> 'a annoted
val is_annoted_with_dummy : 'a annoted -> bool

val merge : t -> t -> t
(** [merge b e] creates the range from beginning of [b] to the end of [e]
 (filename must match) *)

val is_included_in : string -> position -> t -> bool

(** {2 I/O} *)

val to_string : t -> string
val print : Format.formatter -> t -> unit

val print_annoted :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a annoted -> unit

val annoted_of_yojson :
  ?filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  Yojson.Basic.t ->
  'a annoted

val yojson_of_annoted :
  ?filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  'a annoted ->
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
