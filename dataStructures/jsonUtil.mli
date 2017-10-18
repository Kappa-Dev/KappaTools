(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Parsing utils *)

val write_to_channel: (Bi_outbuf.t -> 'a -> unit) -> out_channel -> 'a -> unit
val string_of_write: (Bi_outbuf.t -> 'a -> unit) -> ?len:int -> 'a -> string

val read_between_spaces :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a)

val write_comma: Bi_outbuf.t -> unit

(** Jsonify simple types *)

val build_msg: string -> string

val of_string: string -> Yojson.Basic.json
val to_string: ?error_msg:string -> Yojson.Basic.json  -> string

val of_int: int -> Yojson.Basic.json
val to_int: ?error_msg:string -> Yojson.Basic.json  -> int

val of_bool: bool -> Yojson.Basic.json
val to_bool: ?error_msg:string -> Yojson.Basic.json  -> bool

val of_unit: unit -> Yojson.Basic.json
val to_unit: ?error_msg:string -> Yojson.Basic.json  -> unit

val of_option: ('a -> Yojson.Basic.json) -> 'a option -> Yojson.Basic.json

val to_option: (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a option
(** Beware: `Null is reserved for None *)

val write_option:
  (Bi_outbuf.t -> 'a -> unit) -> Bi_outbuf.t -> 'a option -> unit

val read_option:
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a option

val of_list: ('a -> Yojson.Basic.json) -> 'a list -> Yojson.Basic.json

val to_list:
  ?error_msg:string -> (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a list

val write_list: (Bi_outbuf.t -> 'a -> unit) -> Bi_outbuf.t -> 'a list -> unit

val of_array: ('a -> Yojson.Basic.json) -> 'a array -> Yojson.Basic.json

val to_array:
  ?error_msg:string -> (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a array

val write_array: (Bi_outbuf.t -> 'a -> unit) -> Bi_outbuf.t -> 'a array -> unit

val smart_assoc: (string * Yojson.Basic.json) list -> Yojson.Basic.json
(** Do not put fields whose value is 'null', '[]' or '{}' *)

val of_assoc:
  ('a  -> string * Yojson.Basic.json) -> 'a list -> Yojson.Basic.json

val to_assoc:
  ?error_msg:string -> (string * Yojson.Basic.json -> 'a) ->
  Yojson.Basic.json -> 'a list

val write_field:
  string -> (Bi_outbuf.t -> 'a -> unit) -> Bi_outbuf.t -> 'a -> unit

val of_pair:
  ?lab1:string -> ?lab2:string ->
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a * 'b) -> Yojson.Basic.json

val to_pair:
  ?lab1:string -> ?lab2:string -> ?error_msg:string ->
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> 'a * 'b

val write_compact_pair:
  (Bi_outbuf.t -> 'a -> unit) -> (Bi_outbuf.t -> 'b -> unit) ->
  Bi_outbuf.t -> 'a * 'b -> unit

val read_compact_pair:
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'b) ->
  Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a * 'b

val of_triple:
    ?lab1:string -> ?lab2:string -> ?lab3:string ->
      ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
    ('c -> Yojson.Basic.json) ->
    ('a * 'b * 'c) -> Yojson.Basic.json

val to_triple:
  ?lab1:string -> ?lab2:string -> ?lab3:string -> ?error_msg:string ->
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  (Yojson.Basic.json -> 'c) -> Yojson.Basic.json -> 'a * 'b * 'c

val of_map:
  ?lab_key:string -> ?lab_value:string ->
  fold:(('key -> 'value -> Yojson.Basic.json list -> Yojson.Basic.json list) ->
   'map -> Yojson.Basic.json list -> Yojson.Basic.json list) ->
  ('key -> Yojson.Basic.json) -> ('value -> Yojson.Basic.json) ->
  'map -> Yojson.Basic.json

val to_map:
  ?lab_key:string -> ?lab_value:string -> ?error_msg:string ->
  add:('key -> 'value -> 'map -> 'map) ->
  empty:'map ->
  (Yojson.Basic.json -> 'key) ->
  (Yojson.Basic.json -> 'value) ->
  Yojson.Basic.json -> 'map

val of_unix_label:
  UnixLabels.error -> Yojson.Basic.json

val to_unix_label:
  Yojson.Basic.json -> UnixLabels.error

val of_unix_error:
    Unix.error -> Yojson.Basic.json

val to_unix_error:
    Yojson.Basic.json -> Unix.error
