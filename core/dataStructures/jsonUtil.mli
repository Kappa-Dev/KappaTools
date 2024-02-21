(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Parsing utils *)

val write_to_channel : (Buffer.t -> 'a -> unit) -> out_channel -> 'a -> unit
val string_of_write : (Buffer.t -> 'a -> unit) -> ?len:int -> 'a -> string

val read_of_string :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) -> string -> 'a

val read_between_spaces :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a

val read_next_item :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a

val write_comma : Buffer.t -> unit

(** Jsonify simple types *)

val build_msg : string -> string
val of_string : string -> Yojson.Basic.t
val to_string : ?error_msg:string -> Yojson.Basic.t -> string
val of_int : int -> Yojson.Basic.t
val to_int : ?error_msg:string -> Yojson.Basic.t -> int
val of_bool : bool -> Yojson.Basic.t
val to_bool : ?error_msg:string -> Yojson.Basic.t -> bool
val of_unit : unit -> Yojson.Basic.t
val to_unit : ?error_msg:string -> Yojson.Basic.t -> unit
val of_option : ('a -> Yojson.Basic.t) -> 'a option -> Yojson.Basic.t

val to_option : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a option
(** Beware: `Null is reserved for None *)

val write_option : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit

val read_option :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a option

val of_list : ('a -> Yojson.Basic.t) -> 'a list -> Yojson.Basic.t

val to_list :
  ?error_msg:string -> (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a list

val write_list : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a list -> unit
val of_array : ('a -> Yojson.Basic.t) -> 'a array -> Yojson.Basic.t

val to_array :
  ?error_msg:string -> (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a array

val write_array : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a array -> unit
val write_sequence : Buffer.t -> (Buffer.t -> unit) list -> unit

val read_variant :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a -> 'b) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'b

val smart_assoc : (string * Yojson.Basic.t) list -> Yojson.Basic.t
(** Do not put fields whose value is 'null', '[]' or '\{\}' *)

val of_assoc : ('a -> string * Yojson.Basic.t) -> 'a list -> Yojson.Basic.t

val to_assoc :
  ?error_msg:string ->
  (string * Yojson.Basic.t -> 'a) ->
  Yojson.Basic.t ->
  'a list

val write_field : string -> (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a -> unit

val of_pair :
  ?lab1:string ->
  ?lab2:string ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t) ->
  'a * 'b ->
  Yojson.Basic.t

val to_pair :
  ?lab1:string ->
  ?lab2:string ->
  ?error_msg:string ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'b) ->
  Yojson.Basic.t ->
  'a * 'b

val write_compact_pair :
  (Buffer.t -> 'a -> unit) ->
  (Buffer.t -> 'b -> unit) ->
  Buffer.t ->
  'a * 'b ->
  unit

val read_compact_pair :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'b) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a * 'b

val compact_to_pair :
  (Yojson.Basic.t -> 'a) -> (Yojson.Basic.t -> 'b) -> Yojson.Basic.t -> 'a * 'b

val of_triple :
  ?lab1:string ->
  ?lab2:string ->
  ?lab3:string ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t) ->
  ('c -> Yojson.Basic.t) ->
  'a * 'b * 'c ->
  Yojson.Basic.t

val to_triple :
  ?lab1:string ->
  ?lab2:string ->
  ?lab3:string ->
  ?error_msg:string ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'b) ->
  (Yojson.Basic.t -> 'c) ->
  Yojson.Basic.t ->
  'a * 'b * 'c

val of_map :
  ?lab_key:string ->
  ?lab_value:string ->
  fold:
    (('key -> 'value -> Yojson.Basic.t list -> Yojson.Basic.t list) ->
    'map ->
    Yojson.Basic.t list ->
    Yojson.Basic.t list) ->
  ('key -> Yojson.Basic.t) ->
  ('value -> Yojson.Basic.t) ->
  'map ->
  Yojson.Basic.t

val to_map :
  ?lab_key:string ->
  ?lab_value:string ->
  ?error_msg:string ->
  add:('key -> 'value -> 'map -> 'map) ->
  empty:'map ->
  (Yojson.Basic.t -> 'key) ->
  (Yojson.Basic.t -> 'value) ->
  Yojson.Basic.t ->
  'map

val of_unix_label : UnixLabels.error -> Yojson.Basic.t
val to_unix_label : Yojson.Basic.t -> UnixLabels.error
val of_unix_error : Unix.error -> Yojson.Basic.t
val to_unix_error : Yojson.Basic.t -> Unix.error
val std_json_string_of_float : float -> string
