(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type ('a,'b) t = ('a,'b) Result.result

val write_t :
  (Bi_outbuf.t -> 'ok -> unit) ->
  (Bi_outbuf.t -> 'error -> unit) ->
  Bi_outbuf.t -> ('ok, 'error) t -> unit
(** Output a JSON value of type {!t}. *)

val string_of_t :
  (Bi_outbuf.t -> 'ok -> unit) ->
  (Bi_outbuf.t -> 'error -> unit) ->
  ?len:int -> ('ok, 'error) t -> string
(** Serialize a value of type {!t} into a JSON string.  @param len
    specifies the initial length of the buffer used internally.
    Default: 1024. *)

val read_t :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'ok) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'error) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ('ok, 'error) t
(** Input JSON data of type {!t}. *)

val t_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'ok) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'error) ->
  string -> ('ok, 'error) t
(** Deserialize JSON data of type {!t}. *)

val fold : ok:('ok -> 'a) -> error:('error -> 'a) -> ('ok, 'error) t -> 'a
val bind : ('ok -> ('a, 'error) t) -> ('ok, 'error) t -> ('a, 'error) t
val map : ('ok -> 'a) -> ('ok, 'error) t -> ('a, 'error) t
val map2 :
  ('a -> 'b -> 'ok) -> ('a, 'error) t -> ('b, 'error) t -> ('ok, 'error) t
val error : 'error -> ('ok, 'error) t
val ok : 'ok -> ('ok, 'error) t
