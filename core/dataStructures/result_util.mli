(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type status =
  [ `OK
  | `Accepted
  | `Created
  | `Bad_request
  | `Conflict
  | `Not_found
  | `Request_timeout ]
(** The subset of [Cohttp.Code.status] we need *)

type message = {
  severity: Logs.level;
  text: string; (*should be an algebraic type*)
  range: Loc.t option;
}

type ('a, 'b) t = {
  value: ('a, 'b) Result.result;
  status: status;
  messages: message list;
}

val write_message : Buffer.t -> message -> unit
val read_message : Yojson.Safe.lexer_state -> Lexing.lexbuf -> message
val print_message : Format.formatter -> message -> unit

val write_t :
  (Buffer.t -> 'ok -> unit) ->
  (Buffer.t -> 'error -> unit) ->
  Buffer.t ->
  ('ok, 'error) t ->
  unit
(** Output a JSON value of type {!t}. *)

val string_of_t :
  (Buffer.t -> 'ok -> unit) ->
  (Buffer.t -> 'error -> unit) ->
  ?len:int ->
  ('ok, 'error) t ->
  string
(** Serialize a value of type {!t} into a JSON string.  @param len
    specifies the initial length of the buffer used internally.
    Default: 1024. *)

val read_t :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'ok) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'error) ->
  Yojson.Safe.lexer_state ->
  Lexing.lexbuf ->
  ('ok, 'error) t
(** Input JSON data of type {!t}. *)

val t_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'ok) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'error) ->
  string ->
  ('ok, 'error) t
(** Deserialize JSON data of type {!t}. *)

val lift :
  ?ok_status:status ->
  ?error_status:status ->
  ('a, 'b) Result.result ->
  ('a, 'b) t

val fold : ok:('ok -> 'a) -> error:('error -> 'a) -> ('ok, 'error) t -> 'a

val bind :
  ?overwrite_status:status ->
  ?error_status:status ->
  ('ok -> ('a, 'error) Result.result) ->
  ('ok, 'error) t ->
  ('a, 'error) t

val map : ('ok -> 'a) -> ('ok, 'error) t -> ('a, 'error) t

val map2 :
  ('a -> 'b -> 'ok) -> ('a, 'error) t -> ('b, 'error) t -> ('ok, 'error) t

val error : ?status:status -> 'error -> ('ok, 'error) t
val ok : ?status:status -> 'ok -> ('ok, 'error) t
