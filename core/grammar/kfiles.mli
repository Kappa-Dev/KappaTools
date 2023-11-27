(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type catalog
type catalog_item = { position: int; id: string }

val write_catalog_item : Buffer.t -> catalog_item -> unit
val read_catalog_item : Yojson.lexer_state -> Lexing.lexbuf -> catalog_item
val create : unit -> catalog

val file_create :
  position:int ->
  id:string ->
  content:string ->
  catalog ->
  (unit, string) Result.result
(** Fails if ([id] exists or) [position] is not available *)

val file_move :
  position:int -> id:string -> catalog -> (unit, string) Result.result
(** Fails if [position] is not available *)

val file_patch : id:string -> string -> catalog -> (unit, string) Result.result
val file_delete : id:string -> catalog -> (unit, string) Result.result

val file_get : id:string -> catalog -> (string * int, string) Result.result
(** @return (content, position) *)

val catalog : catalog -> catalog_item list

val parse :
  (unit -> unit Lwt.t) ->
  catalog ->
  (Ast.parsing_compil, Result_util.message list) Result_util.t Lwt.t

val overwrite : string -> Ast.parsing_compil -> catalog -> unit
