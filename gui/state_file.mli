(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val create_file : filename:string -> content:string -> unit Api.lwt_result
(** Create a file *)

val select_file : string -> int option -> unit Api.lwt_result
(** Set current file to file with the specified name *)

val set_content : string -> unit Api.lwt_result
(** Update content of current file *)

val set_compile : string -> bool -> unit Api.lwt_result
(** Update compile of the file of rank [k] *)

val order_files : string list -> unit Api.lwt_result
(** Update the position of a file *)

val get_file : unit -> (string * string) Api.lwt_result
(** get current file *)

val remove_file : unit -> unit Api.lwt_result
(** remove current file from project *)

type refresh = { filename: string; content: string; line: int option }
(** Get current file - the name is not specified to force
   the selection of the file before the fetch.
*)

val refresh_file_hook : refresh Hooked.E.t
(** Meta data of current file *)

val cursor_activity : line:int -> ch:int -> unit
val out_of_sync : bool -> unit

type slot = { local: string option; name: string }
type active = { rank: int; cursor_pos: Loc.position; out_of_sync: bool }
type model = { current: active option; directory: slot Mods.IntMap.t }

val model : model React.signal
val current_filename : string option React.signal

val with_current_pos :
  ?eq:('a -> 'a -> bool) ->
  ?on:bool React.signal ->
  (string -> Loc.position -> 'a option) ->
  'a ->
  'a React.signal

val init : unit -> unit Lwt.t
(** run on application init *)

val sync : ?reset:bool -> unit -> unit Api.lwt_result
(** to synch state of application with runtime *)
