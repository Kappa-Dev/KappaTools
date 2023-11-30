(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* Create a file *)
val create_file : filename:string -> content:string -> unit Api.result Lwt.t

(* Set current file to file with the specified name *)
val select_file : string -> int option -> unit Api.result Lwt.t

(* Update content of current file *)
val set_content : string -> unit Api.result Lwt.t

(* Update compile of the file of rank [k] *)
val set_compile : string -> bool -> unit Api.result Lwt.t

(* Update the position of a file *)
val order_files : string list -> unit Api.result Lwt.t

(* get current file *)
val get_file : unit -> (string * string) Api.result Lwt.t

(* remove current file from project *)
val remove_file : unit -> unit Api.result Lwt.t

(* Get current file - the name is not specified to force
   the selection of the file before the fetch.
*)
type refresh = { filename: string; content: string; line: int option }

val refresh_file : refresh React.event
(* Meta data of current file *)

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

(* run on application init *)
val init : unit -> unit Lwt.t

(* to synch state of application with runtime *)
val sync : ?reset:bool -> unit -> unit Api.result Lwt.t
