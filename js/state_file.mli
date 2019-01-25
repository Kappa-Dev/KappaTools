(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
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
(* Update compile of current file *)
val set_compile : Api_types_j.file_id  -> bool -> unit Api.result Lwt.t
(* Update the position of a file *)
val order_files : string list -> unit Api.result Lwt.t
(* get current file *)
val get_file : unit -> Api_types_j.file Api.result Lwt.t

(* Get current file - the name is not specified to force
   the selection of the file before the fetch.
*)
type refresh = { filename : string ; content : string ; line : int option ; }
val refresh_file : refresh option React.signal
(* remove current file from project *)
val remove_file : unit -> unit Api.result Lwt.t
(* Meta data of current file *)
type t
type model = { model_current : string option ;
               model_directory : t list }
val model : model React.signal
val t_file_id : t -> Api_types_j.file_id
val t_compile : t -> bool

(* run on application init *)
val init : unit -> unit Lwt.t
(* to synch state of application with runtime *)
val sync : ?reset:bool -> unit -> unit Api.result Lwt.t
