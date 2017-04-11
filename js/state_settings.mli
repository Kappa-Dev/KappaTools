(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val get_client_id : unit -> string
val set_client_id : string -> unit

val get_synch : unit -> bool
val set_synch : bool -> unit

val init : unit -> unit Lwt.t
(** run on application init *)

val sync : unit -> unit Lwt.t
(** to synch state of application with runtime *)
