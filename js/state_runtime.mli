(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** An implementation of the the simulation
    of the simulation runtime. *)
type spec

(** Get the label of t.  This is useful when
    displaying t.
*)
val spec_label : spec -> string
(** Get an identifier of t.  This is useful when
    indexing t's.
*)
val spec_id : spec -> string

(** Read t from a string representation. *)
val read_spec : string -> spec option

(** Given a string specification of t (e.g. read_t or t_id)
    configure the current manager.
*)
val set_manager : string -> unit Api.result Lwt.t
(** Create a runtime to be used by the system *)
val create_manager : string -> spec Api.result
(** Get the current manager.  It is assumed that
    when the system is initalized this is set to a
    default, which is currently default web worker
    as it is provides the fastest simulation. *)
val get_manager : unit -> Api.manager
(** Get the t which currently serves as the runtime. *)
type model = { model_current : spec ; model_runtimes : spec list ; }
(** List available runtimes *)
val model : model React.signal

(* run on application init *)
val init : unit -> unit Lwt.t
(* to synch state of application with runtime *)
val sync : unit -> unit Lwt.t
