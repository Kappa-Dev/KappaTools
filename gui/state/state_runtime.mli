(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** An implementation of the the simulation
    of the simulation runtime. *)

type spec

val spec_label : spec -> string
(** Get the label of t.  This is useful when
    displaying t.
*)

val spec_id : spec -> string
(** Get an identifier of t.  This is useful when
    indexing t's.
*)

val read_spec : string -> spec option
(** Read t from a string representation. *)

type model = { model_current: spec; model_runtimes: spec list }

val model : model React.signal

val create_spec : load:bool -> string -> unit Api.result
(** Given a string specification of t (e.g. read_t or t_id)
    configure the current manager.
*)

val create_manager :
  is_new:bool -> string -> Api.concrete_manager Api.lwt_result
(** Create a runtime to be used by the system *)

val init : unit -> string list Lwt.t
(* run on application init *)

val sync : unit -> unit Lwt.t
(* to synch state of application with runtime *)
