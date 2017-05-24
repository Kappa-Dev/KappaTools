(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type parameters = {
  plot_period : float;
  pause_condition : string;
  seed : int option;
  store_trace : bool;
}

type model = {
  model_project_id : Api_types_t.project_id option ;
  model_project_ids : Api_types_t.project_id list ;
  model_project_version : Api_types_t.project_version ;
  model_contact_map : Api_types_t.contact_map option ;
  model_parameters : parameters ;
}

val dummy_model : model
val model : model React.signal

val set_plot_period : float -> unit
val set_pause_condition : string -> unit
val set_seed : int option -> unit
val set_store_trace : bool -> unit

val create_simulation_parameter :
  Api_types_t.project_id -> Api_types_j.simulation_parameter

val set_project : Api_types_t.project_id -> unit Api.result Lwt.t

val create_project : Api_types_t.project_id -> unit Api.result Lwt.t

val remove_project : Api_types_t.project_id -> unit Api.result Lwt.t

val init : Api_types_t.project list -> unit Lwt.t
(* run on application init *)

val sync : unit -> unit Api.result Lwt.t
(* to synch state of application with runtime *)

val with_project : label:string ->
  (Api.concrete_manager -> Api_types_t.project_id -> 'a  Api.result Lwt.t) ->
  'a  Api.result Lwt.t
