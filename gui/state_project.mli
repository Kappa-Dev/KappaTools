(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type parameters = {
  plot_period: float;
  pause_condition: string;
  seed: int option;
  store_trace: bool;
  show_dead_rules: bool;
  show_dead_agents: bool;
  show_non_weakly_reversible_transitions: bool;
}

type project_model = {
  model_project_id: string;
  model_project_is_computing: bool React.S.t;
}

type model = {
  model_current_id: string option;
  model_catalog: project_model list;
  model_project_version: int;
  model_parameters: parameters;
}

val model_equal : model -> model -> bool
val dummy_model : model
val model : model React.signal
val set_plot_period : float -> unit
val set_pause_condition : string -> unit
val set_seed : int option -> unit
val set_store_trace : bool -> unit
val set_show_dead_rules : bool -> unit
val set_show_dead_agents : bool -> unit
val set_show_non_weakly_reversible_transitions : bool -> unit
val set_parameters_as_default : unit -> unit
val set_project : string -> unit Api.result Lwt.t
val create_project : string -> unit Api.result Lwt.t
val remove_project : string -> unit Api.result Lwt.t
val init : string list -> unit Lwt.t
(* run on application init *)

val sync : unit -> unit Api.result Lwt.t
(* to synch state of application with runtime *)

val with_project :
  label:string ->
  (Api.concrete_manager -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t

val on_project_change_async :
  ?eq:('a -> 'a -> bool) ->
  on:bool React.signal ->
  ?others_eq:('b -> 'b -> bool) ->
  'b ->
  'b React.signal ->
  'a ->
  (Api.concrete_manager -> 'b -> 'a Lwt.t) ->
  'a React.signal
