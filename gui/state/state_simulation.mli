(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t

val get_simulation_info : t -> Api_types_j.simulation_info option
val dummy_model : t
val model : t React.signal

type simulation_status = STOPPED | INITALIZING | RUNNING | PAUSED

val simulation_status_to_string : simulation_status -> string
val model_simulation_state : t -> simulation_status

(* run on application init *)
val init : unit -> unit Lwt.t
val refresh : unit -> unit Api.lwt_result

val eval_with_sim_manager :
  label:string ->
  (Api.concrete_manager -> t -> 'a Api.lwt_result) ->
  'a Api.lwt_result
(** [eval_with_sim_manager ~label handler] evaluates the function [handler] applied to the [concrete_manager] of current project and current [simulation_state] *)

val eval_with_sim_manager_and_info :
  label:string ->
  ?stopped:(Api.concrete_manager -> 'a Api.lwt_result) ->
  ?initializing:(Api.concrete_manager -> 'a Api.lwt_result) ->
  ?ready:
    (Api.concrete_manager -> Api_types_j.simulation_info -> 'a Api.lwt_result) ->
  unit ->
  'a Api.lwt_result
(** [eval_with_sim_manager_and_info ~label ~stopped ~initializing ~ready] evaluates the function in argument matching the current [simulation_state], applied to the [concrete_manager] of current project *)

val eval_when_ready :
  label:string ->
  ?handler:(unit Api.result -> unit Lwt.t) ->
  (Api.concrete_manager -> unit Api.lwt_result) ->
  unit

val continue_simulation : string -> unit Api.lwt_result
val pause_simulation : unit -> unit Api.lwt_result
val stop_simulation : unit -> unit Api.lwt_result
val start_simulation : Api_types_j.simulation_parameter -> unit Api.lwt_result
val intervene_simulation : string -> string Api.lwt_result
