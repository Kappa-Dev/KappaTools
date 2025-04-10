(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(* private simulation state *)
type simulation_state =
  | SIMULATION_STATE_STOPPED (* simulation is unavailable *)
  | SIMULATION_STATE_INITALIZING (* simulation is blocked on an operation *)
  | SIMULATION_STATE_READY of Api_types_j.simulation_info
(* the simulation is ready *)

type t = { simulation_state: simulation_state }

let get_simulation_info simulation : Api_types_j.simulation_info option =
  match simulation.simulation_state with
  | SIMULATION_STATE_STOPPED -> None
  | SIMULATION_STATE_INITALIZING -> None
  | SIMULATION_STATE_READY simulation_info -> Some simulation_info

(* public simulation status *)
type simulation_status = STOPPED | INITALIZING | RUNNING | PAUSED

let simulation_status_to_string = function
  | STOPPED -> "Stopped"
  | INITALIZING -> "Initalizing"
  | RUNNING -> "Running"
  | PAUSED -> "Paused"

let dummy_model = { simulation_state = SIMULATION_STATE_STOPPED }

let model_simulation_state model : simulation_status =
  match model.simulation_state with
  | SIMULATION_STATE_STOPPED -> STOPPED
  | SIMULATION_STATE_INITALIZING -> INITALIZING
  | SIMULATION_STATE_READY simulation_info ->
    if
      simulation_info.Api_types_j.simulation_info_progress
        .Api_types_j.simulation_progress_is_running
    then
      RUNNING
    else
      PAUSED

(* private state *)
let state, set_state = React.S.create dummy_model

(* public model *)
let model = state

let update_simulation_state (simulation_state : simulation_state) : unit =
  let () = set_state { simulation_state } in
  ()

let eval_with_sim_manager :
      'a.
      label:string ->
      (Api.concrete_manager -> t -> 'a Api.lwt_result) ->
      'a Api.lwt_result =
 fun ~label handler ->
  let project_handler manager = handler manager (React.S.value state) in
  State_project.eval_with_project ~label project_handler

let fail_lwt error_msg = Lwt.return (Api_common.err_result_of_string error_msg)

let eval_with_sim_manager_and_info ~(label : string)
    ?(stopped : Api.concrete_manager -> 'a Api.lwt_result =
      fun _ -> fail_lwt "Simulation stopped")
    ?(initializing : Api.concrete_manager -> 'a Api.lwt_result =
      fun _ -> fail_lwt "Simulation initalizing")
    ?(ready :
        Api.concrete_manager -> Api_types_j.simulation_info -> 'a Api.lwt_result =
      fun _ _ -> fail_lwt "Simulation ready") () : 'a Api.lwt_result =
  eval_with_sim_manager ~label (fun manager s ->
      match s.simulation_state with
      | SIMULATION_STATE_STOPPED -> stopped manager
      | SIMULATION_STATE_INITALIZING -> initializing manager
      | SIMULATION_STATE_READY simulation_info -> ready manager simulation_info)

let eval_when_ready ~(label : string)
    ?(handler : unit Api.result -> unit Lwt.t = fun _ -> Lwt.return_unit)
    (operation : Api.concrete_manager -> unit Api.lwt_result) : unit =
  Common.async __LOC__ (fun () ->
      eval_with_sim_manager_and_info ~label
        ~stopped:(fun _ -> Lwt.return (Result_util.ok ()))
        ~initializing:(fun _ -> Lwt.return (Result_util.ok ()))
        ~ready:(fun manager _ : unit Api.lwt_result -> operation manager)
        ()
      >>= handler)

(* to synch state of application with runtime *)
let sleep_time = 1.0

let rec sync () =
  match (React.S.value state).simulation_state with
  | SIMULATION_STATE_STOPPED | SIMULATION_STATE_INITALIZING ->
    Lwt.return (Result_util.ok ())
  | SIMULATION_STATE_READY _ ->
    State_project.eval_with_project ~label:"sync" (fun manager ->
        (* get current directory *)
        manager#simulation_info
        >>= Api_common.result_bind_with_lwt ~ok:(fun simulation_info ->
                let () =
                  set_state
                    {
                      simulation_state = SIMULATION_STATE_READY simulation_info;
                    }
                in
                if
                  simulation_info.Api_types_t.simulation_info_progress
                    .Api_types_t.simulation_progress_is_running
                then
                  Js_of_ocaml_lwt.Lwt_js.sleep sleep_time >>= sync
                else
                  Lwt.return (Result_util.ok ())))

let refresh () =
  State_project.eval_with_project ~label:"sync" (fun manager ->
      (* get current directory *)
      manager#simulation_info
      >>= Result_util.fold
            ~ok:(fun simulation_info ->
              let () =
                set_state
                  { simulation_state = SIMULATION_STATE_READY simulation_info }
              in
              sync ())
            ~error:(fun _ ->
              let () =
                set_state { simulation_state = SIMULATION_STATE_STOPPED }
              in
              Lwt.return (Result_util.ok ())))

let init () : unit Lwt.t = Lwt.return_unit

let continue_simulation (pause_condition : string) : unit Api.lwt_result =
  eval_with_sim_manager_and_info ~label:"continue_simulation"
    ~stopped:(fun _ ->
      let error_msg : string =
        "Failed to continue simulation, simulation stopped"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~initializing:(fun _ ->
      let error_msg : string =
        "Failed to continue simulation, simulation initializing"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~ready:(fun manager _ ->
      manager#simulation_continue pause_condition
      >>= Api_common.result_bind_with_lwt ~ok:sync)
    ()

let pause_simulation () : unit Api.lwt_result =
  eval_with_sim_manager_and_info ~label:"pause_simulation"
    ~stopped:(fun _ ->
      let error_msg : string =
        "Failed to pause simulation, simulation stopped"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~initializing:(fun _ ->
      let error_msg : string =
        "Failed to pause simulation, simulation initializing"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~ready:(fun manager (_ : Api_types_j.simulation_info) ->
      manager#simulation_pause)
    ()

let stop_simulation () : unit Api.lwt_result =
  eval_with_sim_manager_and_info ~label:"stop_simulation"
    ~stopped:(fun _ ->
      let error_msg : string =
        "Failed to pause simulation, simulation stopped"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~initializing:(fun _ ->
      let error_msg : string =
        "Failed to stop simulation, simulation initializing"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~ready:(fun manager (_ : Api_types_j.simulation_info) ->
      manager#simulation_delete
      >>= Api_common.result_bind_with_lwt ~ok:(fun () ->
              let () = update_simulation_state SIMULATION_STATE_STOPPED in
              Lwt.return (Result_util.ok ())))
    ()

let start_simulation (simulation_parameter : Api_types_j.simulation_parameter) :
    unit Api.lwt_result =
  eval_with_sim_manager_and_info ~label:"start_simulation"
    ~stopped:(fun manager ->
      let on_error error_msgs : unit Api.lwt_result =
        update_simulation_state SIMULATION_STATE_STOPPED;
        (* turn the lights off *)
        manager#simulation_delete >>= fun _ ->
        Lwt.return (Api_common.err_result_of_msgs error_msgs)
      in
      Lwt.catch
        (fun () ->
          (* set state to initalize *)
          update_simulation_state SIMULATION_STATE_INITALIZING;
          manager#simulation_start simulation_parameter
          >>= Api_common.result_bind_with_lwt ~ok:(fun _ ->
                  manager#simulation_info)
          >>= Api_common.result_bind_with_lwt ~ok:(fun simulation_status ->
                  let simulation_state =
                    SIMULATION_STATE_READY simulation_status
                  in
                  update_simulation_state simulation_state;
                  Lwt.return (Result_util.ok ()))
          >>= Result_util.fold
                ~ok:(fun _ -> Lwt.return (Result_util.ok ()))
                ~error:(fun error_msg ->
                  update_simulation_state SIMULATION_STATE_STOPPED;
                  on_error error_msg)
          >>= Api_common.result_bind_with_lwt ~ok:sync)
        (function
          | Invalid_argument error ->
            let msg = Format.sprintf "Runtime error %s" error in
            on_error [ Api_common.error_msg msg ]
          | Sys_error message -> on_error [ Api_common.error_msg message ]
          | _ -> on_error [ Api_common.error_msg "Initialization error" ]))
    ~initializing:(fun _ ->
      let error_msg : string =
        "Failed to start simulation, simulation initializing"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~ready:(fun _ _ ->
      let error_msg : string =
        "Failed to start simulation, simulation running"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ()

let intervene_simulation (code : string) : string Api.lwt_result =
  eval_with_sim_manager_and_info ~label:"perturb_simulation"
    ~stopped:(fun _ ->
      let error_msg : string =
        "Failed to start simulation, simulation running"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~initializing:(fun _ ->
      let error_msg : string =
        "Failed to start simulation, simulation initializing"
      in
      Lwt.return (Api_common.err_result_of_string error_msg))
    ~ready:(fun manager _ ->
      manager#simulation_intervention code
      >>= Api_common.result_bind_with_lwt ~ok:(fun out ->
              sync ()
              >>= Api_common.result_bind_with_lwt ~ok:(fun () ->
                      Lwt.return (Result_util.ok out))))
    ()
