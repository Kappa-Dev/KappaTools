(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type simulation_state =
  | SIMULATION_STATE_STOPPED (* simulation is unavailable *)
  | SIMULATION_STATE_INITALIZING (* simulation is blocked on an operation *)
  | SIMULATION_STATE_READY of Api_types_j.simulation_info
  (* the simulation is ready *)

type t = { simulation_state : simulation_state ; }

let t_simulation_state simulation = simulation.simulation_state
let t_simulation_info simulation : Api_types_j.simulation_info option =
  match simulation.simulation_state with
  | SIMULATION_STATE_STOPPED -> None
  | SIMULATION_STATE_INITALIZING -> None
  | SIMULATION_STATE_READY simulation_info -> Some simulation_info

type state = t

type model = state
type model_state = STOPPED | INITALIZING | RUNNING | PAUSED

let model_state_to_string =
  function STOPPED -> "Stopped"
         | INITALIZING -> "Initalizing"
         | RUNNING -> "Running"
         | PAUSED -> "Paused"

let dummy_model = { simulation_state = SIMULATION_STATE_STOPPED }

let model_simulation_info model : Api_types_j.simulation_info option=
  t_simulation_info model
let model_simulation_state model : model_state =
  match t_simulation_state model with
  | SIMULATION_STATE_STOPPED -> STOPPED
  | SIMULATION_STATE_INITALIZING -> INITALIZING
  | SIMULATION_STATE_READY simulation_info ->
    if simulation_info.Api_types_j.simulation_info_progress.
         Api_types_j.simulation_progress_is_running then
      RUNNING
    else
      PAUSED

let state, set_state = React.S.create dummy_model

let update_simulation_state
    (simulation_state : simulation_state) : unit =
  let () = set_state { simulation_state } in
  ()

let model : model React.signal = state

let with_simulation :
  'a . label:string ->
  (Api.concrete_manager -> Api_types_j.project_id ->
   t -> 'a  Api.result Lwt.t) ->
  'a  Api.result Lwt.t  =
  fun ~label handler ->
    let project_handler manager project_id =
      handler manager project_id (React.S.value state)
    in
    State_project.with_project ~label project_handler

let fail_lwt error_msg = Lwt.return (Api_common.result_error_msg error_msg)

let with_simulation_info
  ~(label : string)
  ?(stopped : Api.concrete_manager ->
    Api_types_j.project_id ->
    unit Api.result Lwt.t =
    fun _ _ -> fail_lwt "Simulation stopped")
  ?(initializing : Api.concrete_manager ->
    Api_types_j.project_id ->
    unit Api.result Lwt.t =
    fun _ _ -> fail_lwt "Simulation initalizing")
  ?(ready : Api.concrete_manager ->
    Api_types_j.project_id ->
    Api_types_j.simulation_info ->
    unit Api.result Lwt.t =
    fun _ _ _ -> fail_lwt "Simulation ready")
  () =
  with_simulation
  ~label
  (fun manager (project_id : Api_types_j.project_id) s ->
     match s.simulation_state with
     | SIMULATION_STATE_STOPPED -> stopped manager project_id
     | SIMULATION_STATE_INITALIZING -> initializing manager project_id
     | SIMULATION_STATE_READY simulation_info ->
       ready manager project_id simulation_info)

let when_ready
    ~(label : string)
    ?(handler : unit Api.result -> unit Lwt.t = fun _ -> Lwt.return_unit)
    (operation : Api.concrete_manager -> Api_types_j.project_id ->
     unit Api.result Lwt.t) : unit =
    Common.async
      (fun () ->
         with_simulation_info
           ~label
           ~stopped:(fun _ _-> Lwt.return (Api_common.result_ok ()))
           ~initializing:(fun _ _ -> Lwt.return (Api_common.result_ok ()))
           ~ready:(fun manager project_id _ ->
               (operation manager project_id : unit Api.result Lwt.t)
             )
           () >>= handler
      )

(* to synch state of application with runtime *)
let sleep_time = 1.0
let rec sync ~project_id =
  match (React.S.value state).simulation_state with
  | SIMULATION_STATE_STOPPED | SIMULATION_STATE_INITALIZING ->
    Lwt.return (Api_common.result_ok ())
  | SIMULATION_STATE_READY _ ->
    State_project.with_project ~label:"sync"
      (fun manager new_project_id ->
         (* get current directory *)
         (manager#simulation_info new_project_id) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun simulation_info ->
                let () = set_state
                    {simulation_state =
                       SIMULATION_STATE_READY simulation_info} in
                if simulation_info.Api_types_t.simulation_info_progress
                   .Api_types_t.simulation_progress_is_running &&
                project_id = new_project_id then
                  Lwt_js.sleep sleep_time >>=
                  fun  () -> sync ~project_id:new_project_id
                else Lwt.return (Api_common.result_ok ())
              )
         )
      )

let refresh () =
  State_project.with_project ~label:"sync"
    (fun manager project_id ->
       (* get current directory *)
       (manager#simulation_info project_id) >>=
       (Api_common.result_map
          ~ok:(fun _ simulation_info ->
              let () = set_state
                  {simulation_state =
                     SIMULATION_STATE_READY simulation_info} in
              sync ~project_id)
          ~error:(fun _ _ ->
              let () = set_state
                  {simulation_state = SIMULATION_STATE_STOPPED} in
              Lwt.return (Api_common.result_ok ())
            )))

let init () : unit Lwt.t = Lwt.return_unit

let continue_simulation (simulation_parameter : Api_types_j.simulation_parameter) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"continue_simulation"
    ~stopped:
      (fun _ _ ->
         let error_msg : string =
           "Failed to continue simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ ->
         let error_msg : string =
            "Failed to continue simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun manager (project_id : Api_types_j.project_id) _ ->
          manager#simulation_continue
          project_id
          simulation_parameter
        >>=
        (Api_common.result_bind_lwt ~ok:(fun () -> sync ~project_id)))
    ()

let pause_simulation () : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"pause_simulation"
    ~stopped:
      (fun _ _ ->
        let error_msg : string =
          "Failed to pause simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ ->
         let error_msg : string =
           "Failed to pause simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun manager (project_id : Api_types_j.project_id)
        (_ : Api_types_j.simulation_info) ->
        manager#simulation_pause project_id)
    ()

let stop_simulation () : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"stop_simulation"
    ~stopped:
      (fun _ _ ->
        let error_msg : string =
          "Failed to pause simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ ->
         let error_msg : string =
           "Failed to stop simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun manager (project_id : Api_types_j.project_id)
        (_ : Api_types_j.simulation_info)
        ->
          manager#simulation_delete project_id >>=
          (Api_common.result_bind_lwt ~ok:(fun () ->
               let () = update_simulation_state SIMULATION_STATE_STOPPED in
               Lwt.return (Api_common.result_ok ()))))
    ()

let start_simulation (simulation_parameter : Api_types_j.simulation_parameter) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"start_simulation"
    ~stopped:
      (fun manager
        (project_id : Api_types_j.project_id) ->
        let on_error (error_msgs : Api_types_j.errors) : unit Api.result Lwt.t =
          let () =
            update_simulation_state SIMULATION_STATE_STOPPED in
          (* turn the lights off *)
          (manager#simulation_delete project_id)>>=
          (fun _ -> Lwt.return (Api_common.result_messages error_msgs))
        in
        Lwt.catch
          (fun () ->
             (* set state to initalize *)
             let () =
               update_simulation_state SIMULATION_STATE_INITALIZING in
             (manager#simulation_start project_id simulation_parameter)
             >>=
             (Api_common.result_bind_lwt
                ~ok:(fun _ -> manager#simulation_info project_id))
             >>=
             (Api_common.result_bind_lwt
                ~ok:(fun simulation_status ->
                    let simulation_state =
                      SIMULATION_STATE_READY simulation_status in
                    let () =
                      update_simulation_state simulation_state in
                    Lwt.return (Api_common.result_ok ())
                  )
             )
             >>=
             (Api_common.result_map
                ~ok:(fun _ _ -> Lwt.return (Api_common.result_ok ()))
                ~error:(fun _ error_msg ->
                    let () =
                      update_simulation_state SIMULATION_STATE_STOPPED in
                    on_error error_msg))
             >>=
             (Api_common.result_bind_lwt ~ok:(fun () -> sync ~project_id))
          )
          (function
            | Invalid_argument error ->
              let msg = Format.sprintf "Runtime error %s" error in
              on_error [(Api_common.error_msg msg)]
            | Sys_error message -> on_error [(Api_common.error_msg message)]
            | _ -> on_error [(Api_common.error_msg "Initialization error")])
      )
    ~initializing:
      (fun _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun _ _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation running"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ()

let perturb_simulation (code : string) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"perturb_simulation"
    ~stopped:
      (fun _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation running"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun manager (project_id : Api_types_j.project_id)
        _ ->
        manager#simulation_perturbation
          project_id
          { Api_types_j.perturbation_code = code }
        >>=
        (Api_common.result_bind_lwt ~ok:(fun () -> sync ~project_id)))
    ()
