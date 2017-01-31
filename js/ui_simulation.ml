(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let poll_interval : float = 0.5
type ready_state =
  { simulation_id : Api_types_j.simulation_id ;
    simulation_status : Api_types_j.simulation_info ; }

let create_ready_state
    (simulation_id : Api_types_j.simulation_id)
    (simulation_status : Api_types_j.simulation_info)
  : ready_state =
  { simulation_id = simulation_id ;
    simulation_status = simulation_status; }

type simulation_state =
  | SIMULATION_STOPPED              (* simulation is unavailable *)
  | SIMULATION_INITALIZING          (* simulation is blocked on an operation *)
  | SIMULATION_READY of ready_state (* the simulation is ready *)

type ui_status =
  | STOPPED
  | INITALIZING
  | RUNNING
  | PAUSED

type t = { setter : simulation_state -> unit
         ; signal : simulation_state React.signal }

let create () : t =
  let model_simulation_state , set_model_simulation_state =
    React.S.create (SIMULATION_STOPPED : simulation_state)
  in
  { setter = set_model_simulation_state ?step:None ;
    signal = model_simulation_state ; }

let simulation_status (t : t) : ui_status React.signal =
  React.S.map
    (function
      | SIMULATION_STOPPED -> STOPPED
      | SIMULATION_INITALIZING -> INITALIZING
      | SIMULATION_READY ready_state ->
        if ready_state
           .simulation_status
           .Api_types_j
           .simulation_info_progress
           .Api_types_j
           .simulation_progress_is_running then
          RUNNING
        else PAUSED)
    t.signal

let simulation_output (t : t) :
  Api_types_j.simulation_info option React.signal =
  React.S.map
    (function
      | SIMULATION_STOPPED ->
        None
      | SIMULATION_INITALIZING ->
        None
      | SIMULATION_READY ready_state ->
        Some ready_state.simulation_status
    )
    t.signal

(* Sugar for error message *)
let set_error loc msg =
  let () = Common.debug (Js.string msg) in
  Ui_state.set_model_error loc (Api_data.api_message_errors msg)

let lwt_error loc msg _ =
  let () = set_error loc msg in
  Lwt.return_unit


let ready_default (_loc : string) (msg : string) =
  let () = Common.debug (Js.string msg) in
  (fun _ -> Lwt.return_unit)
(* lwt_error loc msg supress messages for now *)

let ready_simulation
    ?(stopped : Api.manager -> unit Lwt.t =
       ready_default __LOC__ "Simulation stopped")
    ?(initializing : Api.manager -> unit Lwt.t =
       ready_default __LOC__ "Simulation initalizing")
    ?(ready : (Api.manager * ready_state) -> unit Lwt.t =
       ready_default __LOC__ "Simulation ready")
    (t : t) :
    unit Lwt.t =
  match !Ui_state.runtime_state with
  | None ->
    let () = Ui_state.set_model_error
                __LOC__
                (Api_data.api_message_errors "Runtime not available")
    in
    Lwt.return_unit
  | Some runtime ->
    (match React.S.value t.signal with
     | SIMULATION_STOPPED ->
       stopped runtime
     | SIMULATION_INITALIZING ->
       initializing runtime
     | SIMULATION_READY ready_state ->
       ready (runtime,ready_state))

let return_project ()  : Api_types_j.project_id Api.result Lwt.t =
    match React.S.value Ui_state.current_project_id with
      | None ->
        Lwt.return (Api_common.result_error_msg "missing project id")
      | Some project_id ->
        Lwt.return (Api_common.result_ok project_id)

let manager_operation
  (t : t)
  (handler :
     Api.manager ->
     Api_types_j.project_id ->
     Api_types_j.simulation_id ->
     unit Lwt.t) : unit =
    Common.async
      (fun () ->
         ready_simulation
           ~stopped:(fun _ -> Lwt.return_unit) (* ignore state errors *)
           ~initializing:(fun _ -> Lwt.return_unit)
           ~ready:
                 (fun (manager,ready_state) ->
                    (return_project ()) >>=
                    (Api_common.result_map
                       ~ok:(fun _  project_id ->
                           handler
                             manager
                             project_id
                             ready_state.simulation_id)
                       ~error:(fun _ _ ->
                               let msg = "unable to create context" in
                               let () = set_error  __LOC__ msg in
                           Lwt.return_unit))
                 )
               t
      )

let create_parameter (simulation_id : Api_types_j.simulation_id ) :
  Api_types_j.simulation_parameter =
  { Api_types_j.simulation_plot_period = React.S.value Ui_state.model_plot_period ;
    Api_types_j.simulation_pause_condition =
      React.S.value Ui_state.model_pause_condition ;
    Api_types_j.simulation_seed = React.S.value Ui_state.model_seed;
    Api_types_j.simulation_id = simulation_id ;
  }

let rec update_simulation (t : t) : unit Lwt.t =
  ready_simulation
    ~stopped:(fun _ -> Lwt.return_unit)
    ~initializing:(fun _ -> Lwt.return_unit)
    ~ready:
      (fun (runtime_state,ready_state) ->
         (return_project ()) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun project_id ->
                runtime_state#simulation_info
                  project_id
                  ready_state.simulation_id))>>=
         (fun result ->
            Api_common.result_map
              ~ok:(fun _ status ->
                  let () = Ui_state.clear_model_error () in
                  let () =
                    t.setter
                      (SIMULATION_READY
                         { ready_state with
                           simulation_status = status }) in
                  if status
                     .Api_types_j
                     .simulation_info_progress
                     .Api_types_j
                     .simulation_progress_is_running
                  then
                    (*sleep poll_interval*)Lwt.return_unit >>=
                    fun () -> update_simulation t
                  else
                    Lwt.return_unit
                )
              ~error:(fun _ errors ->
                  let () = Ui_state.set_model_error __LOC__ errors in
                  Lwt.return_unit)
              result)
      )
    t

let continue_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         (return_project ()) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun project_id ->
                runtime_state#simulation_continue
                  project_id
                  ready_state.simulation_id
                  (create_parameter ready_state.simulation_id))
         )>>=
         (Api_common.result_map
            ~ok:(fun _  _ ->
                let () = Ui_state.clear_model_error () in
                let () = Common.async (fun _ -> update_simulation t) in
                let () = Common.debug (Js.string "continue_simulation.3") in
                Lwt.return_unit)
            ~error:(fun _ errors ->
                let () = Ui_state.set_model_error __LOC__ errors in
                let () = Common.debug (Js.string "continue_simulation.3") in
                Lwt.return_unit))
      )
    t

let pause_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         (return_project ()) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun project_id ->
                runtime_state#simulation_pause
                  project_id
                  ready_state.simulation_id)
         )>>=
         (Api_common.result_map
            ~ok:(fun _  _ ->
                let () = Ui_state.clear_model_error () in
                Lwt.return_unit)
            ~error:(fun _ error ->
                let () = Ui_state.set_model_error __LOC__ error in
                Lwt.return_unit))
      )
    t

let stop_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         let () = t.setter SIMULATION_STOPPED in
         (return_project ()) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun project_id ->
                runtime_state#simulation_delete
                  project_id
                  ready_state.simulation_id)
         )>>=
         (Api_common.result_map
            ~ok:(fun _  _ ->
                let () = Ui_state.clear_model_error () in
                Lwt.return_unit)
            ~error:(fun _ error ->
                let () = Ui_state.set_model_error __LOC__ error in
                Lwt.return_unit))
      )
    t

let flush_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~stopped:(fun _ -> Lwt.return_unit)
    ~initializing:(fun _ -> Lwt.return_unit)
    ~ready:
      (fun (runtime_state,ready_state) ->
         let () = t.setter SIMULATION_STOPPED in
         (return_project ()) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun project_id ->
                runtime_state#simulation_delete
                  project_id
                  ready_state.simulation_id)
         )>>=
         (fun _ -> Lwt.return_unit))
    t

let perturb_simulation
    (t : t)
    ~(code : string)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         if ready_state
           .simulation_status
           .Api_types_j
           .simulation_info_progress
           .Api_types_j
           .simulation_progress_is_running
         then
           lwt_error
             __LOC__
             "pertubation can not be applied to running proccess"
             ()
         else
           (return_project ()) >>=
           (Api_common.result_bind_lwt
              ~ok:(fun project_id ->
                  runtime_state#simulation_perturbation
                    project_id
                    ready_state.simulation_id
                    { Api_types_j.perturbation_code = code }
                )
           )>>=
           (Api_common.result_map
              ~ok:(fun _  _ ->
                  let () = Ui_state.clear_model_error () in
                  update_simulation t)
              ~error:(fun _ error ->
                  let () = Ui_state.set_model_error __LOC__ error in
                  Lwt.return_unit))

      )
    t

let rec simulation_next_id
    (catalog : Api_types_j.simulation_catalog)
    (salt: int) : Api_types_j.simulation_id =
  (* lower the probability of race conditions and
     reduce probes of catalog *)
  let n = salt + Random.int 1000 in
  let simulation_id = string_of_int n in
  if List.mem simulation_id catalog then
    simulation_next_id catalog n
  else
    simulation_id

let simulation_next (runtime_state : Api.manager) :
  Api_types_j.project_id Api.result ->
  (Api_types_j.project_id * Api_types_j.simulation_id) Api.result Lwt.t =
  Api_common.result_bind_lwt
    ~ok:(fun (project_id : Api_types_j.project_id) ->
        (runtime_state#simulation_list project_id)
        >>=
        (Api_common.result_bind_lwt
           ~ok:(fun (catalog : Api_types_j.simulation_catalog)  ->
               let simulation_id  : Api_types_j.simulation_id = simulation_next_id catalog 0 in
               Lwt.return (Api_common.result_ok (project_id,simulation_id))

             )
        )
      )

let start_simulation
    (t : t)
  =
  let on_error (errors : Api_types_j.errors) =
    let () = Ui_state.set_model_error __LOC__ errors in
    let () = t.setter SIMULATION_STOPPED in
    Lwt.return_unit
  in
  ready_simulation
    ~stopped:
      (fun runtime_state ->
         Lwt.catch
           (fun () ->
              let () = Ui_state.clear_model_error () in
              let () = t.setter SIMULATION_INITALIZING in
              (return_project ()) >>=
              (simulation_next runtime_state)>>=
              (Api_common.result_bind_lwt
                 ~ok:(fun (project_id,simulation_id) ->
                     (runtime_state#simulation_start
                        project_id
                        (create_parameter simulation_id))
                     >>=
                     (Api_common.result_bind_lwt
                         ~ok:(fun simulation_id ->
                            (runtime_state#simulation_info
                              project_id
                              simulation_id)
                            >>=
                            (Api_common.result_bind_lwt
                               ~ok:(fun simulation_status ->
                                   let () = Ui_state.clear_model_error () in
                                   let () = Common.debug
                                       (Js.string "simulation started") in
                                   let () =
                                     t.setter
                                       (SIMULATION_READY
                                          (create_ready_state
                                             simulation_id
                                             simulation_status)) in
                                   let () =
                                     Common.async
                                       (fun _ -> update_simulation t) in
                                   Lwt.return (Api_common.result_ok ())
                                   )
                            )
                          )
                     )
                   )
              )>>=
              (Api_common.result_map
                 ~ok:(fun _ _ -> Lwt.return_unit)
                 ~error:(fun _ errors -> on_error errors)
              )
           )
           (function
             | Invalid_argument error ->
               let message = Format.sprintf "Runtime error %s" error in
               on_error
                 (Api_data.api_message_errors message)
             | Sys_error message ->
               on_error
                 (Api_data.api_message_errors message)
             | _ ->
               on_error
                 (Api_data.api_message_errors "Inialization error"))
      )
    t
