open Lwt

let poll_interval : float = 0.5
type ready_state = { simulation_token : ApiTypes_j.token ;
                     simulation_state : ApiTypes_j.state ; }
let create_ready_state (token : ApiTypes_j.token) : ready_state =
  { simulation_token = token ;
    simulation_state =
      { ApiTypes_j.plot = None;
        ApiTypes_j.distances = None;
        ApiTypes_j.time = 0.0;
        ApiTypes_j.time_percentage = None;
        ApiTypes_j.event = 0;
        ApiTypes_j.event_percentage = None;
        ApiTypes_j.tracked_events = None;
        ApiTypes_j.log_messages = [];
        ApiTypes_j.snapshots = [];
        ApiTypes_j.flux_maps = [];
        ApiTypes_j.files = [];
        ApiTypes_j.is_running = true ;
      } ; }

type simulation_state =
  | SIMULATION_STOPPED (* simulation is unavailable *)
  | SIMULATION_INITALIZING (* simulation is blocked on an operation *)
  | SIMULATION_READY of ready_state (* the simulation is ready *)

type simulation_status =
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

let simulation_status (t : t) : simulation_status React.signal =
  React.S.map
    (function
      | SIMULATION_STOPPED -> STOPPED
      | SIMULATION_INITALIZING -> INITALIZING
      | SIMULATION_READY ready_state ->
        if ready_state.simulation_state.ApiTypes_j.is_running then
          RUNNING
        else PAUSED)
    t.signal

let simulation_output (t : t) : ApiTypes_j.state option React.signal =
  React.S.map
    (function
      | SIMULATION_STOPPED ->
        None
      | SIMULATION_INITALIZING ->
        None
      | SIMULATION_READY ready_state ->
        Some ready_state.simulation_state
    )
    t.signal
(* Sugar for error message *)
let lwt_error msg _ =
  let () = Ui_state.set_model_error (Api_data.api_message_errors msg) in
  Lwt.return_unit


let ready_default (msg : string) =
  let () = Common.debug (Js.string msg) in
  lwt_error msg

let ready_simulation
    ?(stopped : Api_v1.api_runtime -> unit Lwt.t =
       ready_default "Simulation stopped")
    ?(initializing : Api_v1.api_runtime -> unit Lwt.t =
       ready_default "Simulation initalizing")
    ?(ready : (Api_v1.api_runtime * ready_state) -> unit Lwt.t =
       ready_default "Simulation ready")
    (t : t) :
    unit Lwt.t =
  match !Ui_state.runtime_state with
  | None ->
    let () = Ui_state.set_model_error
        (Api_data.api_message_errors "Runtime not available")
    in
    Lwt.return_unit
  | Some runtime ->
    (match React.S.value t.signal with
     | SIMULATION_STOPPED -> stopped runtime
     | SIMULATION_INITALIZING -> initializing runtime
     | SIMULATION_READY ready_state -> ready (runtime,ready_state))


let create_parameter () =
  { ApiTypes_j.code = React.S.value Ui_state.model_text;
    ApiTypes_j.nb_plot = React.S.value Ui_state.model_nb_plot;
    ApiTypes_j.max_time = React.S.value Ui_state.model_max_time;
    ApiTypes_j.max_events = React.S.value Ui_state.model_max_events
  }

let rec update_simulation (t : t) : unit Lwt.t =
  ready_simulation
    ~stopped:(fun _ -> Lwt.return_unit)
    ~initializing:(fun _ -> Lwt.return_unit)
    ~ready:
      (fun (runtime_state,ready_state) ->
         let () = Common.debug (Js.string "update_simulation.1") in
         Lwt.join [Lwt_js.sleep poll_interval;
           (runtime_state#status ready_state.simulation_token)
           >>=
           (fun result ->
              (* could have stopped while getting status *)
              ready_simulation
                ~stopped:(fun _ -> Lwt.return_unit)
                ~initializing:(fun _ -> Lwt.return_unit)
                ~ready:(fun (_,ready_state) ->
                    match result with
                      `Left e ->
                      let () = Ui_state.set_model_error e in
                      Lwt.return_unit
                    | `Right state ->
                      let () = Ui_state.set_model_error [] in
                      let () =
                        t.setter
                          (SIMULATION_READY
                             { ready_state with simulation_state = state }) in
                      if state.ApiTypes_j.is_running then
                        update_simulation t
                      else
                        Lwt.return_unit
                  )
                t
           )]
      )
    t

let continue_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         let () = Common.debug (Js.string "continue_simulation.1") in
         (runtime_state#continue
            ready_state.simulation_token
            (create_parameter ()))
         >>=
         (fun response ->
            match response with
            | `Left error ->
              let () = Common.debug (Js.string "continue_error.2") in
              let () = Ui_state.set_model_error error in
              Lwt.return_unit
            | `Right _ ->
              let () = Ui_state.set_model_error [] in
              let () = Lwt.async (fun _ -> update_simulation t) in
              let () = Common.debug (Js.string "continue_simulation.3") in
              Lwt.return_unit
         )
      )
    t

let pause_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         (runtime_state#pause ready_state.simulation_token)
         >>=
         (fun response ->
            let () = match response with
              | `Left error ->
                Ui_state.set_model_error error
              | `Right _ ->
                Ui_state.set_model_error []
            in
            Lwt.return_unit)
      )
    t

let stop_simulation
    (t : t)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         let () = t.setter SIMULATION_STOPPED in
         (runtime_state#stop ready_state.simulation_token) >>=
         (fun result ->
            match result with
              `Left error ->
              let () = Ui_state.set_model_error error in
              Lwt.return_unit
            | `Right () ->
              let () = Ui_state.set_model_error [] in
              Lwt.return_unit))
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
         (runtime_state#stop ready_state.simulation_token) >>=
         (fun _ -> Lwt.return_unit))
    t

let perturb_simulation
    (t : t)
    ~(code : string)
  : unit Lwt.t =
  ready_simulation
    ~ready:
      (fun (runtime_state,ready_state) ->
         if ready_state.simulation_state.ApiTypes_j.is_running then
           lwt_error "pertubation can not be applied to running proccess" ()
         else
         (runtime_state#perturbate
            ready_state.simulation_token
            { ApiTypes_j.perturbation_code = code ; }
         )
         >>=
         (fun response ->
            match response with
            | `Left error ->
              let () = Ui_state.set_model_error error in
              Lwt.return_unit
            | `Right _ ->
              let () = Ui_state.set_model_error [] in
              update_simulation t))
    t


let start_simulation
    (t : t)
  =
  let on_error (errors : ApiTypes_t.errors) =
    let () = Ui_state.set_model_error errors in
    let () = t.setter SIMULATION_STOPPED in
    Lwt.return_unit
  in
  ready_simulation
    ~stopped:
      (fun runtime_state ->
         catch
           (fun () ->
              let () = Ui_state.set_model_error [] in
              let () = t.setter SIMULATION_INITALIZING in
              (runtime_state#start (create_parameter ())) >>=
              (fun result ->
                 match result with
                   `Left errors -> on_error errors
                 | `Right token ->
                   let () = Ui_state.set_model_error [] in
                   let () = Common.debug (Js.string "simulation started") in
                   let () =
                     t.setter
                       (SIMULATION_READY
                          (create_ready_state token)) in
                   let () = Lwt.async (fun _ -> update_simulation t) in
                   Lwt.return_unit
              )
           )
           (function
             | Invalid_argument error ->
               let message = Format.sprintf "Runtime error %s" error in
               on_error (Api_data.api_message_errors message)
             | Sys_error message ->
               on_error (Api_data.api_message_errors message)
             | _ ->
               on_error (Api_data.api_message_errors "Inialization error"))
      )
    t
