(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

module StringMap = Map.Make (struct type t = string let compare = compare end)

type simulation_state =
  | SIMULATION_STATE_STOPPED (* simulation is unavailable *)
  | SIMULATION_STATE_INITALIZING (* simulation is blocked on an operation *)
  | SIMULATION_STATE_READY of Api_types_j.simulation_info (* the simulation is ready *)

type t = { is_pinned : bool ; (* created by ui so it pinned *)
           simulation_id : Api_types_j.simulation_id ;
           simulation_state : simulation_state ; }

let t_simulation_id simulation = simulation.simulation_id
let t_simulation_state simulation = simulation.simulation_state
let t_simulation_info simulation : Api_types_j.simulation_info option =
  match simulation.simulation_state with
  | SIMULATION_STATE_STOPPED -> None
  | SIMULATION_STATE_INITALIZING -> None
  | SIMULATION_STATE_READY simulation_info -> Some simulation_info

type state = { state_current : string option ;
               state_simulations : t list ; }

let current_simulation (state : state) : t option =
  List_util.find_option
    (fun simulation -> Some simulation.simulation_id = state.state_current)
    state.state_simulations


type model = { model_current : t option ;
               model_simulations : Api_types_j.simulation_id list ; }
type model_state = STOPPED | INITALIZING | RUNNING | PAUSED

let model_state_to_string =
  function STOPPED -> "Stopped"
         | INITALIZING -> "Initalizing"
         | RUNNING -> "Running"
         | PAUSED -> "Paused"

let model_simulation_info model : Api_types_j.simulation_info option=
  Tools.option_bind t_simulation_info model.model_current
let model_simulation_state model : model_state option =
  Tools.option_map
    (function
      | SIMULATION_STATE_STOPPED -> STOPPED
      | SIMULATION_STATE_INITALIZING -> INITALIZING
      | SIMULATION_STATE_READY simulation_info ->
        if simulation_info.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_is_running then
          RUNNING
        else
          PAUSED)
    (Tools.option_map t_simulation_state model)


let state , set_state =
  React.S.create { state_current = None ;
                   state_simulations = [] ; }

let update_simulation_state
    (simulation_id : Api_types_j.simulation_id)
    (new_state : simulation_state) : unit =
  let current_state = React.S.value state in
  let update_t t =
    if t.simulation_id = simulation_id then
      { t with simulation_state = new_state }
    else
      t
  in
  let state_simulations = List.map update_t current_state.state_simulations in
  let () = set_state { current_state with state_simulations = state_simulations } in
  ()


let model : model React.signal =
  React.S.bind
    state
    (fun simulation_state ->
       React.S.const
       { model_current = current_simulation simulation_state ;
         model_simulations =
           List.map
             (fun simulation -> simulation.simulation_id)
             simulation_state.state_simulations
       }
    )

let with_simulation :
  'a . label:string ->
  (Api.manager -> Api_types_j.project_id -> t -> 'a  Api.result Lwt.t) ->
  'a  Api.result Lwt.t  =
  fun ~label handler ->
    let project_handler manager project_id =
      let current_state = React.S.value state in
      match
        Tools.option_bind
          (fun simulation_id ->
             List_util.find_option
               (fun t -> simulation_id =  t_simulation_id t)
               current_state.state_simulations)
          current_state.state_current
      with
      | None ->
        let error_msg : string =
          Format.sprintf
            "Failed %s due to unavailable simulation."
            label
        in
        Lwt.return (Api_common.result_error_msg error_msg)
      | Some simulation_id -> handler manager project_id simulation_id
    in
    State_project.with_project ~label project_handler

let fail_lwt error_msg = Lwt.return (Api_common.result_error_msg error_msg)

let with_simulation_info
  ~(label : string)
  ?(stopped : Api.manager ->
    Api_types_j.project_id ->
    Api_types_j.simulation_id ->
    unit Api.result Lwt.t =
    fun _ _ _ -> fail_lwt "Simulation stopped")
  ?(initializing : Api.manager ->
    Api_types_j.project_id ->
    Api_types_j.simulation_id ->
    unit Api.result Lwt.t =
    fun _ _ _ -> fail_lwt "Simulation initalizing")
  ?(ready : Api.manager ->
    Api_types_j.project_id ->
    Api_types_j.simulation_id ->
    Api_types_j.simulation_info ->
    unit Api.result Lwt.t =
    fun _ _ _ _ -> fail_lwt "Simulation ready")
  () =
  with_simulation
  ~label
  (fun (manager : Api.manager) (project_id : Api_types_j.project_id) (t : t) ->
     let simulation_id = t_simulation_id  t in
     match t.simulation_state with
     | SIMULATION_STATE_STOPPED -> stopped manager project_id simulation_id
     | SIMULATION_STATE_INITALIZING -> initializing manager project_id simulation_id
     | SIMULATION_STATE_READY simulation_info -> ready manager project_id simulation_id simulation_info)

let when_ready
    ~(label : string)
    ?(handler : unit Api.result -> unit Lwt.t = fun _ -> Lwt.return_unit)
    (operation : Api.manager -> Api_types_j.project_id -> Api_types_j.simulation_id -> unit Api.result Lwt.t) : unit =
    Common.async
      (fun () ->
         with_simulation_info
           ~label
           ~stopped:(fun _ _ _-> Lwt.return (Api_common.result_ok ()))
           ~initializing:(fun _ _ _ -> Lwt.return (Api_common.result_ok ()))
           ~ready:(fun manager project_id simulation_id _ ->
               (operation manager project_id simulation_id : unit Api.result Lwt.t)
             )
           () >>= handler
      )

let rec index_simulation
    ?(map : Api_types_j.simulation_info StringMap.t = StringMap.empty)
    (manager : Api.manager)
    (project_id : Api_types_j.project_id)
    (simulation_ids : Api_types_j.simulation_id list) : Api_types_j.simulation_info StringMap.t Api.result Lwt.t
  =
  match simulation_ids with
  | [] -> Lwt.return (Api_common.result_ok map)
  | simulation_id::tail_simulation_ids ->
    (manager#simulation_info project_id simulation_id) >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (simulation_info : Api_types_j.simulation_info) ->
           let new_map = StringMap.add simulation_id simulation_info map in
           index_simulation
             ~map:new_map
             manager
             project_id
             tail_simulation_ids
         )
    )

(* If simulation is not in state then add it as an unpinned
   simulation.
*)
let augment_simulation_list
    (simulation_index : Api_types_j.simulation_info StringMap.t)
    (simulation_list : t list) : t list =
  (* simulation id's in the current state *)
  let state_simulation_ids = List.map t_simulation_id simulation_list in
  (* predicate that checks if id is not in state id's *)
  let not_in_state_ids simulation_id _ =
    not (List.mem simulation_id state_simulation_ids) in
  (* add simulations that currently not part of the state *)
  let add_simulation_ids =
    StringMap.filter not_in_state_ids simulation_index in
  StringMap.fold
     (fun simulation_id simulation_info acc ->
       { is_pinned = false ;
         simulation_id = simulation_id ;
         simulation_state = SIMULATION_STATE_READY simulation_info } :: acc)
     add_simulation_ids simulation_list

(* Remove simulation from state if it not pinned or available. *)
let restrict_simulation_list
    (simulation_index : Api_types_j.simulation_info StringMap.t)
    (simulation_list : t list) : t list =
  List.filter
    (fun t ->
       t.is_pinned ||
       StringMap.mem t.simulation_id simulation_index)
    simulation_list

let refresh_simulation_list
    (simulation_index : Api_types_j.simulation_info StringMap.t)
    (simulation_list : t list) : t list =
  List.map
    (fun t -> let simulation_id = t_simulation_id t in
      if StringMap.mem simulation_id simulation_index then
        { t with
          simulation_state = SIMULATION_STATE_READY (StringMap.find simulation_id simulation_index) }
      else
        t
    )
    simulation_list

let update_simulation_list
    (manager : Api.manager)
    (simulation_list : t list)
    (project_id : Api_types_j.project_id)
    (simulation_ids : Api_types_j.simulation_id list) :
  t list Api.result Lwt.t =
  index_simulation ?map:None manager project_id simulation_ids >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (simulation_index : Api_types_j.simulation_info StringMap.t) ->
         Lwt.return (Api_common.result_ok
                       (refresh_simulation_list
                          simulation_index
                          (augment_simulation_list
                             simulation_index
                             (restrict_simulation_list
                                simulation_index
                                simulation_list
                             )
                          )
                       )
                    )
       )
  )

(* to synch state of application with runtime *)
let sync () : unit Api.result Lwt.t =
  let old_state : state = React.S.value state in
    State_project.with_project ~label:"synch"
      (fun manager project_id ->
         (* get current directory *)
         (manager#simulation_catalog project_id) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun (catalog : Api_types_j.simulation_catalog) ->
                update_simulation_list
                  manager
                  old_state.state_simulations
                  project_id
                  catalog.Api_types_j.simulation_ids >>=
                (Api_common.result_bind_lwt
                   ~ok:(fun (new_state_simulations : t list) ->
                       let first_simulation =
                         (match new_state_simulations with
                          | [] -> None
                          | first::_ -> Some first.simulation_id)
                       in
                       let new_state_current : string option =
                         match old_state.state_current with
                         | None -> first_simulation
                         | Some simulation_id ->
                           if List.exists
                               (fun t -> t.simulation_id = simulation_id)
                               new_state_simulations
                           then
                             Some simulation_id
                           else
                             first_simulation
                       in
                       let () =
                         set_state
                           { state_current = new_state_current ;
                             state_simulations = new_state_simulations ; }
                       in
                       Lwt.return (Api_common.result_ok ())
                      )
                )
              )
         )
      )

let create_simulation (simulation_id : Api_types_j.simulation_id) : unit Api.result Lwt.t =
  (* add to the list if it is not there *)
  let current_state : state = React.S.value state in
  let simulation_ids : Api_types_j.simulation_id list =
    List.map t_simulation_id current_state.state_simulations in
  if List.mem simulation_id simulation_ids then
    Lwt.return (Api_common.result_ok ())
  else
    State_project.with_project ~label:"remove_file"
      (fun manager project_id ->
         (manager#simulation_catalog project_id)
         >>=
         (Api_common.result_bind_lwt
            ~ok:(fun (simulation_catalog : Api_types_j.simulation_catalog) ->
                let simulation_ids = simulation_catalog.Api_types_j.simulation_ids in
                let simulation_state : (simulation_state, Api.manager_code) Api_types_j.result Lwt.t =
                  if List.mem simulation_id simulation_ids then
                    (manager#simulation_info project_id simulation_id) >>=
                    (Api_common.result_bind_lwt
                       ~ok:(fun (simulation_info : Api_types_j.simulation_info) ->
                           Lwt.return (Api_common.result_ok (SIMULATION_STATE_READY simulation_info))
                         )
                    )
                  else
                    Lwt.return (Api_common.result_ok SIMULATION_STATE_STOPPED)
                in
                simulation_state >>=
                (Api_common.result_bind_lwt
                   ~ok:(fun simulation_state ->
                       Lwt.return (Api_common.result_ok
                                     { is_pinned = true ;
                                       simulation_id = simulation_id ;
                                       simulation_state = simulation_state;
                       })
                     )
                )
              )
         ) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun (simulation : t) ->
                 let current_state = React.S.value state in
                 let () = set_state
                     { current_state with
                       state_simulations = simulation::current_state.state_simulations}
                 in
                 Lwt.return (Api_common.result_ok ())
                )
         ) >>=
         (Api_common.result_bind_lwt ~ok:sync)
      )

let set_simulation (simulation_id : Api_types_j.simulation_id) : unit Api.result Lwt.t =
  let current_state : state = React.S.value state in
  let simulation_ids : Api_types_j.simulation_id list =
    List.map t_simulation_id current_state.state_simulations in
  (if List.mem simulation_id simulation_ids then
     let () = set_state
         { current_state with state_current =  Some simulation_id }
     in
     Lwt.return (Api_common.result_ok ())
   else
     let error_msg : string =
       Format.sprintf "Simulation %s does not exist" simulation_id in
     Lwt.return (Api_common.result_error_msg error_msg))
  >>=
  (Api_common.result_bind_lwt ~ok:sync)

let check_simulation_state () : unit Api.result Lwt.t =
  match (React.S.value state).state_current with
  | Some _ -> Lwt.return (Api_common.result_ok ())
  | None ->
    let error_msg : string = "No simulation selected" in
    Lwt.return (Api_common.result_error_msg error_msg)

let remove_simulation_manager () : unit Api.result Lwt.t =
  let handler
      (manager : Api.manager)
      (project_id : Api_types_j.project_id)
      (t : t) : unit Api.result Lwt.t =
    let simulation_id : Api_types_j.simulation_id = t_simulation_id t in
    (manager#simulation_catalog project_id)
    >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (simulation_catalog : Api_types_j.simulation_catalog) ->
           let simulation_ids : Api_types_j.simulation_id list =
             simulation_catalog.Api_types_j.simulation_ids in
           if List.mem simulation_id simulation_ids then
             manager#simulation_delete project_id simulation_id
           else
             Lwt.return (Api_common.result_ok ())
         )
    )
  in
  with_simulation
    ~label:"remove_simulation_manager"
    handler


let remove_simulation_state () : unit Api.result Lwt.t =
  let handler
      (_ : Api.manager)
      (_ : Api_types_j.project_id)
      (t : t) : unit Api.result Lwt.t =
    let simulation_id : Api_types_j.simulation_id = t_simulation_id t in
    let current_state : state = React.S.value state in
    let state_current = None in
    let state_simulations =
      List.filter
        (fun t -> simulation_id <> t_simulation_id t)
        current_state.state_simulations in
    let () = set_state
        { state_current = state_current ;
          state_simulations = state_simulations ; } in
    Lwt.return (Api_common.result_ok ())
  in
  with_simulation
    ~label:"remove_simulation_manager"
    handler

let remove_simulation () : unit Api.result Lwt.t =
  Lwt.return (Api_common.result_ok ()) >>=
  (* check simulation state *)
  (Api_common.result_bind_lwt ~ok:check_simulation_state) >>=
  (* remove_simulation_manager *)
  (Api_common.result_bind_lwt ~ok:remove_simulation_manager) >>=
  (* remove_simulation_state_state *)
  (Api_common.result_bind_lwt ~ok:remove_simulation_state) >>=
  (* sync *)
  (Api_common.result_bind_lwt ~ok:sync)

(* run on application init *)
let load_simulations () : unit Lwt.t =
  let simulations = Common_state.url_args ~default:["default"] "simulation" in
  let rec add_simulations simulations : unit Lwt.t =
    match simulations with
    | [] -> Lwt.return_unit
    | simulation_id::simulations ->
      (create_simulation simulation_id) >>=
      (Api_common.result_map
         ~ok:(fun _ () -> add_simulations simulations)
         ~error:(fun _ (errors : Api_types_j.errors) ->
             let msg = Format.sprintf "creating file %s error %s" simulation_id (Api_types_j.string_of_errors errors) in
             let () = Common.debug (Js.string (Format.sprintf "State_file.load_files %s" msg)) in
             add_simulations simulations)
      )
  in
  add_simulations simulations


let init () : unit Lwt.t =
  Lwt.return_unit >>=
  load_simulations

let continue_simulation (simulation_parameter : Api_types_j.simulation_parameter) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"continue_simulation"
    ~stopped:
      (fun _ _ _  ->
         let error_msg : string =
           "Failed to continue simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ _ ->
         let error_msg : string =
            "Failed to continue simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun
        (manager : Api.manager)
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        _
        ->
          manager#simulation_continue
          project_id
          simulation_id
          simulation_parameter
        >>=
        (Api_common.result_bind_lwt
           ~ok:(fun () -> sync ())))
    ()

let pause_simulation () : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"pause_simulation"
    ~stopped:
      (fun _ _ _ ->
        let error_msg : string =
          "Failed to pause simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ _ ->
         let error_msg : string =
           "Failed to pause simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun
        (manager : Api.manager)
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (_ : Api_types_j.simulation_info)
        ->
          manager#simulation_pause
            project_id
            simulation_id
          >>=
          (Api_common.result_bind_lwt ~ok:(fun () -> sync ())))
    ()

let stop_simulation () : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"stop_simulation"
    ~stopped:
      (fun _ _ _ ->
        let error_msg : string =
          "Failed to pause simulation, simulation stopped"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ _ ->
         let error_msg : string =
           "Failed to stop simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun
        (manager : Api.manager)
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (_ : Api_types_j.simulation_info)
        ->
          manager#simulation_delete
            project_id
            simulation_id
          >>=
          (Api_common.result_bind_lwt ~ok:(fun () ->
               let () = update_simulation_state simulation_id SIMULATION_STATE_STOPPED in
               sync ())))
    ()

let start_simulation (simulation_parameter : Api_types_j.simulation_parameter) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"start_simulation"
    ~stopped:
      (fun
        (manager : Api.manager)
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) ->
        let simulation_parameter = { simulation_parameter with Api_types_j.simulation_id = simulation_id } in
        (* set state to initalize *)
        let () = update_simulation_state simulation_id SIMULATION_STATE_INITALIZING in
        manager#simulation_start
          project_id
          simulation_parameter
        >>=
        (Api_common.result_map
           ~ok:(fun _ _ ->
               let () = update_simulation_state simulation_id SIMULATION_STATE_INITALIZING in
               Lwt.return (Api_common.result_ok ()))
           ~error:(fun _ error_msg ->
               let () = update_simulation_state simulation_id SIMULATION_STATE_STOPPED in
               Lwt.return (Api_common.result_messages error_msg)))
        >>=
        (Api_common.result_bind_lwt ~ok:sync)
      )


          (* Api_common.result_bind_lwt ~ok:(fun (_ : Api_types_j.simulation_id) -> sync ()))) *)
    ~initializing:
      (fun _ _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun _ _ _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation running"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ()

let perturb_simulation (code : string) : unit Api.result Lwt.t =
  with_simulation_info
    ~label:"perturb_simulation"
    ~stopped:
      (fun  _ _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation running"
         in
         Lwt.return (Api_common.result_error_msg error_msg))
    ~initializing:
      (fun _ _ _ ->
         let error_msg : string =
           "Failed to start simulation, simulation initializing"
        in
        Lwt.return (Api_common.result_error_msg error_msg))
    ~ready:
      (fun
        (manager : Api.manager)
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        _ ->
        manager#simulation_perturbation
          project_id
          simulation_id
          { Api_types_j.perturbation_code = code }
        >>=
        (Api_common.result_bind_lwt ~ok:(fun () -> sync ())))
    ()
