(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type parameters = {
  plot_period : float;
  pause_condition : string;
  seed : int option;
  store_trace : bool;
}

type a_project = {
  project_id : Api_types_j.project_id;
  project_manager : Api.concrete_manager;
}

type state = {
  project_current : a_project option;
  project_catalog : a_project list;
  project_version : int ;
  project_contact_map : Api_types_j.contact_map option;
  default_parameters : parameters;
  project_parameters : parameters Mods.StringMap.t;
}

type model = {
  model_project_id : Api_types_j.project_id option ;
  model_project_ids : Api_types_j.project_id list ;
  model_project_version : int ;
  model_contact_map : Api_types_j.contact_map option ;
  model_parameters : parameters ;
}

let project_equal a b = a.project_id = b.project_id
let catalog_equal = List.for_all2 project_equal
let state_equal a b =
  Option_util.equal project_equal a.project_current b.project_current &&
  a.project_version = b.project_version &&
  Mods.StringMap.equal
    (fun x y  -> compare x y = 0)
    a.project_parameters b.project_parameters &&
  catalog_equal a.project_catalog b.project_catalog

let default_parameters = {
  plot_period = 1.;
  pause_condition = "[false]";
  seed = None;
  store_trace = false;
}

let state , set_state =
  React.S.create ~eq:state_equal {
    project_current = None;
    project_catalog = [];
    project_version = -1;
    project_contact_map = None;
    default_parameters;
    project_parameters = Mods.StringMap.empty;
  }

let update_parameters handler =
  let st = React.S.value state in
  let default_parameters,project_parameters =
    match st.project_current with
    | None -> handler st.default_parameters, st.project_parameters
    | Some proj ->
      st.default_parameters,
      Mods.StringMap.add proj.project_id
        (handler (Mods.StringMap.find_default
                    st.default_parameters proj.project_id st.project_parameters))
        st.project_parameters in
  set_state {
    project_current = st.project_current;
    project_catalog = st.project_catalog;
    project_version = st.project_version;
    project_contact_map = st.project_contact_map;
    default_parameters; project_parameters;
  }

let set_plot_period plot_period =
  update_parameters (fun param -> { param with plot_period })
let set_pause_condition pause_condition =
  update_parameters (fun param -> { param with pause_condition })
let set_seed seed =
  update_parameters (fun param -> { param with seed })
let set_store_trace store_trace =
  update_parameters (fun param -> { param with store_trace })

let update_state
    project_id me project_catalog default_parameters project_parameters =
    me.project_manager#project_parse project_id >>=
    (Api_common.result_map
       ~ok:(fun _ (project_parse : Api_types_j.project_parse) ->
           me.project_manager#init_static_analyser_raw
             project_parse.Api_types_j.project_parse_raw_ast >>= fun out ->
           let () =
             set_state {
               project_current = Some me;
               project_catalog; default_parameters; project_parameters;
               project_contact_map =
                 Some project_parse.Api_types_j.project_parse_contact_map ;
               project_version =
                 project_parse.Api_types_j.project_parse_project_version ;
             } in
           Lwt.return (Api_common.result_lift out))
       ~error:(fun _ errors ->
           let () = set_state {
               project_current = Some me ;
               project_catalog; default_parameters; project_parameters;
               project_version = -1;
               project_contact_map = None ;
             } in
           Lwt.return (Api_common.result_messages errors))
    )

let add_project is_new (project_id : Api_types_j.project_id) : unit Api.result Lwt.t =
  let catalog = (React.S.value state).project_catalog in
  (try
     Lwt.return (Api_common.result_ok
                   (List.find (fun x -> x.project_id = project_id) catalog,
                    catalog,
                   (React.S.value state).project_parameters))
   with Not_found ->
     State_runtime.create_manager project_id >>=
     (Api_common.result_bind_lwt
        ~ok:(fun project_manager ->
            (if is_new then
               project_manager#project_create
                 { Api_types_j.project_parameter_project_id = project_id }
             else Lwt.return (Api_common.result_ok ())) >>=
            Api_common.result_bind_lwt
              ~ok:(fun () ->
                  let me = {project_id; project_manager } in
                  let default_parameters =
                    (React.S.value state).default_parameters in
                  let params =
                    Mods.StringMap.add project_id default_parameters
                      (React.S.value state).project_parameters in
                  Lwt.return
                    (Api_common.result_ok (me,me::catalog,params)))))) >>=
  Api_common.result_bind_lwt
    ~ok:(fun (me,catalog,params) ->
        update_state project_id me catalog default_parameters params)

let create_project project_id = add_project true project_id
let set_project project_id = add_project false project_id

let dummy_model = {
  model_project_id = None;
  model_project_ids = [];
  model_project_version = -1;
  model_contact_map = None;
  model_parameters = default_parameters;
}

let model : model React.signal =
  React.S.map
    (fun state ->
       let model_project_ids =
         List.map (fun p -> p.project_id) state.project_catalog in
       let model_parameters = match state.project_current with
         | None -> state.default_parameters
         | Some proj -> Mods.StringMap.find_default
           state.default_parameters proj.project_id state.project_parameters in
       { model_project_id =
           Option_util.map (fun x -> x.project_id) state.project_current;
         model_project_ids = model_project_ids ;
         model_project_version = state.project_version ;
         model_contact_map = state.project_contact_map ;
         model_parameters;
       })
    state

let sync () : unit Api.result Lwt.t =
  match (React.S.value state).project_current with
  | None -> Lwt.return (Api_common.result_ok ())
  | Some current ->
    current.project_manager#project_parse current.project_id >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (project_parse : Api_types_j.project_parse) ->
           current.project_manager#init_static_analyser_raw
             project_parse.Api_types_j.project_parse_raw_ast >>= fun out ->
           let () =
             set_state {
               (React.S.value state) with
               project_version =
                 project_parse.Api_types_j.project_parse_project_version;
               project_contact_map =
                 Some project_parse.Api_types_j.project_parse_contact_map; } in
           Lwt.return (Api_common.result_lift out)))

let remove_simulations manager project_id =
  manager#simulation_delete project_id

let remove_files manager project_id =
  (manager#file_catalog project_id) >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (catalog : Api_types_t.file_catalog) ->
         Lwt_list.iter_p
           (fun m ->
              manager#file_delete project_id m.Api_types_j.file_metadata_id>>=
              (fun _ -> Lwt.return_unit))
           catalog.Api_types_t.file_metadata_list >>=
         (fun () -> Lwt.return (Api_common.result_ok ()))))

let remove_project project_id =
  let state = React.S.value state in
  try
    let current =
      List.find (fun x -> x.project_id = project_id)
        state.project_catalog in
    remove_files current.project_manager current.project_id >>=
    fun out -> current.project_manager#project_delete current.project_id >>=
    (fun out' ->
       let () = current.project_manager#terminate () in
       let project_catalog =
         List.filter (fun x -> x.project_id <> current.project_id)
           state.project_catalog in
       let project_current =
         if (match state.project_current with
             | None -> false
             | Some v -> v.project_id = current.project_id) then
           match project_catalog with
           | [] -> None
           | h :: _ -> Some h
         else state.project_current in
       let () =
         set_state
           { project_current; project_catalog;
             default_parameters = state.default_parameters;
             project_parameters =
               Mods.StringMap.remove project_id state.project_parameters;
             project_version = -1; project_contact_map = None } in
       sync () >>= fun out'' ->
       Lwt.return (Api_common.result_combine [out;out';out'']))
  with Not_found ->
    Lwt.return (Api_common.result_error_msg
                  ("Project "^project_id^" does not exists"))

let create_simulation_parameter project_id :
  Api_types_j.simulation_parameter =
  let st = React.S.value state in
  let param = Mods.StringMap.find_default
      st.default_parameters project_id st.project_parameters in
  { Api_types_j.simulation_plot_period = param.plot_period ;
    Api_types_j.simulation_pause_condition = param.pause_condition ;
    Api_types_j.simulation_seed = param.seed;
    Api_types_j.simulation_id = "default" ;
    Api_types_j.simulation_store_trace = param.store_trace ;
  }

let rec init_plot_period (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::t ->
    try set_plot_period (float_of_string h)
    with Failure _ ->
      let msg = Format.sprintf "failed to parse init_plot_period '%s'" h in
      let () = Common.debug (Js.string msg) in
      init_plot_period t

let init_pause_condition (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::_ -> set_pause_condition h

let rec init_model_seed (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::t ->
    try set_plot_period (float_of_string h)
    with Failure _ ->
      let msg = Format.sprintf "failed to parse model_seed '%s'" h in
      let () = Common.debug (Js.string msg) in
      init_model_seed t

let init existing_projects : unit Lwt.t =
  let arg_plot_period = Common_state.url_args "plot_period" in
  let arg_pause_condition =
    Common_state.url_args ~default:["[T] > 100"] "pause_condition" in
  let arg_model_seed = Common_state.url_args "model_seed" in
  let () = init_plot_period arg_plot_period in
  let () = init_pause_condition arg_pause_condition in
  let () = init_model_seed arg_model_seed in

  let existing_projects =
    List.map (fun x -> x.Api_types_t.project_id) existing_projects in
  let projects = Common_state.url_args ~default:["default"] "project" in
  let rec add_projects projects : unit Lwt.t =
    match projects with
    | [] -> Lwt.return_unit
    | project::projects ->
      add_project
        (List.for_all (fun x -> x <> project) existing_projects)
        project >>=
      Api_common.result_map
        ~ok:(fun _ () -> add_projects projects)
        ~error:(fun _ (errors : Api_types_j.errors) ->
            let msg =
              Format.sprintf
                "creating project %s error %s"
                project
                (Api_types_j.string_of_errors errors)
            in
            let () = Common.debug (Js.string (Format.sprintf "State_project.init 2 : %s" msg)) in
            add_projects projects)
  in
  add_projects existing_projects >>= fun () -> add_projects projects

let with_project :
  'a . label:string ->
  (Api.concrete_manager -> Api_types_j.project_id -> 'a  Api.result Lwt.t) ->
  'a  Api.result Lwt.t  =
  fun ~label handler ->
    match (React.S.value state).project_current with
    | None ->
      let error_msg : string =
        Format.sprintf
          "Failed %s due to unavailable project."
          label in
      Lwt.return (Api_common.result_error_msg error_msg)
    | Some current ->
      handler current.project_manager current.project_id
