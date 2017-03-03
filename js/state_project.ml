(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type a_project = {
  project_id : Api_types_j.project_id;
  project_manager : Api.concrete_manager;
}

type state = {
  project_current : a_project option;
  project_catalog : a_project list;
  project_contact_map : Api_types_j.contact_map option;
}

type model = {
  model_project_id : Api_types_j.project_id option ;
  model_project_ids : Api_types_j.project_id list ;
  model_contact_map : Api_types_j.contact_map option ;
}

let project_equal a b = a.project_id = b.project_id
let catalog_equal = List.for_all2 project_equal
let state_equal a b =
  Option_util.equal project_equal a.project_current b.project_current &&
  catalog_equal a.project_catalog b.project_catalog &&
  Option_util.equal (=) a.project_contact_map b.project_contact_map

let state , set_state =
  React.S.create ~eq:state_equal
    {
    project_current = None;
    project_catalog = [];
    project_contact_map = None;
  }

let update_state project_id project_catalog : unit Api.result Lwt.t =
  try
    let me = List.find (fun x -> x.project_id = project_id) project_catalog in
    me.project_manager#project_parse project_id >>=
    (Api_common.result_map
       ~ok:(fun _ (project_parse : Api_types_j.project_parse) ->
           let () =
             set_state {
               project_current = Some me;
               project_catalog;
               project_contact_map =
                 Some project_parse.Api_types_j.project_parse_contact_map ;
             } in
           Lwt.return (Api_common.result_ok ()))
       ~error:(fun _ errors ->
           let () = set_state { project_current = Some me ;
                                project_catalog = project_catalog ;
                                project_contact_map = None ;
                              } in
           Lwt.return (Api_common.result_messages errors))
    )
  with Not_found ->
    Lwt.return (Api_common.result_error_msg
                  ("Project "^project_id^" does not exists"))

let create_project (project_id : Api_types_j.project_id) : unit Api.result Lwt.t =
  let catalog = (React.S.value state).project_catalog in
  if List.exists (fun x -> x.project_id = project_id) catalog then
    Lwt.return (Api_common.result_ok ())
  else
    let spec = (React.S.value (State_runtime.model)).State_runtime.model_current in
    State_runtime.create_manager spec >>=
    (Api_common.result_bind_lwt
       ~ok:(fun project_manager ->
           project_manager#project_create
             { Api_types_j.project_parameter_project_id = project_id } >>=
           (Api_common.result_bind_lwt
              ~ok:(fun project_id ->
                  update_state project_id ({project_id; project_manager;}::catalog)
                ))))

let model : model React.signal =
  React.S.map
    (fun state ->
       let model_project_ids =
         List.map (fun p -> p.project_id) state.project_catalog in
       { model_project_id =
           Option_util.map (fun x -> x.project_id) state.project_current;
         model_project_ids = model_project_ids ;
         model_contact_map = state.project_contact_map ;
       })
    state

let set_project (project_id : Api_types_j.project_id) : unit Api.result Lwt.t =
  update_state project_id (React.S.value state).project_catalog

let sync () : unit Api.result Lwt.t =
  match (React.S.value state).project_current with
  | None -> Lwt.return (Api_common.result_ok ())
  | Some current ->
    current.project_manager#project_parse current.project_id >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (project_parse : Api_types_j.project_parse) ->
           let () =
             set_state { (React.S.value state) with
                         project_contact_map =
                           Some project_parse.Api_types_j.project_parse_contact_map; } in
           Lwt.return (Api_common.result_ok ())))

let remove_project () : unit Api.result Lwt.t =
  match (React.S.value state).project_current with
  | None ->
    let error_msg : string =
      Format.sprintf "Unable to remove project as none is selected" in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some current ->
    (current.project_manager#project_delete current.project_id) >>=
    (Api_common.result_bind_lwt ~ok:(fun () ->
         let project_catalog =
           List.filter (fun x -> x.project_id <> current.project_id)
             (React.S.value state).project_catalog in
         let project_current = match project_catalog with
           | [] -> None
           | h :: _ -> Some h in
         let () =
           set_state
             { project_current; project_catalog; project_contact_map = None } in
         sync ())
    )

let init () : unit Lwt.t =
  let projects = Common_state.url_args ~default:["default"] "project" in
  let rec add_projects projects load_project : unit Lwt.t =
    match projects with
    | [] -> Lwt.return_unit
    | project::projects ->
      (create_project project) >>=
      Api_common.result_map
        ~ok:(fun _ () ->
            if load_project then
              (set_project project) >>=
              (Api_common.result_map
                 ~ok:(fun _ () -> add_projects projects false)
                 ~error:(fun _ (errors : Api_types_j.errors) ->
                     let msg =
                       Format.sprintf
                         "setting up project %s error %s"
                         project
                         (Api_types_j.string_of_errors errors)
                     in
                     let () = Common.debug (Js.string (Format.sprintf "State_project.init 1 : %s" msg)) in
                     add_projects projects true)
              )
             else
               add_projects projects load_project
          )
        ~error:(fun _ (errors : Api_types_j.errors) ->
            let msg =
              Format.sprintf
                "creating project %s error %s"
                project
                (Api_types_j.string_of_errors errors)
            in
            let () = Common.debug (Js.string (Format.sprintf "State_project.init 2 : %s" msg)) in
            add_projects projects load_project)
  in
  add_projects projects true

let with_project :
  'a . label:string ->
  (Api.manager -> Api_types_j.project_id -> 'a  Api.result Lwt.t) ->
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
      handler (current.project_manager :> Api.manager) current.project_id

let close_all () : unit Api.result Lwt.t =
  failwith "State_project.close_all"
