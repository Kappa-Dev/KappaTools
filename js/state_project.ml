(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type state = { project_id : Api_types_j.project_id option ;
               project_catalog : Api_types_j.project_catalog ;
               project_contact_map : Api_types_j.contact_map option ;
             }

type model = { model_current : Api_types_j.project_id option ;
               model_project_ids : Api_types_j.project_id list ;
               model_contact_map : Api_types_j.contact_map option ;
             }

let state , set_state =
  React.S.create
    { project_id = None ;
      project_catalog = { Api_types_j.project_list = [] ; } ;
      project_contact_map = None ; }

let update_state
  (manager : Api.manager)
  (project_id : Api_types_j.project_id)
  (project_catalog : Api_types_j.project_catalog) : unit Api.result Lwt.t =
  (manager#project_parse project_id) >>=
  (Api_common.result_map
     ~ok:(fun _ (project_parse : Api_types_j.project_parse) ->
         let () = set_state { project_id = Some project_id ;
                              project_catalog = project_catalog ;
                              project_contact_map = Some project_parse.Api_types_j.project_parse_contact_map ;
                            } in

         Lwt.return (Api_common.result_ok ()))
     ~error:(fun _ errors ->
         let () = set_state { project_id = Some project_id ;
                              project_catalog = project_catalog ;
                              project_contact_map = None ;
                            } in
                Lwt.return (Api_common.result_messages errors))
  )

let create_project (project_id : Api_types_j.project_id) : unit Api.result Lwt.t =
  let manager = State_runtime.get_manager () in
  (manager#project_catalog ())
  >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (project_catalog : Api_types_j.project_catalog) ->
         if List.mem project_id
             (List.map
                (fun project -> project.Api_types_j.project_id)
                project_catalog.Api_types_j.project_list) then
           Lwt.return (Api_common.result_ok project_catalog)
         else
           (manager#project_create
              { Api_types_j.project_parameter_project_id = project_id })
           >>=
           (Api_common.result_bind_lwt
              ~ok:(fun (_ : Api_types_j.project_id) ->
                  (manager#project_catalog ())
                )
           )
       )
  )
  >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (project_catalog : Api_types_j.project_catalog) ->
         update_state manager project_id project_catalog
       )
  )

let model : model React.signal =
  React.S.map
    (fun state ->
       let model_project_ids =
         List.map (fun p -> p.Api_types_j.project_id)
           state.project_catalog.Api_types_j.project_list
       in
       { model_current = state.project_id ;
         model_project_ids = model_project_ids ;
         model_contact_map = state.project_contact_map ;
       })
    state

let set_project (project_id : Api_types_j.project_id) : unit Api.result Lwt.t =
  let manager = State_runtime.get_manager () in
  (manager#project_catalog ())
  >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (project_catalog : Api_types_j.project_catalog) ->
         if List.mem project_id
             (List.map
                (fun project -> project.Api_types_j.project_id)
                (React.S.value state).project_catalog.Api_types_j.project_list)
         then
           update_state manager project_id project_catalog
         else
           let error_msg : string =
             Format.sprintf "Project %s does not exist" project_id in
           Lwt.return (Api_common.result_error_msg error_msg)
       )
  )

let sync () : unit Api.result Lwt.t =
  let manager = State_runtime.get_manager () in
  let old_state = React.S.value state in
  (manager#project_catalog ()) >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (catalog : Api_types_j.project_catalog) ->
         let new_state (* (new_state,error) *) =
           let current_project_list = catalog.Api_types_j.project_list in
           (* Use the current file's metadata if it is still in the directory. *)
           let current_project =
             match old_state.project_id with
             | None -> None
             | Some project_id ->
               List_util.find_option
                 (fun p -> p.Api_types_j.project_id = project_id)
                 current_project_list in
           match current_project with
           | None ->
             Lwt.return
               (Api_common.result_ok
                  ({ project_id = None ;
                     project_catalog = catalog ;
                     project_contact_map = None ;
                   },None))
           | Some project ->
             (* need to fetch contact map here *)
             (manager#project_parse project.Api_types_j.project_id) >>=
             (Api_common.result_map
                ~ok:(fun _ (project_parse : Api_types_j.project_parse) ->
                    Lwt.return
                      (Api_common.result_ok
                         ({ project_id = Some project.Api_types_j.project_id ;
                            project_catalog = catalog ;
                            project_contact_map = Some project_parse.Api_types_j.project_parse_contact_map ;
                          },None))
                  )
                ~error:(fun _ (errors : Api_types_j.errors) ->
                    Lwt.return
                      (Api_common.result_ok
                         ({ project_id = Some project.Api_types_j.project_id ;
                            project_catalog = catalog ;
                            project_contact_map = None ;
                          },Some errors))
                  )
             )
         in
         new_state >>=
         (Api_common.result_bind_lwt
            ~ok:(fun (new_state,error) ->
                let () = set_state new_state in
                match error with
                | None -> Lwt.return (Api_common.result_ok ())
                | Some error -> Lwt.return (Api_common.result_messages error)
              )
         )
       )
  )


let remove_project() : unit Api.result Lwt.t =
  let manager = State_runtime.get_manager () in
  match (React.S.value state).project_id with
  | None ->
    let error_msg : string =
      Format.sprintf "Unable to remove project as none is selected" in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some project_id ->
    (manager#project_delete project_id) >>=
    (Api_common.result_bind_lwt ~ok:(fun _-> sync ())
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
    let manager = State_runtime.get_manager () in
    match (React.S.value model).model_current with
    | None ->
      let error_msg : string =
        Format.sprintf
          "Failed %s due to unavailable project."
          label
      in
      Lwt.return (Api_common.result_error_msg error_msg)
    | Some project_id -> handler manager project_id

let close_all () : unit Api.result Lwt.t =
  failwith ""
