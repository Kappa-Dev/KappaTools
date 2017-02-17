(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

exception InvalidState of string
let model_parse , set_model_parse =
  React.S.create (None : Api_types_j.contact_map option)

let current_file , set_current_file =
  React.S.create (None : Api_types_j.file option)

(*
let current_project_id, set_current_project_id =
  React.S.create (Some "default" : Api_types_j.project_id option)
*)
let rec delete_files
    (manager : Api.manager)
    (project_id : Api_types_j.project_id) :
  Api_types_t.file_metadata list -> unit Api.result Lwt.t =
  function
  | [] -> Lwt.return (Api_common.result_ok ())
  | h::t ->
    (manager#file_delete project_id h.Api_types_j.file_metadata_id) >>=
    (fun _ -> delete_files manager project_id t)

let format_file () : unit Lwt.t =
  let manager =  State_runtime.get_manager () in
  let project_model = React.S.value State_project.model in
  match project_model.State_project.model_current with
  | None -> Lwt.return_unit
   | Some project_id ->
     (manager#file_catalog project_id) >>=
     (Api_common.result_bind_lwt
        ~ok:(fun catalog -> delete_files manager project_id catalog.Api_types_j.file_metadata_list
          )) >>=
     (fun _ -> Lwt.return_unit)


(* synchronize across managers *)
let synch_lwt () : unit Lwt.t =
  let manager =  State_runtime.get_manager () in
  (manager#project_catalog ())
  >>=
  (* synch project *)
  (let () = Common.debug 2 in
   Api_common.result_bind_lwt
     ~ok:(fun (project_catalog : Api_types_j.project_catalog) ->
         let () = Common.debug 3 in
         let project_list =
           List.map
             (fun project -> project.Api_types_j.project_id)
             (project_catalog.Api_types_j.project_list) in
         let project_model = React.S.value State_project.model in
         match project_model.State_project.model_current with
         | Some project_id ->
           if List.mem project_id project_list then
             Lwt.return (Api_common.result_ok project_id)
           else
             (manager#project_create
                { Api_types_j.project_parameter_project_id = project_id } )
         | None ->
           Lwt.return (Api_common.result_error_msg "missing project")
       )
       )
  >>=
  (Api_common.result_bind_lwt
     ~ok:(fun project_id ->
         match React.S.value current_file with
         | Some current_file ->
           (manager#file_catalog project_id) >>=
           (Api_common.result_bind_lwt
              ~ok:(fun (file_catalog : Api_types_j.file_catalog) ->
                  let file_list = file_catalog.Api_types_j.file_metadata_list in
                  match
                    List.filter
                      (fun metadata ->
                         metadata.Api_types_j.file_metadata_id =
                         current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_id)
                      file_list
                  with
                  | h::_ ->
                    (manager#file_update
                       project_id
                       h.Api_types_j.file_metadata_id
                       { Api_types_j.file_modification_version = File_version.empty;
                         Api_types_j.file_modification_compile =
                           Some current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_compile ;
                         Api_types_j.file_modification_id =
                           None ;
                         Api_types_j.file_modification_position =
                           Some current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_position ;
                         Api_types_j.file_modification_patch =
                           Some { Api_types_j.file_patch_start = None ;
                                  Api_types_j.file_patch_end = None;
                                  Api_types_j.file_patch_content =
                                    current_file.Api_types_j.file_content;
                                } ;
                         Api_types_j.file_modification_hash =
                           current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_hash ; })
                        | [] ->
                          (format_file ()) >>=
                          (fun _ -> manager#file_create project_id current_file)
                )
           )
           >>=
           (Api_common.result_bind_lwt
              ~ok:(fun (file_result : (Api_types_j.file_metadata, Api_types_j.project_parse) Api_types_j.file_result) ->
                  let () = set_model_parse (Some file_result.Api_types_j.file_status_summary.Api_types_j.project_parse_contact_map) in
                  let () = State_error.clear_errors () in
                  Lwt.return
                    (Api_common.result_ok ()))
           )
         | None ->
           Lwt.return (Api_common.result_ok ()))
  ) >>=
  (Api_common.result_map
     ~ok:(fun _ _ -> Lwt.return_unit)
     ~error:(fun _ (errors : Api_types_j.errors) ->
         let () = set_model_parse None in
         let () = State_error.set_errors __LOC__ errors in
         Lwt.return_unit)
  )
let pin = ref None
let init () =
  let () =
    pin := Some
        (React.S.l1
           (fun _ -> Common.async synch_lwt)
           current_file)
  in Lwt.return_unit

let force_synch () = Common.async synch_lwt

(* update everything at once *)
let set_file (filename : string) (filecontent : string) : unit =
  let file_metadata =
    { Api_types_j.file_metadata_compile = true ;
      Api_types_j.file_metadata_hash = None ;
      Api_types_j.file_metadata_id = filename ;
      Api_types_j.file_metadata_position = 0 ;
      Api_types_j.file_metadata_version = File_version.empty ; }
  in
  let file = { Api_types_j.file_metadata = file_metadata ;
               Api_types_j.file_content = filecontent ; }
  in
  set_current_file (Some file)

let set_filecontent (file_content : string) : unit =
  match React.S.value current_file with
  | None ->
    State_error.set_errors
      __LOC__
      [Api_common.error_msg "Attempting to update content without a file."]
  | Some current_file ->
    set_current_file
      (Some { current_file with Api_types_j.file_content = file_content })

let get_filecontent () : string option =
  match React.S.value current_file with
  | None -> None
  | Some current_file -> Some current_file.Api_types_j.file_content
let get_filename () : string option =
  match React.S.value current_file with
  | None -> None
  | Some current_file -> Some current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_id

let agent_count () : int option =
  match (React.S.value model_parse) with
  | None -> None
  | Some contact_map -> Some (Array.length contact_map)
