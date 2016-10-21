open Lwt
let get_file_id (file : Api_types_j.file) = file.Api_types_j.file_metadata.Api_types_j.file_metadata_id
let patch_file
    (file_patch : Api_types_j.file_patch)
    (content : string) : string =
  let max_length = String.length content in
  let start_position : int = 0 in
  let start_length : int =
    match file_patch.Api_types_j.file_patch_start with
    | None -> 0
    | Some file_patch_start -> file_patch_start
  in
  let end_position : int =
    match file_patch.Api_types_j.file_patch_end with
    | None -> max_length
    | Some file_patch_end -> file_patch_end
  in
  let end_length : int =
    match file_patch.Api_types_j.file_patch_end with
    | None -> 0
    | Some file_patch_end -> max_length - file_patch_end
  in
  let start_string = String.sub content start_position start_length in
  let end_string = String.sub content end_position end_length in
  start_string ^
  file_patch.Api_types_j.file_patch_content ^
  end_string

let update_file
    (file : Api_types_j.file)
    (file_modification : Api_types_j.file_modification) : unit =
  let () = file.Api_types_j.file_metadata.Api_types_j.file_metadata_compile <-
      (match file_modification.Api_types_j.
               file_modification_compile
       with
       | None ->
         file.Api_types_j.
           file_metadata.Api_types_j.
           file_metadata_compile
       | Some file_content ->
         file_content)
  in
  let () = file.Api_types_j.file_metadata.Api_types_j.file_metadata_hash <-
      (match file_modification.Api_types_j.
               file_modification_hash
       with
       | None ->
         file.Api_types_j.
           file_metadata.Api_types_j.
           file_metadata_hash
       | Some file_metadata_hash ->
         Some file_metadata_hash)
  in
  let () = file.Api_types_j.file_metadata.Api_types_j.file_metadata_id <-
      (match file_modification.
               Api_types_j.
               file_modification_id
       with
       | None ->
         file.Api_types_j.
           file_metadata.Api_types_j.
           file_metadata_id
       | Some file_modification_id ->
         file_modification_id)
  in
  let () = file.Api_types_j.file_metadata.Api_types_j.file_metadata_position <-
      (match file_modification.Api_types_j.
               file_modification_position
       with
       | None ->
         file.Api_types_j.
           file_metadata.Api_types_j.
           file_metadata_position
       | Some file_metadata_position ->
         file_metadata_position)
  in
  let () = file.Api_types_j.file_content <-
      match file_modification.
              Api_types_j.
              file_modification_patch
           with
           | None -> file.Api_types_j.file_content
           | Some patch ->
             patch_file
               patch
               file.Api_types_j.file_content;
  in
  ()


let project_text (project : Api_environment.project) : string =
  let files : string list =
    List.map
      (fun f -> f.Api_types_j.file_content)
      (List.stable_sort
         (fun l r ->
            compare
              l.Api_types_j.file_metadata.Api_types_j.file_metadata_position
              r.Api_types_j.file_metadata.Api_types_j.file_metadata_position
         )
         (project#get_files ())) in
  String.concat "\n" files
let parse_text :
  Api_environment.project ->
  Kappa_facade.system_process ->
  (Kappa_facade.t -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t =
  fun
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process)
    (f : Kappa_facade.t -> 'a Api.result Lwt.t)
    ->
      ((Kappa_facade.parse
          ~system_process:system_process
          ~kappa_code:(project_text project))
       >>=
       (Api_common.result_data_map
          ~ok:((fun (kappa_facade : Kappa_facade.t) -> f kappa_facade))
          ~error:((fun (errors : Api_types_j.errors) ->
              Lwt.return (Api_common.result_messages errors)) :
                    Api_types_j.errors -> 'a Api.result Lwt.t)
       ) : 'a Api.result Lwt.t)

class manager_file
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process)
  : Api.manager_file =
  object
    method file_info
        (project_id : Api_types_j.project_id) :
      Api_types_j.file_info Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun (project : Api_environment.project) ->
         let files : Api_types_j.file list = (project#get_files ()) in
         let file_info : Api_types_j.file_info =
           List.map (fun file -> file.Api_types_j.file_metadata) files in
         Lwt.return (Api_common.result_ok file_info)
      )

    method file_create
        (project_id : Api_types_j.project_id)
        (file : Api_types_j.file) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      Api_common.ProjectOperations.bind
        project_id
        environment
        (fun (project : Api_environment.project) ->
           let file_list : Api_types_j.file list = (project#get_files ()) in
           let file_eq : Api_types_j.file -> bool =
             fun f -> (get_file_id file) = (get_file_id f) in
           if List.exists file_eq file_list then
             let message : string =
               Format.sprintf
                 "file id %s exists"
	         (Api_common.FileCollection.identifier file)
             in
             Lwt.return
               (Api_common.result_error_msg
                  ~result_code:`CONFLICT message)
           else
             let file_list : Api_types_j.file list = (project#get_files ()) in
             let () =  project#set_files (file::file_list) in
             parse_text
               project
               system_process
               (fun
                 (kappa_facade : Kappa_facade.t) ->
                 let () = project#set_state kappa_facade in
                 Lwt.return
                   (Api_common.result_ok
                      { Api_types_j.file_status_data =
                          file.Api_types_j.file_metadata ;
          		Api_types_j.file_status_contact_map = Kappa_facade.get_contact_map kappa_facade ; }
                   )
               )
        )


    method file_get
      (project_id : Api_types_j.project_id)
      (file_id : Api_types_j.file_id) :
      Api_types_j.file Api.result Lwt.t =
      Api_common.bind_file
        environment
        project_id
        file_id
        (fun _ (file : Api_types_j.file) ->
           Lwt.return (Api_common.result_ok file))

    method file_update
      (project_id : Api_types_j.project_id)
      (file_id : Api_types_j.file_id)
      (file_modification : Api_types_j.file_modification) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      Api_common.bind_file
        environment
        project_id
        file_id
        (fun
          (project : Api_environment.project)
          (file : Api_types_j.file) ->
          let () = update_file file file_modification in
          let file_list : Api_types_j.file list = (project#get_files ()) in
          let () = project#set_files file_list in
          parse_text
            project
            system_process
            (fun
              (kappa_facade : Kappa_facade.t) ->
              let () = project#set_state kappa_facade in
              Lwt.return
                (Api_common.result_ok
                   { Api_types_j.file_status_data =
                       file.Api_types_j.file_metadata ;
          	     Api_types_j.file_status_contact_map = Kappa_facade.get_contact_map kappa_facade ; }
                )
            )
        )

    method file_delete
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      unit Api_types_j.file_result Api.result Lwt.t =
      Api_common.bind_file
        environment
        project_id
        file_id
        (fun (project : Api_environment.project) _ ->
           let file_list : Api_types_j.file list = (project#get_files ()) in
           let file_ne : Api_types_j.file -> bool =
             fun file -> (get_file_id file) <> file_id in
           let () =  project#set_files (List.filter file_ne file_list) in
           parse_text
            project
            system_process
            (fun
              (kappa_facade : Kappa_facade.t) ->
              let () = project#set_state kappa_facade in
              Lwt.return
                (Api_common.result_ok
                   { Api_types_j.file_status_data = ();
          	     Api_types_j.file_status_contact_map =
                     Kappa_facade.get_contact_map kappa_facade ; }
                )
            )
        )

  end;;
