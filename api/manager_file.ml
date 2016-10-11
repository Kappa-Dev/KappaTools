let get_file_id (file : Api_types_j.file) = file.Api_types_j.file_metadata.Api_types_j.file_metadata_id
let update_file
    (file : Api_types_j.file)
    (file_modification : Api_types_j.file_modification) : unit =
  let () = file.Api_types_j.file_content <-
           match file_modification.Api_types_j.
                   file_modification_content
           with
           | None -> file.Api_types_j.file_content
           | Some file_content -> file_content;
  in
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
      (match file_modification.Api_types_j.
               file_modification_id
       with
       | None ->
         file.Api_types_j.
           file_metadata.Api_types_j.
           file_metadata_id
       | Some file_metadata_id ->
         file_metadata_id)
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
    match file_modification.Api_types_j.
            file_modification_content
    with
    | None -> file.Api_types_j.file_content
    | Some file_content -> file_content
  in
  ()


class manager_file
    (environment : Api_environment.environment)
    (_ : Kappa_facade.system_process)
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
      Api_types_j.file_metadata Api.result Lwt.t =
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
               (Api_common.result_error_msg ~result_code:Api.CONFLICT message)
           else
             let file_list : Api_types_j.file list = (project#get_files ()) in
             let () =  project#set_files (file::file_list) in
             Lwt.return (Api_common.result_ok file.Api_types_j.file_metadata)
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
      Api_types_j.file_metadata Api.result Lwt.t =
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
          Lwt.return (Api_common.result_ok file.Api_types_j.file_metadata)

        )

    method file_delete
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      unit Api.result Lwt.t =
      Api_common.bind_file
        environment
        project_id
        file_id
        (fun (project : Api_environment.project) _ ->
           let file_list : Api_types_j.file list = (project#get_files ()) in
           let file_ne : Api_types_j.file -> bool =
             fun file -> (get_file_id file) <> file_id in
           let () =  project#set_files (List.filter file_ne file_list) in
           Lwt.return (Api_common.result_ok ())
        )

  end;;
