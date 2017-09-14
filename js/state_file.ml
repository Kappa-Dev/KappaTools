(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type state = { state_current : string option ;
               state_directory : Api_types_j.file_metadata list }

type t = Api_types_j.file_metadata
type model = { model_current : string option ;
               model_directory : t list }

let t_file_id (t : t) : Api_types_j.file_id =
  t.Api_types_j.file_metadata_id
let t_compile (t : t) : bool =
  t.Api_types_j.file_metadata_compile

let find_metadata
    (file_id : Api_types_j.file_id)
    (directory :  Api_types_j.file_metadata list)
  : Api_types_j.file_metadata option =
  try Some (List.find
              (fun file_metadata ->
                 file_metadata.Api_types_j.file_metadata_id = file_id)
              directory)
  with Not_found -> None

let directory_metadata directory_state : Api_types_j.file_metadata option =
  try
    match directory_state.state_current with
    | None -> None
    | Some file_id ->
      find_metadata file_id directory_state.state_directory

  with Not_found -> None
let blank_state = { state_current = None ; state_directory = [] }
let directory_state , set_directory_state = React.S.create blank_state
type refresh = { filename : string ; content : string ; line : int option ; }
let refresh_file , set_refresh_file = React.S.create (None : refresh option)

let get_file () : Api_types_j.file Api.result Lwt.t =
  State_project.with_project ~label:"get_file"
    (fun manager ->
       (match (React.S.value directory_state).state_current with
        | None ->
          let error_msg : string =
            "Attempt to fetch file with none selected."
          in
          Lwt.return (Api_common.result_error_msg error_msg)
           | Some filename -> manager#file_get filename
       )
    )

let send_refresh
    (filename : string)
    (line : int option) : unit Api.result Lwt.t =
  (* only send refresh if there is a current file *)
  match (React.S.value directory_state).state_current with
  | None -> Lwt.return (Api_common.result_ok ())
  | Some _ ->
    get_file () >>=
    (Api_common.result_bind_lwt
       ~ok:(fun (file : Api_types_j.file) ->
           let () = Common.debug (Js.string file.Api_types_j.file_content) in
           let () = set_refresh_file None in
           let () =
             set_refresh_file
               (Some
                  { filename = filename ;
                    content = file.Api_types_j.file_content ;
                    line = line ; }) in
           Lwt.return (Api_common.result_ok ()))
    )


let model : model React.signal =
  React.S.bind
    directory_state
    (fun directory_state ->
       React.S.const
       { model_current = directory_state.state_current ;
         model_directory =
           List.sort
             (fun l r -> compare l.Api_types_j.file_metadata_position r.Api_types_j.file_metadata_position)
             directory_state.state_directory ;
       }
    )
let update_directory
    (manager : Api.concrete_manager)
  : unit Api.result Lwt.t =
  manager#file_catalog >>=
  (Api_common.result_bind_lwt
     ~ok:(fun (catalog : Api_types_j.file_catalog) ->
         let directory = React.S.value directory_state in
         let () =
           set_directory_state
             { directory with
               state_directory = catalog }
         in
         Lwt.return (Api_common.result_ok ()))
  )

let file_patch
    (metadata : Api_types_j.file_metadata)
    ?content
    ?compile
    ?position
    ()
  : Api_types_j.file_modification =
  let modification_patch =
    match content with
    | None -> None
    | Some content ->
      Some { Api_types_j.file_patch_start = None ;
             Api_types_j.file_patch_end = None;
             Api_types_j.file_patch_content = content ; }
  in
  let client_id = State_settings.get_client_id () in
  { Api_types_j.file_modification_version =
      File_version.increment
        client_id
        metadata.Api_types_j.file_metadata_version ;
    Api_types_j.file_modification_compile = compile ;
    Api_types_j.file_modification_id = None ;
    Api_types_j.file_modification_position = position ;
    Api_types_j.file_modification_patch = modification_patch ;
    Api_types_j.file_modification_hash = None ; }




let new_file ~position filename content : Api_types_t.file =
  let client_id = State_settings.get_client_id () in
  let file_metadata = { Api_types_j.file_metadata_compile = true ;
                        Api_types_j.file_metadata_hash = None ;
                        Api_types_j.file_metadata_id = filename ;
                        Api_types_j.file_metadata_position = position ;
                        Api_types_j.file_metadata_version =
                          File_version.create client_id ;
                    }
  in
  { Api_types_j.file_metadata = file_metadata ;
    Api_types_j.file_content = content ; }



let create_file
    ~(filename:string)
    ~(content:string) :
  unit Api.result Lwt.t =
  State_project.with_project ~label:"create_file"
    (fun manager ->
       manager#file_catalog >>=
       (Api_common.result_bind_lwt
          ~ok:(fun (catalog : Api_types_j.file_catalog) ->
              (let matching_file : Api_types_j.file_metadata list =
                 List.filter
                   (fun file_metadata ->
                      filename = file_metadata.Api_types_j.file_metadata_id)
                   catalog in
               (match matching_file with
                | [] ->
                  manager#file_create
                    (new_file
                       ~position:(List.length catalog)
                       filename content)
                | metadata::_ ->
                  let file_modification : Api_types_t.file_modification =
                    file_patch
                      metadata
                      ~content
                      ?compile:None
                      ?position:None
                      ()
                  in
                  manager#file_update filename file_modification)
               >>=
               (Api_common.result_bind_lwt
                  ~ok:(fun _ -> Lwt.return (Api_common.result_ok ()))) >>=
               (fun r_create ->
                  let () = set_directory_state
                      { (React.S.value directory_state) with
                        state_current = Some filename } in
                  let r_dir_t = update_directory manager in
                  send_refresh filename None >>=
                  fun r_txt -> r_dir_t >>=
                  fun r_dir -> Lwt.return (Api_common.result_combine [r_create; r_dir; r_txt])
               )
              )
            )
       )
    )

let choose_file
    (choice : string)
    (directory : Api_types_j.file_metadata list) :
  Api_types_j.file_metadata option =
  try
    Some
      (List.find
         (fun metadata -> choice = metadata.Api_types_j.file_metadata_id)
         directory)
    with Not_found -> None

let select_file (filename : string) (line : int option) : unit Api.result Lwt.t =
  State_project.with_project ~label:"select_file"
    (fun manager ->
       manager#file_catalog >>=
       (Api_common.result_bind_lwt
          ~ok:(fun (catalog : Api_types_j.file_catalog) ->
              let current = choose_file filename catalog in
              let () =
                set_directory_state
                  { state_current =
                      (match current with
                       | None ->
                         None
                       | Some current ->
                         Some current.Api_types_j.file_metadata_id) ;
                    state_directory = catalog ; }
              in
              match current with
              | None ->
                let error_msg : string =
                  Format.sprintf
                    "Failed to switch file %s ."
                    filename
                in
                Lwt.return (Api_common.result_error_msg error_msg)
              | Some _ ->
                send_refresh filename line
            )
       )
    )


let modify_file
  (* when peforming multiple modifications skip
     multiple directory updates.
  *)
    ?(skip_directory_update : bool = false)
    (file_id : Api_types_j.file_id option)
    (file_modification : Api_types_j.file_metadata -> Api_types_j.file_modification) : unit Api.result Lwt.t =
  State_project.with_project ~label:"select_file"
    (fun manager ->
       match file_id with
       | None ->
         let error_msg : string =
           "Failed to update file as there is no selected file."
         in
         Lwt.return (Api_common.result_error_msg error_msg)
       | Some filename ->
         manager#file_catalog >>=
         (Api_common.result_bind_lwt
            ~ok:(fun (catalog : Api_types_j.file_catalog) ->
                (* lets fetch the latest metadata to get the verison *)
                let current =
                  choose_file filename catalog in
                match current with
                | None ->
                  let error_msg : string =
                    Format.sprintf
                      "Failed to find file '%s' for update."
                    filename
                  in
                  Lwt.return (Api_common.result_error_msg error_msg)
                | Some metadata ->
                  (manager#file_update
                     filename (file_modification metadata))
                  >>=
                  (Api_common.result_bind_lwt
                     ~ok:(fun _ ->
                         if skip_directory_update then
                           Lwt.return (Api_common.result_ok ())
                         else
                           update_directory manager)
                  )
              )
         )
    )

let set_content (content : string) : unit Api.result Lwt.t =
  modify_file
    (React.S.value directory_state).state_current
    (fun metadata ->
       file_patch
         metadata
         ?content:(Some content)
         ?compile:None
         ?position:None
         ())

let set_compile
  (file_id : Api_types_j.file_id)
  (compile : bool) : unit Api.result Lwt.t =
  modify_file
    (Some file_id)
    (fun metadata ->
       file_patch
         metadata
         ?content:None
         ?compile:(Some compile)
         ?position:None
         ())

let set_position
    ?(skip_directory_update : bool = false)
    (file_id : Api_types_j.file_id)
    (position : int) : unit Api.result Lwt.t =
  modify_file
    ~skip_directory_update
    (Some file_id)
    (fun metadata ->
       file_patch
         metadata
         ?content:None
         ?compile:None
         ?position:(Some position)
         ())

let order_files (filenames : string list) : unit Api.result Lwt.t =
  let rec _order_file (filenames : string list) (index : int) : unit Api.result Lwt.t =
    match filenames with
    | [] -> Lwt.return (Api_common.result_ok ())
    | file_id::tail ->
      (set_position
         (* only update directory on the last element *)
         ~skip_directory_update:(match tail with | [] -> false | _::_ -> true)
         file_id index) >>=
      (fun _ ->
         (* No bind as intermediate errors are ignored.  Things may
            be in an incosistent state while files are being rearranged.
         *)
         (_order_file tail (index + 1)))
  in
  _order_file filenames 0

let get_file () : Api_types_j.file Api.result Lwt.t =
  State_project.with_project ~label:"get_file"
    (fun manager ->
       (match (React.S.value directory_state).state_current with
        | None ->
          let error_msg : string =
            "Attempt to fetch file with none selected."
          in
          Lwt.return (Api_common.result_error_msg error_msg)
           | Some filename -> manager#file_get filename
       )
    )


(* old_state
   Get the current state of the directory.
   current_metadata
   Use the current file's metadata if it is still in the directory.
   Check value of current_metadata
   None : If current meta data is missing
          pick the first file from the directory and queue an update of
          the ui.
   Some : old_metadata
          Find the metadata of the file in the old state.

          Check the current_metadata is newer than the old metadata or

          Out of date : If it is not up to date pull the new version and
                        update the ui.
          Current : Do nothing
   new_metadata : Create new directory state and update.
   Update UI.

*)

let sync ?(reset=false) () : unit Api.result Lwt.t =
  (* Save the current state of the directory.*)
  let old_state = React.S.value directory_state in
  State_project.with_project ~label:"synch"
    (fun manager ->
       (* get current directory *)
       manager#file_catalog >>=
       (Api_common.result_bind_lwt
          ~ok:(fun (current_directory : Api_types_j.file_catalog) ->
              (* Save the new state of the directory.*)
              let (new_state,refresh_ui) =
                (* Use the current file's metadata if it is still in the directory. *)
                let current_metadata : Api_types_j.file_metadata option =
                  match old_state.state_current with
                  | None -> None
                  | Some current_file_id ->
                    find_metadata current_file_id current_directory
                in
                match current_metadata with
                | None ->
                  (* If current meta data is missing pick the first
                        file from the directory and queue an update of
                        the ui. *)
                  let state_current =
                    match current_directory with
                    | [] -> None
                    | first::_ -> Some first.Api_types_j.file_metadata_id in
                  ({ state_current; state_directory = current_directory },
                    state_current)
                | Some current_metadata ->
                  (* Find the metadata of the file in the old state. *)
                  let old_metadata : Api_types_j.file_metadata option =
                    find_metadata
                      current_metadata.Api_types_j.file_metadata_id
                      old_state.state_directory in
                  (* Check if old metadata it is out of date *)
                  let is_out_of_date =
                    if reset ||
                       match old_metadata with
                       | None -> true
                       (* not sure how this would happen but okay *)
                       | Some old_metadata ->
                         let client_id = State_settings.get_client_id () in
                         File_version.gt
                           ~client_id
                           current_metadata.Api_types_j.file_metadata_version
                           old_metadata.Api_types_j.file_metadata_version then
                      Some current_metadata.Api_types_j.file_metadata_id
                    else
                      None
                  in
                  ({ state_current =
                       Some current_metadata.Api_types_j.file_metadata_id ;
                     state_directory = current_directory },
                   (* If it is not up to date pull the new version and
                        update the ui. *)
                   is_out_of_date)
              in
              let () = set_directory_state new_state in
              match refresh_ui with
              | None -> Lwt.return (Api_common.result_ok ())
              | Some filename -> send_refresh filename None
            )
       )
    )

let remove_file () : unit Api.result Lwt.t =
  State_project.with_project ~label:"remove_file"
    (fun manager ->
       (match (React.S.value directory_state).state_current with
        | None ->
          let error_msg : string =
            "Attempt to remove file when none selected."
          in
          Lwt.return (Api_common.result_error_msg error_msg)
           | Some filename ->
             (manager#file_delete filename) >>=
             (Api_common.result_bind_lwt
                ~ok:(fun _ -> sync ())   )
       )
    )

let load_default () : unit Lwt.t =
 (create_file ~filename:"model.ka" ~content:"") >>=
 (Api_common.result_map
    ~ok:(fun _ () -> Lwt.return_unit)
    ~error:(fun _ (errors : Api_types_j.errors) ->
        let msg =
          Format.sprintf
            "State_file.load_default : creating default file error %s"
            (Api_types_j.string_of_errors errors) in
        let () = Common.debug (Js.string msg) in
        Lwt.return_unit))

let load_files () : unit Lwt.t =
  let files = Common_state.url_args "files" in
  let rec add_files files load_file : unit Lwt.t =
    match files with
    | [] -> Lwt.return_unit
    | filename::files ->
      let content = "" in
      (create_file ~filename ~content) >>=
      (Api_common.result_map
         ~ok:(fun _ () ->
             if load_file then
               (select_file filename None) >>=
               (Api_common.result_map
                  ~ok:(fun _ _ -> add_files files false)
                  ~error:(fun _ _ -> add_files files load_file)
               )
             else
               add_files files load_file)
         ~error:(fun _ (errors : Api_types_j.errors) ->
             let msg =
               Format.sprintf
                 "creating file %s error %s"
                 filename
                 (Api_types_j.string_of_errors errors)
             in
             let () = Common.debug (Js.string (Format.sprintf "State_file.load_files %s" msg)) in
             add_files files load_file)
      )
  in
  add_files files true

let load_models () : unit Lwt.t =
  let models = Common_state.url_args "model" in
  let rec add_models models load_file : unit Lwt.t =
    match models with
    | [] -> Lwt.return_unit
    | model::models ->
      (* fetch model *)
      Lwt_xmlHttpRequest.get model >>=
      (fun content ->
         if content.Lwt_xmlHttpRequest.code <> 200 then
           Lwt.return
             (Api_common.result_error_msg
                (Format.sprintf "bad response code %d fetching url %s" content.Lwt_xmlHttpRequest.code model))
         else
           match Url.url_of_string content.Lwt_xmlHttpRequest.url with
           | None ->
             Lwt.return
               (Api_common.result_error_msg
                  (Format.sprintf "failed to retrieve url %s" model))
           | Some u ->
             let filename =
               List_util.last
                 (match u with
                  | (Url.Http h | Url.Https h) -> h.Url.hu_path
                  | Url.File f -> f.Url.fu_path) in
             let filecontent : string =
               content.Lwt_xmlHttpRequest.content
             in
             Lwt.return (Api_common.result_ok (filename,filecontent))
      )
      >>=
      (* add content *)
      (Api_common.result_bind_lwt
         ~ok:(fun (filename,content) ->
             (create_file ~filename ~content) >>=
             (Api_common.result_bind_lwt
                ~ok:(fun () -> Lwt.return (Api_common.result_ok filename))
             ))) >>=
      (* select model if needed *)
      (Api_common.result_map
         ~ok:(fun _ filename ->
             if load_file then
               (select_file filename None) >>=
               (Api_common.result_map
                  ~ok:(fun _ _ -> add_models models false)
                  ~error:(fun _ _ -> add_models models load_file)
               )
             else
               add_models models load_file
           )
         ~error:(fun _ (errors : Api_types_j.errors) ->
             let msg =
               Format.sprintf
                 "creating loading url %s error %s"
                 model
                 (Api_types_j.string_of_errors errors)
             in
             let () = Common.debug (Js.string (Format.sprintf "State_file.load_model %s" msg)) in
             add_models models load_file)
      )
  in
  match models with
  [] -> load_default ()
  |_::_ -> add_models models true

let init () : unit Lwt.t =
  Lwt.return_unit >>=
  load_files >>=
  load_models
