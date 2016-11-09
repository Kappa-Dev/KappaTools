open Lwt.Infix

exception InvalidState of string
let editor_full , set_editor_full =
  React.S.create (false : bool)
let model_parse , set_model_parse =
  React.S.create (None : Api_types_j.contact_map option)
(* Add the location where the error was emitted from *)
type localized_errors =
  { model_error_messages : Api_types_j.errors ;
    model_error_location : string }
let model_error, _set_model_error =
  React.S.create (None : localized_errors option)
let set_model_error location messages =
  _set_model_error (Some ({ model_error_messages = messages;
                            model_error_location =  location; }))
let clear_model_error () =
  _set_model_error None
let has_model_error () =
  match React.S.value model_error with
  | None -> false
  | Some _ -> true
let model_max_events, set_model_max_events =
  React.S.create (None : int option)
let model_max_time, set_model_max_time =
  React.S.create (None : float option)
let model_seed, set_model_seed =
  React.S.create (None : int option)
let model_plot_period, set_model_plot_period = React.S.create 1.
let current_file , set_current_file =
  React.S.create (None : Api_types_j.file option)
let current_project_id, set_current_project_id =
  React.S.create
    (Some "default" : Api_types_j.project_id option)
    (*None : Api_types_j.project_id option)*)

type cli = { url : string;
             command : string;
             args : string list }


type protocol = | HTTP of string
                | CLI of cli
type remote = { label : string ;
                protocol : protocol ; }
type runtime = | WebWorker
               | Embedded
               | Remote of remote


let runtime_label runtime =
  match runtime with
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote remote -> remote.label

let runtime_value runtime =
  match runtime with
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote { label = _ ;
             protocol = HTTP http } -> http
  | Remote { label = _ ;
             protocol = CLI { url = url ;
                              command = _;
                              args = _ } } -> url

class embedded_manager () : Api.manager =
  object
    inherit Api_runtime.manager
        (object
          method min_run_duration () = 0.1
          method yield = Lwt_js.yield
          method log ?exn (msg: string) =
            let () = ignore(exn) in
            let () = Common.debug (Js.string msg) in
            Lwt.return_unit
          end : Kappa_facade.system_process)
end

let parse_remote url : remote option =
  let () = Common.debug ("parse_remote:"^url) in
  let format_url url =
    let length = String.length url in
    if length > 0 && String.get url (length - 1) == '/' then
      String.sub url 0 (length - 1)
    else
      url
  in
  let cleaned_url http =
    let cleaned =
      Format.sprintf
        "%s:%d/%s"
        http.Url.hu_host
        http.Url.hu_port
        http.Url.hu_path_string
    in
    let () = Common.debug ("cleaned"^cleaned) in
    format_url cleaned
  in
  match Url.url_of_string url with
  | None -> None
  | Some parsed ->
    let protocol : protocol =
      match parsed with
      | Url.Http http -> HTTP ("http://"^(cleaned_url http))
      | Url.Https https -> HTTP ("https://"^(cleaned_url https))
      | Url.File file -> CLI { url = "file://"^file.Url.fu_path_string ;
                               command = file.Url.fu_path_string ;
                               args = [] }
    in
    let label =
      try
        (List.assoc "label"
           (match parsed with
            | Url.Http http -> http.Url.hu_arguments
            | Url.Https https -> https.Url.hu_arguments
            | Url.File file -> file.Url.fu_arguments
           )
        )
      with Not_found ->
      match parsed with
      | Url.Http http -> http.Url.hu_host
      | Url.Https https -> https.Url.hu_host
      | Url.File file -> file.Url.fu_path_string
    in
    Some { label = label;
           protocol = protocol }

let default_runtime = WebWorker

let runtime_state : Api.manager option ref = ref None

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
  (match !runtime_state with
   | None -> Lwt.return_unit
   | Some manager ->
     (match React.S.value current_project_id with
      | None -> Lwt.return_unit
      | Some project_id ->
        (manager#file_info project_id) >>=
        (Api_common.result_bind_lwt
           ~ok:(delete_files manager project_id)) >>=
        (fun _ -> Lwt.return_unit)
     )
  )

(* synchronize across managers *)
let synch_lwt () : unit Lwt.t =
       match !runtime_state with
       | None -> Lwt.return_unit
       | Some manager ->
       (manager#project_info ())
       >>=
       (* synch project *)
       (Api_common.result_bind_lwt
          ~ok:(fun (project_info : Api_types_j.project_info) ->
              match React.S.value current_project_id with
              | Some project_id ->
                if List.mem project_id project_info then
                  Lwt.return (Api_common.result_ok project_id)
                else
                  (manager#project_create { Api_types_j.project_id = project_id } )
              | None -> Lwt.return (Api_common.result_error_msg "missing project")
            )
       )
       >>=
       (Api_common.result_bind_lwt
          ~ok:(fun project_id ->
              match React.S.value current_file with
              | Some current_file ->
                (manager#file_info project_id) >>=
                (Api_common.result_bind_lwt
                   ~ok:(fun (file_info : Api_types_j.file_info) ->
                       match
                         List.filter
                           (fun metadata ->
                              metadata.Api_types_j.file_metadata_id =
                              current_file.Api_types_j.file_metadata.Api_types_j.file_metadata_id)
                           file_info
                       with
                       | h::_ ->
                         (manager#file_update
                            project_id
                            h.Api_types_j.file_metadata_id
                            { Api_types_j.file_modification_compile =
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
                         (fun _ ->
                         manager#file_create
                           project_id
                           current_file)
                     )
                )
                >>=
                (Api_common.result_bind_lwt
                   ~ok:(fun (file_result : Api_types_j.file_metadata Api_types_j.file_result) ->
                       let () = set_model_parse (Some file_result.Api_types_j.file_status_contact_map) in
                       let () = clear_model_error () in
                       Lwt.return
                         (Api_common.result_ok ()))
                )
              | None -> Lwt.return (Api_common.result_ok ()))
       ) >>=
       (Api_common.result_map
          ~ok:(fun _ _ -> Lwt.return_unit)
          ~error:(fun _ (errors : Api_types_j.errors) ->
              let () = set_model_parse None in
              let () = set_model_error __LOC__ errors in
              Lwt.return_unit)
       )

let _ =
  React.S.l1
    (fun _ -> Lwt_js_events.async synch_lwt)
    current_file

let force_synch () = Lwt_js_events.async synch_lwt

(* update everything at once *)
let set_file (filename : string) (filecontent : string) : unit =
  let file_metadata = { Api_types_j.file_metadata_compile = true ;
                        Api_types_j.file_metadata_hash = None ;
                        Api_types_j.file_metadata_id = filename ;
                        Api_types_j.file_metadata_position = 0 ; }
  in
  let file = { Api_types_j.file_metadata = file_metadata ;
               Api_types_j.file_content = filecontent ; }
  in
  set_current_file (Some file)

let set_filecontent (file_content : string) : unit =
  match React.S.value current_file with
  | None ->
    set_model_error
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

let toggle_compile () : unit =
  match React.S.value current_file with
  | None -> ()
  | Some file ->
    set_current_file
      (Some { file with Api_types_j.file_metadata =
                          { file.Api_types_j.file_metadata with
                            Api_types_j.file_metadata_compile =
                              not file.Api_types_j.file_metadata.Api_types_j.file_metadata_compile
                          } }
      )

let set_runtime_url
    (url : string)
    (continuation : bool -> unit) : unit =
  try
    let () = clear_model_error () in
    if url = "WebWorker" then
      let () = runtime_state :=
          Some (new Web_worker_api.manager () :> Api.manager) in
      let () = force_synch () in
      let () = continuation true in
      ()
    else if url = "Embedded" then
      let () = runtime_state :=
          Some (new embedded_manager () :> Api.manager) in
      let () = force_synch () in
      let () = continuation true in
      ()
    else
      let () = Common.debug ("parse_remote:1") in
      match parse_remote url with
      | Some { label = _ ; protocol = HTTP url } ->
        let version_url : string = Format.sprintf "%s/v1/version" url in
        let () = Common.debug ("set_runtime_url:"^version_url) in
        Lwt.async
          (fun () ->
             (XmlHttpRequest.perform_raw
                ~response_type:XmlHttpRequest.Text
                version_url)
             >>=
             (fun frame ->
                let is_valid_server : bool = frame.XmlHttpRequest.code = 200
                in
                let () =
                  if is_valid_server then
                    let () = runtime_state :=
                        Some (new Rest_api.manager url :> Api.manager); in
                    let () = force_synch () in
                    ()
                  else
                    let error_msg : string =
                      Format.sprintf "Bad Response %d from %s "
                        frame.XmlHttpRequest.code
                        url
                    in
                    set_model_error __LOC__ (Api_data.api_message_errors error_msg)
                in
                let () = continuation is_valid_server in
                Lwt.return_unit
             )
          )
      | Some { label = _ ;
               protocol = CLI { url = _ ;
                                command = command;
                                args = _ } } ->
        let () = Common.debug ("set_runtime_url:"^url) in
        let js_node_runtime = new JsNode.manager command ["--development"] in
        if js_node_runtime#is_running () then
          let () = Common.debug (Js.string "sucess") in
          let () = runtime_state := Some (js_node_runtime :> Api.manager) in
          let () = force_synch () in
          continuation true
        else
          let () = Common.debug (Js.string "failure") in
          continuation false
      | None -> continuation false
  with _ -> continuation false


(* return the agent count *)
let agent_count () : int option =
  match (React.S.value model_parse) with
  | None -> None
  | Some contact_map -> Some (Array.length contact_map)
