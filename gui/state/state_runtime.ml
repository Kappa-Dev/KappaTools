(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cli = { url: string; command: string; args: string list }
type protocol = HTTP of string | CLI of cli
type remote = { label: string; protocol: protocol }
type spec = KasimAsWebWorker | KaSimAsEmbedded | Remote of remote
type state = { state_current: spec; state_runtimes: spec list }
type model = { model_current: spec; model_runtimes: spec list }

let spec_label : spec -> string = function
  | KasimAsWebWorker -> "KasimAsWebWorker"
  | KaSimAsEmbedded -> "KaSimAsEmbedded"
  | Remote remote -> remote.label

let spec_id : spec -> string = function
  | KasimAsWebWorker -> "KasimAsWebWorker"
  | KaSimAsEmbedded -> "KaSimAsEmbedded"
  | Remote { label = _; protocol = HTTP http } -> http
  | Remote { label = _; protocol = CLI cli } -> cli.url

let read_spec : string -> spec option = function
  | "KasimAsWebWorker" -> Some KasimAsWebWorker
  | "KaSimAsEmbedded" -> Some KaSimAsEmbedded
  | url ->
    let () =
      Common.log_group
        (Format.sprintf "[State_runtime.read_spec] parse_remote: %s" url)
    in
    let format_url url =
      let length = String.length url in
      if length > 0 && String.get url (length - 1) == '/' then
        String.sub url 0 (length - 1)
      else
        url
    in
    let cleaned_url http =
      let cleaned =
        Format.sprintf "%s:%d/%s" http.Url.hu_host http.Url.hu_port
          http.Url.hu_path_string
      in
      let () =
        Common.debug ~loc:__LOC__
          (Js.string (Format.sprintf "cleaned : %s" cleaned))
      in
      format_url cleaned
    in
    let () = Common.log_group_end () in

    (match Url.url_of_string url with
    | None -> None
    | Some raw_parsed ->
      (* On windows, removes the leading `/` left by js_of_ocaml url parsing on file:///C:/path` windows URLs *)
      let clean_parsed_when_on_windows raw_parsed =
        match raw_parsed with
        | Url.Http _ | Url.Https _ -> raw_parsed
        | Url.File file_url ->
          if
            file_url.Url.fu_path_string.[0] == '/'
            && file_url.Url.fu_path_string.[2] == ':'
          then (
            let () =
              Common.debug ~loc:__LOC__
                "Windows file address translated from js_of_ocaml error"
            in
            Url.File
              {
                file_url with
                Url.fu_path_string =
                  String.sub file_url.Url.fu_path_string 1
                    (String.length file_url.Url.fu_path_string - 1);
              }
          ) else
            raw_parsed
      in
      let parsed = clean_parsed_when_on_windows raw_parsed in
      let protocol : protocol =
        match parsed with
        | Url.Http http -> HTTP ("http://" ^ cleaned_url http)
        | Url.Https https -> HTTP ("https://" ^ cleaned_url https)
        | Url.File file ->
          CLI
            {
              url = "file://" ^ file.Url.fu_path_string;
              command = file.Url.fu_path_string;
              args = [];
            }
      in
      let label =
        try
          List.assoc "label"
            (match parsed with
            | Url.Http http -> http.Url.hu_arguments
            | Url.Https https -> https.Url.hu_arguments
            | Url.File file -> file.Url.fu_arguments)
        with Not_found ->
          (match parsed with
          | Url.Http http -> http.Url.hu_host
          | Url.Https https -> https.Url.hu_host
          | Url.File file -> file.Url.fu_path_string)
      in
      Some (Remote { label; protocol }))

let state, set_state =
  React.S.create
    {
      state_current = KasimAsWebWorker;
      state_runtimes = [ KasimAsWebWorker; KaSimAsEmbedded ];
    }

let create_manager ~is_new _project_id =
  (* Line below to specify unused var, as je cannot change the named argument to _is_new *)
  let _unused = is_new in

  match (React.S.value state).state_current with
  | KasimAsWebWorker ->
    Lwt.return
      (Result_util.ok
         (new Runtime_web_workers.runtime_kasim_as_web_worker ()
           : Api.concrete_manager))
  | KaSimAsEmbedded ->
    Lwt.return
      (Result_util.ok
         (new Runtime_web_workers.runtime_kasim_embedded_in_main_thread ()
           : Api.concrete_manager))
  | Remote { label = _; protocol = HTTP url } ->
    let error_msg : string =
      Format.sprintf
        "REST server is not supported anymore and url \"%s\" was provided." url
    in
    Lwt.return (Api_common.err_result_of_string error_msg)
  | Remote { label; protocol = CLI cli } ->
    let () =
      Common.debug ~loc:__LOC__
        (Js.string
           ("[State_runtime.create_manager] set_runtime_url CLI: " ^ cli.url))
    in
    (try
       let js_node_runtime =
         new Runtime_processes.manager cli.command cli.args
       in
       if js_node_runtime#is_running then (
         let () =
           Common.debug ~loc:__LOC__
             (Js.string
                "[State_runtime.create_manager] set_runtime_url: success")
         in
         Lwt.return (Result_util.ok (js_node_runtime :> Api.concrete_manager))
       ) else (
         let () =
           Common.debug ~loc:__LOC__
             (Js.string
                "[State_runtime.create_manager] set_runtime_url: failure")
         in
         let error_msg : string =
           Format.sprintf "Could not start cli runtime %s " label
         in
         Lwt.return (Api_common.err_result_of_string error_msg)
       )
     with Failure x -> Lwt.return (Api_common.err_result_of_string x))

let set_spec runtime =
  let current_state = React.S.value state in
  set_state
    { state_current = runtime; state_runtimes = current_state.state_runtimes }

let create_spec ~load (id : string) : unit Api.result =
  match read_spec id with
  | None ->
    let error_msg : string =
      Format.sprintf "Failed to create spec: could not parse identifier %s" id
    in
    Api_common.err_result_of_string error_msg
  | Some runtime ->
    let current_state = React.S.value state in
    let () =
      if not (List.mem runtime current_state.state_runtimes) then
        set_state
          {
            state_current = current_state.state_current;
            state_runtimes = runtime :: current_state.state_runtimes;
          }
    in
    let () = if load then set_spec runtime in
    Result_util.ok ()

let model : model React.signal =
  React.S.map
    (fun state ->
      {
        model_current = state.state_current;
        model_runtimes = state.state_runtimes;
      })
    state

(* run on application init *)
let init () =
  (* get url of host *)
  let hosts = Common_state.url_args "host" in
  let rec add_urls urls load =
    match urls with
    | [] -> ()
    | url :: urls ->
      (match create_spec ~load url with
      | { Result_util.value = Result.Ok (); _ } -> add_urls urls false
      | { Result_util.value = Result.Error _; _ } -> add_urls urls load)
  in
  let () = add_urls hosts true in
  match (React.S.value state).state_current with
  | Remote { protocol = CLI _; _ } | KasimAsWebWorker | KaSimAsEmbedded ->
    Lwt.return_nil
  | Remote { label = _; protocol = HTTP url } ->
    let error_msg : string =
      Format.sprintf
        "REST server is not supported anymore and url \"%s\" was provided." url
    in
    let () = Common.error ~loc:__LOC__ (Js.string error_msg) in
    Lwt.return_nil

(* to sync state of application with runtime *)
let sync () = Lwt.return_unit
