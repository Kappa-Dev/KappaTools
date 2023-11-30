(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type cli = { url: string; command: string; args: string list }
type protocol = HTTP of string | CLI of cli
type remote = { label: string; protocol: protocol }
type spec = WebWorker | Embedded | Remote of remote
type state = { state_current: spec; state_runtimes: spec list }
type model = { model_current: spec; model_runtimes: spec list }

let spec_label : spec -> string = function
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote remote -> remote.label

let spec_id : spec -> string = function
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote { label = _; protocol = HTTP http } -> http
  | Remote { label = _; protocol = CLI cli } -> cli.url

let read_spec : string -> spec option = function
  | "WebWorker" -> Some WebWorker
  | "Embedded" -> Some Embedded
  | url ->
    let () = Common.debug (Js.string (Format.sprintf "parse_remote: %s" url)) in
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
        Common.debug (Js.string (Format.sprintf "cleaned : %s" cleaned))
      in
      format_url cleaned
    in
    (match Url.url_of_string url with
    | None -> None
    | Some parsed ->
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

class embedded () : Api.concrete_manager =
  let kasa_worker = Worker.create "KaSaWorker.js" in
  let kasa_mailbox = Kasa_client.new_mailbox () in
  let kamoha_worker = Worker.create "KaMoHaWorker.js" in
  let kamoha_mailbox = Kamoha_client.new_mailbox () in
  let kastor_worker = Worker.create "KaStorWorker.js" in
  let stor_state, update_stor_state = Kastor_client.init_state () in
  object (self)
    initializer
      let () =
        kasa_worker##.onmessage :=
          Dom.handler
            (fun (response_message : string Worker.messageEvent Js.t) ->
              let response_text : string = response_message##.data in
              let () = Kasa_client.receive kasa_mailbox response_text in
              Js._true)
      in
      let () =
        kamoha_worker##.onmessage :=
          Dom.handler
            (fun (response_message : string Worker.messageEvent Js.t) ->
              let response_text : string = response_message##.data in
              let () = Kamoha_client.receive kamoha_mailbox response_text in
              Js._true)
      in
      let () =
        kastor_worker##.onmessage :=
          Dom.handler
            (fun (response_message : string Worker.messageEvent Js.t) ->
              let response_text : string = response_message##.data in
              let () = Kastor_client.receive update_stor_state response_text in
              Js._true)
      in
      ()

    inherit
      Api_runtime.manager
        (object
           method min_run_duration () = 0.1
           method yield = Js_of_ocaml_lwt.Lwt_js.yield

           method log ?exn (msg : string) =
             let () = ignore exn in
             let () =
               Common.debug
                 (Js.string (Format.sprintf "embedded_manager#log: %s" msg))
             in
             Lwt.return_unit
         end
          : Kappa_facade.system_process)

    inherit
      Kasa_client.new_uniform_client
        ~is_running:(fun () -> true)
        ~post:(fun message_text -> kasa_worker##postMessage message_text)
        kasa_mailbox

    inherit
      Kamoha_client.new_client
        ~post:(fun message_text -> kamoha_worker##postMessage message_text)
        kamoha_mailbox

    inherit
      Kastor_client.new_client
        ~post:(fun message_text -> kastor_worker##postMessage message_text)
        stor_state

    method is_running = true

    method terminate =
      let () = kasa_worker##terminate in
      () (*TODO*)

    method is_computing = true (*TODO*)
    val mutable kasa_locator = []

    method project_parse ~patternSharing overwrites =
      self#secret_project_parse
      >>= Api_common.result_bind_lwt ~ok:(fun out ->
              let load =
                self#secret_simulation_load patternSharing out overwrites
              in
              let init = self#init_static_analyser out in
              let locators =
                init
                >>= Result_util.fold
                      ~error:(fun e ->
                        let () = kasa_locator <- [] in
                        Lwt.return (Result_util.error e))
                      ~ok:(fun () ->
                        self#secret_get_pos_of_rules_and_vars
                        >>= Result_util.fold
                              ~ok:(fun infos ->
                                let () = kasa_locator <- infos in
                                Lwt.return (Result_util.ok ()))
                              ~error:(fun e ->
                                let () = kasa_locator <- [] in
                                Lwt.return (Result_util.error e)))
              in
              load >>= Api_common.result_bind_lwt ~ok:(fun () -> locators))

    method get_influence_map_node_at ~filename pos : _ Api.result Lwt.t =
      List.find_opt
        (fun (_, x) -> Loc.is_included_in filename pos x)
        kasa_locator
      |> Option_util.map fst
      |> Result_util.ok ?status:None
      |> Lwt.return
  end

let state, set_state =
  React.S.create
    { state_current = WebWorker; state_runtimes = [ WebWorker; Embedded ] }

let create_manager ~is_new project_id =
  match (React.S.value state).state_current with
  | WebWorker ->
    let () = State_settings.set_synch false in
    Lwt.return
      (Result_util.ok (new Web_worker_api.manager () : Api.concrete_manager))
  | Embedded ->
    let () = State_settings.set_synch false in
    Lwt.return (Result_util.ok (new embedded () : Api.concrete_manager))
  | Remote { label = _; protocol = HTTP url } ->
    let version_url : string = Format.sprintf "%s/v2" url in
    let () =
      Common.debug
        (Js.string (Format.sprintf "set_runtime_url: %s" version_url))
    in
    Js_of_ocaml_lwt.XmlHttpRequest.perform_raw
      ~response_type:XmlHttpRequest.Text version_url
    >>= fun frame ->
    let is_valid_server : bool =
      frame.Js_of_ocaml_lwt.XmlHttpRequest.code = 200
    in
    if is_valid_server then (
      let () = State_settings.set_synch true in
      let manager = new Rest_api.manager ~timeout:None ~url ~project_id in
      (if is_new then
         manager#project_create
           { Api_types_j.project_parameter_project_id = project_id }
       else
         Lwt.return (Result_util.ok ()))
      >>= Api_common.result_bind_lwt ~ok:(fun () ->
              Lwt.return (Result_util.ok (manager :> Api.concrete_manager)))
    ) else (
      let error_msg : string =
        Format.sprintf "Bad Response %d from %s "
          frame.Js_of_ocaml_lwt.XmlHttpRequest.code url
      in
      Lwt.return (Api_common.result_error_msg error_msg)
    )
  | Remote { label; protocol = CLI cli } ->
    let () = Common.debug (Js.string ("set_runtime_url: " ^ cli.url)) in
    (try
       let js_node_runtime = new JsNode.manager cli.command cli.args in
       if js_node_runtime#is_running then (
         let () = Common.debug (Js.string "set_runtime_url:sucess") in
         let () = State_settings.set_synch false in
         Lwt.return (Result_util.ok (js_node_runtime :> Api.concrete_manager))
       ) else (
         let () = Common.debug (Js.string "set_runtime_url:failure") in
         let error_msg : string =
           Format.sprintf "Could not start cli runtime %s " label
         in
         Lwt.return (Api_common.result_error_msg error_msg)
       )
     with Failure x -> Lwt.return (Api_common.result_error_msg x))

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
    Api_common.result_error_msg error_msg
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
  | Remote { protocol = CLI _; _ } | WebWorker | Embedded -> Lwt.return_nil
  | Remote { label = _; protocol = HTTP url } ->
    let version_url : string = Format.sprintf "%s/v2" url in
    let () =
      Common.debug
        (Js.string (Format.sprintf "set_runtime_url: %s" version_url))
    in
    Js_of_ocaml_lwt.XmlHttpRequest.perform_raw
      ~response_type:XmlHttpRequest.Text version_url
    >>= fun frame ->
    let is_valid_server : bool =
      frame.Js_of_ocaml_lwt.XmlHttpRequest.code = 200
    in
    if is_valid_server then (
      let () = State_settings.set_synch true in
      let manager = new Rest_api.manager ~timeout:None ~url ~project_id:"" in
      manager#project_catalog
      >>= Result_util.fold
            ~ok:(fun projects -> Lwt.return projects)
            ~error:(fun _ -> Lwt.return_nil)
    ) else
      Lwt.return_nil

(* to sync state of application with runtime *)
let sync () = Lwt.return_unit
