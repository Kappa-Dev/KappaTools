(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type cli = { url : string;
             command : string;
             args : string list }


type protocol = | HTTP of string
                | CLI of cli
type remote = { label : string ;
                protocol : protocol ; }

type spec = | WebWorker
            | Embedded
            | Remote of remote

type state = { state_manager : Api.manager ;
               state_current : spec ;
               state_runtimes : spec list ;
             }

type model = { model_current : spec ; model_runtimes : spec list ; }

let spec_label : spec -> string =
  function
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote remote -> remote.label

let spec_id : spec -> string =
  function
  | WebWorker -> "WebWorker"
  | Embedded -> "Embedded"
  | Remote { label = _ ; protocol = HTTP http } -> http
  | Remote { label = _ ; protocol = CLI cli } -> cli.url

let read_spec : string -> spec option =
  function
  | "WebWorker" -> Some WebWorker
  | "Embedded" -> Some Embedded
  | url ->
  let () =
    Common.debug (Js.string (Format.sprintf "parse_remote: %s" url)) in
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
    let () =
      Common.debug (Js.string (Format.sprintf "cleaned : %s" cleaned)) in
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
    Some (Remote { label = label;
                   protocol = protocol ; })

class embedded () : Api.manager =
  object
    inherit Api_runtime.manager
        (object
          method min_run_duration () = 0.1
          method yield = Lwt_js.yield
          method log ?exn (msg: string) =
            let () = ignore(exn) in
            let () = Common.debug (Js.string (Format.sprintf "embedded_manager#log: %s" msg))
            in
            Lwt.return_unit
          end : Kappa_facade.system_process)
end

(* One global embedded and webworker
   this allows the state to be preserved
   when other runtimes are selected. *)
let webworker_manager : Api.manager = (new Web_worker_api.manager () :> Api.manager)
let embedded_manager : Api.manager = (new embedded () :> Api.manager)

let state , set_state =
  React.S.create
    { state_manager =  webworker_manager;
      state_current = WebWorker ;
      state_runtimes = [ WebWorker ; Embedded ; ] ;
    }


let create_manager (id : string) : spec Api.result =
  match read_spec id with
  | None ->
    let error_msg : string =
      Format.sprintf "Failed to create manager : could not parse identifier %s " id
    in
    Api_common.result_error_msg error_msg
  | Some runtime ->
    let current_state = React.S.value state  in
    let () = set_state
        { current_state
          with state_runtimes = runtime::current_state.state_runtimes } in
    Api_common.result_ok runtime



let set_manager (id : string) : unit Api.result Lwt.t =
  let current_state : state = React.S.value state in
  match read_spec id with
  | Some WebWorker ->
    let () = set_state { current_state with
                         state_manager = webworker_manager ;
                         state_current = WebWorker ; }
    in
    Lwt.return (Api_common.result_ok ())
  | Some Embedded ->
    let () = set_state { current_state with
                         state_manager = embedded_manager ;
                         state_current = Embedded ; }
    in
    Lwt.return (Api_common.result_ok ())
  | Some (Remote { label = label ; protocol = HTTP url }) ->
    let version_url : string = Format.sprintf "%s/v2" url in
    let () = Common.debug (Format.sprintf "set_runtime_url: %s" version_url) in
    (XmlHttpRequest.perform_raw
       ~response_type:XmlHttpRequest.Text
       version_url)
    >>=
    (fun frame ->
       let is_valid_server : bool = frame.XmlHttpRequest.code = 200 in
       if is_valid_server then
         let () = set_state { current_state with
                              state_manager = (new Rest_api.manager url :> Api.manager) ;
                              state_current = Remote { label = label ; protocol = HTTP url } ; }
         in
         Lwt.return (Api_common.result_ok ())
       else
         let error_msg : string =
           Format.sprintf "Bad Response %d from %s "
             frame.XmlHttpRequest.code
             url
         in
         Lwt.return (Api_common.result_error_msg error_msg)
    )

  | Some (Remote { label = label ; protocol = CLI cli }) ->
        let () = Common.debug (Format.sprintf "set_runtime_url: %s" cli.url) in
        let js_node_runtime =
          new JsNode.manager
            cli.command
            (["--development"]@cli.args )in
        if js_node_runtime#is_running () then
          let () = Common.debug (Js.string "set_runtime_url:sucess") in
          let () = set_state { current_state with
                               state_manager = (js_node_runtime :> Api.manager) ;
                               state_current = Remote { label = label ;
                                                        protocol = CLI cli } }
          in
          Lwt.return (Api_common.result_ok ())
        else
          let () = Common.debug (Js.string "set_runtime_url:failure") in
          let error_msg : string =
            Format.sprintf "Could not start cli runtime %s " id
          in
          Lwt.return (Api_common.result_error_msg error_msg)
  | None ->
    let error_msg : string =
      Format.sprintf "Failed to set manager : could not parse identifier %s " id
    in
    Lwt.return (Api_common.result_error_msg error_msg)

let model : model React.signal =
  React.S.map
    (fun state -> { model_current = state.state_current ;
                    model_runtimes = state.state_runtimes ; })
    state
let get_manager () : Api.manager =
  (React.S.value state).state_manager

(* run on application init *)
let init () =
  (* get url of host *)
  let hosts = Common_state.url_args "host" in
  let rec add_urls urls load_url : unit Lwt.t =
    match urls with
    | [] -> Lwt.return_unit
    | url::urls ->
      Api_common.result_map
        ~ok:(fun _ (runtime : spec) ->
            (* If a user specified runtime has not been successfuly loaded
               try to load.
            *)
            if load_url then
              (set_manager (spec_id runtime)) >>=
              (Api_common.result_map
                 ~ok:(fun _ () -> add_urls urls false)
                 ~error:(fun _ (errors : Api_types_j.errors) ->
                     let msg =
                       Format.sprintf
                         "failed loading url %s error %s"
                         url
                         (Api_types_j.string_of_errors errors)
                     in
                     let () = Common.debug (Js.string (Format.sprintf "State_runtime.init : 1 : %s" msg)) in
                     add_urls urls true)
              )
            else
              (* If not successful keep trying. *)
              add_urls urls load_url
          )
        ~error:(fun _ (errors : Api_types_j.errors) ->
            let msg =
              Format.sprintf
                "failed parsing url %s error %s"
                url
                (Api_types_j.string_of_errors errors)
            in
            (* If a url failed to parse continue on. *)
            let () = Common.debug (Js.string (Format.sprintf "State_runtime.init : 2 : %s" msg)) in
            add_urls urls load_url
          )
      (create_manager url)
  in
  add_urls hosts true

(* to sync state of application with runtime *)
let sync () = Lwt.return_unit
