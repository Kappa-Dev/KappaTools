module ApiTypes = Api_types_v1_j

open Lwt.Infix

exception InvalidState of string
let editor_full , set_editor_full =
  React.S.create (false : bool)
let model_parse , set_model_parse =
  React.S.create (None : ApiTypes.parse option)
let model_text, set_model_text =
  React.S.create ""
let model_error, set_model_error =
  React.S.create ([] : ApiTypes.errors)
let model_max_events, set_model_max_events =
  React.S.create (None : int option)
let model_max_time, set_model_max_time =
  React.S.create (None : float option)
let model_seed, set_model_seed =
  React.S.create (None : int option)
let model_plot_period, set_model_plot_period = React.S.create 1.
let opened_filename, set_opened_filename =
  React.S.create "model.ka"

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

class embedded_runtime ()  =
  object
    method yield = Lwt_js.yield
    method log ?exn (msg: string) =
      let () = Common.debug (Js.string msg) in
      Lwt.return_unit
    inherit Api_v1.Base.base_runtime 0.1
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

let runtime_state : Api_v1.api_runtime option ref = ref None
let set_runtime_url
    (url : string)
    (continuation : bool -> unit) : unit =
  try
    let () = set_model_error [] in
    if url = "WebWorker" then
      let () = runtime_state :=
          Some (new Web_worker_api_v1.runtime () :> Api_v1.api_runtime)
      in
      let () = continuation true in
      ()
    else if url = "Embedded" then
      let () = runtime_state :=
          Some (new embedded_runtime () :> Api_v1.api_runtime)
      in
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
                    runtime_state :=
                      Some (new Rest_api_v1.runtime url :> Api_v1.api_runtime)
                  else
                    let error_msg : string =
                      Format.sprintf "Bad Response %d from %s "
			frame.XmlHttpRequest.code
                        url
                    in
                    set_model_error
                      (Api_data_v1.api_message_errors error_msg)
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
        let js_node_runtime = new JsNode.runtime command [] in
        if js_node_runtime#is_running () then
          let () =
            Common.debug (Js.string "sucess")
          in
          let () = runtime_state :=
              Some (js_node_runtime :> Api_v1.api_runtime)
          in
          continuation true
        else
          let () = Common.debug (Js.string "failure") in
          continuation false
      | None -> continuation false
  with _ -> continuation false

let set_text text = set_model_text text
let parse_text text =
  let () = set_model_text text in
  let () = Lwt.async
      (fun () ->
	 (match !runtime_state with
            None ->
            Lwt.fail (InvalidState "Runtime state not available")
	  | Some runtime_state -> runtime_state#parse text)
	 >>=
         (fun error ->
            match error with
              `Left error ->
              let () = set_model_error error in
              let () = set_model_parse None in
              Lwt.return_unit
            | `Right parse ->
              let () = set_model_error [] in
              let () = set_model_parse (Some parse) in
              Lwt.return_unit
         )
      )
  in
  ()

let update_text text =
  let () = set_model_text text in
  (* Should reset everyting before
     running the parse.
  *)
  let () = set_model_error [] in
  let () = set_model_parse None in
  (* Then run parse *)
  let () = Lwt.async
      (fun () ->
	 (match !runtime_state with
            None -> Lwt.fail (InvalidState "Runtime state not available")
	  | Some runtime_state -> runtime_state#parse text)
	 >>=
         (fun error ->
            match error with
              `Left error ->
              let () = set_model_error error in
              let () = set_model_parse None in
              Lwt.return_unit
            | `Right parse ->
              let () = set_model_error [] in
              let () = set_model_parse (Some parse) in
              Lwt.return_unit
         )
      )
  in
  ()



(* return the agent count *)
let agent_count () : int option =
  match (React.S.value model_parse) with
  | None -> None
  | Some data ->
    let site_graph : ApiTypes.site_graph =
      Api_data_v1.api_contactmap_site_graph data in
    Some (Array.length site_graph)
