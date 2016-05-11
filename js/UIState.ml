module ApiTypes = ApiTypes_j

open ApiTypes
open Lwt

exception InvalidState of string
let model_parse , set_model_parse =
  React.S.create (None : ApiTypes.parse option)
let model_text, set_model_text =
  React.S.create ""
let model_error, set_model_error =
  React.S.create ([] : Api_types.error)
let model_is_running , set_model_is_running =
  React.S.create false
let model_max_events, set_model_max_events =
  React.S.create None
let model_max_time, set_model_max_time =
  React.S.create None
let model_nb_plot, set_model_nb_plot =
  React.S.create 150
let opened_filename, set_opened_filename =
  React.S.create "model.ka"
let model_runtime_state , set_model_runtime_state =
  React.S.create (None : ApiTypes.state option)

type runtime_remote = { label : string ; url : string }
type runtime = WebWorker | Embedded | Remote of runtime_remote

let runtime_label runtime = match runtime with
                             WebWorker -> "WebWorker"
                           | Embedded -> "Embedded"
                           | Remote remote -> remote.label

let runtime_value runtime = match runtime with
                             WebWorker -> "WebWorker"
                           | Embedded -> "Embedded"
                           | Remote remote -> remote.url

class embedded_runtime ()  = object
  val delayed_yield : unit -> unit Lwt.t = Api.time_yield 0.1 Lwt_js.yield
  method yield () = delayed_yield ()
  method log ?exn (_: string) = Lwt.return_unit
  inherit Api.Base.runtime
end

let default_runtime = WebWorker
let runtime_state : Api.runtime option ref = ref None
let set_runtime_url (url : string) (continuation : bool -> unit) : unit =
  try
  let () = set_model_error [] in
  if url = "WebWorker" then
    let () = runtime_state := Some (new JsWorker.runtime () :> Api.runtime) in
    let () = continuation true in
    ()
  else if url = "Embedded" then
    let () = runtime_state := Some (new embedded_runtime () :> Api.runtime) in
    let () = continuation true in
    ()
  else
    let version_url : string = Format.sprintf "%s/v1/version" url  in
    let () =
      Lwt.async
        (fun () ->
          (XmlHttpRequest.perform_raw
             ~response_type:XmlHttpRequest.Text
             version_url)
          >>=
            (fun frame ->
              let is_valid_server : bool = frame.XmlHttpRequest.code = 200 in
              let () = if is_valid_server then
                  runtime_state :=
                    Some (new JsRemote.runtime url :> Api.runtime)
                else
                  let error_msg : string =
                    Format.sprintf "Bad Response %d from %s "
                    frame.XmlHttpRequest.code
                    url
                  in
                  set_model_error [error_msg]
              in
              let () = continuation is_valid_server in
              Lwt.return_unit
            )
        )
    in
    ()
  with _ -> continuation false
let set_runtime (runtime : runtime) (continuation : bool -> unit) : unit =
  set_runtime_url (runtime_value runtime) continuation

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
let poll_interval : float = 2.
let update_runtime_state
      thread_is_running
      on_error token =
  let do_update () =
    match !runtime_state with
      None -> set_model_error ["Runtime not available"];
    | Some runtime_state ->
       Lwt.async (fun () ->
                  if Lwt_switch.is_on thread_is_running then
                  (runtime_state#status token)
                  >>=
                    (fun result ->
                      match result with
                        `Left e ->
                          let () = set_model_error e in
                          let () = List.iter Common.error e in
                          on_error ()
                      | `Right state ->
                        let () = set_model_error [] in
                        let () = set_model_runtime_state (Some state) in
                        if state.is_running then
                          Lwt.return_unit
                        else
                          (Lwt_switch.turn_off thread_is_running)
                           >>=
                            (fun _ -> Lwt.return_unit))
                  else
                    return_unit
                 ) in
  let rec aux () =
    let () = if Lwt_switch.is_on thread_is_running then do_update () else () in
    if Lwt_switch.is_on thread_is_running then
      Lwt_js.sleep poll_interval >>= aux
    else
      Lwt.return_unit
  in aux ()


let start_model ~start_continuation
                ~stop_continuation =
  let thread_is_running = Lwt_switch.create () in
  let on_error () = set_model_is_running false;
                    stop_continuation ();
                    Lwt_switch.turn_off thread_is_running
  in
  match !runtime_state with
    None -> set_model_error ["Runtime not available"];
            on_error ()
  | Some runtime_state ->
     let () = set_model_error [] in
     catch
       (fun () ->
        let () =
          set_model_runtime_state (Some { plot = None;
                                          time = 0.0;
                                          time_percentage = None;
                                          event = 0;
                                          event_percentage = None;
                                          tracked_events = None;
                                          log_messages = [];
                                          snapshots = [];
                                          flux_maps = [];
                                          files = [];
                                          is_running = true
                                        }) in
        (runtime_state#start { code = React.S.value model_text;
                               nb_plot = React.S.value model_nb_plot;
                               max_time = React.S.value model_max_time;
                               max_events = React.S.value model_max_events
                             })
        >>=
          (fun result ->
            match result with
              `Left error ->
                let () = set_model_error error in
               set_model_is_running false;
                on_error ()
            | `Right token ->
              let () = set_model_error [] in
              let stop_process () : unit Lwt.t =
                (Lwt_switch.turn_off thread_is_running)
                >>=
                  (* sleep to give time for update to stop running *)
                  (fun _ -> Lwt_js.sleep (1.5 *. poll_interval))
                >>=
                  (fun _ -> runtime_state#stop token)
                >>=
                  (fun response ->
                     let () = match response with
                         (* safe to ignore as the process may have gone away
                            after the sleep *)
                         `Left error -> let () = Common.debug error in
                                        set_model_error []
                       | `Right _ -> set_model_error []
                     in
                     Lwt.return_unit)
              in
              let () = start_continuation stop_process in
              Lwt.join [ update_runtime_state thread_is_running on_error token ]
              >>=
                (fun _ -> Lwt_js.sleep 1.)
              >>=
                (fun _ -> stop_continuation ();
                  set_model_is_running false;
                  Lwt.return_unit)
          )
       )
       (function

         | Invalid_argument error ->
            let message = Format.sprintf "Runtime error %s" error in
            let () = set_model_error (message::[]) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | Sys_error message ->
            let () = set_model_error (message::[]) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | e -> on_error ()
                >>= (fun _ -> fail e)
       )

let stop_model token = match !runtime_state with
    None -> set_model_error ["Runtime not available"];
  | Some runtime_state ->
     let () = Lwt.async (fun () -> (runtime_state#stop token)
                                   >>=
                                     (fun result ->
                                      match result with
                                        `Left error ->
                                        let () = set_model_error error in
                                        Lwt.return_unit
                                      | `Right () ->
                                         let () = set_model_error [] in
                                         Lwt.return_unit))
     in ()
