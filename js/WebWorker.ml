module WebMessage = WebMessage_j

open Lwt
open WebMessage

let time_yield (seconds : float) (yield : (unit -> unit Lwt.t)) : (unit -> unit Lwt.t) =
  let lastyield = ref (Sys.time ()) in
  fun () -> let t = Sys.time () in
            if t -. !lastyield > seconds then
              let () = lastyield := t in
              yield ()
            else Lwt.return_unit

let runtime = (new Api.Base.runtime (time_yield 0.01 Lwt_js.yield) :> Api.runtime)

let request_handler
      (id : WebMessage.id)
      (request : 'a)
      (api_call : 'a -> 'b Lwt.t)
      (response : 'b -> WebMessage.response) : unit
  =
  let () = Lwt.async (fun () -> (api_call request)
                                >>=
                                  (fun (result : 'b) ->
                                   let message :  WebMessage.response WebMessage.message =
                                     { id = id ; data = response result} in
                                   let message_text : string =
                                     WebMessage.string_of_message
                                       WebMessage.write_response
                                       message in
                                   let () = Worker.post_message message_text in
                                   return_unit)
                     )
  in ()

let on_message (text_message : string) =
  let message : WebMessage.request WebMessage.message =
    WebMessage.message_of_string
      WebMessage.read_request
      text_message
  in
  match message.WebMessage.data with
    `Parse code ->
     request_handler
      message.WebMessage.id
      code
      runtime#parse
      (fun response -> `Parse response)
  | `Start parameter ->
     request_handler
      message.WebMessage.id
      parameter
      runtime#start
      (fun response -> `Start response)
  | `Status token ->
     request_handler
      message.WebMessage.id
      token
      runtime#status
      (fun state -> `Status state)
  | `List unit ->
     request_handler
      message.WebMessage.id
      unit
      runtime#list
      (fun catalog -> `List catalog)
  | `Stop token ->
     request_handler
       message.WebMessage.id
       token
       runtime#stop
       (fun result -> `Stop result)

let () = Worker.set_onmessage on_message
