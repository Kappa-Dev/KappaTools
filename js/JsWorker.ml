module IntMap = Map.Make(struct type t = int let compare = compare end)
module WebMessage = WebMessage_j
module ApiTypes = ApiTypes_j

open WebMessage
open Lwt

type context = { worker : (string, string) Worker.worker Js.t
               ; mailboxes : WebMessage.response option Lwt_mvar.t IntMap.t
               ; id : int }
exception TimeOut
exception BadResponse of WebMessage.response

class runtime ?(timeout : float = 10.) ()
  =
object(self)
  val mutable context = { worker = Worker.create "WebWorker.js"
                        ; mailboxes = IntMap.empty
                        ; id = 0 }
  method private send (request : WebMessage.request) : WebMessage.response option Lwt_mvar.t =
    let var : WebMessage.response option Lwt_mvar.t = Lwt_mvar.create_empty () in
    let () = Lwt.async (fun () -> Lwt_js.sleep timeout >>= (fun _ -> Lwt_mvar.put var None)) in
    let () = context <- { context with id = context.id + 1 } in
    let message :  WebMessage.request WebMessage.message =
      { WebMessage.id = context.id ; data = request } in
    let message_text : string =
      WebMessage.string_of_message
        WebMessage.write_request
        message in
    let () = (context.worker)##postMessage(message_text) in
    let () = context <- { context with mailboxes = IntMap.add context.id var context.mailboxes } in
    let () = context.worker##onmessage <-
               (Dom.handler (fun (response_message : string Worker.messageEvent Js.t) ->
                             let response_text : string = response_message##data in
                             let message : WebMessage.response WebMessage.message =
                               WebMessage.message_of_string
                                 WebMessage.read_response
                                 response_text in
                             try let value : WebMessage.response option Lwt_mvar.t =
                                   IntMap.find message.WebMessage.id context.mailboxes in
                                 let () = Lwt.async (fun () -> Lwt_mvar.put value (Some message.data)) in
                                 Js._true
                             with Not_found -> Js._true
                            ))
                             in
    var

  method parse (code : ApiTypes.code) : ApiTypes.error Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`Parse code) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       let () = Firebug.console##log (Js.string "Parse ...") in
       match response with
         None -> Lwt.fail TimeOut
       | Some (`Parse error) -> Lwt.return error
       | Some response -> Lwt.fail (BadResponse response)
      )

  method start (parameter : ApiTypes.parameter) : ApiTypes.token ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`Start parameter) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       let () = Firebug.console##log (Js.string "Start ...") in
       match response with
         None ->
         let () = Firebug.console##log (Js.string "Timeout ...") in
         Lwt.fail TimeOut
       | Some (`Start token) -> Lwt.return token
       | Some response -> Lwt.fail (BadResponse response)
      )

  method status (token : ApiTypes.token) : ApiTypes.state ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`Status token) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       let () = Firebug.console##log (Js.string "Status ...") in
       match response with
         None -> Lwt.fail TimeOut
       | Some (`Status state) -> Lwt.return state
       | Some response -> Lwt.fail (BadResponse response)
      )

  method list () : ApiTypes.catalog ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`List ()) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       let () = Firebug.console##log (Js.string "List ...") in
       match response with
         None -> Lwt.fail TimeOut
       | Some (`List catalog) -> Lwt.return catalog
       | Some response -> Lwt.fail (BadResponse response)
      )

  method stop (token : ApiTypes.token) : unit ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`Stop token) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       let () = Firebug.console##log (Js.string "Stop ...") in
       match response with
         None -> Lwt.fail TimeOut
       | Some (`Stop unit) -> Lwt.return unit
       | Some response -> Lwt.fail (BadResponse response)
      )
end
