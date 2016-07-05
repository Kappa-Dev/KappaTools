module IntMap = Mods.IntMap
module WebMessage = WebMessage_j
module ApiTypes = ApiTypes_j

open Lwt

type context = { worker : (string, string) Worker.worker Js.t
               ; mailboxes : WebMessage.response option Lwt_mvar.t IntMap.t
               ; id : int }
exception TimeOut
exception BadResponse of WebMessage.response

class runtime ?(timeout : float = 10.) ()
  =
object(self)
  val mutable context =
    { worker = Worker.create "WebWorker.js"
    ; mailboxes = IntMap.empty
    ; id = 0 }

  method private send (request : WebMessage.request) :
    WebMessage.response option Lwt_mvar.t =
    let var : WebMessage.response option Lwt_mvar.t =
      Lwt_mvar.create_empty ()
    in
    let () =
      Lwt.async
        (fun () ->
          Lwt_js.sleep timeout >>=
          (fun _ ->
            Lwt_mvar.put var None))
    in
    let () = context <-
      { context with id = context.id + 1 }
    in
    let message :  WebMessage.request WebMessage.message =
      { WebMessage.id = context.id ;
        data = request }
    in
    let message_text : string =
      WebMessage.string_of_message
        WebMessage.write_request
        message in
    let () =
      (context.worker)
        ##
        postMessage(message_text)
    in
    let () = context <-
      { context with
        mailboxes = IntMap.add context.id var context.mailboxes }
    in
    let () = context.worker##onmessage <-
      (Dom.handler
         (fun (response_message : string Worker.messageEvent Js.t) ->
           let response_text : string =
             response_message##data
           in
           let message : WebMessage.response WebMessage.message =
             WebMessage.message_of_string
               WebMessage.read_response
               response_text in

           let () =
             match IntMap.find_option
                     message.WebMessage.id context.mailboxes with
             | Some value ->
               Lwt.async
                 (fun () ->
                    Lwt_mvar.put value (Some message.WebMessage.data))
             | None -> () in
           Js._true
         ))
    in
    var

  method info () :
    Api_types_j.info ApiTypes_j.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`Info ()) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       match response with
         None ->
         Lwt.fail TimeOut
       | Some (`Info info) ->
         Lwt.return info
       | Some response ->
         Lwt.fail (BadResponse response)
      )

  method parse (code : ApiTypes.code) :
    ApiTypes_j.parse ApiTypes_j.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t =
      self#send (`Parse code)
    in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       match response with
         None ->
         Lwt.fail TimeOut
       | Some (`Parse error) ->
         Lwt.return error
       | Some response ->
         Lwt.fail (BadResponse response)
      )

  method start (parameter : ApiTypes.parameter) :
    ApiTypes.token ApiTypes.result Lwt.t =
    let var :
        WebMessage.response option Lwt_mvar.t =
      self#send (`Start parameter)
    in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
        match response with
         None -> Lwt.fail TimeOut
        | Some (`Start token) -> Lwt.return token
        | Some response -> Lwt.fail (BadResponse response)
      )

  method status (token : ApiTypes.token) :
    ApiTypes.state ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t =
      self#send (`Status token)
    in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       match response with
         None ->
         Lwt.fail TimeOut
       | Some (`Status state) ->
         Lwt.return state
       | Some response ->
         Lwt.fail (BadResponse response)
      )

  method list () : ApiTypes.catalog ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t = self#send (`List ()) in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       match response with
         None ->
         Lwt.fail TimeOut
       | Some (`List catalog) ->
         Lwt.return catalog
       | Some response ->
         Lwt.fail (BadResponse response)
      )

  method stop (token : ApiTypes.token) : unit ApiTypes.result Lwt.t =
    let var : WebMessage.response option Lwt_mvar.t =
      self#send (`Stop token)
    in
    (Lwt_mvar.take var)
    >>=
      (fun (response : WebMessage.response option) ->
       match response with
         None ->
         Lwt.fail TimeOut
       | Some (`Stop unit) ->
         Lwt.return unit
       | Some response ->
         Lwt.fail (BadResponse response)
      )

  method shutdown () : unit ApiTypes.result Lwt.t = Lwt.return (`Right ())

end
