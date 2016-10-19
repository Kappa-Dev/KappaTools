module WebMessage = Mpi_message_v1_j
module ApiTypes = Api_types_v1_j
module IntMap = Mods.IntMap

open Lwt.Infix

let request_handler
    (post_message : string -> unit Lwt.t)
    (id : WebMessage.id)
    (request : 'a)
    (api_call : 'a -> 'b Lwt.t)
    (response : 'b -> WebMessage.response)
  : unit Lwt.t
  =
  api_call request >>=
  (fun result ->
     let message :  WebMessage.response WebMessage.message =
       { WebMessage.id = id ; WebMessage.data = response result} in
     let message_text : string =
       WebMessage.string_of_message WebMessage.write_response message in
     post_message message_text)

let on_message
    (runtime : Api_v1.api_runtime)
    (post_message : string -> unit Lwt.t)
    (text_message : string) : unit Lwt.t =
  let message : WebMessage.request WebMessage.message =
    WebMessage.message_of_string WebMessage.read_request text_message
  in
  match message.WebMessage.data with
  | `Parse code ->
    request_handler
      post_message
      message.WebMessage.id
      code
      runtime#parse
      (fun response -> `Parse response)
  | `Start parameter ->
    request_handler
      post_message
      message.WebMessage.id
      parameter
      runtime#start
      (fun response -> `Start response)
  | `Status token ->
    request_handler
      post_message
      message.WebMessage.id
      token
      runtime#status
      (fun state -> `Status state)
  | `List unit ->
    request_handler
      post_message
      message.WebMessage.id
      unit
      runtime#list
      (fun catalog -> `List catalog)
  | `Stop token ->
    request_handler
      post_message
      message.WebMessage.id
      token
      runtime#stop
      (fun result -> `Stop result)
  | `Peturbation peturbation ->
    request_handler
      post_message
      message.WebMessage.id
      (peturbation.WebMessage.perturbation_token,
       { Api_types_v1_j.perturbation_code =
           peturbation.WebMessage.perturbation_code })
      (fun (token,peturbation) -> runtime#perturbate token peturbation)
      (fun result -> `Peturbation result)
  | `Pause token ->
    request_handler
      post_message
      message.WebMessage.id
      token
      runtime#pause
      (fun result -> `Pause result)
  | `Continue continuation ->
    request_handler
      post_message
      message.WebMessage.id
      (continuation.WebMessage.continuation_token,
       continuation.WebMessage.continuation_parameter)
      (fun (token,parameter) -> runtime#continue token parameter)
      (fun result -> `Continue result)


type context = { mailboxes : WebMessage.response Lwt.u IntMap.t
               ; id : int }
exception TimeOut
exception BadResponse of WebMessage.response

class virtual runtime ?(timeout : float = 10.) () =
  object(self)
    val mutable context =
      { mailboxes = IntMap.empty ; id = 0 }
    method virtual sleep : float -> unit Lwt.t

    method private send (request : WebMessage.request):
      WebMessage.response Lwt.t =
      let result,feeder = Lwt.task () in
      let () = context <- { context with id = context.id + 1 } in
      let message :  WebMessage.request WebMessage.message =
        { WebMessage.id = context.id ; data = request } in
      let message_text : string =
        WebMessage.string_of_message WebMessage.write_request message in
      let () = self#post_message message_text in
      let () = context <-
          { context with
            mailboxes = IntMap.add context.id feeder context.mailboxes } in
      Lwt.pick [self#sleep timeout >>= (fun () -> Lwt.fail TimeOut); result]

    method virtual post_message : string -> unit
    method receive (response_text : string) =
      let message : WebMessage.response WebMessage.message =
        WebMessage.message_of_string WebMessage.read_response response_text in
      match IntMap.find_option message.WebMessage.id context.mailboxes with
      | Some value -> Lwt.wakeup value message.WebMessage.data
      | None -> ()

    method parse (code : ApiTypes.code) :
      Api_types_v1_j.parse Api_types_v1_j.result Lwt.t =
      self#send (`Parse code) >>=
      (function
        | `Parse error -> Lwt.return error
        | response -> Lwt.fail (BadResponse response)
      )

    method start (parameter : ApiTypes.parameter) :
      ApiTypes.token ApiTypes.result Lwt.t =
      self#send (`Start parameter) >>=
      (function
        | `Start token -> Lwt.return token
        | response -> Lwt.fail (BadResponse response)
      )

    method status (token : ApiTypes.token) :
      ApiTypes.state ApiTypes.result Lwt.t =
      self#send (`Status token) >>=
      (function
        | `Status state -> Lwt.return state
        | response -> Lwt.fail (BadResponse response)
      )

    method list () : ApiTypes.catalog ApiTypes.result Lwt.t =
      self#send (`List ()) >>=
      (function
        | `List catalog -> Lwt.return catalog
        | response -> Lwt.fail (BadResponse response)
      )

    method stop (token : ApiTypes.token) : unit ApiTypes.result Lwt.t =
      self#send (`Stop token) >>=
      (function
        | `Stop unit -> Lwt.return unit
        | response -> Lwt.fail (BadResponse response)
      )

    method perturbate
        (token : ApiTypes.token)
        (perturbation : ApiTypes.perturbation) :
      unit ApiTypes.result Lwt.t =
      let perturbation = { WebMessage.perturbation_token = token ;
                           perturbation_code = perturbation.ApiTypes.perturbation_code ; } in
      self#send (`Peturbation perturbation) >>=
      (function
        | `Peturbation unit -> Lwt.return unit
        | response -> Lwt.fail (BadResponse response)
      )

    method pause (token : ApiTypes.token) :
      unit ApiTypes.result Lwt.t =
      self#send (`Pause token) >>=
      (function
        | `Pause r -> Lwt.return r
        | response -> Lwt.fail (BadResponse response)
      )

    method continue
        (token : ApiTypes.token)
        (parameter : ApiTypes.parameter) :
      unit ApiTypes.result Lwt.t =
      self#send (`Continue { WebMessage.continuation_token = token ;
                             WebMessage.continuation_parameter = parameter ; })
      >>=
      (function
         | `Continue unit -> Lwt.return unit
         | response -> Lwt.fail (BadResponse response)
      )

  end

let message_delimter : char = '\x1e' (* "\t" *)
