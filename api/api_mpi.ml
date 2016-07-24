module WebMessage = Mpi_message_j
module ApiTypes = ApiTypes_j
module IntMap = Mods.IntMap

open Lwt
open WebMessage

let request_handler
    (post_message : string -> unit)
    (id : WebMessage.id)
    (request : 'a)
    (api_call : 'a -> 'b Lwt.t)
    (response : 'b -> WebMessage.response)
  : unit
  =
  let () =
    Lwt.async
      (fun () ->
         (api_call request)
         >>=
         (fun (result : 'b) ->
            let message :  WebMessage.response WebMessage.message =
              { id = id ; data = response result} in
            let message_text : string =
              WebMessage.string_of_message
                WebMessage.write_response
                message in
            let () = post_message message_text in
            return_unit)
      )
  in ()

let on_message
    (runtime : Api_v1.runtime)
    (post_message : string -> unit)
    (text_message : string) : unit =
  let message : WebMessage.request WebMessage.message =
    WebMessage.message_of_string
      WebMessage.read_request
      text_message
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

type context = { mailboxes : WebMessage.response option Lwt_mvar.t IntMap.t
               ; id : int }
exception TimeOut
exception BadResponse of WebMessage.response

class virtual runtime ?(timeout : float = 10.) ()
  =
  object(self)
    val mutable context =
      { mailboxes = IntMap.empty
      ; id = 0 }
    method virtual sleep : float -> unit Lwt.t


    method private send (request : WebMessage.request) :
      WebMessage.response option Lwt_mvar.t =
      let var : WebMessage.response option Lwt_mvar.t =
	Lwt_mvar.create_empty ()
      in
      let () =
	Lwt.async
          (fun () ->
             self#sleep timeout >>=
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
      let () = self#post_message message_text in
      let () = context <-
	  { context with
            mailboxes = IntMap.add context.id var context.mailboxes }
      in
      var

    method virtual post_message : string -> unit
    method receive (response_text : string) =
      let message : WebMessage.response WebMessage.message =
	WebMessage.message_of_string WebMessage.read_response response_text in
      let () =
	match IntMap.find_option message.WebMessage.id context.mailboxes with
        | Some value ->
          Lwt.async
            (fun () ->
	       Lwt_mvar.put value (Some message.WebMessage.data))
	| None -> ()
      in
      ()

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

  end
