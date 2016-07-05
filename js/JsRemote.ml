open Lwt
open XmlHttpRequest
module WebMessage = WebMessage_j
module Api_types = ApiTypes_j

exception BadResponseCode of int
exception TimeOut

let hydrate
    (type a)
    (frame:((Js.js_string Js.t) XmlHttpRequest.generic_http_frame))
    (success:(string -> a))
    (fail:(string -> Api_types.errors))
    : a Api_types.result Lwt.t =
  if frame.code = 200 then
    Lwt.return
      (`Right
          (success
             (Js.to_string frame.content)))
  else if frame.code = 400 then
    Lwt.return
      (`Left
          (fail
             (Js.to_string frame.content)))
  else
    Lwt.fail
      (BadResponseCode frame.code)


let post
      (type a)
      (timeout : float)
      (data : string)
      (url : string)
      (success : (string -> a))
      (fail:(string -> Api_types.errors))
    : a Api_types.result Lwt.t =
    let var : (a Api_types.result) option Lwt_mvar.t =
      Lwt_mvar.create_empty ()
    in
    let () =
      Lwt.async
        (fun () ->
          Lwt_js.sleep timeout >>= (fun _ -> Lwt_mvar.put var None))
    in
    let request : XmlHttpRequest.xmlHttpRequest Js.t =
      XmlHttpRequest.create()
    in
    let () =
      request##_open(Js.string "POST",Js.string url, Js._true) in
    let () =
      request##setRequestHeader
        (Js.string "Content-type"
        ,Js.string "application/x-www-form-urlencoded") in
    let () =
      request##onload <-
        Dom.handler
        (fun e ->
          let msg : string =
            Js.to_string request##responseText in
          let () = Lwt.async  (fun () ->
            let result : a Api_types.result =
              if request##status == 200 then
                `Right(success msg)
              else
                `Left (fail msg)
            in
            Lwt_mvar.put var (Some result))
          in Js._true)
    in
    let () = request##send(Js.some (Js.string data)) in
    (Lwt_mvar.take var)
    >>=
    (fun response -> match response with
      None -> Lwt.fail TimeOut
    | Some response -> Lwt.return response)

class runtime ?(timeout:float = 10.0) ?shutdown_key url =
object(self)
  method private hydrate :
    'a. ((Js.js_string Js.t) XmlHttpRequest.generic_http_frame)
    -> (string -> 'a)
      -> (string -> Api_types.errors)
        -> 'a Api_types.result Lwt.t =
    fun frame success fail ->
    try
      if frame.code = 200 then
        Lwt.return
          (`Right
              (success
                 (Js.to_string frame.content)))
      else if frame.code = 400 then
        Lwt.return
          (`Left
              (fail
                 (Js.to_string frame.content)))
      else
        Lwt.return
          (`Left
              (Api_data.api_message_errors
                 (Format.sprintf "Unexpected Response code %d" frame.code)))
    with e ->
      Lwt.return
        (`Left
            (Api_data.api_message_errors
               (Printexc.to_string e)))

  method info () :
    Api_types_j.info Api_types.result Lwt.t =
    let url : string = Format.sprintf "%s" url in
    (XmlHttpRequest.perform_raw
       ~get_args:[]
       ~override_method:`GET
       ~response_type:Text
       url)
    >>=
      (fun frame ->
        self#hydrate
          frame
          Api_types_j.info_of_string
          Api_types.errors_of_string)


  method parse (code : Api_types.code) :
    Api_types.parse Api_types.result Lwt.t =
    let url : string = Format.sprintf "%s/v1/parse" url  in
    (XmlHttpRequest.perform_raw
       ~get_args:[("code",code)]
       ~override_method:`GET
       ~response_type:Text
       url)
    >>=
      (fun frame -> self#hydrate frame
                            Api_types.parse_of_string
                            Api_types.errors_of_string)

  method start (parameter : Api_types.parameter) :
    Api_types.token Api_types.result Lwt.t =
    let url : string = Format.sprintf "%s/v1/process" url in
    (post timeout
          (Api_types.string_of_parameter parameter)
          url
          Api_types.token_of_string
          Api_types.errors_of_string
    )

  method status (token : Api_types.token) :
    Api_types.state Api_types.result Lwt.t =
    let url : string = Format.sprintf "%s/v1/process/%d" url token  in
    (XmlHttpRequest.perform_raw
       ~get_args:[]
       ~override_method:`GET
       ~response_type:Text
       url)
    >>=
      (fun frame -> self#hydrate frame
                            Api_types.state_of_string
                            Api_types.errors_of_string)

  method list () : Api_types.catalog Api_types.result Lwt.t =
    let url : string = Format.sprintf "%s/v1/process" url in
    (XmlHttpRequest.perform_raw
       ~get_args:[]
       ~override_method:`GET
       ~response_type:Text
       url)
    >>=
      (fun frame -> self#hydrate
                      frame
                      Api_types.catalog_of_string
                      Api_types.errors_of_string)

  method stop (token : Api_types.token) : unit Api_types.result Lwt.t =
    let () = Common.debug "stopping" in
    let url : string = Format.sprintf "%s/v1/process/%d" url token in
    (XmlHttpRequest.perform_raw
       ~get_args:[]
       ~override_method:`DELETE
       ~response_type:Text
       url)
    >>=
      (fun frame -> self#hydrate
                      frame
                      Api_types.alias_unit_of_string
                      Api_types.errors_of_string)

  method shutdown () : unit Api_types.result Lwt.t =
    match shutdown_key with
    | None -> Lwt.return (`Right ())
    | Some shutdown_key ->
      let () = Common.debug "shutting down" in
      post timeout
        shutdown_key
        (url ^ "/v1/shutdown")
        (fun _ -> ())
        Api_types.errors_of_string

end;;
