open Lwt
open XmlHttpRequest
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
  let reply,feeder = Lwt.task () in
  let request : XmlHttpRequest.xmlHttpRequest Js.t =
    XmlHttpRequest.create()
  in
  let () =
    request##_open(Js.string "POST") (Js.string url) Js._true in
  let () =
    request##setRequestHeader
      (Js.string "Content-type")
      (Js.string "application/x-www-form-urlencoded") in
  let () =
    request##.onload :=
      Dom.handler
        (fun e ->
           let msg : string = Js.to_string request##.responseText in
           let () =
             let result : a Api_types.result =
               if request##.status == 200 then
                 `Right(success msg)
               else
                 `Left (fail msg) in
             Lwt.wakeup feeder result
           in Js._true)
  in
  let () = request##send(Js.some (Js.string data)) in
  Lwt.pick [reply; (Lwt_js.sleep timeout >>= fun () -> Lwt.fail TimeOut)]

class runtime
    ?(timeout:float = 10.0)
    (url:string) =
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
      (fun frame ->
         self#hydrate frame
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

    method perturbate
        (token : Api_types.token)
        (perturbation : Api_types.perturbation) :
      unit Api_types.result Lwt.t =
      let url : string = Format.sprintf "%s/v1/process/%d/perturbate" url token in
      (post timeout
         (Api_types.string_of_perturbation perturbation)
         url
         Api_types.alias_unit_of_string
         Api_types.errors_of_string
      )

    method pause
        (token : Api_types.token) :
      unit Api_types.result Lwt.t =
      let url : string = Format.sprintf "%s/v1/process/%d/pause" url token in
      (post timeout
         (Api_types.string_of_alias_unit ())
         url
         Api_types.alias_unit_of_string
         Api_types.errors_of_string
      )

    method continue
      (token : Api_types.token)
      (parameter : Api_types.parameter) :
      unit Api_types.result Lwt.t =
      let url : string = Format.sprintf "%s/v1/process/%d/continue" url token in
      (post timeout
         (Api_types.string_of_parameter parameter)
         url
         Api_types.alias_unit_of_string
         Api_types.errors_of_string
      )

  end;;
