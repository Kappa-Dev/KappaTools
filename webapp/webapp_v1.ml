module Runtime = Api_v1.Base
module ApiTypes = ApiTypes_j

open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open Runtime
open ApiTypes
open Conduit_lwt_unix
open Unix
open Lwt_log
open Re

(*  Lwt_log_core.log *)

let unit_of_lwt lwt = Lwt.async (fun () -> lwt)

let fatal ~exn (msg : string) : unit Lwt.t =
  Lwt_log_core.log ~exn ~level:Lwt_log_core.Fatal msg

class runtime ()  = object
  method yield () = Lwt_main.yield ()
  method log ?exn (msg : string) =
    Lwt_log_core.log
      ~level:Lwt_log_core.Info
      ?exn
      msg
  inherit Api_v1.Base.runtime 1.0
end

let runtime_state = new runtime ()
let headers =
  let h = Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Header.add h "content-type" "application/json" in
  h

let server_respond (body : string) =
  Server.respond_string
    ?headers:(Some headers)
    ~status:`OK
    ~body:body
    ()

let result_response string_of_success result =
  match result with
    `Left errors ->
    let error_msg : string = ApiTypes.string_of_errors errors in
    (Lwt_log_core.log ~level:Lwt_log_core.Error error_msg)
    >>=
    (fun _ ->
       Server.respond_string
         ?headers:(Some headers)
         ~status:`Bad_request
         ~body:error_msg ())
  | `Right success ->
    let success_msg : string = string_of_success success in
    Server.respond_string
      ?headers:(Some headers)
      ~status:`OK
      ~body:success_msg ()

let process_url_pattern = Re.compile (Re_perl.re "^/v1/process/([0-9]+$)")
let get_token path : int option =
  try
    let matches = Re.exec process_url_pattern path in
    Some (int_of_string (Re.get matches 1))
  with Failure _ | Not_found -> None

let handler
    ?(shutdown_key : string option  = None)
    (conn : Cohttp_lwt_unix.Server.conn)
    (request : Cohttp.Request.t)
    (body : Cohttp_lwt_body.t)
  : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  let uri = Request.uri request in
  let bad_request =
    Server.respond_string
      ?headers:(Some headers)
      ~status:`Bad_request
      ~body:""
      ()
  in
  match Uri.path uri with
  (* SHUTDOWN *)
  | "/v1/shutdown" when request.meth = `POST  ->
    (Cohttp_lwt_body.to_string body)
    >>= (fun body ->
        let shutdown_okay =
          server_respond "shutting down"
        in
        let shutdown_fail =
          Server.respond_string
            ?headers:(Some headers)
            ~status:`Unauthorized
            ~body:"unathorized"
            ()
        in
        match shutdown_key with
          None -> shutdown_fail
        | Some shutdown_key ->
          if shutdown_key = body then
            let () =
              async
                (fun () ->
                   Lwt_unix.sleep 1.0 >>=
                   fun () ->
                   exit 0)
            in
            shutdown_okay
          else
            shutdown_fail)
  (* GET /version *)
  | "/v1/version" ->
    server_respond
      (ApiTypes.string_of_version
         { build = Version.version_msg ;
           version = "v1" })
  (* GET /parse *)
  | "/v1/parse" ->
    Uri.get_query_param uri "code" |>
    (fun code ->
       match code with
         None ->
         Server.respond_string
           `Unprocessable_entity
           "missing code"
           ()
       | Some code ->
         (Lwt_log_core.log
	    ~level:Lwt_log_core.Info
	    code)
         >>=
         (fun () -> runtime_state#parse code)
         >>=
         (fun parse ->
            result_response ApiTypes.string_of_parse parse)
    )(* GET /process *)
  | "/v1/process" when request.meth = `GET ->
    (runtime_state#list ())
    >>=
    (fun catalog -> result_response ApiTypes.string_of_catalog catalog)
  (* POST /process *)
  | "/v1/process" when request.meth = `POST ->
    (Cohttp_lwt_body.to_string body)
    >>= (fun body ->
        (try let parameter : ApiTypes.parameter =
               ApiTypes.parameter_of_string body in
           runtime_state#start parameter
         with  Yojson.Json_error error ->
           Api_data.lwt_msg error
             | Ag_oj_run.Error error ->
               Api_data.lwt_msg error
             | e ->
               Api_data.lwt_msg (Printexc.to_string e)
        )
        >>=
        (fun token -> result_response ApiTypes.string_of_token token)

      )
  (* OPTIONS /v1/process/[token] *)
  | x when request.meth = `OPTIONS && None != get_token x ->
    let h =
      Header.init_with
        "Access-Control-Allow-Origin"
        "*"
    in
    let h =
      Header.add
        h
        "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE"
    in
    let h =
      Header.add
        h
        "Access-Control-Request-Headers" "X-Custom-Header"
    in
    Server.respond_string
      ?headers:(Some h)
      ~status:`OK
      ~body:""
      ()
  (* DELETE /v1/process/[token] *)
  | x when request.meth = `DELETE && None != get_token x ->
    (match get_token x with
     | None ->
       bad_request
     | Some token ->
       (runtime_state#stop token)
       >>=
       (fun unit ->
          result_response ApiTypes.string_of_alias_unit unit)
    )
  (* GET /v1/process/[token] *)
  | x when request.meth = `GET && None != get_token x ->
    (match get_token x with
     | None ->
       bad_request
     | Some token ->
       (runtime_state#status token)
       >>=
       (fun status ->
          result_response ApiTypes.string_of_state status)
    )
  | _ -> bad_request;;
