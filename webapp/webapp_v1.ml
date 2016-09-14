module Runtime = Api_v1.Base

open Lwt.Infix
open Cohttp_lwt_unix
open Cohttp.Request

(*  Lwt_log_core.log *)

let unit_of_lwt lwt = Lwt.async (fun () -> lwt)

let fatal ~exn (msg : string) : unit Lwt.t =
  Lwt_log_core.log ~exn ~level:Lwt_log_core.Fatal msg

class runtime ()  = object
  method yield () = Lwt_unix.sleep 0.001
  method log ?exn (msg : string) =
    Lwt_log_core.log
      ~level:Lwt_log_core.Info
      ?exn
      msg
  inherit Api_v1.Base.base_runtime 0.1
end

let runtime_state = new runtime ()
let headers =
  let h = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Cohttp.Header.add h "content-type" "application/json" in
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
    let error_msg : string = ApiTypes_j.string_of_errors errors in
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

type command = PERTURBATE | PAUSE | CONTINUE
type url_parameters = { id : int ;  command : command option ; }
let process_url_string = "^/v1/process/([0-9]+)(/perturbate|/pause|/continue)?$"
let process_url_pattern = Re.compile (Re_perl.re process_url_string)
let parse_url_parameters path : url_parameters option =
  try
    let matches = Re.exec process_url_pattern path in
    Some { id = int_of_string (Re.get matches 1) ;
           command =
             try match Re.Group.get matches 2 with
               | "/perturbate" -> Some PERTURBATE
               | "/pause" -> Some PAUSE
               | "/continue" -> Some CONTINUE
               | _ -> None
             with
             | _ -> None }

  with
  | Failure _
  | Not_found -> None
  | _ -> None

let handler
    ?(shutdown_key : string option  = None)
    (_ : Cohttp_lwt_unix.Server.conn)
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
              Lwt.async
                (fun () ->
                   Lwt_unix.sleep 1.0 >>=
                   fun () -> exit 0)
            in
            shutdown_okay
          else
            shutdown_fail)
  (* GET /version *)
  | "/v1/version" ->
    server_respond
      (ApiTypes_j.string_of_version
         { ApiTypes_j.version_build = Version.version_msg ;
           ApiTypes_j.version_id = "v1" })
  (* GET /parse *)
  | "/v1/parse" ->
    Uri.get_query_param uri "code" |>
    (fun code ->
       match code with
         None ->
         Server.respond_string
           ~status:`Unprocessable_entity
           ~body:"missing code"
           ()
       | Some code ->
         (Lwt_log_core.log
	    ~level:Lwt_log_core.Info
	    code)
         >>=
         (fun () -> runtime_state#parse code)
         >>=
         (fun parse ->
            result_response (ApiTypes_j.string_of_parse ?len:None) parse)
    )(* GET /process *)
  | "/v1/process" when request.meth = `GET ->
    (runtime_state#list ())
    >>=
    (fun catalog ->
       result_response (ApiTypes_j.string_of_catalog ?len:None) catalog)
  (* POST /process *)
  | "/v1/process" when request.meth = `POST ->
    (Cohttp_lwt_body.to_string body)
    >>= (fun body ->
        (try
           let parameter : ApiTypes_j.parameter =
             ApiTypes_j.parameter_of_string body in
           runtime_state#start parameter
         with
         | Yojson.Json_error error -> Api_data.lwt_msg error
         | Ag_oj_run.Error error -> Api_data.lwt_msg error
         | e -> Api_data.lwt_msg (Printexc.to_string e)
        )
        >>=
        (fun token ->
           result_response (ApiTypes_j.string_of_token ?len:None) token)
      )
  (* OPTIONS /v1/process/[token] *)
  | x when request.meth = `OPTIONS
        && None != parse_url_parameters x ->
    let h =
      Cohttp.Header.init_with
        "Access-Control-Allow-Origin"
        "*"
    in
    let h =
      Cohttp.Header.add
        h
        "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE"
    in
    let h =
      Cohttp.Header.add
        h
        "Access-Control-Request-Headers" "X-Custom-Header"
    in
    Server.respond_string
      ?headers:(Some h)
      ~status:`OK
      ~body:""
      ()
  (* DELETE /v1/process/[token] *)
  | x when request.meth = `DELETE
        && None != parse_url_parameters x ->
    (match parse_url_parameters x with
     | None -> bad_request
     | Some url_parameters ->
       (runtime_state#stop url_parameters.id)
       >>=
       (fun unit ->
          result_response (ApiTypes_j.string_of_alias_unit ?len:None) unit)
    )
  (* GET /v1/process/[token] *)
  | x when request.meth = `GET && None != parse_url_parameters x ->
    (match parse_url_parameters x with
     | None -> bad_request
     | Some url_parameters ->
       (runtime_state#status url_parameters.id)
       >>=
       (fun status ->
          result_response (ApiTypes_j.string_of_state ?len:None) status)
    )
  | x when request.meth = `POST
        && None != parse_url_parameters x ->
    (match parse_url_parameters x with
     | Some { id = id ; command = None } ->
       (runtime_state#status id)
       >>=
       (fun status ->
          result_response (ApiTypes_j.string_of_state ?len:None) status)
     | Some { id = id ; command = Some PAUSE } ->
       (runtime_state#pause id)
       >>=
       (fun status ->
          result_response (ApiTypes_j.string_of_alias_unit ?len:None) status)
     | Some { id = id ; command = Some PERTURBATE } ->
       (Cohttp_lwt_body.to_string body)
       >>=
       (fun body ->
          (try let perturbation : ApiTypes_j.perturbation =
                 ApiTypes_j.perturbation_of_string body in
             runtime_state#perturbate id perturbation
           with
           | Yojson.Json_error error -> Api_data.lwt_msg error
           | Ag_oj_run.Error error -> Api_data.lwt_msg error
           | e -> Api_data.lwt_msg (Printexc.to_string e)
          ))
       >>=
       (fun status ->
          result_response (ApiTypes_j.string_of_alias_unit ?len:None) status)
     | Some { id = id ; command = Some CONTINUE } ->
       (Cohttp_lwt_body.to_string body)
       >>=
       (fun body ->
          (try let parameter : ApiTypes_j.parameter =
                 ApiTypes_j.parameter_of_string body in
             runtime_state#continue id parameter
           with
           | Yojson.Json_error error -> Api_data.lwt_msg error
           | Ag_oj_run.Error error -> Api_data.lwt_msg error
           | e -> Api_data.lwt_msg (Printexc.to_string e)
          )
       )
       >>=
       (fun status ->
          result_response (ApiTypes_j.string_of_alias_unit ?len:None) status)
     | None -> bad_request
    )
  | _ -> bad_request;;
