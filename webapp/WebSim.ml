module Runtime = Api.Base
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

let log (msg : string) : unit Lwt.t = Lwt_log_core.log Lwt_log_core.Info msg
let unit_of_lwt lwt = Lwt.async (fun () -> lwt)

let fatal (msg : string) : unit Lwt.t = Lwt_log_core.log Lwt_log_core.Fatal msg

class runtime ()  = object
  method yield () = Lwt_main.yield ()
  method log (msg : string) = log msg
  inherit Api.Base.runtime
end

let runtime_state = new runtime ()
let headers =
  let h = Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Header.add h "content-type" "application/json" in
  h

let server_respond (body : string) =
  Server.respond_string ?headers:(Some headers) ~status:`OK ~body:body ()

let result_response string_of_success result =
  match result with
    `Left error ->
    let error_msg : string = ApiTypes.string_of_error error in
    (log error_msg)
    >>=
      (fun _ -> Server.respond_string ?headers:(Some headers) ~status:`Bad_request ~body:error_msg ())
  | `Right success ->
     let success_msg : string = string_of_success success in
     (Server.respond_string ?headers:(Some headers) ~status:`OK ~body:success_msg ())

let process_url_pattern = Re.compile (Re_perl.re "^/v1/process/([0-9]+$)")
let get_token path : int option =
      try
        let matches = Re.exec process_url_pattern path in
        Some (int_of_string (Re.get matches 1))
      with Failure _ | Not_found -> None

let logger (handler : Cohttp_lwt_unix.Server.conn ->
                      Cohttp.Request.t ->
                      Cohttp_lwt_body.t ->  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
           (conn : Cohttp_lwt_unix.Server.conn)
           (request : Cohttp.Request.t)
           (body : Cohttp_lwt_body.t)
    : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  (try handler conn request body
   with e -> let () = Lwt.async (fun () -> (fatal (Printexc.to_string e))) in
             let () = Lwt.async (fun () -> (fatal (Printexc.get_backtrace ()))) in
             raise e
  )
  >>=
    (fun (response,body) ->
     let ip : string = match fst conn with
         Conduit_lwt_unix.TCP {Conduit_lwt_unix.fd; ip; port} ->
         (match Lwt_unix.getpeername fd with
          | Lwt_unix.ADDR_INET (ia,port) ->
             Printf.sprintf "%s:%d" (Ipaddr.to_string (Ipaddr_unix.of_inet_addr ia)) port
          | Lwt_unix.ADDR_UNIX path -> Printf.sprintf "sock:%s" path)
        | _ -> "unknown"
     in
     let t = Unix.localtime (Unix.time ()) in
     let timestamp : string = Printf.sprintf "[%02d/%02d/%04d:%02d:%02d:%02d]"
                                             t.tm_mday
                                             t.tm_mon
                                             t.tm_year
                                             t.tm_hour
                                             t.tm_min
                                             t.tm_sec
     in
     let request_method : string = Code.string_of_method request.meth in
     let uri : Uri.t = Request.uri request in
     let request_path : string = Uri.path uri in
     let response_code : string = Code.string_of_status response.Cohttp.Response.status in
     (* size_of_response *)
     let log_entry : string = Printf.sprintf "%s\t%s\t\"%s %s\"\t%s"
                                             ip
                                             timestamp
                                             request_method
                                             request_path
                                             response_code
     in
     (log log_entry)
       >>=
         (fun _ -> Lwt.return (response,body))
    )

let handler
      ?(shutdown_key : string option  = None)
      (conn : Cohttp_lwt_unix.Server.conn)
      (request : Cohttp.Request.t)
      (body : Cohttp_lwt_body.t)
    : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  let uri = Request.uri request in
  let bad_request = Server.respond_string ?headers:(Some headers)
                                          ~status:`Bad_request
                                          ~body:""
                                          () in
  let not_found = Server.respond_string ?headers:(Some headers)
                                        ~status:`Not_found
                                        ~body:""
                                        () in
  match Uri.path uri with
  (* SHUTDOWN *)
  | "/v1/shutdown" when request.meth = `POST  ->
     (Cohttp_lwt_body.to_string body)
     >>= (fun body ->
          let shutdown_okay = server_respond "shutting down" in
          let shutdown_fail = Server.respond_string ?headers:(Some headers)
                                                    ~status:`Unauthorized
                                                    ~body:"unathorized"
                                                    () in
          match shutdown_key with
            None -> shutdown_fail
          | Some shutdown_key ->
             if shutdown_key = body then
               let () = async (fun () -> Lwt_unix.sleep 1.0 >>= fun () -> exit 0) in
               shutdown_okay
             else
               shutdown_fail)
  (* GET /version *)
  | "/v1/version" ->
     server_respond (ApiTypes.string_of_version { build = Version.version_msg ;
                                                  version = "v1" })
  (* GET /parse *)
  | "/v1/parse" ->
     Uri.get_query_param uri "code" |>
       (fun code -> match code with
                      None ->
                      Server.respond_string
                        `Unprocessable_entity
                        "missing code"
                        ()
                    | Some code ->
                       (log code)
                       >>=
                           (fun () -> runtime_state#parse code)
                       >>=
                           (fun parse -> result_response ApiTypes.string_of_parse parse)
       )(* GET /process *)
  | "/v1/process" when request.meth = `GET ->
     (runtime_state#list ())
     >>=
       (fun catalog -> result_response ApiTypes.string_of_catalog catalog)
  (* POST /process *)
  | "/v1/process" when request.meth = `POST ->
     (Cohttp_lwt_body.to_string body)
     >>= (fun body ->
          (try let parameter : ApiTypes.parameter = ApiTypes.parameter_of_string body in
               runtime_state#start parameter
           with  Yojson.Json_error error ->
                 Lwt.return (`Left [error])
               | Ag_oj_run.Error error ->
                  Lwt.return (`Left [error])
               | e ->
                  Lwt.return (`Left [Printexc.to_string e])
          )
          >>=
            (fun token -> result_response ApiTypes.string_of_token token)

         )
  (* OPTIONS /v1/process/[token] *)
  | x when request.meth = `OPTIONS && None != get_token x ->
       let h = Header.init_with "Access-Control-Allow-Origin" "*" in
       let h = Header.add h "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE" in
       let h = Header.add h "Access-Control-Request-Headers" "X-Custom-Header" in
       Server.respond_string ?headers:(Some h) ~status:`OK ~body:"" ()
  (* DELETE /v1/process/[token] *)
  | x when request.meth = `DELETE && None != get_token x ->
     (match get_token x with
        None -> bad_request
      | Some token ->
         (runtime_state#stop token)
         >>=
           (fun unit -> result_response ApiTypes.string_of_alias_unit unit)
     )
  (* GET /v1/process/[token] *)
  | x when request.meth = `GET ->
     (match get_token x with
        None -> bad_request
      | Some token ->
         (runtime_state#status token)
         >>=
           (fun status -> result_response ApiTypes.string_of_state status)
     )
  | _ -> not_found

let server =
  let parameter_backtrace : bool ref = ref false in
  let parameter_seed_value : int option ref = ref None in
  let parameter_port : int ref = ref 8080 in
  let parameter_cert_dir : string option ref = ref None in
  let parameter_shutdown_key : string option ref = ref None in
  let options  : (string * Arg.spec * string) list = [ ("--version",
                                                        Arg.Unit (fun () -> Format.print_string Version.version_msg;
                                                                            Format.print_newline () ; exit 0),
                                                        "display KaSim version");
                                                       ("--backtrace", Arg.Set parameter_backtrace,
                                                        "Backtracing exceptions") ;
                                                       ("-seed", Arg.Int (fun i -> parameter_seed_value := Some i),
                                                        "Seed for the random number generator") ;
                                                       ("--gluttony",
                                                        Arg.Unit (fun () -> Gc.set { (Gc.get()) with
                                                                                     Gc.space_overhead = 500 (*default 80*) } ;),
                                                        "Lower gc activity for a faster but memory intensive simulation") ;
                                                       ("--port",
                                                        Arg.Int (fun port -> parameter_port := port),
                                                        "port to serve on");
                                                       ("--shutdown-key",
                                                        Arg.String (fun key -> parameter_shutdown_key := Some key),
                                                        "key to shutdown server");
                                                       ("--cert-dir",
                                                        Arg.String (fun key -> parameter_cert_dir := Some key),
                                                        "Directory where to find cert.pem and privkey.pem");
                                                       ("--log",
                                                        Arg.String (fun file_name -> if file_name = "-" then
                                                                                       let _ = Lwt_log.channel ~close_mode:(`Keep) ~channel:(Lwt_io.stdout) () in
                                                                                       ()
                                                                                     else
                                                                                       let _ = Lwt_log.file ?mode:(Some `Append) ~file_name:file_name () in
                                                                                       ()
                                                                   ),
                                                        "path to log file path '-' logs to stdout");
                                                       ("--level",
                                                        Arg.String (fun level ->
                                                                     Lwt_log_core.append_rule "*"
                                                                                              (match level with
                                                                                               | "debug" -> Lwt_log_core.Debug
                                                                                               | "info" -> Lwt_log_core.Info
                                                                                               | "notice" -> Lwt_log_core.Notice
                                                                                               | "warning" -> Lwt_log_core.Warning
                                                                                               | "error" -> Lwt_log_core.Error
                                                                                               | "fatal" -> Lwt_log_core.Fatal
                                                                                               | level -> raise (Arg.Bad ("\""^level^"\" is not a valid level"))
                                                                                              )),
                                                        "levels : debug,info,notice,warning,error,fatal"
                                                       )
                                                     ] in
  let usage : string = "WebSim --port port --shutdown-key key\n" in
  let () = Arg.parse options (fun _ -> ()) usage in
  let () = Printexc.record_backtrace !parameter_backtrace in
  let theSeed =
    match !parameter_seed_value with
    | Some seed -> seed
    | None ->
       begin
         unit_of_lwt (log "+ Self seeding...@.");
         Random.self_init() ;
         Random.bits ()
       end
  in
  let () = Random.init theSeed ;
           unit_of_lwt (log (Printf.sprintf "+ Initialized random number generator with seed %d@." theSeed)) in
  let mode = match !parameter_cert_dir with
    | None -> `TCP (`Port !parameter_port)
    | Some dir ->
       `TLS (`Crt_file_path (dir^"cert.pem"), `Key_file_path (dir^"privkey.pem"),
             `No_password, `Port !parameter_port) in
  Server.create ~mode (Server.make (logger (handler ~shutdown_key:!parameter_shutdown_key)) ())

let () = ignore (Lwt_main.run server)
