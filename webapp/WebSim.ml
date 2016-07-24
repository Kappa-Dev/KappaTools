module ApiTypes = ApiTypes_j

open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open ApiTypes
open Conduit_lwt_unix
open Unix
open Lwt_log

let unit_of_lwt lwt = Lwt.async (fun () -> lwt)

let logger (handler : Cohttp_lwt_unix.Server.conn ->
            Cohttp.Request.t ->
            Cohttp_lwt_body.t ->
            (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
    (conn : Cohttp_lwt_unix.Server.conn)
    (request : Cohttp.Request.t)
    (body : Cohttp_lwt_body.t)
  : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  (Lwt.catch
     (fun () -> handler conn request body)
     (fun exn -> fatal ~exn "" >>=
       (fun () -> Lwt.fail exn))
  )
  >>=
  (fun (response,body) ->
     let ip : string =
       match fst conn with
         Conduit_lwt_unix.TCP {Conduit_lwt_unix.fd; ip; port} ->
         (match Lwt_unix.getpeername fd with
          | Lwt_unix.ADDR_INET (ia,port) ->
            Printf.sprintf
              "%s:%d"
              (Ipaddr.to_string (Ipaddr_unix.of_inet_addr ia))
              port
          | Lwt_unix.ADDR_UNIX path -> Printf.sprintf "sock:%s" path)
       | _ -> "unknown"
     in
     let t = Unix.localtime (Unix.time ()) in
     let timestamp : string =
       Printf.sprintf "[%02d/%02d/%04d:%02d:%02d:%02d]"
         t.tm_mday
         t.tm_mon
         t.tm_year
         t.tm_hour
         t.tm_min
         t.tm_sec
     in
     let request_method : string =
       Code.string_of_method request.meth
     in
     let uri : Uri.t = Request.uri request in
     let request_path : string = Uri.path uri in
     let response_code : string =
       Code.string_of_status
         response.Cohttp.Response.status
     in
     (* size_of_response *)
     let log_entry : string =
       Printf.sprintf "%s\t%s\t\"%s %s\"\t%s"
         ip
         timestamp
         request_method
         request_path
         response_code
     in
     (Lwt_log_core.log
	~level:Lwt_log_core.Info
	log_entry)
     >>=
     (fun _ -> Lwt.return (response,body))
  )

type api_version = V1 | V2;;
let server =
  let parameter_backtrace : bool ref = ref false in
  let parameter_seed_value : int option ref = ref None in
  let parameter_port : int ref = ref 8080 in
  let parameter_cert_dir : string option ref = ref None in
  let parameter_shutdown_key : string option ref = ref None in
  let parameter_api : api_version ref = ref V1 in
  let parameter_stdio : bool ref = ref false in
  let options  : (string * Arg.spec * string) list =
    [ ("--stdio",
       Arg.Unit
         (fun () -> parameter_stdio := true),
       "communicate over stdio instead of http");
      ("--development",
       Arg.Unit
	 (fun () ->
	    let () =
	      Lwt.async
		(fun () ->
		   Lwt_log_core.log
		     ~level:Lwt_log_core.Warning
		     "development features - not for public use or comment")
	    in
	    parameter_api := V2),
       "enable experimental api - not intended for public use or comment");
      ("--version",
       Arg.Unit
         (fun () ->
            Format.print_string Version.version_msg ;
	    Format.print_newline () ;
            exit 0),
       "display KaSim version");
      ("--backtrace",
       Arg.Set parameter_backtrace,
       "Backtracing exceptions") ;
      ("-seed",
       Arg.Int
         (fun i -> parameter_seed_value := Some i),
       "Seed for the random number generator") ;
      ("--gluttony",
       Arg.Unit
         (fun () -> Gc.set { (Gc.get()) with
			     Gc.space_overhead = 500 (*default 80*) } ;),
       "Lower gc activity for a faster but memory intensive simulation") ;
      ("--port",
       Arg.Int
         (fun port -> parameter_port := port),
       "port to serve on");
      ("--shutdown-key",
       Arg.String
         (fun key -> parameter_shutdown_key := Some key),
       "key to shutdown server");
      ("--cert-dir",
       Arg.String
         (fun key -> parameter_cert_dir := Some key),
       "Directory where to find cert.pem and privkey.pem");
      ("--log",
       Arg.String
	 (fun file_name ->
	    if file_name = "-" then
              let _ =
		Lwt_log.channel
		  ~close_mode:(`Keep)
		  ~channel:(Lwt_io.stderr) ()
              in
              ()
            else
              let _ =
		Lwt_log.file
		  ?mode:(Some `Append)
		  ~file_name:file_name ()
              in
              ()
	 ),
       "path to log file path '-' logs to stdout");
      ("--level",
       Arg.String
         (fun level ->
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
        unit_of_lwt
	  (Lwt_log_core.log
	     ~level:Lwt_log_core.Info
	     "+ Self seeding...@.");
        Random.self_init() ;
        Random.bits ()
      end
  in
  let () =
    Random.init theSeed ;
    unit_of_lwt
      (Lwt_log_core.log
	 ~level:Lwt_log_core.Info
         (Printf.sprintf
	    "+ Initialized random number generator with seed %d@."
            theSeed)
      )
  in
  let mode = match !parameter_cert_dir with
    | None -> `TCP (`Port !parameter_port)
    | Some dir ->
      `TLS (`Crt_file_path (dir^"cert.pem"),
	    `Key_file_path (dir^"privkey.pem"),
            `No_password,
	    `Port !parameter_port) in
  let route_handler :
    Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt_body.t ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
    = Webapp.route_handler
      ~shutdown_key:!parameter_shutdown_key
      ()
  in
  if !parameter_stdio then
    Server_stdio.serve ()
  else
    Server.create
      ~mode
      (Server.make
	 (logger
            (match !parameter_api with
	     | V1 -> Webapp_v1.handler
		       ~shutdown_key:!parameter_shutdown_key
	     | V2 -> route_handler
	    )
	 )
	 ()
      )
let () = ignore (Lwt_main.run server)
