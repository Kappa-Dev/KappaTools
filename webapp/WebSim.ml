(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let logger
    (handler :
      Cohttp_lwt_unix.Server.conn ->
      Cohttp.Request.t ->
      Cohttp_lwt.Body.t ->
      (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
    (conn : Cohttp_lwt_unix.Server.conn) (request : Cohttp.Request.t)
    (body : Cohttp_lwt.Body.t) : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
  Lwt.catch
    (fun () -> handler conn request body)
    (fun exn ->
      Logs_lwt.err (fun m -> m "%s" (Printexc.to_string exn)) >>= fun () ->
      Lwt.fail exn)
  >>= fun (response, body) ->
  let ip : string =
    match fst conn with
    | Conduit_lwt_unix.TCP { Conduit_lwt_unix.fd; _ } ->
      (match Lwt_unix.getpeername fd with
      | Lwt_unix.ADDR_INET (ia, port) ->
        Printf.sprintf "%s:%d"
          (Ipaddr.to_string (Ipaddr_unix.of_inet_addr ia))
          port
      | Lwt_unix.ADDR_UNIX path -> Printf.sprintf "sock:%s" path)
    | Conduit_lwt_unix.Vchan _ | Conduit_lwt_unix.Domain_socket _ -> "unknown"
  in
  let t = Unix.localtime (Unix.time ()) in
  let request_method : string =
    Cohttp.Code.string_of_method request.Cohttp_lwt_unix.Request.meth
  in
  let uri : Uri.t = Cohttp_lwt_unix.Request.uri request in
  let request_path : string = Uri.path uri in
  let response_code : string =
    Cohttp.Code.string_of_status response.Cohttp.Response.status
  in
  Logs_lwt.info (fun m ->
      m "%s\t[%02d/%02d/%04d %02d:%02d:%02d]\t\"%s %s\"\t%s" ip t.Unix.tm_mday
        t.Unix.tm_mon (1900 + t.Unix.tm_year) t.Unix.tm_hour t.Unix.tm_min
        t.Unix.tm_sec request_method request_path response_code)
  >>= fun _ -> Lwt.return (response, body)

let server =
  let app_args = App_args.default in
  let common_args = Common_args.default in
  let websim_args = Websim_args.default in
  let options =
    App_args.options app_args
    @ Websim_args.options websim_args
    @ Common_args.options common_args
  in
  let usage_msg : string = "kappa webservice" in
  let () = Arg.parse options (fun _ -> ()) usage_msg in
  let () =
    Logs.set_reporter (Agent_common.lwt_reporter app_args.App_args.log_channel)
  in
  let () = Printexc.record_backtrace common_args.Common_args.backtrace in
  let mode =
    match websim_args.Websim_args.cert_dir with
    | None -> `TCP (`Port websim_args.Websim_args.port)
    | Some dir ->
      `TLS
        ( `Crt_file_path (dir ^ "cert.pem"),
          `Key_file_path (dir ^ "privkey.pem"),
          `No_password,
          `Port websim_args.Websim_args.port )
  in
  let route_handler :
      Cohttp_lwt_unix.Server.conn ->
      Cohttp.Request.t ->
      Cohttp_lwt.Body.t ->
      (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
    Webapp.route_handler ~shutdown_key:websim_args.Websim_args.shutdown_key ()
  in
  Cohttp_lwt_unix.Server.create ~mode
    (Cohttp_lwt_unix.Server.make
       ~callback:
         (logger
            (match app_args.App_args.api with
            | App_args.V2 -> route_handler))
       ())
  >>= fun () ->
  match app_args.App_args.log_channel with
  | None -> Lwt.return_unit
  | Some ch -> Lwt_io.close ch

let () =
  let () = Lwt.async_exception_hook := ignore in
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  ignore (Lwt_main.run server)
