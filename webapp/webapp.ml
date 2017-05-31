(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class system_process () : Kappa_facade.system_process =
  let () =
    Lwt.async
      (fun () ->
         Lwt_log_core.log
           ~level:Lwt_log_core.Debug
           (Format.sprintf " + system process"))
  in
  object
    method log ?exn (msg : string) =
      Lwt_log_core.log ~level:Lwt_log_core.Info ?exn msg
    method yield () : unit Lwt.t = Lwt_main.yield ()
    method min_run_duration () = 0.1
  end

let route_handler
    ?(shutdown_key : string option  = None)
    ()
  :
    Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt_body.t ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
  =
  let sytem_process : Kappa_facade.system_process = new system_process () in
  let manager = new Api_runtime.manager sytem_process in
  let intermediate =
    (Webapp_common.route_handler
       ((Route_root.route
           ~manager:manager
           ~shutdown_key:shutdown_key)
        @ Route_sessions.route
          ~manager:manager)) in
  fun (conn : Cohttp_lwt_unix.Server.conn)
    (request : Cohttp.Request.t)
    (body : Cohttp_lwt_body.t)
    ->
      let context = { Webapp_common.arguments = []
                    ; Webapp_common.connection = conn
                    ; Webapp_common.request = request
                    ; Webapp_common.body = body }
      in
      intermediate ~context
