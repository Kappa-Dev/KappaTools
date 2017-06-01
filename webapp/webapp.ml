(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

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
  let re = Re.compile (Re.str "WebSim") in
  let sa_command = Re.replace_string re ~by:"KaSaAgent" Sys.argv.(0) in
  let sa_process = Lwt_process.open_process (sa_command,[|sa_command|]) in
  let manager = object
    initializer
      let () =
        Lwt.ignore_result
          (Agent_common.serve
             sa_process#stdout Tools.default_message_delimter
             (fun r -> let () = Kasa_client.receive r in Lwt.return_unit)) in
      ()
    inherit Api_runtime.manager sytem_process
    inherit Kasa_client.new_client
        ~post:(fun message_text ->
            Lwt.ignore_result
              (Lwt_io.atomic
                 (fun f ->
                    Lwt_io.write f message_text >>= fun () ->
                    Lwt_io.write_char f Tools.default_message_delimter)
                 sa_process#stdin))
    method terminate () =
      sa_process#terminate
  end in
  let intermediate =
    (Webapp_common.route_handler
       ((Route_root.route ~manager ~shutdown_key)
        (*@ Route_sessions.route
          ~manager:manager*))) in
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
