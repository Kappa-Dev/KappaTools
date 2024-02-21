(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(* system process for v2 *)
class system_process () : Kappa_facade.system_process =
  object
    method log ?exn (msg : string) =
      Logs_lwt.info (fun m ->
          m "%s%a" msg
            (Pp.option (fun f e -> Fmt.pf f ": %s" (Printexc.to_string e)))
            exn)

    method yield () : unit Lwt.t = Lwt.pause ()
    method min_run_duration () = 0.1
  end

(* set up handlers for v2 *)
let process_comand_v2 (message_delimiter : char) : string -> unit Lwt.t =
  let sytem_process : Kappa_facade.system_process = new system_process () in
  let manager : Api.manager_simulation =
    new Api_runtime.manager sytem_process
  in
  Mpi_api.on_message manager (fun message ->
      Lwt_io.atomic
        (fun f ->
          Lwt_io.write f message >>= fun () ->
          Lwt_io.write_char f message_delimiter)
        Lwt_io.stdout)

(* start server *)
let () =
  let app_args = App_args.default in
  let common_args = Common_args.default in
  let stdsim_args = Agent_args.default in
  let options =
    App_args.options app_args
    @ Common_args.options common_args
    @ Agent_args.options stdsim_args
  in
  let usage_msg = "kappa stdio simulator" in
  let () = Arg.parse options (fun _ -> ()) usage_msg in
  (* set protocol version *)
  let () =
    Logs.set_reporter (Agent_common.lwt_reporter app_args.App_args.log_channel)
  in
  let process_comand : string -> unit Lwt.t =
    (match app_args.App_args.api with
    | App_args.V2 -> process_comand_v2)
      stdsim_args.Agent_args.delimiter
  in
  Lwt_main.run
    ( Agent_common.serve Lwt_io.stdin stdsim_args.Agent_args.delimiter
        process_comand
    >>= fun () ->
      match app_args.App_args.log_channel with
      | None -> Lwt.return_unit
      | Some ch -> Lwt_io.close ch )
