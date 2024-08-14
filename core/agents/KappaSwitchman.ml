(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Main file for the kappaswitchman process, managing contact with the agents in jsnode/cli mode, used when built in electron.
    The switchman process parses the command from the switchman_client, then calls the kappaswitchman_agents_client , which then communicates with the different processes that will do the operation and return the data, which is then translated in this file to be forwarded in answer to the switchman_client
    *)

(* start server *)
let () =
  let common_args = Common_args.default in
  let stdsim_args = Agent_args.default in
  let options =
    Common_args.options common_args @ Agent_args.options stdsim_args
  in
  let usage_msg = "Kappa Model Handler" in
  let () =
    Arg.parse options
      (fun x -> raise (Arg.Bad ("Don't know what to do of " ^ x)))
      usage_msg
  in
  let () = Printexc.record_backtrace common_args.Common_args.backtrace in
  Lwt_main.run
    (Agent_common.serve Lwt_io.stdin stdsim_args.Agent_args.delimiter
       (Kappaswitchman_mpi.on_message Sys.executable_name
          stdsim_args.Agent_args.delimiter))
