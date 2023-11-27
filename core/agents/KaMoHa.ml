(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let process_command (message_delimiter : char) : string -> unit Lwt.t =
  Kamoha_mpi.on_message Lwt.pause (fun message ->
      Lwt_io.atomic
        (fun f ->
          Lwt_io.write f message >>= fun () ->
          Lwt_io.write_char f message_delimiter)
        Lwt_io.stdout)

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
       (process_command stdsim_args.Agent_args.delimiter))
