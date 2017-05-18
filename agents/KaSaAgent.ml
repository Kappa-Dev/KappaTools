(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
(* system process for v2 *)
let process_command (message_delimter : char) : string -> unit Lwt.t =
  Kasa_mpi.on_message
    (fun message ->
       Lwt_io.write Lwt_io.stdout (message^(String.make 1 message_delimter)))

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let serve process_command delimiter : unit Lwt.t =
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char Lwt_io.stdin >>= fun char ->
    if char = delimiter then
      let m = Buffer.contents buffer in
      process_command m >>= fun () ->
      let () = Buffer.reset buffer in aux_serve ()
    else
      let () = Buffer.add_char buffer char in
      aux_serve () in
  aux_serve ()

(* start server *)
let () =
  let common_args = Common_args.default in
  let stdsim_args = Agent_args.default in
  let options =
    Common_args.options common_args @ Agent_args.options stdsim_args in
  let usage_msg = "Kappa Static Analyser agent" in
  let () = Arg.parse options (fun _ -> ()) usage_msg in
  let () = Printexc.record_backtrace common_args.Common_args.backtrace in
  Lwt_main.run
    (serve (process_command stdsim_args.Agent_args.delimiter)
       stdsim_args.Agent_args.delimiter)
