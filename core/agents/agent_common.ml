(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let lwt_reporter chan =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | Logs.Debug | Logs.Info | Logs.Warning | Logs.Error ->
          Lwt_io.write (Option_util.unsome Lwt_io.stderr chan) (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let serve chan delimiter process_command : unit Lwt.t =
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char_opt chan >>= function
    | Some char ->
      if char = delimiter then (
        let m = Buffer.contents buffer in
        process_command m >>= fun () ->
        let () = Buffer.reset buffer in
        aux_serve ()
      ) else (
        let () = Buffer.add_char buffer char in
        aux_serve ()
      )
    | None -> Lwt.return_unit
  in
  aux_serve ()
