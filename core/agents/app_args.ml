(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type api_version = V2

type t = {
  mutable api: api_version;
  mutable log_channel: Lwt_io.output_channel option;
}

let default : t = { api = V2; log_channel = None }

let options (t : t) : (string * Arg.spec * string) list =
  [
    ( "--development",
      Arg.Unit (fun () -> t.api <- V2),
      "enable experimental api - not intended for public use or comment" );
    ( "--log",
      Arg.String
        (fun file_name ->
          let () =
            Lwt.ignore_result
              (match t.log_channel with
              | None -> Lwt.return_unit
              | Some c -> Lwt_io.close c)
          in
          if file_name = "-" then
            t.log_channel <- None
          else (
            let fd =
              Unix.openfile file_name
                [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK ]
                0o640
            in
            let () = Unix.set_close_on_exec fd in
            t.log_channel <- Some (Lwt_io.of_unix_fd ~mode:Lwt_io.output fd)
          )),
      "path to log file path '-' logs to stdout" );
    ( "--level",
      Arg.String
        (fun level ->
          Logs.set_level
            (Some
               (match level with
               | "debug" -> Logs.Debug
               | "info" -> Logs.Info
               | "warning" -> Logs.Warning
               | "error" -> Logs.Error
               | "app" -> Logs.App
               | level ->
                 raise (Arg.Bad ("\"" ^ level ^ "\" is not a valid level"))))),
      "levels : debug,info,warning,error,app" );
  ]
