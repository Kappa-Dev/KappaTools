(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type api_version = V2;;

type t = { mutable seed_value : int option ;
           mutable api : api_version ; }

let default : t = { seed_value = None;
                    api = V2; }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--development",
   Arg.Unit
     (fun () -> t.api <- V2),
   "enable experimental api - not intended for public use or comment");
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
       ~mode:`Append
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
  ]
