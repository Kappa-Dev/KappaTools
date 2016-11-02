let options ()  : (string * Arg.spec * string) list = [
  ("--version",
   Arg.Unit
     (fun () ->
        Format.print_string Version.version_msg ;
        Format.print_newline () ;
        exit 0),
   "display KaSim version");
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
       ?mode:(Some `Append)
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
