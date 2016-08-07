open Lwt_log
open Lwt

type api_version = V1 | V2;;

type t = {
  mutable port : int;
  mutable cert_dir : string option;
  mutable shutdown_key : string option;
  mutable api : api_version
}

let default : t = {
  port = 8080;
  cert_dir = None;
  shutdown_key = None;
  api = V1
}
let usage_msg = "[--development] | [-port port] [--shutdownkey shulog file] [--level [debug|info|notice|warning|error|fatal]]"

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--development",
   Arg.Unit
     (fun () ->
	let () = Lwt.async
		(fun () ->
		   Lwt_log_core.log
		     ~level:Lwt_log_core.Warning
		     "development features - not for public use or comment")
        in
        t.api <- V2),
   "enable experimental api - not intended for public use or comment");
  ("--port",
   Arg.Int
     (fun port -> t.port <- port),
   "port to serve on");
  ("--shutdown-key",
   Arg.String
     (fun key -> t.shutdown_key <- Some key),
   "key to shutdown server");
  ("--cert-dir",
   Arg.String
     (fun key -> t.cert_dir <- Some key),
   "Directory where to find cert.pem and privkey.pem")
  ]
