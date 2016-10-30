open Lwt_log
open Lwt

type t = {
  mutable port : int;
  mutable cert_dir : string option;
  mutable shutdown_key : string option;
}

let default : t = {
  port = 8080;
  cert_dir = None;
  shutdown_key = None;
}
let usage_msg = "[--development] | [-port port] [--shutdownkey shulog file] [--level [debug|info|notice|warning|error|fatal]]"

let options (t :t)  : (string * Arg.spec * string) list = [
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
