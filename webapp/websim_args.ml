(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  mutable port: int;
  mutable cert_dir: string option;
  mutable shutdown_key: string option;
}

let default : t = { port = 8080; cert_dir = None; shutdown_key = None }

let usage_msg =
  "[--development] | [-port port] [--shutdownkey shulog file] [--level \
   [debug|info|notice|warning|error|fatal]]"

let options (t : t) : (string * Arg.spec * string) list =
  [
    "--port", Arg.Int (fun port -> t.port <- port), "port to serve on";
    ( "--shutdown-key",
      Arg.String (fun key -> t.shutdown_key <- Some key),
      "key to shutdown server" );
    ( "--cert-dir",
      Arg.String (fun key -> t.cert_dir <- Some key),
      "Directory where to find cert.pem and privkey.pem" );
  ]
