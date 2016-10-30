open Lwt
open Unix
open Lwt_log

type t = { mutable delimiter : char option; }
let default : t = { delimiter = None; }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--delimiter",
   Arg.String
     (fun d ->
        try
          let d = Scanf.unescaped d in
          let d = if 1 = String.length d then
              String.get d 0
            else
              raise (Arg.Bad (Format.sprintf "delimeter has multiple characters '%s'" d))
          in
          let () = t.delimiter <- Some d in
          ()
        with Scanf.Scan_failure _ ->
          raise (Arg.Bad (Format.sprintf "failed to parse delimeter '%s'" d))
     ),
   "Delimiter for message passing") ;
  ]
