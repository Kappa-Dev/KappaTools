(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = { mutable delimiter: char }

let default : t = { delimiter = Tools.default_message_delimter }

let options (t : t) : (string * Arg.spec * string) list =
  [
    ( "--delimiter",
      Arg.String
        (fun d ->
          try
            let d = Scanf.unescaped d in
            let d =
              if 1 = String.length d then
                String.get d 0
              else
                raise
                  (Arg.Bad
                     (Format.sprintf "delimeter has multiple characters '%s'" d))
            in
            let () = t.delimiter <- d in
            ()
          with Scanf.Scan_failure _ ->
            raise (Arg.Bad (Format.sprintf "failed to parse delimeter '%s'" d))),
      "Delimiter for message passing" );
  ]
