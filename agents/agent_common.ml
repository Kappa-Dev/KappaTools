(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let serve chan delimiter process_command : unit Lwt.t =
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char_opt chan >>= function
    | Some char ->
      if char = delimiter then
        let m = Buffer.contents buffer in
        process_command m >>= fun () ->
        let () = Buffer.reset buffer in aux_serve ()
      else
        let () = Buffer.add_char buffer char in
        aux_serve ()
    | None -> Lwt.return_unit in
  aux_serve ()

