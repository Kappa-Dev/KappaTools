(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let with_file (handler : (string * string) Api.result -> unit Api.result Lwt.t)
    =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__ (State_file.get_file () >>= handler) >>= fun _ ->
      Lwt.return_unit)

let set_content ~(filename : string) ~(filecontent : string) : unit =
  with_file
    (Api_common.result_bind_lwt ~ok:(fun (_, current_filename) ->
         if filename = current_filename then
           State_file.set_content filecontent >>= fun r ->
           State_project.sync () >>= fun r' ->
           Lwt.return (Api_common.result_combine [ r; r' ])
         else (
           let msg =
             Format.sprintf "file name mismatch %s %s" filename current_filename
           in
           Lwt.return (Api_common.result_error_msg msg)
         )))
