(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let with_file (handler : (string * string) Api.result -> unit Api.lwt_result) =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__ (State_file.get_file () >>= handler) >>= fun _ ->
      Lwt.return_unit)

let set_content ~(filename : string) ~(filecontent : string) : unit =
  with_file
    (Api_common.result_bind_with_lwt ~ok:(fun (_, current_filename) ->
         if filename = current_filename then
           State_file.set_content filecontent >>= fun r ->
           State_project.sync false () >>= fun r' ->
           Lwt.return (Api_common.result_combine [ r; r' ])
         else (
           let msg =
             Format.sprintf "file name mismatch %s %s" filename current_filename
           in
           Lwt.return (Api_common.err_result_of_string msg)
         )))

let enable_or_disable_rule rule_id (enable : bool) =
  let rule_id = int_of_string rule_id in
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( State_project.eval_with_project ~label:__LOC__ (fun manager ->
              manager#enable_or_disable_rule rule_id enable)
        >>= fun r ->
          State_project.sync_no_compilation () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)
