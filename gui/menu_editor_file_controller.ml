(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let create_file ?(text = Lwt.return (Js.string "")) (file_id : string) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( text >>= fun txt ->
          let content = Js.to_string txt in
          State_file.create_file ~filename:file_id ~content
          >>= (* get new contact map *)
          fun r ->
          State_project.sync () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      >>= fun _ -> Lwt.return_unit)

let set_file (file_id : string) : unit =
  let () = Common.debug (Js.string (Format.sprintf "set_file:%s" file_id)) in
  Common.async __LOC__ (fun () ->
      State_error.wrap ~append:true __LOC__
        ( State_file.select_file file_id None >>= fun r ->
          State_project.sync () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)

let close_file () : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( State_file.remove_file () >>= fun r ->
          State_project.sync () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)

let set_file_compile rank (compile : bool) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( State_file.set_compile rank compile >>= fun r ->
          State_project.sync () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)

let order_files (filenames : string list) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        ( State_file.order_files filenames >>= fun r ->
          State_project.sync () >>= fun r' ->
          Lwt.return (Api_common.result_combine [ r; r' ]) )
      (* get new contact map *)
      >>= fun _ -> Lwt.return_unit)

let export_current_file () =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__ (State_file.get_file ())
      >>= Result_util.fold
            ~ok:(fun (data, filename) ->
              let () =
                Common.saveFile ~data:(Js.string data)
                  ~mime:"application/octet-stream" ~filename
              in
              Lwt.return_unit)
            ~error:(fun _ -> Lwt.return_unit))
