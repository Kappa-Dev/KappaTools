(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let sync () : unit Lwt.t =
  State_settings.sync () >>= State_runtime.sync >>= State_project.sync
  >>= fun _ ->
  State_file.sync () >>= fun _ -> Lwt.return_unit

let init () : unit Lwt.t =
  Lwt.return_unit >>= State_settings.init >>= State_runtime.init
  >>= State_project.init >>= State_file.init >>= State_simulation.init >>= sync

let rec loop (h : unit -> unit Lwt.t) (t : float) () : unit Lwt.t =
  h () >>= (fun _ -> Js_of_ocaml_lwt.Lwt_js.sleep t) >>= loop h t

let loop_sync () : unit Lwt.t =
  if React.S.value State_settings.synch then (
    let () = Common.debug (Js.string "loop sync") in
    sync ()
  ) else
    Lwt.return_unit

let onload () : unit =
  Common.async __LOC__ (fun () ->
      Lwt.return_unit >>= init >>= loop loop_sync 20.0)
