(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let sync () : unit Lwt.t =
  Lwt.return_unit >>=
  State_settings.sync >>=
  State_runtime.sync >>=
  (fun _ -> State_project.sync () >>= (fun _ -> Lwt.return_unit)) >>=
  (fun _ -> State_file.sync () >>= (fun _ -> Lwt.return_unit)) >>=
  (fun _ -> State_simulation.sync () >>= (fun _ -> Lwt.return_unit))

let init () : unit Lwt.t =
  Lwt.return_unit >>=
  State_parameter.init >>=
  State_settings.init >>=
  State_runtime.init >>=
  State_project.init >>=
  State_file.init >>=
  State_simulation.init >>=
  sync


let rec loop
    (h : unit -> unit Lwt.t)
    (t : float)
    ()
  : unit Lwt.t =
  h () >>= (fun _ -> Lwt_js.sleep t) >>= loop h t

let loop_sync () : unit Lwt.t =
  let () = Common.debug (Js.string "loop sync") in
  sync ()

let onload () : unit =
  Common.async (fun () -> Lwt.return_unit >>= init >>= loop loop_sync 1.0)
