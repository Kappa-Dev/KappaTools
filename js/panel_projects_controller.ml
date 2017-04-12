(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let create_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.create_project project_id) >>=
       (Api_common.result_map
          ~ok:(fun _ () ->
              (State_file.sync () >>= (fun _ -> Lwt.return_unit)) <&>
              (State_simulation.refresh () >>= (fun _ -> Lwt.return_unit)))
          ~error:(fun _ _ -> Lwt.return_unit)
       ))

let set_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__

         (let () = State_file.reset () in
          State_project.set_project project_id) >>=
       (Api_common.result_map
          ~ok:(fun _ () ->
              (State_file.sync () >>= (fun _ -> Lwt.return_unit)) <&>
              (State_simulation.refresh () >>= (fun _ -> Lwt.return_unit)))
          ~error:(fun _ _ -> Lwt.return_unit)
       ))

let close_project project_id : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.remove_project project_id >>=
          (Api_common.result_bind_lwt
             ~ok:(fun () ->
                 ((State_file.sync () >>= (fun _ -> Lwt.return_unit)) <&>
                  (State_simulation.refresh () >>= (fun _ -> Lwt.return_unit)))>>=
                 (fun _ -> Lwt.return (Api_common.result_ok ())))
          )) >>=
       (fun _ -> Lwt.return_unit))
