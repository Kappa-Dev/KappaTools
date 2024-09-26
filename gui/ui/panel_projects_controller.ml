(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let refresh result_before =
  let lwt_result_state_file_sync = State_file.sync ~reset:true () in
  let lwt_result_simulation_refresh = State_simulation.refresh () in
  lwt_result_state_file_sync >>= fun result_state_file_sync ->
  lwt_result_simulation_refresh >>= fun result_simulation_refresh ->
  Lwt.return
    (Api_common.result_combine
       [ result_before; result_state_file_sync; result_simulation_refresh ])

let create_project (project_id : string) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        (State_project.create_project project_id >>= refresh)
      >>= fun _ -> Lwt.return_unit)

let set_project (project_id : string) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__ (State_project.set_project project_id >>= refresh)
      >>= fun _ -> Lwt.return_unit)

let close_project project_id : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        (State_project.remove_project project_id >>= refresh)
      >>= fun _ -> Lwt.return_unit)

let set_manager (runtime_id : string) : unit =
  Common.async __LOC__ (fun () ->
      State_error.wrap __LOC__
        (Api_common.result_bind_with_lwt
           ~ok:(fun () -> Lwt.return (Result_util.ok ()))
           (State_runtime.create_spec ~load:true runtime_id))
      >>= fun _ -> Lwt.return_unit)
