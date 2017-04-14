(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let continue_simulation () =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ p_id _ ->
               let simulation_parameter =
                 State_project.create_simulation_parameter p_id in
               State_simulation.continue_simulation simulation_parameter))
       >>= (fun _ -> Lwt.return_unit)
    )

let pause_simulation () =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ _ _ ->
               State_simulation.pause_simulation ()))
       >>= (fun _ -> Lwt.return_unit)
    )

let stop_simulation () =
  Common.async
    (fun () ->
       let () = Common.debug (Js.string "subpanel_editor_controller.stop") in
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ _ _ ->
               State_simulation.stop_simulation ()))
       >>= (fun _ -> Lwt.return_unit)
    )

let start_simulation () =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ p_id _t ->
               let simulation_parameter =
                 State_project.create_simulation_parameter p_id in
               State_simulation.start_simulation simulation_parameter))
       >>= (fun _ -> Lwt.return_unit)
    )

let perturb_simulation () =
    Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ _ _ ->
               let model_perturbation = React.S.value State_perturbation.model_perturbation in
               State_simulation.perturb_simulation model_perturbation))
       >>= (fun _ -> Lwt.return_unit)
    )

let focus_range (range : Locality.t) : unit =
  let file_id = range.Locality.file in
  let line = range.Locality.from_position.Locality.line in
  Common.async
    (fun () ->
       State_error.wrap
         ~append:true
         __LOC__
         (State_file.select_file file_id (Some line))
       >>= (fun _ -> Lwt.return_unit)
    )

let simulation_trace () =
  State_simulation.when_ready
    ~label:__LOC__
      (fun manager project_id ->
         State_error.wrap
           __LOC__
           (manager#simulation_raw_trace project_id) >>=
         (Api_common.result_bind_lwt
            ~ok:(fun data ->
                let () =
                  Common.saveFile
                    ~data ~mime:"application/octet-stream"
                    ~filename:"trace.json" in
                Lwt.return (Api_common.result_ok ()))
         )
      )
