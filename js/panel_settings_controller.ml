(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let create_simulation_parameter param :
  Api_types_j.simulation_parameter = {
  Api_types_j.simulation_plot_period = param.State_project.plot_period ;
  Api_types_j.simulation_pause_condition = param.State_project.pause_condition ;
  Api_types_j.simulation_seed = param.State_project.seed;
  Api_types_j.simulation_id = "default" ;
  Api_types_j.simulation_store_trace = param.State_project.store_trace ;
}

let continue_simulation () =
  Common.async
    (fun () ->
       let simulation_parameter =
         create_simulation_parameter
           (React.S.value State_project.model).State_project.model_parameters in
       State_error.wrap
         __LOC__
         (State_simulation.continue_simulation simulation_parameter)
       >>= (fun _ -> Lwt.return_unit)
    )

let pause_simulation () =
  Common.async
    (fun () ->
       State_error.wrap __LOC__ (State_simulation.pause_simulation ())
       >>= (fun _ -> Lwt.return_unit)
    )

let stop_simulation () =
  Common.async
    (fun () ->
       let () = Common.debug (Js.string "subpanel_editor_controller.stop") in
       State_error.wrap __LOC__ (State_simulation.stop_simulation ())
       >>= (fun _ -> Lwt.return_unit)
    )

let start_simulation () =
  Common.async
    (fun () ->
       let simulation_parameter =
         create_simulation_parameter
           (React.S.value State_project.model).State_project.model_parameters in
       State_error.wrap
         __LOC__ (State_simulation.start_simulation simulation_parameter)
       >>= (fun _ -> Lwt.return_unit)
    )

let perturb_simulation () =
    Common.async
    (fun () ->
      let model_perturbation =
        React.S.value State_perturbation.model_perturbation in
      State_error.wrap
        __LOC__ (State_simulation.perturb_simulation model_perturbation)
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
      (fun manager ->
         State_error.wrap
           __LOC__
           manager#simulation_raw_trace >>=
         (Api_common.result_bind_lwt
            ~ok:(fun data ->
                let () =
                  Common.saveFile
                    ~data ~mime:"application/octet-stream"
                    ~filename:"trace.json" in
                Lwt.return (Api_common.result_ok ()))
         )
      )
