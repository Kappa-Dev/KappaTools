(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let set_content (filecontent : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.set_content filecontent)
       >>= (fun _ -> Lwt.return_unit)
    )


let set_manager (runtime_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_runtime.set_manager runtime_id) >>=
       (fun _ -> State_project.sync ()) >>=
       (Api_common.result_bind_lwt ~ok:State_file.sync) >>=
       (fun _ -> Lwt.return_unit)
    )

let create_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.create_project project_id) >>=
       (Api_common.result_bind_lwt ~ok:State_file.sync) >>=
       (fun _ -> Lwt.return_unit)
    )

let set_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.set_project project_id) >>=
       (Api_common.result_bind_lwt ~ok:State_file.sync) >>=
       (fun _ -> Lwt.return_unit)
    )

let close_project () : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.remove_project ()) >>=
       (Api_common.result_bind_lwt ~ok:State_file.sync) >>=
       (fun _ -> Lwt.return_unit)
    )

let create_file (file_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.create_file ~filename:file_id ~content:"")
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let set_file (file_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.select_file file_id)
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let close_file () : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.remove_file ())
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let set_file_compile (file_id: string) (compile : bool) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.set_compile
            file_id
            compile)
       >>=
       (fun _ -> State_project.sync ()) (* get new contact map *)
       >>=
       (fun _ -> Lwt.return_unit)
    )

let order_files (filenames : string list) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.order_files filenames)
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let close_simulation () : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.remove_simulation ())
       >>= (fun _ -> Lwt.return_unit)
    )

let create_simulation (simulation_id : Api_types_j.simulation_id) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.create_simulation simulation_id)
       >>= (fun _ -> Lwt.return_unit)
    )

let set_simulation (simulation_id : Api_types_j.simulation_id) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.set_simulation simulation_id)
       >>= (fun _ -> Lwt.return_unit)
    )

let continue_simulation () =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_simulation.with_simulation
            ~label:__LOC__
            (fun _ _ t ->
               let simulation_id = State_simulation.t_simulation_id t in
               let simulation_parameter = State_parameter.create_parameter simulation_id  in
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
            (fun _ _ t ->
               let simulation_id = State_simulation.t_simulation_id t in
               let simulation_parameter = State_parameter.create_parameter simulation_id  in
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
