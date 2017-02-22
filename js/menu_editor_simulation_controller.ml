(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

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
