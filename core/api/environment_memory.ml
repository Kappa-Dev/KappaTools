(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

class simulation (runtime_state : Kappa_facade.t)
  (simulation_parameter : Api_types_j.simulation_parameter) :
  Api_environment.simulation =
  object
    val mutable _runtime_state = runtime_state
    val mutable _simulation_parameter = simulation_parameter
    method get_runtime_state () = _runtime_state

    method set_runtime_state (runtime_state : Kappa_facade.t) =
      _runtime_state <- runtime_state

    method get_simulation_parameter () = _simulation_parameter

    method set_simulation_parameter
        (simulation_parameter : Api_types_j.simulation_parameter) : unit =
      _simulation_parameter <- simulation_parameter
  end

class project : Api_environment.project =
  object
    val mutable _simulation = None

    val mutable _state : Api_environment.parse_state option Lwt.t =
      Lwt.return_none

    method get_simulation () = _simulation
    method unset_simulation () = _simulation <- None

    method set_simulation
        (simulation_parameter : Api_types_j.simulation_parameter)
        (runtime_state : Kappa_facade.t) =
      _simulation <-
        Some
          (new simulation runtime_state simulation_parameter
            :> Api_environment.simulation)

    method set_state (state : Api_environment.parse_state Lwt.t) =
      let () = Lwt.cancel _state in
      _state <- (state >>= fun x -> Lwt.return (Some x))

    method get_state () : Api_environment.parse_state option Lwt.t = _state
  end
