(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* data structures *)
(* Manager state *)
type parse_state = (Kappa_facade.t, Result_util.message list) Result_util.t

class type simulation = object
  method get_runtime_state : unit -> Kappa_facade.t
  method set_runtime_state : Kappa_facade.t -> unit
  method get_simulation_parameter : unit -> Api_types_j.simulation_parameter
  method set_simulation_parameter : Api_types_j.simulation_parameter -> unit
end

class type project = object
  method unset_simulation : unit -> unit

  method set_simulation :
    Api_types_j.simulation_parameter -> Kappa_facade.t -> unit

  method get_simulation : unit -> simulation option
  method get_state : unit -> parse_state option Lwt.t
  method set_state : parse_state Lwt.t -> unit
end

class type environment = object
  method get_projects : unit -> project list
  method set_projects : project list -> unit
  method create_project : string -> project
end
