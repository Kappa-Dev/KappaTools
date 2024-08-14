(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception BadResponse of Mpi_message_j.response_content

class type virtual manager_simulation_mpi = object
  method private virtual sleep : float -> unit Lwt.t
  method private message : Mpi_message_j.request -> Mpi_message_j.response Lwt.t
  method private receive : string -> unit
  inherit Api.manager_simulation
  method private sim_is_computing : bool
  method virtual is_running : bool
end

class virtual new_client :
  post:(string -> unit) ->
  unit ->
  manager_simulation_mpi
