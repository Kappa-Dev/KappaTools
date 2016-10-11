(* data structures *)
(* Manager state *)
class type simulation =
  object
    method get_simulation_id : unit -> Api_types_j.simulation_id
    method get_runtime_state : unit -> Kappa_facade.t
    method set_runtime_state : Kappa_facade.t -> unit
  end

class type project =
  object
    method create_simulation :
      Api_types_j.simulation_id ->
      Kappa_facade.t ->
      simulation
    method get_project_id : unit -> Api_types_j.project_id
    method get_simulations : unit -> simulation list
    method set_simulations : simulation list -> unit
    method get_files : unit -> Api_types_j.file list
    method set_files : Api_types_j.file list -> unit
    method get_state : unit -> Kappa_facade.t
    method set_state : Kappa_facade.t -> unit
  end

class type environment =
  object
    method get_projects : unit -> project list
    method set_projects : project list -> unit
    method create_project : Api_types_j.project_id -> Kappa_facade.t -> project
  end
