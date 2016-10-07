(* data structures *)
(* Manager state *)
type parse_state = ParseOk of Api_types_j.project_parse
                 | ParseErrors of Api_types_j.errors

class type simulation =
  object
    method get_simulation_id : unit -> Api_types_j.simulation_id
    method get_runtime_state : unit -> Kappa_facade.t
    method set_runtime_state : Kappa_facade.t -> unit
  end

class type project =
  object
    method get_project_id : unit -> Api_types_j.project_id
    method get_simulations : unit -> simulation list
    method set_simulations : simulation list -> unit
    method get_files : unit -> Api_types_j.file list
    method set_files : Api_types_j.file list -> unit
    method get_parse_state : unit -> parse_state
    method set_parse_state : parse_state -> unit
  end

class type workspace =
  object
    method get_workspace_id : unit -> Api_types_j.workspace_id
    method get_projects : unit -> project list
    method set_projects : project list -> unit
  end


class type environment =
  object
    method set_workspaces : workspace list -> unit
    method get_workspaces : unit -> workspace list
  end
