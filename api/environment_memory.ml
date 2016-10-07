class simulation (simulation_id : Api_types_j.simulation_id) (runtime_state : Kappa_facade.t) : Api_environment.simulation =
  object
    val mutable _runtime_state = runtime_state
    method get_simulation_id () = simulation_id
    method get_runtime_state () = _runtime_state
    method set_runtime_state (runtime_state : Kappa_facade.t) =
      _runtime_state <- runtime_state
  end

class project (project_id : Api_types_j.project_id) (parse_state : Api_environment.parse_state) : Api_environment.project =
  object
    val mutable _simulations = []
    val mutable _files = []
    val mutable _parse_state = parse_state
    method get_project_id () : Api_types_j.project_id = project_id
    method get_simulations () = _simulations
    method set_simulations (simulation : Api_environment.simulation list) = _simulations <- simulation
    method get_files () = _files
    method set_files (files : Api_types_j.file list) = _files <- files
    method get_parse_state () = _parse_state
    method set_parse_state (parse_state : Api_environment.parse_state) = _parse_state <- parse_state
  end

class workspace (workspace_id : Api_types_j.workspace_id) : Api_environment.workspace =
  object
    val mutable _projects : Api_environment.project list = []
    method get_workspace_id () = workspace_id
    method get_projects () = _projects
    method set_projects (projects : Api_environment.project list) = _projects <- projects
  end

class environment () : Api_environment.environment =
  object
    val mutable _workspaces : Api_environment.workspace list = []
    method set_workspaces (workspaces : Api_environment.workspace list) : unit = _workspaces <- workspaces
    method get_workspaces () : Api_environment.workspace list = _workspaces
  end;;
