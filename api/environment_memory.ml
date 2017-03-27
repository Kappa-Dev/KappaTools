open Lwt.Infix

class simulation
    (simulation_id : Api_types_j.simulation_id)
    (runtime_state : Kappa_facade.t) :
  Api_environment.simulation =
  object
    val mutable _runtime_state =
      runtime_state
    method get_simulation_id () =
      simulation_id
    method get_runtime_state () =
      _runtime_state
    method set_runtime_state (runtime_state : Kappa_facade.t) =
      _runtime_state <- runtime_state

  end

class project
    (project_id : Api_types_j.project_id)
    (state : Api_environment.parse_state) :
  Api_environment.project =
  object
    val mutable _simulations = []
    val mutable _files = []
    val mutable _state : Api_environment.parse_state option Lwt.t =
      Lwt.return_none
    val mutable _version : Api_types_j.project_version = 0
    method create_simulation
        (simulation_id : Api_types_j.simulation_id)
        (state : Kappa_facade.t) : Api_environment.simulation =
      (new simulation
        simulation_id
        state :> Api_environment.simulation)

    method get_project_id () : Api_types_j.project_id =
      project_id
    method get_simulations () =
      _simulations
    method set_simulations (simulation : Api_environment.simulation list) =
      _simulations <- simulation

    method get_version () = _version

    method get_files () = _files
    method set_files (files : Api_types_j.file list) =
      let () = _files <- files in
      let () = Lwt.cancel _state in
      let () = _state <- Lwt.return_none in
      _version

    method set_state (state : Api_environment.parse_state Lwt.t)
      : Api_types_j.project_version =
      let () = _version <- 1 + _version in
      let () = _state <- (state >>= fun x -> Lwt.return (Some x)) in
      _version
    method get_state () : Api_environment.parse_state option Lwt.t =
      _state

  end

class environment () : Api_environment.environment =
  object
    val mutable _projects : Api_environment.project list = []
    method get_projects () = _projects
    method set_projects (projects : Api_environment.project list) =
      _projects <- projects
    method create_project
        (project_id : Api_types_j.project_id)
        (state : Api_environment.parse_state) = new project project_id state
  end;;
