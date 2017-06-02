open Lwt.Infix

class simulation
    (runtime_state : Kappa_facade.t)
    (simulation_parameter : Api_types_j.simulation_parameter) :
  Api_environment.simulation =
  object
    val mutable _runtime_state = runtime_state
    val mutable _simulation_parameter = simulation_parameter
    method get_simulation_id () = _simulation_parameter.Api_types_j.simulation_id
    method get_runtime_state () = _runtime_state
    method set_runtime_state (runtime_state : Kappa_facade.t) =
      _runtime_state <- runtime_state
    method get_simulation_parameter () = _simulation_parameter
    method set_simulation_parameter (simulation_parameter : Api_types_j.simulation_parameter) : unit =
      _simulation_parameter <- simulation_parameter
  end

class project : Api_environment.project =
  object
    val mutable _simulation = None
    val mutable _files = []
    val mutable _state : Api_environment.parse_state option Lwt.t =
      Lwt.return_none
    val mutable _version : Api_types_j.project_version = 0

    method get_simulation () = _simulation
    method unset_simulation () = _simulation <- None
    method set_simulation
        (simulation_parameter : Api_types_j.simulation_parameter)
        (runtime_state : Kappa_facade.t) =
      _simulation <-
        Some
          (new simulation
            runtime_state simulation_parameter :> Api_environment.simulation)

    method get_version () = _version

    method get_files () = _files
    method set_files (files : Api_types_j.file list) =
      let () = _files <- files in
      let () = _version <- 1 + _version in
      let () = Lwt.cancel _state in
      let () = _state <- Lwt.return_none in
      _version

    method set_state (state : Api_environment.parse_state Lwt.t)
      : Api_types_j.project_version =
      let () = _state <- (state >>= fun x -> Lwt.return (Some x)) in
      _version
    method get_state () : Api_environment.parse_state option Lwt.t =
      _state

  end
