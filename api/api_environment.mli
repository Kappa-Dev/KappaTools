(* data structures *)
(* Manager state *)
type parse_state = (Kappa_facade.t,Api_types_j.errors) Api_types_j.result_data
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

    (* The version keeps track of the files and facade.
       The simulations are ignored as they don't change
       how the kappa program in interpreted. *)
    method get_version : unit -> Api_types_j.project_version

    method get_files : unit -> Api_types_j.file list
    method set_files : Api_types_j.file list -> Api_types_j.project_version

    method get_state : unit -> parse_state option
    method set_state : parse_state -> Api_types_j.project_version

  end

class type environment =
  object
    method get_projects : unit -> project list
    method set_projects : project list -> unit
    method create_project : Api_types_j.project_id -> parse_state -> project
  end
