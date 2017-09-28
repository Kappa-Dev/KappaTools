(* data structures *)
(* Manager state *)
type parse_state = (Kappa_facade.t,Api_types_j.errors) Result.result

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

  (* The version keeps track of the files and facade.
       The simulations are ignored as they don't change
       how the kappa program in interpreted. *)
  method get_version : unit -> int

  method get_files : unit -> Api_types_j.file list
  method set_files : Api_types_j.file list -> int

  method get_state : unit -> parse_state option Lwt.t
  method set_state : parse_state Lwt.t -> int
end

class type environment = object
  method get_projects : unit -> project list
  method set_projects : project list -> unit
  method create_project : string -> project
end
