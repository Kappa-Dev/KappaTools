(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type manager_code =
  [ `OK | `Accepted | `Created |
    `Bad_request | `Conflict | `Not_found | `Request_timeout ]
type result_code = manager_code
type 'ok result = ('ok,manager_code) Api_types_t.result

type project_id = string

class type manager_environment = object
  method environment_info:
    unit -> Api_types_t.environment_info result Lwt.t
end

class type manager_project = object
  method project_parse :
    Api_types_t.overwritten_var list -> Api_types_t.project_parse result Lwt.t
end

class type manager_file = object
  method file_catalog : Api_types_t.file_catalog result Lwt.t

  method file_create :
    Api_types_t.file -> Api_types_t.file_metadata result Lwt.t

  method file_get : Api_types_t.file_id -> Api_types_t.file result Lwt.t

  method file_update :
    Api_types_t.file_id ->
    Api_types_t.file_modification ->
    Api_types_t.file_metadata result Lwt.t

  method file_delete : Api_types_t.file_id -> unit result Lwt.t
end

class type manager_file_line = object
  method simulation_catalog_file_line :
    Api_types_t.file_line_catalog result Lwt.t
  method simulation_detail_file_line : string -> string list result Lwt.t
end

class type manager_din = object
  method simulation_catalog_din : Api_types_t.din_catalog result Lwt.t
  method simulation_detail_din :
    Api_types_t.din_id -> Api_types_t.din result Lwt.t
end

class type manager_log_message = object
  method simulation_detail_log_message :
    Api_types_t.log_message result Lwt.t
end

class type manager_plot = object
  method simulation_detail_plot :
    Api_types_t.plot_parameter -> Api_types_t.plot result Lwt.t
end

class type manager_snapshot = object
  method simulation_catalog_snapshot : Api_types_t.snapshot_catalog result Lwt.t
  method simulation_detail_snapshot :
    Api_types_t.snapshot_id -> Api_types_t.snapshot result Lwt.t
end

class type manager_simulation = object
  method simulation_delete : unit result Lwt.t

  method simulation_start :
    Api_types_t.simulation_parameter ->
    Api_types_t.simulation_artifact result Lwt.t

  method simulation_pause : unit result Lwt.t

  method simulation_intervention :
    Api_types_t.simulation_intervention -> string result Lwt.t

  method simulation_continue : string -> unit result Lwt.t

  method simulation_info : Api_types_t.simulation_info result Lwt.t

  method simulation_efficiency : Counter.Efficiency.t result Lwt.t

  method simulation_parameter : Api_types_t.simulation_parameter result Lwt.t

  method simulation_raw_trace : string result Lwt.t

  method simulation_outputs_zip : Bigbuffer.bigstring result Lwt.t

  inherit manager_file_line
  inherit manager_din
  inherit manager_log_message
  inherit manager_plot
  inherit manager_snapshot
end

class type virtual manager_static_analysis = object
  method virtual is_running : bool
  method init_static_analyser :
    Ast.parsing_compil -> (unit, string) Lwt_result.t
  method init_static_analyser_raw :
    string -> (unit, string) Lwt_result.t
  (** The string has to be the json corresponding to an [Ast.parsing_compil] *)

  method get_contact_map :
    Public_data.accuracy_level option -> (Yojson.Basic.json,string) Lwt_result.t
  method get_influence_map :
    Public_data.accuracy_level option -> (Yojson.Basic.json,string) Lwt_result.t
  method get_local_influence_map :
    Public_data.accuracy_level option -> ?fwd:int -> ?bwd:int ->
    ?origin:(int,int) Public_data.influence_node -> total:int ->
    (Yojson.Basic.json,string) Lwt_result.t
  method get_initial_node : (Yojson.Basic.json,string) Lwt_result.t
  method get_next_node :
    (int,int) Public_data.influence_node option ->
    (Yojson.Basic.json,string) Lwt_result.t
  method get_previous_node :
    (int,int) Public_data.influence_node option ->
    (Yojson.Basic.json,string) Lwt_result.t
  method get_dead_rules : (Yojson.Basic.json,string) Lwt_result.t
  method get_non_weakly_reversible_transitions :
    (Yojson.Basic.json,string) Lwt_result.t
  method get_constraints_list : (Yojson.Basic.json,string) Lwt_result.t
  method get_potential_polymers :
    Public_data.accuracy_level option ->
    Public_data.accuracy_level option ->
    (Yojson.Basic.json,string) Lwt_result.t
end

class type manager = object
  inherit manager_project
  inherit manager_file
  inherit manager_simulation
end

class type concrete_manager = object
  inherit manager
  inherit manager_static_analysis
  method is_running : bool
  method terminate : unit
  method is_computing : bool
end

class type rest_manager = object
  inherit manager_environment
  inherit concrete_manager
  method project_catalog : string list result Lwt.t
  method project_create :
    Api_types_t.project_parameter -> unit result Lwt.t
  method project_delete : project_id -> unit result Lwt.t
end
