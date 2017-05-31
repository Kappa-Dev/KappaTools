(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type manager_code =
  [ `ACCEPTED | `CONFLICT | `CREATED | `ERROR | `NOT_FOUND | `OK ]
type result_code = manager_code
type 'ok result = ('ok,manager_code) Api_types_t.result

type project_id = Api_types_t.project_id

class type manager_environment =
  object
    method environment_info:
      unit -> Api_types_j.environment_info result Lwt.t
  end

class type manager_project =
  object
    method project_catalog :
      unit -> Api_types_j.project_catalog result Lwt.t
    method project_get :
      project_id -> Api_types_j.project result Lwt.t
    method project_parse :
      project_id -> Api_types_j.project_parse result Lwt.t
    method project_create :
      Api_types_j.project_parameter -> unit result Lwt.t
    method project_delete :
      project_id ->
      unit result Lwt.t
  end
(* The type is parameterized here are there are
   implementations of the file manager that cannot
   generate the contact map.

   'file_status_summary = Api_types_j.contact_map
*)
class type manager_file =
  object
    method file_catalog :
      project_id ->
      Api_types_j.file_catalog result Lwt.t

    method file_create :
      project_id ->
      Api_types_j.file ->
      Api_types_j.file_metadata result Lwt.t

    method file_get :
      project_id ->
      Api_types_j.file_id ->
      Api_types_j.file result Lwt.t

    method file_update :
      project_id ->
      Api_types_j.file_id ->
      Api_types_j.file_modification ->
      Api_types_j.file_metadata result Lwt.t

    method file_delete :
      project_id ->
      Api_types_j.file_id ->
      unit result Lwt.t
  end

class type  manager_file_line =
  object
    method simulation_catalog_file_line :
      project_id ->
      Api_types_j.file_line_catalog result Lwt.t
    method simulation_detail_file_line :
      project_id ->
      Api_types_j.file_line_id ->
      Api_types_j.file_line list result Lwt.t
  end

class type  manager_flux_map =
  object
    method simulation_catalog_flux_map :
      project_id ->
      Api_types_j.flux_map_catalog result Lwt.t
    method simulation_detail_flux_map :
      project_id ->
      Api_types_j.flux_map_id ->
      Api_types_j.flux_map result Lwt.t
  end

class type  manager_log_message =
  object
    method simulation_detail_log_message :
      project_id ->
      Api_types_j.log_message result Lwt.t
  end

class type  manager_plot =
  object
    method simulation_detail_plot :
      project_id ->
      Api_types_j.plot_parameter ->
      Api_types_j.plot_detail result Lwt.t
  end

class type  manager_snapshot =
  object
    method simulation_catalog_snapshot :
      project_id ->
      Api_types_j.snapshot_catalog result Lwt.t
    method simulation_detail_snapshot :
      project_id ->
      Api_types_j.snapshot_id ->
      Api_types_j.snapshot result Lwt.t
  end

class type  manager_simulation =
  object
    method simulation_delete :
      project_id ->
      unit result Lwt.t

    method simulation_start :
      project_id ->
      Api_types_j.simulation_parameter->
      Api_types_j.simulation_artifact result Lwt.t

    method simulation_pause :
      project_id ->
      unit result Lwt.t

    method simulation_perturbation :
      project_id ->
      Api_types_j.simulation_perturbation ->
      unit result Lwt.t

    method simulation_continue :
      project_id ->
      Api_types_j.simulation_parameter ->
      unit result Lwt.t

    method simulation_info :
      project_id ->
      Api_types_j.simulation_info result Lwt.t

    method simulation_efficiency :
      project_id -> Counter.Efficiency.t result Lwt.t

    method simulation_parameter :
      project_id ->
      Api_types_j.simulation_parameter result Lwt.t

    method simulation_raw_trace :
      project_id -> string result Lwt.t

    inherit  manager_file_line
    inherit  manager_flux_map
    inherit  manager_log_message
    inherit  manager_plot
    inherit  manager_snapshot
  end

(* project_id is there only for now and for the REST API *)
class type manager_static_analysis =
  object
    method init_static_analyser :
      project_id -> Ast.parsing_compil -> (unit, string) Lwt_result.t
    method init_static_analyser_raw :
      project_id -> string -> (unit, string) Lwt_result.t
    method get_contact_map :
      project_id -> Public_data.accuracy_level option ->
      (Yojson.Basic.json,string) Lwt_result.t
    method get_influence_map :
      project_id -> Public_data.accuracy_level option ->
      (Yojson.Basic.json,string) Lwt_result.t
    method get_dead_rules :
      project_id -> (Yojson.Basic.json,string) Lwt_result.t
    method get_constraints_list :
      project_id -> (Yojson.Basic.json,string) Lwt_result.t
  end

class type manager =
  object
    inherit manager_environment
    inherit manager_project
    inherit manager_file
    inherit manager_simulation
  end

class type concrete_manager =
  object
    inherit manager
    inherit manager_static_analysis
    method terminate : unit -> unit
  end
