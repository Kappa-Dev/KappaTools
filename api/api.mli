(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type manager_code = [ `ACCEPTED |
                      `CONFLICT |
                      `CREATED |
                      `ERROR |
                      `NOT_FOUND |
                      `OK ]
type result_code = manager_code
type 'ok result = ('ok,manager_code) Api_types_j.result

class type manager_environment =
  object
    method environment_info :
      unit ->
      Api_types_j.environment_info result Lwt.t
  end;;

class type manager_project =
  object
    method project_info :
      unit -> Api_types_j.project_info result Lwt.t
    method project_create :
      Api_types_j.project_parameter ->
      Api_types_j.project_id result Lwt.t
    method project_delete :
      Api_types_j.project_id ->
      unit result Lwt.t
  end;;

class type manager_file =
  object
    method file_info :
      Api_types_j.project_id ->
      Api_types_j.file_info result Lwt.t

    method file_create :
      Api_types_j.project_id ->
      Api_types_j.file ->
      Api_types_j.file_metadata Api_types_j.file_result result Lwt.t

    method file_get :
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      Api_types_j.file result Lwt.t

    method file_update :
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      Api_types_j.file_modification ->
      Api_types_j.file_metadata Api_types_j.file_result result Lwt.t

    method file_delete :
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      unit Api_types_j.file_result result Lwt.t

  end;;

class type  manager_file_line =
  object
    method simulation_info_file_line :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.file_line_info result Lwt.t
    method simulation_detail_file_line :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.file_line_id ->
      Api_types_j.file_line list result Lwt.t
  end;;

class type  manager_flux_map =
  object
    method simulation_info_flux_map :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.flux_map_info result Lwt.t
    method simulation_detail_flux_map :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.flux_map_id ->
      Api_types_j.flux_map result Lwt.t
  end;;

class type  manager_log_message =
  object
    method simulation_detail_log_message :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.log_message list result Lwt.t
  end;;

class type  manager_plot =
  object
    method simulation_detail_plot :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.plot result Lwt.t
  end;;

class type  manager_snapshot =
  object
    method simulation_info_snapshot :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.snapshot_info result Lwt.t
    method simulation_detail_snapshot :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.snapshot_id ->
      Api_types_j.snapshot result Lwt.t
  end;;

class type  manager_simulation =
  object
    method simulation_list :
      Api_types_j.project_id ->
      Api_types_j.simulation_catalog result Lwt.t

    method simulation_delete :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      unit result Lwt.t

    method simulation_start :
      Api_types_j.project_id ->
      Api_types_j.simulation_parameter->
      Api_types_j.simulation_id result Lwt.t

    method simulation_pause :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      unit result Lwt.t

    method simulation_perturbation :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_perturbation ->
      unit result Lwt.t

    method simulation_continue :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_parameter ->
      unit result Lwt.t

    method simulation_info :
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_info result Lwt.t

    inherit  manager_file_line
    inherit  manager_flux_map
    inherit  manager_log_message
    inherit  manager_plot
    inherit  manager_snapshot

  end;;

class type manager =
  object
    inherit manager_environment
    inherit manager_file
    inherit manager_project
    inherit manager_simulation
  end;;
