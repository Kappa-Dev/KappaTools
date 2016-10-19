let on_message :
  Api.manager -> (string -> unit Lwt.t) -> string -> unit Lwt.t = failwith ""

class virtual manager ?(timeout : float = 10.) () =
  object(self)
    method virtual post_message : string -> unit
    method virtual sleep : float -> unit Lwt.t
    method receive : string -> unit

    method environment_info () : Api_types_j.environment_info Api.result Lwt.t =
      failwith ""

    method file_create
        (project_id : Api_types_j.project_id)
        (file : Api_types_j.file) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      failwith ""

    method file_delete
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      unit Api_types_j.file_result Api.result Lwt.t =
      failwith ""

    method file_get
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      Api_types_j.file Api.result Lwt.t =
      failwith ""

    method file_info
      (project_id : Api_types_j.project_id) :
      Api_types_j.file_info Api.result Lwt.t =
      failwith ""

    method file_update
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id)
        (file_modification : Api_types_j.file_modification) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      failwith ""

    method project_create
      (project_parameter : Api_types_j.project_parameter) :
      Api_types_j.project_id Api.result Lwt.t =
      failwith ""

    method project_delete
        (project_id : Api_types_j.project_id) :
      unit Api.result Lwt.t =
      failwith ""

    method project_info () : Api_types_j.project_info Api.result Lwt.t =
      failwith ""

    method simulation_continue
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (simulation_parameter :Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      failwith ""

    method simulation_delete
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      failwith ""

    method simulation_get_distance
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (distance_id : Api_types_j.distance_id) :
      Api_types_j.distance Api.result Lwt.t =
      failwith ""

    method simulation_get_file_line
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (file_line_id : Api_types_j.file_line_id) :
      Api_types_j.file_line list Api.result Lwt.t =
      failwith ""

    method simulation_get_flux_map
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      failwith ""
    method simulation_get_log_message
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.log_message list Api.result Lwt.t =
      failwith ""
    method simulation_get_plot
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.plot Api.result Lwt.t =
      failwith ""
    method simulation_get_snapshot
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (snapshot_id : Api_types_j.snapshot_id) :
      Api_types_j.snapshot Api.result Lwt.t =
      failwith ""
    method simulation_info
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.simulation_info Api.result Lwt.t =
      failwith ""
    method simulation_info_distance
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.distance_info Api.result Lwt.t =
      failwith ""
    method simulation_info_file_line
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.file_line_info Api.result Lwt.t =
      failwith ""
    method simulation_info_flux_map
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.flux_map_info Api.result Lwt.t =
      failwith ""
    method simulation_info_snapshot
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.snapshot_info Api.result Lwt.t =
      failwith ""
    method simulation_list
      (project_id : Api_types_j.project_id)
      : Api_types_j.simulation_catalog Api.result Lwt.t =
      failwith ""
    method simulation_pause
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      failwith ""

    method simulation_perturbation
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      failwith ""

    method simulation_start
      (project_id : Api_types_j.project_id)
      (simulation_parameter : Api_types_j.simulation_parameter)
      : Api_types_j.simulation_id Api.result Lwt.t =
      failwith ""
    method simulation_stop
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      failwith ""

    inherit Api.manager
  end

let message_delimter : char = '\x1e' (* "\t" *)
