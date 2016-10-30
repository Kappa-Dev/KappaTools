val msg_process_not_running : string
val msg_token_not_found : string
val msg_observables_less_than_zero : string

val assemble_distance :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_t.distances Api.result Lwt.t


val assemble_file_line :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_j.file_line list Api.result Lwt.t

val assemble_flux_map :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_j.flux_map list Api.result Lwt.t

val assemble_log_message :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_j.log_message list Api.result Lwt.t

val assemble_plot :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_j.plot option Api.result Lwt.t

val assemble_plot :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_j.plot option Api.result Lwt.t

val assemble_snapshot :
  Api.manager ->
  Api_types_j.project_id ->
  Api_types_j.simulation_id ->
  Api_types_v1_j.snapshot list Api.result Lwt.t

class type api_runtime =
  object
    method version :
      unit ->
      Api_types_v1_j.version Api_types_v1_j.result Lwt.t
    method parse :
      Api_types_v1_j.code ->
      Api_types_v1_j.parse Api_types_v1_j.result Lwt.t
    method start :
      Api_types_v1_j.parameter ->
      Api_types_v1_j.token Api_types_v1_j.result Lwt.t
    method status :
      Api_types_v1_j.token ->
      Api_types_v1_j.state Api_types_v1_j.result Lwt.t
    method list :
      unit ->
      Api_types_v1_j.catalog Api_types_v1_j.result Lwt.t
    method stop :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method perturbate :
      Api_types_v1_j.token ->
      Api_types_v1_j.perturbation ->
      unit Api_types_v1_j.result Lwt.t
    method pause :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method continue :
      Api_types_v1_j.token ->
      Api_types_v1_j.parameter ->
      unit Api_types_v1_j.result Lwt.t
  end;;

module Base : sig
  class virtual base_runtime :
    float -> object
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
      inherit api_runtime
    end;;
end;;
