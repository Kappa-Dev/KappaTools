val msg_process_not_running : string
val msg_token_not_found : string
val msg_observables_less_than_zero : string

class type api_runtime =
  object
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
