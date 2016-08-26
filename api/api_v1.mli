val msg_process_not_running : string
val msg_token_not_found : string
val msg_observables_less_than_zero : string

class type api_runtime =
  object
    method parse :
      ApiTypes_j.code ->
      ApiTypes_j.parse ApiTypes_j.result Lwt.t
    method start :
      ApiTypes_j.parameter ->
      ApiTypes_j.token ApiTypes_j.result Lwt.t
    method status :
      ApiTypes_j.token ->
      ApiTypes_j.state ApiTypes_j.result Lwt.t
    method list :
      unit ->
      ApiTypes_j.catalog ApiTypes_j.result Lwt.t
    method stop :
      ApiTypes_j.token ->
      unit ApiTypes_j.result Lwt.t
    method perturbate :
      ApiTypes_j.token ->
      ApiTypes_j.perturbation ->
      unit ApiTypes_j.result Lwt.t
    method pause :
      ApiTypes_j.token ->
      unit ApiTypes_j.result Lwt.t
    method continue :
      ApiTypes_j.token ->
      ApiTypes_j.parameter ->
      unit ApiTypes_j.result Lwt.t
  end;;

module Base : sig
  class virtual base_runtime :
    float -> object
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
      inherit api_runtime
    end;;
end;;
