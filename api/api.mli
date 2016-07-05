val msg_process_not_running : string
val msg_token_not_found : string
val msg_observables_less_than_zero : string

type runtime =
  < info : unit -> Api_types_j.info ApiTypes_j.result Lwt.t;
    parse : ApiTypes_j.code -> ApiTypes_j.parse ApiTypes_j.result Lwt.t;
    start : ApiTypes_j.parameter -> ApiTypes_j.token ApiTypes_j.result Lwt.t;
    status : ApiTypes_j.token -> ApiTypes_j.state ApiTypes_j.result Lwt.t;
    list : unit -> ApiTypes_j.catalog ApiTypes_j.result Lwt.t;
    stop : ApiTypes_j.token -> unit ApiTypes_j.result Lwt.t;
    shutdown : unit -> unit ApiTypes_j.result Lwt.t;
  >;;

module Base : sig
  class session :
  object
    method sessionList :
      unit ->
        string list ApiTypes_j.result Lwt.t
    method sessionCreate :
      sessoinId:string ->
        unit ApiTypes_j.result Lwt.t
    method sessionDelete :
      sessoinId:string ->
        unit ApiTypes_j.result Lwt.t
    method sessionFeedback :
      sessoinId:string ->
        message:string ->
          unit ApiTypes_j.result Lwt.t
  end;;

  class virtual runtime :
    float (** min_run_duration *) -> object
    inherit session
    method info :
      unit ->
        Api_types_j.info ApiTypes_j.result Lwt.t
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
    method shutdown : unit -> unit ApiTypes_j.result Lwt.t
    method virtual log :
        ?exn:exn ->
          string ->
            unit Lwt.t
    method virtual yield :
        unit ->
          unit Lwt.t

  end;;
end;;
