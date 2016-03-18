val time_yield : float -> (unit -> unit Lwt.t) -> unit -> unit Lwt.t

val msg_process_not_running : string
val msg_token_not_found : string
val msg_observables_less_than_zero : string

type runtime = < parse : ApiTypes_j.code -> ApiTypes_j.parse ApiTypes_j.result Lwt.t;
                 start : ApiTypes_j.parameter -> ApiTypes_j.token ApiTypes_j.result Lwt.t;
                 status : ApiTypes_j.token -> ApiTypes_j.state ApiTypes_j.result Lwt.t;
                 list : unit -> ApiTypes_j.catalog ApiTypes_j.result Lwt.t;
                 stop : ApiTypes_j.token -> unit ApiTypes_j.result Lwt.t
               >;;
module Base : sig

  class virtual runtime : object
                    method parse : ApiTypes_j.code -> ApiTypes_j.parse ApiTypes_j.result Lwt.t
                    method start : ApiTypes_j.parameter -> ApiTypes_j.token ApiTypes_j.result Lwt.t
                    method status : ApiTypes_j.token -> ApiTypes_j.state ApiTypes_j.result Lwt.t
                    method list : unit -> ApiTypes_j.catalog ApiTypes_j.result Lwt.t
                    method stop : ApiTypes_j.token -> unit ApiTypes_j.result Lwt.t
                    method virtual log : string -> unit Lwt.t
                    method virtual yield : unit -> unit Lwt.t
                  end;;
end;;
