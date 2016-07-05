class runtime : ?timeout:float -> ?shutdown_key: string -> string -> object
  method info : unit -> Api_types_j.info ApiTypes_j.result Lwt.t
  method parse : ApiTypes_j.code -> ApiTypes_j.parse ApiTypes_j.result Lwt.t
  method start : ApiTypes_j.parameter -> ApiTypes_j.token ApiTypes_j.result Lwt.t
  method status : ApiTypes_j.token -> ApiTypes_j.state ApiTypes_j.result Lwt.t
  method list : unit -> ApiTypes_j.catalog ApiTypes_j.result Lwt.t
  method stop : ApiTypes_j.token -> unit ApiTypes_j.result Lwt.t
  method shutdown : unit -> unit ApiTypes_j.result Lwt.t
end;;
