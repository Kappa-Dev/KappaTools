class runtime : ?timeout:float -> unit ->
  object
    val worker : (string, string) Worker.worker Js.t
    method list : unit -> ApiTypes_j.catalog ApiTypes_j.result Lwt.t
    method parse :
      ApiTypes_j.code -> ApiTypes_j.parse ApiTypes_j.result Lwt.t
    method post_message : string -> unit
    method receive : string -> unit
    method sleep : float -> unit Lwt.t
    method start : ApiTypes_j.parameter ->
      ApiTypes_j.token ApiTypes_j.result Lwt.t
    method status :
      ApiTypes_j.token -> ApiTypes_j.state ApiTypes_j.result Lwt.t
    method stop : ApiTypes_j.token -> unit ApiTypes_j.result Lwt.t
  end
