val on_message : Api_v1.runtime -> (string -> unit) -> string -> unit

class virtual runtime : ?timeout:float -> unit -> object
    method virtual post_message : string -> unit
    method virtual sleep : float -> unit Lwt.t
    method receive : string -> unit
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

  end

val message_delimter : char
