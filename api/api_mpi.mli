val on_message : Api_v1.api_runtime -> (string -> unit) -> string -> unit

class virtual runtime : ?timeout:float -> unit -> object
    method virtual post_message : string -> unit
    method virtual sleep : float -> unit Lwt.t
    method receive : string -> unit
    inherit Api_v1.api_runtime
  end

val message_delimter : char
