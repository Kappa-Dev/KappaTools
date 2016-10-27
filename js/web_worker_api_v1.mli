class runtime : ?timeout:float -> unit ->  object
    method post_message : string -> unit
    method sleep : float -> unit Lwt.t
    method receive : string -> unit
    inherit Api_v1.api_runtime
  end
