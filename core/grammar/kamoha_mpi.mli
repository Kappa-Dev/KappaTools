val on_message :
  (unit -> unit Lwt.t) -> (string -> 'a Lwt.t) -> string -> 'a Lwt.t
