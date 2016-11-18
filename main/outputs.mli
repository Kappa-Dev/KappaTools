(** Deal with simulation output *)

val initialize :
  string option -> (string * string * string array) option ->
  Environment.t -> unit
val create_distances : string array -> bool -> unit

val go : Signature.s -> Data.t -> unit
val close : unit -> unit
