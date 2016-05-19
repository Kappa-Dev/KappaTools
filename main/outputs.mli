(** Deal with simulation output *)

val create_plot : string * string * string array -> bool -> unit

val go : Signature.s -> Data.t -> unit
val close : unit -> unit
