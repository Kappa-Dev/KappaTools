(** Deal with simulation output *)

val create_plot : string -> string -> string array -> unit

val go : Signature.s -> Data.t -> unit
val close : unit -> unit
