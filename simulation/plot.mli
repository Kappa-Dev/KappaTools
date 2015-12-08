(** Plot point storage and rendering *)

val create : string -> unit
(** [create filename] *)

val value : int -> string
(** [value width] *)

val close : unit -> unit

val plot_now : Environment.t -> float -> Nbr.t array -> unit

val fill : Counter.t -> Environment.t -> Nbr.t array -> unit
