(** Plot point storage and rendering *)

val create : string -> string array -> unit
(** [create filename observable_names] *)

val value : int -> string
(** [value width] *)

val close : unit -> unit

val plot_now : float -> Nbr.t array -> unit

val fill : Counter.t -> Nbr.t array -> unit
