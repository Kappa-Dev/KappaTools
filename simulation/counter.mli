(** Simulation progress keeper *)

type t

val create : ?init_t:float -> ?init_e:int -> ?max_t:float -> ?max_e:int ->
	     nb_points:int -> t

val reinitialize : t -> unit

val tick : Format.formatter -> t -> unit
val fill : outputs:(Data.t -> unit) -> t -> Nbr.t array -> unit
val complete_progress_bar : Format.formatter -> t -> unit

val one_constructive_event : t -> float -> bool
val one_clashing_instance_event : t -> float -> bool
val one_no_more_unary_event : t -> float -> bool
val one_no_more_binary_event : t -> float -> bool
val one_time_correction_event : t -> Nbr.t -> bool

val max_time : t -> float option
val max_events : t -> int option
val event_percentage : t -> int option
val event : t -> int
val time_percentage : t -> int option
val time : t -> float
val tracked_events : t -> int

val plot_points : t -> int
val current_time : t -> float
val current_event : t -> int
val nb_null_event : t -> int
val consecutive_null_event : t -> int

val print_efficiency : Format.formatter -> t -> unit

val current_story : t -> unit Mods.simulation_info
val next_story : t -> unit Mods.simulation_info
