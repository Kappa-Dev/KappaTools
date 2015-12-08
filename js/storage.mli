val opened_filename : string React.S.t
val set_opened_filename : ?step:React.step -> string -> unit

val model_max_events : int option React.S.t
val set_model_max_events : ?step:React.step -> int option -> unit
val model_max_time : float option React.S.t
val set_model_max_time : ?step:React.step -> float option -> unit
val model_nb_plot : int React.S.t
val set_model_nb_plot : ?step:React.step -> int -> unit
val model_text : string React.S.t
val set_model_text : ?step:React.step -> string -> unit

val model_counter : Counter.t React.S.t
val model_env : Environment.t React.S.t
