(** {5 Utilities on files } *)
val open_out : string -> out_channel
val mk_dir_r : string -> unit
val setCheckFileExists : unit -> unit

val set_dir : string -> unit
val set_data : string -> unit
val get_data : unit -> string

val set_marshalized : string -> unit
val with_marshalized : (out_channel -> unit) -> unit

val set_cflow : string -> unit
val fresh_cflow_filename : string list -> string -> out_channel

val open_profiling : unit -> out_channel

val set_flux : string -> int -> unit
val with_flux : string -> (Format.formatter -> unit) -> unit

val open_snapshot : string -> int -> string -> out_channel

val with_dump : (out_channel -> unit) -> unit

val set_influence : string -> unit
val set_up_influence : unit -> unit
val with_influence : (Format.formatter -> unit) -> unit

val add_out_desc : out_channel -> unit
val close_out_desc : out_channel -> unit
val close_all_out_desc : unit -> unit
