(** {5 Utilities on files } *)
val open_out : string -> out_channel
val path : string -> string
val mk_dir_r : string -> unit
val setCheckFileExists : batchmode:bool -> string -> unit
val setCheckFileExistsODE : batchmode:bool -> mode:Loggers.encoding -> unit
val set_dir : string -> unit
val get_dir : unit -> string

val set_ode : mode:Loggers.encoding -> string -> unit
val get_ode : mode:Loggers.encoding -> string

val set_distances : string -> unit
val get_distances : unit -> string

val set_marshalized : string -> unit
val with_marshalized : (out_channel -> unit) -> unit

val set_cflow : string -> unit
val get_cflow : string list -> string -> string
val with_cflow_file :
  string list -> string -> (Format.formatter -> unit) -> unit

val open_tasks_profiling : unit -> out_channel
val open_branch_and_cut_engine_profiling: unit -> out_channel

val set_flux : string -> int -> unit
val with_flux : string -> (Format.formatter -> unit) -> unit

val with_unary_distances : (Format.formatter -> unit) -> unit

val with_snapshot :
  string -> int -> string -> (Format.formatter -> unit) -> unit

val set_influence : string -> unit
val set_up_influence : unit -> unit
val with_influence : (Format.formatter -> unit) -> unit

val with_channel : string -> (out_channel -> unit) -> unit
(** [with_channel path f] *)
