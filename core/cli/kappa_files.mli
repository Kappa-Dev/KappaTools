(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Utilities on files
 * Stores file paths used by the kappa system and access to them *)

val open_out : string -> out_channel

val open_out_fresh : string -> string list -> string -> string -> out_channel
(** [open_out_fresh base concat_list facultative ext] *)

val path : string -> string
val mk_dir_r : string -> unit
val check_not_exists : string -> unit
val setCheckFileExists : batchmode:bool -> string -> unit
val set_dir : string -> unit
val get_dir : unit -> string
val set_marshalized : string -> unit
val with_marshalized : (out_channel -> unit) -> unit
val set_cflow : string -> unit

val with_cflow_file :
  string list -> string -> (Format.formatter -> unit) -> unit

val open_tasks_profiling : unit -> out_channel
val open_branch_and_cut_engine_profiling : unit -> out_channel
val set_flux : string -> int -> unit
val with_flux : string -> (out_channel -> unit) -> unit
val with_snapshot : string -> string -> int -> (out_channel -> unit) -> unit

val with_channel : string -> (out_channel -> unit) -> unit
(** [with_channel path f] *)

val wrap_formatter : (Format.formatter -> unit) -> out_channel -> unit
