(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val model_parse : Api_types_j.contact_map option React.signal
val current_file : Api_types_j.file option React.signal
val agent_count : unit -> int option
val set_file : string -> string -> unit
val current_file : Api_types_j.file option React.signal
val set_filecontent : string -> unit
val init : unit -> unit Lwt.t
