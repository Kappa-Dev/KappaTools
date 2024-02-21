(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type state

val print_compression_modes : Format.formatter -> Api.compression_modes -> unit
val init_state : unit -> state * (unit Story_json.message -> unit)
val receive : (unit Story_json.message -> 'a) -> string -> 'a

class virtual new_client : post:(string -> unit) -> state -> Api.manager_stories
