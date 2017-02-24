(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val create_file : ?content:string -> string -> unit
val set_file : string -> unit
val close_file : unit -> unit
val set_file_compile : string -> bool -> unit
val order_files : string list -> unit
