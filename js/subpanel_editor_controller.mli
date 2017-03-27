(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val set_manager : string -> unit
val set_content : filename:string -> filecontent:string -> unit
val with_file : (Api_types_j.file Api.result -> unit Api.result Lwt.t) -> unit
