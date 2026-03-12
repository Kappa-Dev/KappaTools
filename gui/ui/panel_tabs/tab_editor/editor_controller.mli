(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val set_content : filename:string -> filecontent:string -> unit

val with_file :
  ((string * string * bool) Api.result -> unit Api.lwt_result) -> unit

val working_set_gutter : string
val set_working_set_rules : Codemirror.codemirror Js.t -> unit
