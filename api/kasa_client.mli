(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type mailbox

val receive : mailbox -> string -> unit

val new_mailbox : unit -> mailbox
val is_computing : mailbox -> bool

class virtual new_client :
  post:(string -> unit) -> mailbox -> Api.manager_static_analysis
