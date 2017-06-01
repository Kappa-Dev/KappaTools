(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type mailbox

val reply_of_string : string -> (Yojson.Basic.json,string) Result.result
val receive : mailbox -> string -> unit

val new_mailbox : unit -> mailbox

class new_client :
  post:(string -> unit) -> mailbox -> Api.manager_static_analysis
