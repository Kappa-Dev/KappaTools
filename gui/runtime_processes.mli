(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type process_configuration = object
  method command : Js.js_string Js.t Js.prop
  method args : Js.js_string Js.t Js.js_array Js.t Js.prop
  method onStdout : (Js.js_string Js.t -> unit) Js.prop
  method onStderr : (Js.js_string Js.t -> unit) Js.prop
  method onClose : (unit -> unit) Js.prop
  method onError : (unit -> unit) Js.prop
end

class type process = object
  method write : Js.js_string Js.t -> unit Js.meth
  method kill : unit Js.meth
end

class manager :
  ?message_delimiter:char ->
  string ->
  string list ->
  Api.concrete_manager
