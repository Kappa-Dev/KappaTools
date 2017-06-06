(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type contact_map =
  object
    method exportJSON  : Js.js_string Js.t -> unit Js.meth
    method setData : Js.js_string Js.t -> (*int Js.Opt.t ->*) unit Js.meth
    method clearData : unit Js.meth
  end;;

let create_contact_map
    (id : string)
    (is_snapshot : bool) : contact_map Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "ContactMap")
    [| Js.Unsafe.inject (Js.string id) ;
       Js.Unsafe.inject (Js.bool is_snapshot)

    |]
