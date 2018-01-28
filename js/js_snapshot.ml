(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type snapshot =
  object
    method exportJSON  : Js.js_string Js.t -> unit Js.meth
    method setData : contact_map : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    method redraw : unit Js.meth
    method clearData : unit Js.meth
  end

let create_snapshot (id : string) (coloring : unit Js.js_array Js.t) : snapshot Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "Snapshot")
    [| Js.Unsafe.inject (Js.string id); Js.Unsafe.inject coloring |]
