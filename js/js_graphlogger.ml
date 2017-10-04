(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type graph_logger =
  object
    method setData : Js.js_string Js.t -> unit Js.meth
    method clearData : unit Js.meth
  end;;

let create_graph_logger (id : string) : graph_logger Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "GraphLogger")
    [| Js.Unsafe.inject (Js.string id) |]
