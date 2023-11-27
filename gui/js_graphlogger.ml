(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class type graph_logger = object
  method setData : Js.js_string Js.t -> unit Js.meth
  method clearData : unit Js.meth
end

let create_graph_logger (id : string) (on_click : Js.js_string Js.t -> unit) :
    graph_logger Js.t =
  Js.Unsafe.new_obj
    (Js.Unsafe.pure_js_expr "GraphLogger")
    [| Js.Unsafe.inject (Js.string id); Js.Unsafe.inject on_click |]
