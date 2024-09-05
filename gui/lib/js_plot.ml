(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(*module ApiTypes = Api_types_j*)

class type observable_plot = object
  method setData : Js.js_string Js.t -> unit Js.meth
  method clearData : unit Js.meth
  method redraw : unit Js.meth
end

let create_observable_plot main_div_id : observable_plot Js.t =
  Js.Unsafe.new_obj
    (Js.Unsafe.pure_js_expr "ObservablePlot")
    [| Js.Unsafe.inject (Js.string main_div_id) |]
