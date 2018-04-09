(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module ApiTypes = Api_types_j

class type plot_observable =
  object
    method time : float Js.prop
    method values : float Js.opt Js.js_array Js.t Js.prop
  end
let constructor_observable : plot_observable Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_observable ~observable : plot_observable Js.t  =
  let configuration : plot_observable Js.t = new%js constructor_observable in
  let () =
    configuration##.time := Option_util.unsome nan observable.(0);
    configuration##.values :=
      Js.array
        (Array.map  Js.Opt.option
           (Array.sub observable
              1 (Array.length observable - 1)));


  in configuration


class type plot_data =
  object
    method legend : Js.js_string Js.t Js.js_array Js.t Js.prop
    method timeSeries : (plot_observable Js.t) Js.js_array Js.t Js.prop
  end
let constructor_data : plot_data Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_data ~(plot : ApiTypes.plot)
  : plot_data Js.t  =
  let configuration : plot_data Js.t = new%js constructor_data in
  let () =
    configuration##.legend := Js.array
        (Array.map
           Js.string
           (Array.sub plot.Data.plot_legend
              1 (Array.length plot.Data.plot_legend - 1)));
    configuration##.timeSeries := Js.array
        (Tools.array_map_of_list (fun o -> create_observable ~observable:o)
           plot.Data.plot_series);
    ()
  in configuration


class type observable_plot =
  object
    method setData : Js.js_string Js.t -> unit Js.meth
    method clearData : unit Js.meth
    method redraw : unit Js.meth
  end

let create_observable_plot main_div_id : observable_plot Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "observable_plot")
    [| Js.Unsafe.inject (Js.string main_div_id) |]
