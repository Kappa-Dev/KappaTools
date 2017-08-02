(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module ApiTypes = Api_types_j

class type plot_configuration =
  object
    val plotDivId : Js.js_string Js.t Js.prop
    val plotDivAxisSelectId : Js.js_string Js.t Js.prop
    val plotLabelDivId:Js.js_string Js.t Js.prop
    val plotStyleId : Js.js_string Js.t Js.prop
    val plotShowLegendCheckboxId : Js.js_string Js.t Js.prop
    val plotXAxisLogCheckboxId : Js.js_string Js.t Js.prop
    val plotYAxisLogCheckboxId : Js.js_string Js.t Js.prop
  end
let constructor_configuration : plot_configuration Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_configuration
    ~(plot_div_id : string)
    ~(plot_div_select_id : string)
    ~(plot_label_div_id:string)
    ~(plot_style_id : string)
    ~(plot_show_legend_checkbox_id : string)
    ~(plot_x_axis_log_checkbox_id : string)
    ~(plot_y_axis_log_checkbox_id : string)
  : plot_configuration Js.t  =
  let configuration : plot_configuration Js.t =
    new%js constructor_configuration in
  let () =
    (Js.Unsafe.coerce configuration)
    ##.
      plotDivId := Js.string plot_div_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotDivAxisSelectId := Js.string plot_div_select_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotLabelDivId := Js.string plot_label_div_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotStyleId := Js.string plot_style_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotShowLegendCheckboxId := Js.string plot_show_legend_checkbox_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotXAxisLogCheckboxId := Js.string plot_x_axis_log_checkbox_id;
    (Js.Unsafe.coerce configuration)
    ##.
      plotYAxisLogCheckboxId := Js.string plot_y_axis_log_checkbox_id;
    ()
  in configuration

class type plot_observable =
  object
    method time : float Js.prop
    method values : float Js.opt Js.js_array Js.t Js.prop
  end
let constructor_observable : plot_observable Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_observable ~(observable : ApiTypes.observable)
  : plot_observable Js.t  =
  let configuration : plot_observable Js.t = new%js constructor_observable in
  let () =
    match observable with
    | Some time :: l ->
      configuration##.time := time;
      configuration##.values :=
        Js.array (Tools.array_map_of_list  Js.Opt.option l)
    | _ -> failwith "problematic output line"
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
        (Tools.array_map_of_list
           Js.string
           (List.tl plot.ApiTypes.plot_legend));
    configuration##.timeSeries := Js.array
        (Tools.array_map_of_list (fun o -> create_observable ~observable:o)
           plot.ApiTypes.plot_time_series);
    ()
  in configuration


class type observable_plot =
  object
    method setPlot : plot_data Js.t -> unit Js.meth
    method getPlot : (unit -> plot_data Js.t) Js.meth
    method handlePlotPNG : unit -> unit Js.meth
    method handlePlotSVG : unit -> unit Js.meth
    method handlePlotTSV : unit -> unit Js.meth
    method getPlotName : (unit -> Js.js_string Js.t) Js.meth
    method setPlotName : Js.js_string Js.t -> unit Js.meth
  end;;

let create_observable_plot
    (configuration : plot_configuration Js.t)
  : observable_plot Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "observable_plot")
    [| Js.Unsafe.inject configuration |]
