module ApiTypes = ApiTypes_j

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

class type plot_dimension = object
  val height : int Js.t Js.prop
  val width : int Js.t Js.prop
end

let constructor_dimension : plot_dimension Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_dimension ~(height : int)
    ~(width : int)
  : plot_dimension Js.t  =
  let configuration : plot_dimension Js.t = new%js constructor_dimension in
  let () = (Js.Unsafe.coerce configuration)##.height := height;
    (Js.Unsafe.coerce configuration)##.width := width;
    ()
  in configuration

class type plot_observable =
  object
    val time : float Js.t Js.prop
    val values : (float Js.t) Js.js_array Js.t Js.prop
  end
let constructor_observable : plot_observable Js.t Js.constr =
  (Js.Unsafe.variable "Object")
let create_observable ~(observable : ApiTypes.observable)
  : plot_observable Js.t  =
  let configuration : plot_observable Js.t = new%js constructor_observable in
  let () = (Js.Unsafe.coerce configuration)
           ##.
             time := observable.ApiTypes.time;
    (Js.Unsafe.coerce configuration)
    ##.
      values := Js.array (Array.of_list observable.ApiTypes.values);
    ()
  in configuration


class type plot_data =
  object
    val legend : Js.js_string Js.js_array Js.t Js.prop
    val timeSeries : (plot_observable Js.t) Js.js_array Js.t Js.prop
  end
let constructor_data : plot_data Js.t Js.constr = (Js.Unsafe.variable "Object")
let create_data ~(plot : ApiTypes.plot)
  : plot_data Js.t  =
  let configuration : plot_observable Js.t = new%js constructor_observable in
  let () =
    (Js.Unsafe.coerce configuration)##.legend := Js.array
	(Array.map
           (fun l -> Js.string l)
           (Array.of_list plot.ApiTypes.legend));
    (Js.Unsafe.coerce configuration)##.timeSeries := Js.array
	(Array.map (fun o -> create_observable ~observable:o)
           (Array.of_list plot.ApiTypes.time_series));
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
    method getDimensions : (unit -> plot_dimension Js.t) Js.meth
    method setDimensions : plot_dimension Js.t -> unit Js.meth
  end;;

let create_observable_plot
    (configuration : plot_configuration Js.t)
  : observable_plot Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "observable_plot")
    [| Js.Unsafe.inject configuration |]
