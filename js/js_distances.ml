class type distances_plot =
  object
    method exportJSON  : Js.js_string Js.t -> unit Js.meth
    method setData : Js.js_string Js.t -> unit Js.meth
    method clearData : unit Js.meth
  end;;

let create_distances_plot (id : string) : distances_plot Js.t =
  Js.Unsafe.new_obj (Js.Unsafe.variable "DistancesPlot")
    [| Js.Unsafe.inject (Js.string id) |]
