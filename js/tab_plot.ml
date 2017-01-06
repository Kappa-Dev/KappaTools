(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let div_axis_select_id  = "plot-axis-select"
let display_id = "plot-display"
let export_id = "plot-export"

let has_plot
    (state : Api_types_j.simulation_info option) :
  bool =
  match state with
  | None -> false
  | Some state ->
    state.Api_types_j.simulation_info_output.Api_types_j.simulation_output_plot

let serialize_json : (unit -> string) ref = ref (fun _ -> "null")
let serialize_csv : (unit -> string) ref = ref (fun _ -> "")

let configuration (t : Ui_simulation.t) : Widget_export.configuration =
  let simulation_output = (Ui_simulation.simulation_output t) in
  { Widget_export.id = export_id
  ; Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
          ~serialize_json:!serialize_json
      ; { Widget_export.suffix = "csv"
        ; Widget_export.label = "csv"
        ; Widget_export.export =
            fun (filename : string) ->
              let data = !serialize_csv ()
              in
              Common.saveFile
		~data:data
		~mime:"text/csv"
		~filename:filename
        }
      ];
    show = React.S.map has_plot simulation_output

  }

let content (t : Ui_simulation.t) =
  let plot_show_legend =
    Html.input ~a:[ Html.a_id "plot-show-legend"
                  ; Html.a_input_type `Checkbox
                  ; Html.a_class ["checkbox-control"]
                  ; Html.Unsafe.string_attrib "checked" "true"
                  ] () in
  let plot_x_axis_log_checkbox =
    Html.input ~a:[ Html.a_id "plot-x-axis-log-checkbox"
                  ; Html.a_class ["checkbox-control"]
                  ; Html.a_input_type `Checkbox
                  ] () in
  let plot_y_axis_log_checkbox =
    Html.input ~a:[ Html.a_id "plot-y-axis-log-checkbox"
                  ; Html.a_class ["checkbox-control"]
                  ; Html.a_input_type `Checkbox
                  ] () in
  let export_controls =
    Widget_export.content (configuration t)
  in
  [%html {|
  <div class="navcontent-view">
      <div class="row">
         <div id="plot-label-div" class="center-block display-header">
            Plot
         </div>
      </div>
      <div class="row">
         <div id="|}display_id{|" class="col-sm-12"> |}[ Html.entity "nbsp"]{| </div>
      </div>
      <div class="row">
	 <div class="col-sm-2">
	    |}[plot_show_legend]{| Legend
	 </div>
         <div class="col-sm-3">
 	     Log X |}[plot_x_axis_log_checkbox]{|
             Log Y |}[plot_y_axis_log_checkbox]{|
         </div>

         <div class="col-sm-4" id="|}div_axis_select_id{|">
	 </div>
      </div>
  </div>
  <div class="navcontent-controls"> |}[export_controls]{| </div> |}]

let navcontent (t : Ui_simulation.t) : [> Html_types.div ] Html.elt list =
  [Ui_common.toggle_element
     t
     (fun s -> has_plot s )
     (content t) ]

let dimension_ref : Js_plot.plot_dimension Js.t option ref = ref None
let calculate_dimension () =
    let min_width = 400 in
    let min_height = 100 in
    let offset_width = 100 in
    let offset_height = 250 in
    let width =
      max
        min_width
        ((Js.Optdef.get
            (Dom_html.window##.innerWidth)
            (fun () -> assert false)) - offset_width)
    in
    let height =
      max
        min_height
        ((Js.Optdef.get
            (Dom_html.window##.innerHeight)
            (fun () -> assert false)) - offset_height)
    in
    let dimension =
      Js_plot.create_dimension
        ~height:height
        ~width:width
    in
    let () = dimension_ref := Some dimension
    in dimension

let get_dimension () =
  match !dimension_ref with
  | None -> calculate_dimension ()
  | Some dimension -> dimension

let update_plot
  (t : Ui_simulation.t)
  (plot : Js_plot.observable_plot Js.t) : unit =
  Ui_simulation.manager_operation
    t
    (fun
     manager
     project_id
     simulation_id ->
      (manager#simulation_detail_plot
         project_id
         simulation_id
      ) >>=
      (Api_common.result_map
         ~ok:(fun _ (data : Api_types_t.plot)  ->
             let () = serialize_json  := (fun _ -> Api_types_j.string_of_plot data) in
             let () = plot##setDimensions(get_dimension ()) in
             let () = serialize_csv := fun _ -> Api_data.plot_values data in
             let data : Js_plot.plot_data Js.t = Js_plot.create_data ~plot:data in
             let () = plot##setPlot(data) in
             Lwt.return_unit
          )
        ~error:(fun _ errors  ->
            let () = Ui_state.set_model_error __LOC__ errors in
            Lwt.return_unit)
      )
    )

let plot_ref = ref None
let onload (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let () = Widget_export.onload (configuration t) in
  let configuration : Js_plot.plot_configuration Js.t =
    Js_plot.create_configuration
      ~plot_div_id:display_id
      ~plot_div_select_id:div_axis_select_id
      ~plot_label_div_id:"plot-label-div"
      ~plot_style_id:"plot-svg-style"
      ~plot_show_legend_checkbox_id:"plot-show-legend"
      ~plot_x_axis_log_checkbox_id:"plot-x-axis-log-checkbox"
      ~plot_y_axis_log_checkbox_id:"plot-y-axis-log-checkbox"
  in
  let plot : Js_plot.observable_plot Js.t =
    Js_plot.create_observable_plot configuration in
  (* The elements size themselves using the div's if they are hidden
     it will default to size zero.  so they need to be sized when shown.
  *)
  let () = plot_ref := Some plot in
  let () = Common.jquery_on
      "#navplot"
      "shown.bs.tab"
      (fun _ ->
	 match (React.S.value simulation_output) with
         | None -> ()
	 | Some _ -> update_plot t plot)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
           None -> ()
	 | Some _ -> update_plot t plot)
      simulation_output
  in
  ()

let navli (_ : Ui_simulation.t) = []

let onresize (t : Ui_simulation.t) =
  (* recalcuate size *)
  let simulation_output = (Ui_simulation.simulation_output t) in
  let _ = calculate_dimension () in
  let () =
    match !plot_ref with
    | None -> ()
    | Some plot ->
      (match React.S.value simulation_output with
      | None -> ()
      | Some _ -> update_plot t plot)
  in
  ()
