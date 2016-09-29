module ApiTypes = Api_types_v1_j

module Html = Tyxml_js.Html5
module UIState = Ui_state

let div_axis_select_id  = "plot-axis-select"
let display_id = "plot-display"
let export_id = "plot-export"

let state_plot state =
  match state with
  | None -> None
  | Some state ->
    (match state.ApiTypes.plot with
     (* ignore empty plots for now *)
     | Some { ApiTypes.time_series = [] ; _ } -> None
     | _ -> state.ApiTypes.plot
    )

let configuration (t : Ui_simulation.t) : Widget_export.configuration =
  let simulation_output = (Ui_simulation.simulation_output t) in
  { Widget_export.id = export_id
  ; Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
          ~serialize_json:(fun () ->
              (match
		 state_plot (React.S.value simulation_output)
               with
               | None -> "null"
               | Some plot -> ApiTypes.string_of_plot plot
              )
            )
      ; { Widget_export.suffix = "csv"
        ; Widget_export.label = "csv"
        ; Widget_export.export =
            fun (filename : string) ->
              let data =
		match
                  state_plot (React.S.value simulation_output)
		with
                | None -> ""
                | Some p -> Api_data_v1.plot_values p
              in
              Common.saveFile
		~data:data
		~mime:"text/csv"
		~filename:filename
        }
      ];
    show = React.S.map
        (fun state ->
           match state_plot state with
           | None -> false
           | Some _ -> true
        )
        simulation_output

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
     (fun s -> match state_plot s with None -> [] | Some p -> [p])
     (content t) ]

let state_plot state = match state with
    None -> None
  | Some state -> state.ApiTypes.plot

let dimension_ref : Js_plot.plot_dimension Js.t option ref = ref None
let calculate_dimension () =
    let min_width = 400 in
    let min_height = 100 in
    let offset_width = 100 in
    let offset_height = 250 in
    let width =
      max
        min_width
        ((Js.Optdef.get (Dom_html.window##.innerWidth) (fun () -> assert false)) - offset_width)
    in
    let height =
      max
        min_height
        ((Js.Optdef.get (Dom_html.window##.innerHeight)(fun () -> assert false)) - offset_height)
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
    (plot : Js_plot.observable_plot Js.t)
    (data : ApiTypes.plot option) : unit =
  match data with
  | None -> ()
  | Some data ->
    let () = plot##setDimensions(get_dimension ()) in
    let data : Js_plot.plot_data Js.t = Js_plot.create_data ~plot:data in
    plot##setPlot(data)
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
	 | Some state -> update_plot plot state.ApiTypes.plot)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
           None -> ()
	 | Some state -> update_plot plot state.ApiTypes.plot)
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
      | Some state -> update_plot plot state.ApiTypes.plot)
  in
  ()
