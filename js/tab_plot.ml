module ApiTypes = ApiTypes_j

module Html = Tyxml_js.Html5
module UIState = Ui_state

open Js_plot

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
                | Some p -> Api_data.plot_values p
              in
              Common.saveFile
		~data:data
		~mime:"text/tab-separated-values"
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
  let simulation_output = (Ui_simulation.simulation_output t) in
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
  [%html {|<div>
	   <div class="row">
           <div id="plot-label-div" class="center-block display-header">
           Plot
           </div>
	   </div>
	   <div class="row">
           <div id="|}display_id{|" class="col-sm-12"></div>
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
							 <div class="row"><p></p></div>
							 |}[export_controls]{|
  </div>|}]

let navcontent (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  [ Html.div
      ~a:[Tyxml_js.R.Html.a_class
            (React.S.bind
               simulation_output
               (fun state ->
                  React.S.const
                    (match state_plot state with
                       None -> ["hidden"]
                     | Some _ -> ["show"])
               )
            )]
      [ content t ]
  ]

let state_plot state = match state with
    None -> None
  | Some state -> state.ApiTypes.plot

let update_plot
    (plot : observable_plot Js.t)
    (data : ApiTypes.plot option) : unit =
  match data with
    None -> ()
  | Some data ->
    let div : Dom_html.element Js.t =
      Js.Opt.get (Ui_common.document##getElementById
                    (Js.string display_id))
        (fun () -> assert false) in
    let width = max 400 (div##.offsetWidth - 20)  in
    let height = width/2 in
    let dimension =
      Js_plot.create_dimension
        ~height:height
        ~width:width
    in
    let () = plot##setDimensions(dimension) in
    let data : plot_data Js.t = Js_plot.create_data ~plot:data in
    plot##setPlot(data)

let onload (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let () = Widget_export.onload (configuration t) in
  let configuration : plot_configuration Js.t =
    Js_plot.create_configuration
      ~plot_div_id:display_id
      ~plot_div_select_id:div_axis_select_id
      ~plot_label_div_id:"plot-label-div"
      ~plot_style_id:"plot-svg-style"
      ~plot_show_legend_checkbox_id:"plot-show-legend"
      ~plot_x_axis_log_checkbox_id:"plot-x-axis-log-checkbox"
      ~plot_y_axis_log_checkbox_id:"plot-y-axis-log-checkbox"
  in
  let plot : observable_plot Js.t =
    Js_plot.create_observable_plot configuration in
  (* The elements size themselves using the div's if they are hidden
     it will default to size zero.  so they need to be sized when shown.
  *)
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
