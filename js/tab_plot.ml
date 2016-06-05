module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5
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
    | Some { ApiTypes.observables = [] ; _ } -> None
    | _ -> state.ApiTypes.plot
    )

let configuration : Widget_export.configuration =
  { Widget_export.id = export_id
  ; Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
        ~serialize_json:(fun () ->
          (match
              state_plot (React.S.value UIState.model_runtime_state)
           with
              | None -> "null"
              | Some plot -> ApiTypes.string_of_plot plot
          )
        )
      ; { Widget_export.suffix = "tsv"
        ; Widget_export.label = "tsv"
        ; Widget_export.export =
          fun (filename : string) ->
            let data =
              match
                state_plot (React.S.value UIState.model_runtime_state)
              with
                | None -> ""
                | Some p -> Api_data.plot_tsv p
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
           UIState.model_runtime_state

  }

let content =
  let plot_show_legend =
    Html5.input ~a:[ Html5.a_id "plot-show-legend"
                   ; Html5.a_input_type `Checkbox
                   ; Html5.a_class ["checkbox-control"]
                   ; Html5.Unsafe.string_attrib "checked" "true"
                   ] () in
  let plot_x_axis_log_checkbox =
    Html5.input ~a:[ Html5.a_id "plot-x-axis-log-checkbox"
                   ; Html5.a_class ["checkbox-control"]
                   ; Html5.a_input_type `Checkbox
                   ] () in
  let plot_y_axis_log_checkbox =
    Html5.input ~a:[ Html5.a_id "plot-y-axis-log-checkbox"
                   ; Html5.a_class ["checkbox-control"]
                   ; Html5.a_input_type `Checkbox
                   ] () in
  let export_controls =
    Widget_export.content configuration
  in
  <:html5<<div>
      <div class="row">
          <div id="plot-label-div" class="center-block display-header">
          Plot
          </div>
      </div>
      <div class="row">
        <div $list:Html5.a_id display_id$ class="col-sm-12"></div>
      </div>
      <div class="row">

         <div class="col-sm-2">
            $plot_show_legend$ Legend
         </div>

         <div class="col-sm-3">
               Log X $plot_x_axis_log_checkbox$
               Log Y $plot_y_axis_log_checkbox$
         </div>

         <div class="col-sm-4" $list:Html5.a_id div_axis_select_id$>
         </div>

      </div>
      <div class="row"><p></p></div>
      $export_controls$
  </div> >>

let navcontent =
  [ Html5.div
      ~a:[Tyxml_js.R.Html5.a_class
             (React.S.bind
                UIState.model_runtime_state
                (fun state ->
                  React.S.const
                    (match state_plot state with
                      None -> ["hidden"]
                    | Some _ -> ["show"])
                )
             )]
      [ content ]
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
      Js.Opt.get (Display_common.document##getElementById
                    (Js.string display_id))
        (fun () -> assert false) in
    let width = max 400 (div##offsetWidth - 20)  in
    let height = width/2 in
    let dimension =
      Js_plot.create_dimension
        ~height:height
        ~width:width
    in
    let () = plot##setDimensions(dimension) in
    let data : plot_data Js.t = Js_plot.create_data data in
    plot##setPlot(data)

let onload () =
  let () = Widget_export.onload configuration in
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
    "#navgraph"
    "shown.bs.tab"
    (fun _ ->
      match (React.S.value UIState.model_runtime_state) with
        None -> ()
      | Some state -> update_plot plot state.ApiTypes.plot)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
        None -> ()
      | Some state -> update_plot plot state.ApiTypes.plot)
      UIState.model_runtime_state
  in
  ()

let navli = []
