module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5

open Js_plot
open Js
open Lwt
open ApiTypes

let plot_div_id = "plot-div"
let plot_export_filename_id = "plot-export-filename"
let plot_export_button_id =  "plot-export-button"
let plot_export_format_id = "export-file-format"

let nav_tab_id = "navtabs"

let fluxmap_div_id = "fluxmap_div"
let fluxmap_svg_id = "fluxmap_svg"
let fluxmap_select_id = "fluxmap_select"



let rules_checkboxes_id = "rules-checkboxes"
let checkbox_self_influence_id = "checkbox_self_influence"
let state_fluxmap state = match state with
    None -> []
  | Some state -> (match state.ApiTypes.flux_maps with
                     [] -> []
                   | l -> l)
let plot_div_id = "plot-div"
let state_plot state = match state with
    None -> None
  | Some state -> state.ApiTypes.plot
let plot_render _ : unit = ()

let flux_map_render flux_map : unit =
  let flux_configuration : Flux.flux_configuration Js.t =
    Flux.create_configuration ~begin_time_id:("begin_time")
                              ~end_time_id:("end_time")
                              ~select_correction_id:("select_correction")
                              ~checkbox_self_influence_id:("checkbox_self_influence")
                              ~nb_events_id:("nb_events")
                              ~svg_id:fluxmap_svg_id
                              ~rules_checkboxes_id:rules_checkboxes_id
                              ~height:450
                              ~width:360

  in
  let flux_data : Flux.flux_data Js.t =
    Flux.create_data ~flux_begin_time:flux_map.flux_begin_time
                     ~flux_end_time:flux_map.flux_end_time
                     ~flux_rules:flux_map.flux_rules
                     ~flux_hits:flux_map.flux_hits
                     ~flux_fluxs:flux_map.flux_fluxs
  in
  ignore (Flux.create_flux_map flux_configuration
                               flux_data)
let document = Dom_html.window##document

let flux_map_content =
  let checkbox = Html5.input ~a:[ Html5.a_id "checkbox_self_influence"
                                ; Html5.a_input_type `Checkbox ] () in
  let flux_select = Tyxml_js.R.Html5.select
                      ~a:[ Html5.a_class ["form-control"]
                         ; Html5.a_id fluxmap_select_id ]
                      (let flux_list, flux_handle = ReactiveData.RList.create [] in
                       let _ = React.S.map
                                 (fun state ->
                                  ReactiveData.RList.set
                                    flux_handle
                                    (List.mapi (fun i flux -> Html5.option
                                                                ~a:[ Html5.a_value (string_of_int i) ]
                                                                (Html5.pcdata flux.flux_name))
                                              (state_fluxmap state)
                                    )
                                 )
                                 UIState.model_runtime_state in
                       flux_list
                      )
                       in
  <:html5<<div>
          <div class="row">
               <div class="center-block display-header">
                Dynamic influence map between t = <span id="begin_time"/>s
                and t = <span id="end_time"/>s (<span id="nb_events"/>events)
               </div>
             </div>

             <div class="row">
                <div id="fluxmap_control" class="col-sm-4">
                <ul class="list-group">
                   <li class="list-group-item">
                      <h4 class="list-group-item-heading">Rules</h4>
                      <p id="rules-checkboxes"></p>
                   </li>
                   <li class="list-group-item">
                    <div class="input-group">
                      <label class="checkbox-inline">
                         $checkbox$
                         Self influence<br/>
                      </label>
                    </div>
                   </li>
                   <li class="list-group-item">
                    <div class="input-group">
                      <label>Correction
                      <select class="form-control"
                              id="select_correction">
                           <option value="none">None</option>
                           <option value="hits">Occurences</option>
                           <option value="time">Time</option>
                      </select>
                      </label>
                    </div>
                   </li>
                </ul>
             </div>
          <div id="fluxmap_div" class="col-sm-8"></div>
          </div>
          <div class="row">
             <div class="col-sm-6">
             $flux_select$
           </div>
             </div>
          </div>
          >>
let plot_content =
  let plot_show_legend = Html5.input ~a:[ Html5.a_id "plot-show-legend"
					; Html5.a_input_type `Checkbox
					; Html5.Unsafe.string_attrib "checked" "true"
					] () in
  let plot_x_axis_log_checkbox = Html5.input ~a:[ Html5.a_id "plot-x-axis-log-checkbox"
						; Html5.a_input_type `Checkbox
						] () in
  let plot_y_axis_log_checkbox = Html5.input ~a:[ Html5.a_id "plot-y-axis-log-checkbox"
						; Html5.a_input_type `Checkbox
						] () in
  let plot_export_filename = Html5.input ~a:[ Html5.a_id plot_export_filename_id ;
					      Html5.a_input_type `Text;
					      Html5.a_class ["form-control"];
					      Html5.a_placeholder "file name"]
					 () in
  let plot_export_button =  Html5.button ~a:[ Html5.a_id plot_export_button_id
					    ; Html5.Unsafe.string_attrib "role" "button"
					    ; Html5.a_class ["btn";"btn-default";"pull-right"]
					    ]
					 [ Html5.cdata "export" ] in
  <:html5<<div>
      <div class="row">
	  <div id="plot-label-div" class="center-block display-header">
	  Plot
	  </div>
      </div>
      <div class="row">
	<div id="plot-control" class="col-sm-4">
	  <ul class="list-group">
	    <li class="list-group-item">
	      <h4 class="list-group-litem-heading">Observables</h4>
	      <div id="plot-controls-div"/>
	    </li>
	    <li class="list-group-item">
	      <h4 class="list-group-item-heading">Settings</h4>
	        <div class="input-group"><label class="checkbox-inline">$plot_show_legend$ Legend</label></div>
		<div class="input-group"><label class="checkbox-inline">$plot_x_axis_log_checkbox$ X Axis Log</label></div>
		<div class="input-group"><label class="checkbox-inline">$plot_y_axis_log_checkbox$ Y Axis Log</label></div>
	    </li>
	  </ul>
	</div>
	<div id="plot-div" class="col-sm-8"></div>
      </div>
      <div class="row">
	  <div class="col-sm-12">
	  <div class="form-inline">
	  <div class="form-group">
	  <select class="form-control" id="export-file-format">
		<option value="tsv">tsv</option>
		<option value="png">png</option>
		<option value="svg">svg</option>
	  </select>
	  </div>
	  <div class="form-group">
	    <label class="checkbox-inline">
	      $plot_export_filename$
	    </label>
	  </div>
	  <div class="form-group">
	    <label class="checkbox-inline">
	      $plot_export_button$
	    </label>
	  </div>
	  </div>
	  </div>
      </div>

          </div> >>

let single_disabled fluxes =
  let fluxmap_select_dom : Dom_html.selectElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string fluxmap_select_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  fluxmap_select_dom##disabled <- Js.bool (1 == (List.length fluxes))


let xml =
  let navli label active decorations =
    let default_attributes = [ Html5.a_id ("nav"^label)
                             ; Html5.Unsafe.string_attrib "role" "presentation" ]
    in
    let attributes = if active then
                       (Html5.a_class ["active"])::default_attributes
                     else default_attributes
    in
    Html5.li ~a:attributes
             [ Html5.a ~a:[ Html5.Unsafe.string_attrib "data-toggle" "tab"
                          ; Html5.Unsafe.string_attrib "role" "tab"
                          ; Html5.Unsafe.string_attrib "aria-controls" label
                          ; Html5.a_href ("#"^label) ]
                       (List.append [ Html5.cdata label ]  decorations)
             ] in
  let navcontent label active content = Html5.div
                                          ~a:[ Html5.a_id label
                                             ; if active then
                                                 Html5.a_class ["tab-pane";"active"]
                                               else
                                                 Html5.a_class ["tab-pane"]
                                             ; Html5.Unsafe.string_attrib "role" "tabpanel" ] content in
  let navtabs = Html5.ul
                  ~a:[ Html5.a_id nav_tab_id
                     ; Html5.a_class ["nav";"nav-tabs"]
                     ; Html5.Unsafe.string_attrib "role" "tablist" ]
                  [ navli "graph" true []
                  ; navli "flux" false [ Tyxml_js.R.Html5.span
                                          (let badge_list, badge_handle = ReactiveData.RList.create [] in
                                           let _ = React.S.map
                                                      (fun state -> match state_fluxmap state with
                                                                      [] -> ReactiveData.RList.set
                                                                              badge_handle []
                                                                    | l ->
                                                                       ReactiveData.RList.set
                                                                         badge_handle
                                                                         [ Html5.pcdata " ";
                                                                           Html5.span ~a:[ Html5.a_class ["badge"]]
                                                                                      [ Html5.pcdata (string_of_int (List.length l)) ]
                                                                         ]

                                                      )
                                                      UIState.model_runtime_state in
                                           badge_list
                                          );
                                        Tyxml_js.R.Html5.ul ~a:[ Html5.a_class ["dropdown-menu"] ]
                                           (let flux_list, flux_handle = ReactiveData.RList.create [] in
                                            let _ = React.S.map
                                                      (fun state -> match state_fluxmap state with
                                                                      [] -> ReactiveData.RList.set
                                                                              flux_handle []
                                                                    | l ->
                                                                       let () = single_disabled l in
                                                                       ReactiveData.RList.set
                                                                             flux_handle
                                                                             [ Html5.li
                                                                                 ~a:[ Html5.Unsafe.string_attrib "role" "presentation" ]
                                                                                 [Html5.pcdata "flux" ]
                                                                             ]

                                                      )
                                                      UIState.model_runtime_state in
                                            flux_list
                                           )
                                       ]
                  ; navli "log" false [] ]
  in
  let navcontent = Html5.div
                     ~a:[ Html5.a_class ["panel-content";"tab-content"]]
                     [ navcontent "graph"
				  true
                                  [ Html5.div
				      ~a:[Tyxml_js.R.Html5.a_class
                                            (React.S.bind
                                               UIState.model_runtime_state
                                               (fun state -> React.S.const (match state_plot state with
                                                                              None -> ["hidden"]
                                                                            | Some p ->
                                                                               let () = plot_render p in
                                                                                   ["show"]

                                                                           )
                                               )
                                            )]
				      [plot_content]
				  ]
                     ; navcontent "flux"
                                  false
                                  [ Html5.div
                                      ~a:[Tyxml_js.R.Html5.a_class
                                            (React.S.bind
                                               UIState.model_runtime_state
                                               (fun state -> React.S.const (match state_fluxmap state with
                                                                              [] -> ["hidden"]
                                                                            | h::_ ->
                                                                               let () = flux_map_render h in
                                                                                   ["show"]

                                                                           )
                                               )
                                            )]
                                      [flux_map_content]
                                  ]
                     ; navcontent "log" false
                                  [ Html5.div
                                      ~a:[Html5.a_class ["panel-pre" ]]
                                      [ Tyxml_js.R.Html5.pcdata
                                          (React.S.bind
                                             UIState.model_runtime_state
                                             (fun state -> React.S.const (match (state : ApiTypes.state option) with
                                                                            Some state -> String.concat "" state.log_messages
                                                                          | _ -> ""
                                                                         ))) ]
                                  ]
                     ] in
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class
          (React.S.bind
             UIState.model_runtime_state
             (fun state -> React.S.const (match state with
                                            Some _ -> ["col-md-6";"show"]
                                          | None  -> ["col-md-6";"hidden"]
                                         ))
          )]
    [navtabs;navcontent]

let update_plot
      (plot : observable_plot Js.t)
      (data : ApiTypes.plot option) : unit =
  match data with
    None -> ()
  | Some plot_data ->
     let plot_div : Dom_html.element Js.t =
       Js.Opt.get (document##getElementById (Js.string plot_div_id))
		                    (fun () -> assert false) in
     let width = max 400 (plot_div##offsetWidth - 20)  in
     let height = width/2 in
     let dimension = Js_plot.create_dimension ~height:height ~width:width in
     let () = plot##setDimensions(dimension) in
     let plot_data : plot_data Js.t = Js_plot.create_data plot_data in
     plot##setPlot(plot_data)

let select_fluxmap fileindex =
  match (React.S.value UIState.model_runtime_state) with
    None -> ()
  | Some state -> flux_map_render (List.nth state.ApiTypes.flux_maps (int_of_string fileindex))

let onload () =
  let plot_export_button : Dom_html.buttonElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string plot_export_button_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let plot_export_filename : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string plot_export_filename_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let plot_export_filename : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string plot_export_filename_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in

  let plot_export_format : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string plot_export_format_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let plot_export_button_toggle () : unit = let filename : string = Js.to_string (plot_export_filename##value) in
					    let () = Common.debug filename in
					    let is_disabled : bool Js.t = Js.bool (String.length (String.trim filename) == 0) in
					    let () = Common.debug is_disabled in
					    let () = plot_export_button##disabled <- is_disabled in
					    ()
  in
  let () = plot_export_button_toggle () in
  let () = plot_export_filename##oninput <- Dom_html.handler
					      (fun _ -> let () = plot_export_button_toggle () in
						        Js._true)
  in
  let configuration : plot_configuration Js.t = Js_plot.create_configuration ~plot_div_id:plot_div_id
									     ~plot_label_div_id:"plot-label-div"
									     ~plot_style_id:"plot-svg-style"
									     ~plot_show_legend_checkbox_id:"plot-show-legend"
									     ~plot_x_axis_log_checkbox_id:"plot-x-axis-log-checkbox"
									     ~plot_y_axis_log_checkbox_id:"plot-y-axis-log-checkbox"
									     ~plot_controls_div_id:"plot-controls-div"
  in
  let plot : observable_plot Js.t = Js_plot.create_observable_plot configuration in
  let () = Common.debug plot in
  let () = plot_export_button##onclick <- Dom_html.handler
					    (fun _ ->
					     let () = Common.debug "click" in
					     let () = plot##setPlotName(plot_export_filename##value) in
					     let () = match Js.to_string (plot_export_format##value) with
						 "svg" -> plot##handlePlotSVG(())
					       | "png" -> plot##handlePlotPNG(())
					       | "tsv" -> plot##handlePlotTSV(())
					       | f -> Common.error ("Unknown format"^f)
					     in
					     Js._true)
  in

  let fluxmap_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string fluxmap_select_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let () = fluxmap_select_dom##onchange <- Dom_html.handler
                                             (fun _ ->
                                              let () = select_fluxmap  (Js.to_string (fluxmap_select_dom##value))
                                              in Js._true)
  in
  let fluxmap_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string fluxmap_div_id))
               (fun () -> assert false) in
  let () = fluxmap_div##innerHTML <- Js.string ("<svg id=\""^fluxmap_svg_id^"\" width=\"300\" height=\"300\"><g/></svg>") in
  let () = Common.jquery_on "#navgraph"
                            "shown.bs.tab"
                            (fun _ ->
                             let () = Common.debug("show log plot") in
                             match (React.S.value UIState.model_runtime_state) with
                               None -> ()
                             | Some state -> update_plot plot state.plot)
  in
  let plot_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string plot_div_id))
               (fun () -> assert false) in
  React.S.l1 (fun state -> match state with
                             None -> ()
                           | Some state -> update_plot plot state.plot)
             UIState.model_runtime_state
