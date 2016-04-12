module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5

open Js_plot
open Js
open Lwt
open ApiTypes

let plot_svg_style_id = "plot-svg-style"
let plot_div_id = "plot-div"
let plot_export_filename_id = "plot-export-filename"
let plot_export_button_id =  "plot-export-button"
let plot_export_format_id = "export-file-format"

let nav_tab_id = "navtabs"

let fluxmap_div_id = "fluxmap_div"
let fluxmap_svg_id = "fluxmap_svg"
let fluxmap_select_id = "fluxmap_select"
let fluxmap_export_format_id = "fluxmap-export-file-format"
let fluxmap_export_filename_id =  "fluxmap-export-filename"
let fluxmap_export_button_id =  "fluxmap-export-button"

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


let document = Dom_html.window##document

let flux_map_content =
  let flux_label label = if String.length label > 10 then
			   (String.sub label 0 7)^"..."
			 else
			   label
  in
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
                                                                (Html5.pcdata (flux_label flux.flux_name)))
                                              (state_fluxmap state)
                                    )
                                 )
                                 UIState.model_runtime_state in
                       flux_list
                      )
  in
  let flux_label = Tyxml_js.R.Html5.li
		     ~a:[ Html5.a_class ["list-group-item"] ]
		     (let flux_list, flux_handle = ReactiveData.RList.create [] in
		      let _ = React.S.map
                                (fun state ->
                                 ReactiveData.RList.set
                                   flux_handle
				   (match state_fluxmap state with
				      head::[] -> [Html5.h4 [ Html5.pcdata (flux_label head.flux_name)]]
				    | _ -> [flux_select]
				   )
				)
                                UIState.model_runtime_state
		      in
		      flux_list
		     )
  in
		      (*		     [Html5.h4 [Html5.pcdata "flux_name"]] in *)
  let checkbox = Html5.input ~a:[ Html5.a_id "checkbox_self_influence"
                                ; Html5.a_input_type `Checkbox ] () in
  let fluxmap_export_filename = Html5.input ~a:[ Html5.a_id fluxmap_export_filename_id ;
					      Html5.a_input_type `Text;
					      Html5.a_class ["form-control"];
					      Html5.a_placeholder "file name"]
					 () in
  let fluxmap_export_button =  Html5.button ~a:[ Html5.a_id fluxmap_export_button_id
					    ; Html5.Unsafe.string_attrib "role" "button"
					    ; Html5.a_class ["btn";"btn-default";"pull-right"]
					    ]
					    [ Html5.cdata "export" ] in
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
	           $flux_label$
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
	     <div class="col-sm-12">
	        <div class="form-inline">
	           <div class="form-group">
	              <select class="form-control" id="fluxmap-export-file-format">
		         <option value="dat">json</option>
		         <option value="png">png</option>
		         <option value="svg">svg</option>
	              </select>
	           </div>
	           <div class="form-group">
	              <label class="checkbox-inline">
	                 $fluxmap_export_filename$
	              </label>
	           </div>
	           <div class="form-group">
	              <label class="checkbox-inline">
	                 $fluxmap_export_button$
	              </label>
	           </div>
	        </div>
	     </div>
          </div>
     </div> >>
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
		<option value="dat">tsv</option>
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
                                                                       (* let () = single_disabled l in *)
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
                                                                            | Some _ -> ["show"])
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
                                                                            | _::_ -> ["show"])
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
let update_flux_map
      (flux_js : Flux.flux_map Js.t)
      (flux_data : ApiTypes.flux_map) : unit =
  let flux_data : Flux.flux_data Js.t =
    Flux.create_data ~flux_begin_time:flux_data.flux_begin_time
                     ~flux_end_time:flux_data.flux_end_time
                     ~flux_rules:flux_data.flux_rules
                     ~flux_hits:flux_data.flux_hits
                     ~flux_fluxs:flux_data.flux_fluxs
  in
  flux_js##setFlux(flux_data)

let select_fluxmap flux_map =
  let index = Js.Opt.bind (document##getElementById (Js.string fluxmap_select_id))
			  (fun dom -> let fluxmap_select_dom : Dom_html.inputElement Js.t = Js.Unsafe.coerce dom in
				      let fileindex = Js.to_string (fluxmap_select_dom##value) in
				      try Js.some (int_of_string fileindex) with
					_ -> Js.null
			  )
  in
  match (React.S.value UIState.model_runtime_state) with
    None -> ()
  | Some state -> let index = Js.Opt.get index (fun _ -> 0) in
		  if List.length state.ApiTypes.flux_maps > 0 then
		    update_flux_map flux_map (List.nth state.ApiTypes.flux_maps index)
		  else
		    ()


let save_plot_ui export_data
		 title
		 export_button_id
		 export_filename_id
		 export_format_id
		 svg_div_id
		 svg_style_id
		 dat_file_extension
  =
  let export_button : Dom_html.buttonElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string export_button_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_filename : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string export_filename_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in

  let export_format : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string export_format_id))
                   (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_button_toggle () : unit = let filename : string = Js.to_string (export_filename##value) in
					    let is_disabled : bool Js.t = Js.bool (String.length (String.trim filename) == 0) in
					    let () = export_button##disabled <- is_disabled in
					    ()
  in
  let () = export_button_toggle () in
  let () = export_filename##oninput <- Dom_html.handler
					      (fun _ -> let () = export_button_toggle () in
						        Js._true)
  in
  let () = export_button##onclick <- Dom_html.handler
					    (fun _ ->

					     let suffix : string = Js.to_string (export_format##value) in
					     let filename default : string = let root : string = Js.to_string (export_filename##value) in
									     if String.contains root '.' then
									       root
									     else
									       root^"."^default
					     in
					     let () = match suffix with
						 "svg" -> Common.plotSVG svg_div_id title (filename "svg") svg_style_id
					       | "png" -> Common.plotPNG svg_div_id title (filename "png") svg_style_id
					       | "dat" -> export_data (filename dat_file_extension)
					       | f -> Common.error ("Unknown format"^f)
					     in
					     Js._true)
  in
  ()


let onload () =
  let configuration : plot_configuration Js.t = Js_plot.create_configuration ~plot_div_id:plot_div_id
									     ~plot_label_div_id:"plot-label-div"
									     ~plot_style_id:"plot-svg-style"
									     ~plot_show_legend_checkbox_id:"plot-show-legend"
									     ~plot_x_axis_log_checkbox_id:"plot-x-axis-log-checkbox"
									     ~plot_y_axis_log_checkbox_id:"plot-y-axis-log-checkbox"
									     ~plot_controls_div_id:"plot-controls-div"
  in
  let plot : observable_plot Js.t = Js_plot.create_observable_plot configuration in
  let () = save_plot_ui
	     (fun f -> let filename = Js.string f in
		       let () = plot##setPlotName(filename) in
		       plot##handlePlotTSV(())
	     )
	     "kappa plot"
	     plot_export_button_id
	     plot_export_filename_id
	     plot_export_format_id
	     plot_div_id
	     (Some plot_svg_style_id)
	     "tsv"
  in
 let () = Common.jquery_on "#navgraph"
                           "shown.bs.tab"
                            (fun _ ->
                             match (React.S.value UIState.model_runtime_state) with
                               None -> ()
                             | Some state -> update_plot plot state.plot)
  in
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
  let flux = Flux.create_flux_map flux_configuration in
  let () = save_plot_ui
	    (fun f -> let filename = Js.string f in
		      flux##exportJSON(filename)
	     )
	     "kappa plot"
	     fluxmap_export_button_id
	     fluxmap_export_filename_id
	     fluxmap_export_format_id
	     fluxmap_div_id
	     None
	     "json"
 in
 let fluxmap_select_dom : Dom_html.inputElement Js.t =
   Js.Unsafe.coerce
     ((Js.Opt.get (document##getElementById (Js.string fluxmap_select_id))
                  (fun () -> assert false))
      : Dom_html.element Js.t) in
 let () = fluxmap_select_dom##onchange <- Dom_html.handler
                                            (fun _ ->
                                             let () = select_fluxmap flux
                                              in Js._true)
 in
 let fluxmap_div : Dom_html.element Js.t =
   Js.Opt.get (document##getElementById (Js.string fluxmap_div_id))
              (fun () -> assert false) in
 let () = fluxmap_div##innerHTML <- Js.string ("<svg id=\""^fluxmap_svg_id^"\" width=\"300\" height=\"300\"><g/></svg>") in
 let () = Common.jquery_on "#navflux"
                           "shown.bs.tab"
                            (fun _ -> select_fluxmap flux)
 in
 React.S.l1 (fun state -> match state with
			    None -> ()
			  | Some state -> update_plot plot state.plot;
					  select_fluxmap flux
	    )
	    UIState.model_runtime_state
