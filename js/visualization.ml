module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5


open Js
open Lwt
open ApiTypes

let plot_div_id = "plot-div"
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
  let () = Common.debug flux_configuration in
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
               <div class="center-block flux-header">
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
                     [ navcontent "graph" true
                                  [ Html5.p [] ; Html5.div ~a:[ Html5.a_id  plot_div_id ] [ ] ]
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

let update_plot (plot : ApiTypes.plot option) : unit =
  match plot with
    None -> ()
  | Some plot ->
     let plot_div : Dom_html.element Js.t =
       Js.Opt.get (document##getElementById (Js.string plot_div_id))
                  (fun () -> assert false) in
     let width = max 400 (plot_div##offsetWidth - 20)  in
     let svg_store : Pp_svg.store =
       Api_data.plot_pg_store ~plot:plot
                              ~file:(React.S.value UIState.opened_filename)
                              ~title:"plot"
                              ~descr:(Printf.sprintf "{ model_max_time : %s , model_max_time : %d }"
                                                     (match React.S.value UIState.model_max_time with
                                                        None -> "None"
                                                      | Some model_max_time -> string_of_float model_max_time)
                                                     (React.S.value UIState.model_nb_plot))
     in
     let svg_text :string = Pp_svg.to_string ~width:width svg_store in
     plot_div##innerHTML <- Js.string svg_text

let select_fluxmap fileindex =
  match (React.S.value UIState.model_runtime_state) with
    None -> ()
  | Some state -> flux_map_render (List.nth state.ApiTypes.flux_maps (int_of_string fileindex))

let onload () =
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
  let plot_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string plot_div_id))
               (fun () -> assert false) in
  React.S.l1 (fun state -> match state with
                             None -> ()
                           | Some state -> update_plot state.plot)
             UIState.model_runtime_state
