(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let display_id = "flux-map-display"
let export_id = "flux-export"
let svg_id = "fluxmap-svg"
let select_id = "fluxmap-select"

let rules_checkboxes_id = "fluxmap-rules-checkboxes"
let checkbox_self_influence_id = "fluxmap-checkbox_self_influence"

let tab_is_active, set_tab_is_active = React.S.create false

let has_fluxmap
    (simulation_info : Api_types_j.simulation_info option) :
  bool =
  match simulation_info with
  | None -> false
  | Some simulation_info ->
    simulation_info.Api_types_j.simulation_info_output.Api_types_j.simulation_output_flux_maps > 0

let serialize_json : (string -> unit) ref = ref (fun _ -> ())

let configuration () : Widget_export.configuration =
  { Widget_export.id = export_id
  ; Widget_export.handlers =
      [ Widget_export.export_svg
          ~svg_div_id:display_id ()
      ; Widget_export.export_png
          ~svg_div_id:display_id ()
      ; { Widget_export.suffix = "json"
        ; Widget_export.label = "json"
        ; Widget_export.export =
            (fun filename -> (!serialize_json) filename)
        }
      ];
    show = React.S.map
        (fun model ->
           has_fluxmap
             (State_simulation.t_simulation_info model))
        State_simulation.model ;
  }

let flux_list, flux_handle = ReactiveData.RList.create []
let _ = React.S.map
    (fun _ ->
       State_simulation.when_ready
         ~label:__LOC__
         (fun manager ->
           manager#simulation_catalog_flux_map >>=
           (Api_common.result_bind_lwt
              ~ok:(fun (data : Api_types_t.flux_map_catalog) ->
                  let () = ReactiveData.RList.set
                      flux_handle
                      (match data.Api_types_t.flux_map_ids with
                       | [] -> []
                       | head::[] -> [Html.h4
                                        [ Html.pcdata
                                            (Ui_common.option_label
                                               head)]]
                       |_::_::_ as l -> [
                           Html.select
                             ~a:[ Html.a_class ["form-control"]
                                ; Html.a_id select_id ]
                             (List.mapi
                                (fun i id -> Html.option
                                    ~a:[ Html.a_value (string_of_int i) ]
                                    (Html.pcdata
                                       (Ui_common.option_label id))) l) ])
                  in
                  Lwt.return (Api_common.result_ok ()))
           )
         )
    )
    (React.S.on
       tab_is_active State_simulation.dummy_model State_simulation.model)

let xml () =
  let flux_label =
    Tyxml_js.R.Html.li ~a:[ Html.a_class ["list-group-item"] ] flux_list in
  let checkbox =
    Html.input ~a:[ Html.a_id "checkbox_self_influence"
                  ; Html.a_class ["checkbox-control"]
                  ; Html.a_input_type `Checkbox ] () in
  let export_controls =
    Widget_export.content (configuration ())
  in
  [%html {|<div class="navcontent-view">
           <div class="row">
               <div class="center-block display-header">
                Dynamic influence map between t = <span id="begin_time"></span>s
                and t = <span id="end_time"></span>s
                (<span id="nb_events"></span>events)
               </div>
             </div>

             <div class="row">
                <div id="control" class="col-sm-4">
                <ul class="list-group">
         |}[flux_label]{|
                   <li class="list-group-item">
                      <h4 class="list-group-item-heading">Rules</h4>
                      <button class="btn,btn-default"
                              id="toggle_rule_selection">
                      Toggle selected rules
                      </button>
                      <p id="|} rules_checkboxes_id {|"></p>
                   </li>
                   <li class="list-group-item">
                    <div class="input-group">
                      <label class="checkbox-control">
                         |}[checkbox]{|
                         Self influence
                      </label>
                    </div>
                   </li>
                   <li class="list-group-item">
                    <div class="input-group">
                      <p>Correction
                      <select class="form-control"
                              id="select_correction"><option value="none">None</option><option value="hits">Occurences</option><option value="time">Time</option></select>
                      </p>
                    </div>
                   </li>
                </ul>
             </div>
          <div id="|}display_id{|" class="col-sm-8"> |}[ Html.entity "nbsp" ]{| </div>
        </div>
     </div>
     <div class="navcontent-controls"> |}[export_controls]{| </div> |}]

let content () =
  [Ui_common.toggle_element
     has_fluxmap
     (xml ()) ]

let update_flux_map
    (flux_js : Js_flux.flux_map Js.t)
    (index : int ): unit =
  State_simulation.when_ready
    ~label:__LOC__
    (fun manager ->
      manager#simulation_catalog_flux_map >>=
      (Api_common.result_bind_lwt
         ~ok:(fun  (flux_map_info : Api_types_j.flux_map_catalog) ->
             try
               let fluxmap_id : string =
                 List.nth flux_map_info.Api_types_t.flux_map_ids index in
               (manager#simulation_detail_flux_map fluxmap_id)
             with
             | Failure f ->
               Lwt.return
                 (Api_common.result_error_msg f)
             | Invalid_argument f ->
               Lwt.return
                 (Api_common.result_error_msg f)

           )
      ) >>=
      (Api_common.result_bind_lwt
         ~ok:(fun (flux_map : Api_types_j.flux_map) ->
             let flux_data : Js_flux.flux_data Js.t =
               Js_flux.create_data
                 ~flux_begin_time:flux_map.Api_types_j.flux_data.Data.flux_start
                 ~flux_end_time:flux_map.Api_types_j.flux_end
                 ~normalized:(flux_map.Api_types_j.flux_data.Data.flux_kind <> Primitives.ABSOLUTE)
                 ~flux_rules:flux_map.Api_types_j.flux_rules
                 ~flux_hits:flux_map.Api_types_j.flux_data.Api_types_j.flux_hits
                 ~flux_fluxs:flux_map.Data.flux_data.Data.flux_fluxs in
             let () = flux_js##setFlux(flux_data) in
            Lwt.return (Api_common.result_ok ())))
    )


let select_fluxmap flux_map =
  let index = Js.Opt.bind
      (Ui_common.document##getElementById (Js.string select_id))
      (fun dom -> let select_dom : Dom_html.inputElement Js.t =
                    Js.Unsafe.coerce dom in
        let fileindex = Js.to_string (select_dom##.value) in
        try Js.some (int_of_string fileindex) with
          _ -> Js.null
      )
  in
  let o : Api_types_t.simulation_info option =
      State_simulation.t_simulation_info (React.S.value State_simulation.model)
  in
  if has_fluxmap o then
    let index = Js.Opt.get index (fun _ -> 0) in
    update_flux_map flux_map index

let navli () =
  Ui_common.badge
    (fun state ->
       match state with
       | None -> 0
       | Some state -> state.Api_types_j.simulation_info_output.Api_types_j.simulation_output_flux_maps)

let onload () =
  let () = Widget_export.onload (configuration ()) in
  let flux_configuration : Js_flux.flux_configuration Js.t =
    Js_flux.create_configuration
      ~short_labels:true
      ~begin_time_id:("begin_time")
      ~end_time_id:("end_time")
      ~select_correction_id:("select_correction")
      ~checkbox_self_influence_id:("checkbox_self_influence")
      ~toggle_rules_id:("toggle_rule_selection")
      ~nb_events_id:("nb_events")
      ~svg_id:svg_id
      ~rules_checkboxes_id:rules_checkboxes_id
      ~height:450
      ~width:360
  in
  let flux =
    Js_flux.create_flux_map flux_configuration in
  let () = serialize_json :=
      (fun f -> let filename = Js.string f in
        flux##exportJSON(filename))
  in
    let () =
    Common.jquery_on
      (Format.sprintf "#%s" select_id)
      ("change")
      (fun _ -> let () = select_fluxmap flux in Js._true)
  in

  let div : Dom_html.element Js.t = Ui_common.id_dom display_id in
  let () = div##.innerHTML := Js.string
        ("<svg id=\""^
         svg_id^
         "\" width=\"300\" height=\"300\"><g/></svg>") in
  let () = Common.jquery_on "#navflux"
      "shown.bs.tab"
      (fun _ -> let () = set_tab_is_active true in select_fluxmap flux)
  in
  let () = Common.jquery_on "#navflux"
      "hide.bs.tab" (fun _ -> set_tab_is_active false) in
  select_fluxmap flux
let onresize () : unit = ()
