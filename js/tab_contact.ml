(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module ApiTypes = Api_types_v1_j
module Html = Tyxml_js.Html5

let navli (_ : Ui_simulation.t) = []

let display_id = "contact-map-display"
let export_id = "contact-export"

let configuration : Widget_export.configuration =
  { Widget_export.id = export_id ;
    Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
          ~serialize_json:(fun () ->
              (match
                 React.S.value Ui_state.model_parse
               with
               | None -> "null"
               | Some parse -> Api_types_j.string_of_contact_map parse
              )
            )
      ];
    show = React.S.map
        (fun model_parse ->
           match model_parse with
           | None -> false
           | Some data -> Array.length data > 0
        )
        Ui_state.model_parse
  }


let content =
  let export_controls =
    Widget_export.content configuration
  in
  [%html {|<div>
             <div class="row">
                <div id="|}display_id{|" class="col-sm-8">
                 |}[ Html.entity "nbsp" ]{|
	        </div>
	    </div>
	   |}[ export_controls ]{|
        </div>|}]

let navcontent (_ : Ui_simulation.t) = [ Html.div [content] ]

let update
    (data : Api_types_j.contact_map)
    (contactmap : Js_contact.contact_map Js.t) : unit =
  let () = Common.debug (Js.string "updating") in
  let site_graph : Api_types_v1_j.site_graph =
    Api_data_v1.api_contact_map data in
  (* quick cheat to get the count of the agents *)
  let json : string =
    Api_types_v1_j.string_of_site_graph site_graph in
  let () = Common.debug (Js.string json) in
  contactmap##setData
    (Js.string json)
    (Js.Opt.option (Ui_state.agent_count ()))

let onload (_ : Ui_simulation.t) =
  let () = Widget_export.onload configuration in
  let contactmap : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id false in
  let _ =
    React.S.map
      (fun data ->
         match data with
         | None -> (contactmap##clearData)
         | Some data ->
           if Array.length data > 0 then
             update data contactmap
           else
             contactmap##clearData)
      Ui_state.model_parse

  in
  Common.jquery_on
    "#navcontact"
    "shown.bs.tab"
    (fun _ ->
       match (React.S.value Ui_state.model_parse) with
       | None -> (contactmap##clearData)
       | Some data -> update data contactmap)

let onresize (_ : Ui_simulation.t) : unit = ()
