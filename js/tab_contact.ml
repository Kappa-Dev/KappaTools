module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5
module UIState = Ui_state

let navli = []

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
                 React.S.value UIState.model_parse
               with
               | None -> "null"
               | Some parse -> ApiTypes.string_of_parse parse
              )
            )
      ];
    show = React.S.map
        (fun model_parse ->
           match model_parse with
           | None -> false
           | Some _ -> true
        )
        UIState.model_parse
  }


let content =
  let export_controls =
    Widget_export.content configuration
  in
  [%html {|<div>
             <div class="row">
                <div id="|}display_id{|" class="col-sm-8">
				       </div>
				       </div>
				       |}[export_controls]{|
        </div>|}]

let navcontent = [ Html.div [content] ]

let onload () =
  let () = Widget_export.onload configuration in
  let contactmap : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id false in
  let _ =
    React.S.map
      (fun data ->
         match data with
         | None -> (contactmap##clearData)
         | Some data ->
           let json : string =
             ApiTypes_j.string_of_site_graph data.ApiTypes.contact_map
           in
           (contactmap##setData
              (Js.string json)
              (Js.Opt.option (Ui_state.agent_count ()))
           )
      )
      UIState.model_parse
  in
  Common.jquery_on
    "#navcontact"
    "shown.bs.tab"
    (fun _ ->
       match (React.S.value UIState.model_parse) with
       | None -> (contactmap##clearData)
       | Some data ->
         let site_graph : ApiTypes.site_graph =
           Api_data.api_contactmap_site_graph data in
         (* quick cheat to get the count of the agent *)
         let json : string =
           ApiTypes_j.string_of_site_graph site_graph in
         contactmap##setData
           (Js.string json)
           (Js.Opt.option (Ui_state.agent_count ()))
    )
