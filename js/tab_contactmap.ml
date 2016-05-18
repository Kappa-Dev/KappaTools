module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5
module UIState = Ui_state

let navli = []

let export_format_id = "contact-export-file-format"
let export_filename_id = "contact-export-filename"
let export_button_id =  "contact-export-button"
let display_id = "contact-map-display"

let content =
  let export_controls =
    Html5.div
      ~a:[Tyxml_js.R.Html5.a_class
             (React.S.bind
                UIState.model_parse
                (fun parse -> React.S.const
                  (match parse with
                  | None -> ["hidden"]
                  | Some data ->
                    if Api_data.api_parse_is_empty data then
                      ["hidden"]
                    else
                      ["show"])
                )
             )]
      [ Display_common.export_controls
          export_format_id
          export_filename_id
          export_button_id ]
  in
  <:html5<<div>
             <div class="row">
                <div $list:Html5.a_id display_id$ class="col-sm-8">
                </div>
             </div>
             $export_controls$
        </div> >>

let navcontent = [ Html5.div [content] ]

let onload () =
  let contactmap : Contactmap.contact_map Js.t =
    Contactmap.create_contact_map display_id false in

  let () = Display_common.save_plot_ui
    (fun f -> let filename = Js.string f in
              contactmap##exportJSON(filename)
    )
    "contact map"
    export_button_id
    export_filename_id
    export_format_id
    display_id
    "json"
  in
  let () = Display_common.save_plot_ui
    (fun f -> let filename = Js.string f in
              contactmap##exportJSON(filename)
    )
    "contact map"
    export_button_id
    export_filename_id
    export_format_id
    display_id
    "json"
  in
  let _ =
    React.S.map
      (fun data ->
        match data with
        | None -> (contactmap##clearData())
        | Some data ->
          let json : string =
            ApiTypes_j.string_of_site_graph data.ApiTypes.contact_map
          in
          contactmap##setData (Js.string json))
      UIState.model_parse
  in
  let () = Common.jquery_on
    "#navcontact"
    "shown.bs.tab"
    (fun _ ->
      match (React.S.value UIState.model_parse) with
      | None -> (contactmap##clearData())
      | Some data ->
        let site_graph : ApiTypes.site_graph =
          Api_data.api_contactmap_site_graph data in
        let json : string =
          ApiTypes_j.string_of_site_graph site_graph in
        contactmap##setData (Js.string json))
  in
  ()
