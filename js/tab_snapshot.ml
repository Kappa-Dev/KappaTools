module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5
module UIState = Ui_state

let current_snapshot, set_current_snapshot =
  React.S.create (None : ApiTypes.snapshot option)

let state_snapshot state = match state with
    None -> []
  | Some state ->
    (match state.ApiTypes.snapshots with
      [] -> []
    | l -> l)

let navli = Display_common.badge
  (fun state -> List.length (state_snapshot state))

let select_id = "snapshot-select-id"
let export_format_id = "snapshot-export-file-format"
let export_filename_id = "snapshot-export-filename"
let export_button_id = "snapshot-export-button"
let display_id = "snapshot-map-display"
let content =
  let select =
    Tyxml_js.R.Html5.select
      ~a:[ Html5.a_class ["form-control"]
         ; Html5.a_id select_id ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
         (fun state ->
           ReactiveData.RList.set
             handle
             (List.mapi
                (fun i snapshot ->
                  Html5.option
                    ~a:([ Html5.a_value (string_of_int i)]
                       @
                       if (match (React.S.value current_snapshot) with
                          | None ->
                            false
                          | Some s ->
                            s.ApiTypes.snap_file = snapshot.ApiTypes.snap_file
                          )
                       then [Html5.a_selected `Selected]
                       else [])
                    (Html5.pcdata
                       (Display_common.option_label snapshot.ApiTypes.snap_file)))
                (state_snapshot state)
             )
         )
         UIState.model_runtime_state in
       list
      )
  in
  let snapshot_select =
  Display_common.toggle_element
    state_snapshot
    [
    Tyxml_js.R.Html5.div
      ~a:[ Html5.a_class ["list-group-item"] ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
         (fun state ->
           ReactiveData.RList.set
             handle
             (match state_snapshot state with
               head::[] ->
                 [Html5.h4
                     [ Html5.pcdata
                         (Display_common.option_label head.ApiTypes.snap_file)]]
             | _ -> [select]
             )
         )
         UIState.model_runtime_state
       in
       list
      )
    ]
  in
  let export_controls =
    Display_common.toggle_element
      state_snapshot
      [Display_common.export_controls
          export_format_id
          export_filename_id
          export_button_id]
  in
  <:html5<<div>
             <div class="row">
               <div class="center-block display-header">
                $snapshot_select$
               </div>
             </div>
             <div class="row">
                <div class="col-sm-12" $list:Html5.a_id display_id$>
                </div>
             </div>
             $export_controls$
        </div> >>


let navcontent = [content]

let update_snapshot
      (snapshot_js : Js_contact.contact_map Js.t)
      (snapshot : ApiTypes.snapshot) : unit =
  let () =
    Common.debug
      (Js.string
         (ApiTypes_j.string_of_snapshot snapshot))
  in
  let site_graph : ApiTypes.site_graph =
    Api_data.api_snapshot_site_graph snapshot in
  let () =
    Common.debug
      (Js.string
         (ApiTypes_j.string_of_site_graph site_graph))
  in
  let json : string =
    ApiTypes_j.string_of_site_graph site_graph
  in
  snapshot_js##setData (Js.string json)

let select_snapshot () =
  let snapshot_js : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id true in
  let index = Js.Opt.bind
    (Display_common.document##getElementById (Js.string select_id))
    (fun dom ->
      let snapshot_select_dom : Dom_html.inputElement Js.t =
        Js.Unsafe.coerce dom in
      let fileindex = Js.to_string (snapshot_select_dom##value) in
      try Js.some (int_of_string fileindex) with
        _ -> Js.null
    )
  in
  match (React.S.value UIState.model_runtime_state) with
    None -> ()
  | Some state ->
    let index = Js.Opt.get index (fun _ -> 0) in
    if List.length state.ApiTypes.snapshots > 0 then
      let snapshot_selected : ApiTypes.snapshot =
        List.nth state.ApiTypes.snapshots index in
      let () = set_current_snapshot (Some snapshot_selected) in
      update_snapshot
        snapshot_js
        snapshot_selected
    else
      set_current_snapshot None

let onload () : unit =
  let snapshot : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id true in
  let snapshot_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Display_common.document##getElementById
             (Js.string select_id))
          (fun () -> assert false))
      : Dom_html.element Js.t) in
  let () =
    snapshot_select_dom
      ##
      onchange <- Dom_html.handler
      (fun _ ->
        let () = select_snapshot ()
        in Js._true)
  in
  let () =
    Common.jquery_on
      "#navsnapshot"
      "shown.bs.tab"
      (fun _ ->
        match (React.S.value UIState.model_runtime_state) with
          None -> ()
        | Some state -> select_snapshot ())
  in
  let () =
    Display_common.save_plot_ui
      (fun f -> let filename = Js.string f in
                snapshot##exportJSON(filename)
      )
      "snapshot map"
      export_button_id
      export_filename_id
      export_format_id
      display_id
      "json"
  in
  let _ : unit React.signal = React.S.l1
    (fun state -> match state with
      None -> ()
    | Some state -> select_snapshot ()
    )
    UIState.model_runtime_state
  in
  ()
