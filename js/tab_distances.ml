module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5
module UIState = Ui_state

open Js_distances

let div_id = "distances-div"

let export_format_id = "distances-export-file-format"
let export_filename_id = "distances-export-filename"
let export_button_id =  "distances-export-button"


let state_distances state = match state with
    None -> None
  | Some state -> state.ApiTypes.distances

let content =
  let export_controls =
  Html.div
      ~a:[Tyxml_js.R.Html.a_class
             (React.S.bind
                UIState.model_runtime_state
                (fun state ->
                  React.S.const
                    (match state_distances state with
                      None -> ["hidden"]
                    | Some _ -> ["show"])
                )
             )]
      [ Display_common.export_controls
          ~export_select_id:export_format_id
          ~export_filename_id:export_filename_id
          ~export_button_id:export_button_id
          ~export_data_label:"dat"
      ]
  in
  <:html<<div>
             <div class="row">
                <div $list:Html.a_id div_id$ class="col-sm-8">
                </div>
             </div>
             $export_controls$
        </div> >>

let navcontent = [ Html.div [content] ]

let update_distances
    (distances : distances_plot Js.t)
    (data : ApiTypes.distances option) : unit =
  match data with
    None -> ()
  | Some data ->
    let distances_string : string = ApiTypes_j.string_of_distances data in
    let distances_data : Js.js_string Js.t = Js.string distances_string in
    distances##setData(distances_data)

let onload () =
  let distances_plot : distances_plot Js.t =
    Js_distances.create_distances_plot div_id in
  let () = Display_common.save_plot_ui
    (fun f -> let filename = Js.string f in
              distances_plot##exportJSON(filename)
    )
    "distances plot"
    export_button_id
    export_filename_id
    export_format_id
    div_id
    "json"
  in
  (* The elements size themselves using the div's if they are hidden
     it will default to size zero.  so they need to be sized when shown.
  *)
  let () = Common.jquery_on
    "#navgraph"
    "shown.bs.tab"
    (fun _ ->
      match (React.S.value UIState.model_runtime_state) with
        None -> ()
      | Some state -> update_distances distances_plot state.ApiTypes.distances)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
        None -> ()
      | Some state -> update_distances distances_plot state.ApiTypes.distances)
      UIState.model_runtime_state
  in
  ()

let navli = []
