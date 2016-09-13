module ApiTypes = ApiTypes_j

module Html = Tyxml_js.Html
module UIState = Ui_state

open Js_distances

let div_id = "distances-div"

let export_format_id = "distances-export-file-format"
let export_filename_id = "distances-export-filename"
let export_button_id =  "distances-export-button"


let state_distances state = match state with
    None -> None
  | Some state -> state.ApiTypes.distances

let content (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let export_controls =
    Html.div
      ~a:[Tyxml_js.R.Html.a_class
            (React.S.bind
               simulation_output
               (fun state ->
                  React.S.const
                    (match state_distances state with
                       None -> ["hidden"]
                     | Some _ -> ["show"])
               )
            )]
      [ Ui_common.export_controls
          ~export_select_id:export_format_id
          ~export_filename_id:export_filename_id
          ~export_button_id:export_button_id
          ~export_data_label:"dat"
      ]
  in
  [%html {|<div class="navcontent-view">
             <div class="row">
                <div id="|}div_id{|" class="col-sm-12">
		</div>
             </div>
           </div>
           <div class="navcontent-controls"> |}[export_controls]{| </div>
          |}]

let navcontent (t : Ui_simulation.t) =
  [Ui_common.toggle_element
     t
     (fun s -> match state_distances s with None -> [] | Some d -> [s])
     (content t) ]

let update_distances
    (distances : distances_plot Js.t)
    (data : ApiTypes.distances option) : unit =
  match data with
    None -> ()
  | Some data ->
    let distances_string : string = ApiTypes_j.string_of_distances data in
    let distances_data : Js.js_string Js.t = Js.string distances_string in
    distances##setData(distances_data)

let onload (t : Ui_simulation.t) =
  let distances_plot : distances_plot Js.t =
    Js_distances.create_distances_plot div_id in
  let () = Ui_common.save_plot_ui
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
  let simulation_output = (Ui_simulation.simulation_output t) in
  let () = Common.jquery_on
      "#navdistances"
      "shown.bs.tab"
      (fun _ ->
         match (React.S.value simulation_output) with
           None -> ()
         | Some state ->
           update_distances distances_plot state.ApiTypes.distances)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
           None -> ()
         | Some state ->
           update_distances distances_plot state.ApiTypes.distances)
      simulation_output
  in
  ()

let navli ( _ : Ui_simulation.t) = []
