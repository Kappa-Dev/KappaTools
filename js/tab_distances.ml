(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix
open Js_distances

let div_id = "distances-div"

let export_format_id = "distances-export-file-format"
let export_filename_id = "distances-export-filename"
let export_button_id =  "distances-export-button"


let state_distances
    (state : Api_types_j.simulation_info option) :
  int option =
  match state with
  | None -> None
  | Some state ->
    Some
      state
      .Api_types_j
      .simulation_info_output
      .Api_types_j
      .simulation_output_distances

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
                 |}[ Html.entity "nbsp" ]{|
		</div>
             </div>
           </div>
           <div class="navcontent-controls"> |}[export_controls]{| </div>
          |}]

let navcontent (t : Ui_simulation.t) =
  [Ui_common.toggle_element
     t
     (fun s -> match state_distances s with None -> false | Some _ -> true)
     (content t) ]

let update_distances
    (distances : distances_plot Js.t)
    (t : Ui_simulation.t) : unit =
 Ui_simulation.manager_operation
   t
   (fun
     manager
     project_id
     simulation_id ->
     (Api_v1.assemble_distance manager project_id simulation_id)
     >>=
     (Api_common.result_map
        ~ok:(fun _ (data : Api_types_v1_t.distances) ->
            let () = match data with
              | [] -> ()
              | _ ->
                let distances_string : string =
                  Api_types_v1_j.string_of_distances data in
                let distances_data : Js.js_string Js.t =
                  Js.string distances_string in
                distances##setData(distances_data)
            in
            Lwt.return_unit)
        ~error:(fun _ errors  ->
            let () = Ui_state.set_model_error __LOC__ errors in
            Lwt.return_unit)
     )
   )

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
         | Some _ -> update_distances distances_plot t)
  in
  let _ =
    React.S.l1
      (fun state -> match state with
           None -> ()
         | Some _ -> update_distances distances_plot t)
      simulation_output
  in
  ()

let navli ( _ : Ui_simulation.t) = []
let onresize (_ : Ui_simulation.t) : unit = ()
