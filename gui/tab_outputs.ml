(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let select_id = "output-select-id"
let tab_is_active, set_tab_is_active = React.S.create false
let current_file, set_current_file = React.S.create None

let update_outputs key : unit =
  State_simulation.when_ready ~label:__LOC__ (fun manager ->
      manager#simulation_detail_file_line key
      >>= Api_common.result_bind_lwt ~ok:(fun lines ->
              let () = set_current_file (Some (key, lines)) in
              Lwt.return (Result_util.ok ())))

let file_count state =
  match state with
  | None -> 0
  | Some state ->
    state.Api_types_t.simulation_info_output
      .Api_types_t.simulation_output_file_lines

let navli () = Ui_common.badge (fun state -> file_count state)
let dont_gc_me = ref []

let xml () =
  let select file_line_ids =
    let lines = React.S.value current_file in
    let current_file_id : string =
      match file_line_ids, lines with
      | [], _ -> assert false
      | file :: _, None | _ :: _, Some (file, _) -> file
    in
    let file_options =
      List.map
        (fun key ->
          Html.option
            ~a:
              ([ Html.a_value key ]
              @
              if key = current_file_id then
                [ Html.a_selected () ]
              else
                [])
            (Html.txt (Ui_common.option_label key)))
        file_line_ids
    in
    let () = update_outputs current_file_id in
    Tyxml_js.Html.select
      ~a:[ Html.a_class [ "form-control" ]; Html.a_id select_id ]
      file_options
  in
  let file_select =
    Tyxml_js.R.Html.div
      ~a:[ Html.a_class [ "list-group-item" ] ]
      (let list, handle = ReactiveData.RList.create [] in
       let () =
         dont_gc_me :=
           [
             React.S.map
               (fun _ ->
                 State_simulation.when_ready ~label:__LOC__ (fun manager ->
                     manager#simulation_catalog_file_line
                     >>= Api_common.result_bind_lwt
                           ~ok:(fun
                               (file_line_ids : Api_types_j.file_line_catalog)
                             ->
                             let () =
                               ReactiveData.RList.set handle
                                 (match file_line_ids with
                                 | [] -> []
                                 | key :: [] ->
                                   let () = update_outputs key in
                                   [
                                     Html.h4
                                       [ Html.txt (Ui_common.option_label key) ];
                                   ]
                                 | _ :: _ :: _ -> [ select file_line_ids ])
                             in
                             Lwt.return (Result_util.ok ()))))
               (React.S.on tab_is_active State_simulation.dummy_model
                  State_simulation.model);
           ]
       in
       list)
  in
  let file_content =
    [
      Tyxml_js.R.Html.div
        ~a:[ Html.a_class [ "panel-scroll"; "flex-content" ] ]
        (ReactiveData.RList.from_signal
           (React.S.map
              (function
                | None -> []
                | Some (_, lines) ->
                  List.map (fun line -> Html.p [ Html.txt line ]) lines)
              current_file));
    ]
  in
  [
    [%html
      {|<div class="navcontent-view">
             <div class="row">
             <div class="center-block display-header">
           |}
        [ file_select ]
        {|
                   </div>
                </div>
             |}
        file_content
        {|
             </div> |}];
  ]

let select_outputs () : unit =
  let select_dom = Ui_common.id_dom select_id in
  let fileindex = Js.to_string select_dom##.value in
  update_outputs fileindex

let content () =
  [ Ui_common.toggle_element (fun t -> file_count t > 0) (xml ()) ]

let onload () =
  let () =
    Common.jquery_on "#navoutputs" "hide.bs.tab" (fun _ ->
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navoutputs" "shown.bs.tab" (fun _ ->
        set_tab_is_active true)
  in
  let () =
    Common.jquery_on (Format.sprintf "#%s" select_id) "change" (fun _ ->
        let () = select_outputs () in
        Js._true)
  in
  ()
(* TODO
     let select_dom : Dom_html.inputElement Js.t =
       Js.Unsafe.coerce
         ((Js.Opt.get
             (Ui_common.document##getElementById
                (Js.string select_id))
             (fun () -> assert false))
          : Dom_html.element Js.t) in
     let () = select_dom##.onchange := Dom_html.handler
   (fun _ ->
      let () = select_outputs t
     in Js._true)
     in
*)

let onresize () : unit = ()
