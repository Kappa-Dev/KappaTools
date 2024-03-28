(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let tab_is_active, set_tab_is_active = React.S.create false
let din_id, set_din_id = React.S.create ""

let din_list =
  ReactiveData.RList.from_event []
    (Lwt_react.E.map_s
       (fun _ ->
         Lwt.map
           (Result_util.fold
              ~ok:(fun din_ids ->
                let out =
                  List.rev_map
                    (fun id -> Html.option ~a:[ Html.a_value id ] (Html.txt id))
                    din_ids
                in
                ReactiveData.RList.Set out)
              ~error:(fun _ -> ReactiveData.RList.Set []))
           (State_simulation.eval_with_sim_manager_and_info ~label:__LOC__
              ~stopped:(fun _ -> Lwt.return (Result_util.ok []))
              ~initializing:(fun _ -> Lwt.return (Result_util.ok []))
              ~ready:(fun manager _ -> manager#simulation_catalog_din)
              ()))
       (React.S.changes
          (React.S.on tab_is_active State_simulation.dummy_model
             State_simulation.model)))

let din_select =
  Tyxml_js.R.Html5.select ~a:[ Html.a_class [ "form-control" ] ] din_list

let din_data =
  React.S.bind (ReactiveData.RList.signal din_list) (function
    | [] -> React.S.const None
    | _ :: _ ->
      let () =
        set_din_id (Js.to_string (Tyxml_js.To_dom.of_select din_select)##.value)
      in
      React.S.bind din_id (fun din_id ->
          React.S.hold None
            (Lwt_react.E.from (fun () ->
                 Lwt.map
                   (Result_util.fold ~ok:(fun x -> x) ~error:(fun _ -> None))
                   (State_simulation.eval_with_sim_manager_and_info
                      ~label:__LOC__
                      ~stopped:(fun _ -> Lwt.return (Result_util.ok None))
                      ~initializing:(fun _ -> Lwt.return (Result_util.ok None))
                      ~ready:(fun manager _ ->
                        Lwt.map
                          (Result_util.map Option.some)
                          (manager#simulation_detail_din din_id))
                      ())))))

let din_header =
  ReactiveData.RList.from_signal
    (React.S.map
       (function
         | None -> []
         | Some din ->
           [
             Html.tr
               (Html.th [ Html.txt "affects" ]
               :: Array.fold_right
                    (fun r acc -> Html.th [ Html.txt r ] :: acc)
                    din.Data.din_rules []);
           ])
       din_data)

let din_table =
  ReactiveData.RList.from_signal
    (React.S.map
       (function
         | None -> []
         | Some din ->
           let open Data in
           let all = din.din_data.din_kind = Primitives.PROBABILITY in
           let body =
             Tools.array_fold_righti
               (fun i data acc ->
                 if all || din.din_data.din_hits.(i) > 0 then
                   Html.tr
                     (Html.th
                        [
                          Html.txt
                            (din.din_rules.(i) ^ " ("
                            ^ string_of_int din.din_data.din_hits.(i)
                            ^ " hits)");
                        ]
                     :: Array.fold_right
                          (fun v acc ->
                            Html.td
                              ~a:
                                [
                                  Html.a_class
                                    (if v > 0. then
                                       [ "success" ]
                                     else if v < 0. then
                                       [ "info" ]
                                     else
                                       []);
                                ]
                              [ Html.txt (string_of_float v) ]
                            :: acc)
                          data [])
                   :: acc
                 else
                   acc)
               din.din_data.din_fluxs []
           in
           [ Html.tbody body ])
       din_data)

let din =
  let thead = React.S.const (Tyxml_js.R.Html5.thead din_header) in
  Tyxml_js.R.Html5.tablex
    ~a:[ Html.a_class [ "table"; "table-condensed"; "table-bordered" ] ]
    ~thead din_table

let export_current_din to_string mime filename =
  let din_id = Js.to_string (Tyxml_js.To_dom.of_select din_select)##.value in
  State_simulation.eval_when_ready ~label:__LOC__ (fun manager ->
      manager#simulation_detail_din din_id
      >>= Api_common.result_bind_lwt ~ok:(fun din ->
              let data = Js.string (to_string din) in
              let () = Common.saveFile ~data ~mime ~filename in
              Lwt.return (Result_util.ok ())))

let export_configuration =
  {
    Widget_export.id = "din-export";
    Widget_export.show = React.S.const true;
    Widget_export.handlers =
      [
        {
          Widget_export.suffix = "json";
          Widget_export.label = "json";
          Widget_export.export =
            export_current_din (Data.string_of_din ?len:None) "application/json";
        };
        {
          Widget_export.suffix = "dot";
          Widget_export.label = "dot";
          Widget_export.export =
            export_current_din
              (Format.asprintf "@[%a@]" (Data.print_dot_din ?uuid:None))
              "text/vnd.graphviz";
        };
        {
          Widget_export.suffix = "html";
          Widget_export.label = "html";
          Widget_export.export =
            export_current_din
              (Format.asprintf "@[%a@]" Data.print_html_din)
              "text/html";
        };
      ];
  }

let content () =
  [
    Html.div
      ~a:[ Html.a_class [ "flex_content"; "table-responsive" ] ]
      [
        Html.form [ din_select ];
        din;
        Widget_export.content export_configuration;
      ];
  ]

let navli () =
  Ui_common.badge (fun state ->
      match state with
      | None -> 0
      | Some state ->
        state.Api_types_j.simulation_info_output
          .Api_types_j.simulation_output_dins)

let onload () =
  let () =
    (Tyxml_js.To_dom.of_select din_select)##.onchange
    := Dom.handler (fun _ ->
           let () =
             set_din_id
               (Js.to_string (Tyxml_js.To_dom.of_select din_select)##.value)
           in
           Js._false)
  in
  let () = Widget_export.onload export_configuration in
  let () =
    Common.jquery_on "#navDIN" "shown.bs.tab" (fun _ -> set_tab_is_active true)
  in
  Common.jquery_on "#navDIN" "hide.bs.tab" (fun _ -> set_tab_is_active false)

let onresize () : unit = ()
