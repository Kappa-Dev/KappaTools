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
let din_table, table_handle = ReactiveData.RList.create []
let din_header, set_din_header = React.S.create []

let din =
  let thead = React.S.map (fun x -> Html.thead x) din_header in
  Tyxml_js.R.Html5.tablex
    ~a:[ Html.a_class [ "table"; "table-condensed"; "table-bordered" ] ]
    ~thead din_table

let fill_table din =
  let open Data in
  let all = din.din_data.din_kind = Primitives.PROBABILITY in
  let header =
    Html.tr
      (Html.th [ Html.txt "affects" ]
      :: Array.fold_right
           (fun r acc -> Html.th [ Html.txt r ] :: acc)
           din.din_rules [])
  in
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
  let () = set_din_header [ header ] in
  ReactiveData.RList.set table_handle [ Html.tbody body ]

let update_din din_id : unit =
  State_simulation.when_ready ~label:__LOC__ (fun manager ->
      manager#simulation_detail_din din_id
      >>= Result_util.fold
            ~ok:(fun (din : Api_types_t.din) ->
              let () = fill_table din in
              Lwt.return (Result_util.ok ()))
            ~error:(fun e ->
              let () = ReactiveData.RList.set table_handle [] in
              Lwt.return (Api_common.result_messages e)))

let din_list, din_handle = ReactiveData.RList.create []

let din_select =
  Tyxml_js.R.Html5.select ~a:[ Html.a_class [ "form-control" ] ] din_list

let select_din () =
  let din_id = Js.to_string (Tyxml_js.To_dom.of_select din_select)##.value in
  update_din din_id

let dont_gc_me =
  React.S.map
    (fun _ ->
      State_simulation.with_simulation_info ~label:__LOC__
        ~stopped:(fun _ ->
          let () = ReactiveData.RList.set din_handle [] in
          let () = ReactiveData.RList.set table_handle [] in
          Lwt.return (Result_util.ok ()))
        ~initializing:(fun _ ->
          let () = ReactiveData.RList.set din_handle [] in
          let () = ReactiveData.RList.set table_handle [] in
          Lwt.return (Result_util.ok ()))
        ~ready:(fun manager _ ->
          manager#simulation_catalog_din
          >>= Api_common.result_bind_lwt ~ok:(fun din_ids ->
                  let () =
                    ReactiveData.RList.set din_handle
                      (List.rev_map
                         (fun id ->
                           Html.option ~a:[ Html.a_value id ] (Html.txt id))
                         din_ids)
                  in
                  let () = select_din () in
                  Lwt.return (Result_util.ok ())))
        ())
    (React.S.on tab_is_active State_simulation.dummy_model
       State_simulation.model)

let export_current_din to_string mime filename =
  let din_id = Js.to_string (Tyxml_js.To_dom.of_select din_select)##.value in
  State_simulation.when_ready ~label:__LOC__ (fun manager ->
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
  let () = ignore dont_gc_me in
  let () =
    (Tyxml_js.To_dom.of_select din_select)##.onchange
    := Dom.handler (fun _ ->
           let () = select_din () in
           Js._false)
  in
  let () = Widget_export.onload export_configuration in
  let () =
    Common.jquery_on "#navDIN" "shown.bs.tab" (fun _ -> set_tab_is_active true)
  in
  Common.jquery_on "#navDIN" "hide.bs.tab" (fun _ -> set_tab_is_active false)

let onresize () : unit = ()
