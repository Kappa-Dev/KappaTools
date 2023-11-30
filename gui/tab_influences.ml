(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

type model_graph = { fwd: int option; bwd: int option; total: int }

type influence_sphere = {
  positive_on:
    ((Public_data.rule, Public_data.var) Public_data.influence_node
    * Public_data.location Public_data.pair list)
    list;
  negative_on:
    ((Public_data.rule, Public_data.var) Public_data.influence_node
    * Public_data.location Public_data.pair list)
    list;
  positive_by:
    ((Public_data.rule, Public_data.var) Public_data.influence_node
    * Public_data.location Public_data.pair list)
    list;
  negative_by:
    ((Public_data.rule, Public_data.var) Public_data.influence_node
    * Public_data.location Public_data.pair list)
    list;
}

let empty_sphere =
  { positive_on = []; positive_by = []; negative_on = []; negative_by = [] }

type model_rendering = DrawGraph of model_graph | DrawTabular of unit

type model = {
  rendering: model_rendering;
  accuracy: Public_data.accuracy_level option;
  origin: (int, int) Public_data.influence_node option;
  origin_label: string option;
}

let navli () = ReactiveData.RList.empty
let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false
let track_cursor, set_track_cursor = React.S.create false

let dummy_model =
  {
    rendering = DrawTabular ();
    accuracy = Some Public_data.Low;
    origin = None;
    origin_label = None;
  }

let model, set_model = React.S.create dummy_model
let total_input_id = "total_input"
let fwd_input_id = "fwd_input"
let bwd_input_id = "bwd_input"

let influence_node_label = function
  | Public_data.Rule r ->
    if r.Public_data.rule_label = "" then
      r.Public_data.rule_ast
    else
      r.Public_data.rule_label
  | Public_data.Var r ->
    if r.Public_data.var_label = "" then
      r.Public_data.var_ast
    else
      r.Public_data.var_label

let update_model_graph f =
  let m = React.S.value model in
  match m.rendering with
  | DrawTabular _ -> ()
  | DrawGraph g -> set_model { m with rendering = DrawGraph (f g) }

let update_model f = set_model (f (React.S.value model))
let display_id = "influence_map_display"

let influencemap =
  Js_graphlogger.create_graph_logger display_id (fun x ->
      update_model (fun m ->
          let node =
            Public_data.refined_influence_node_of_json
              (Yojson.Basic.from_string (Js.to_string x))
          in
          let () =
            Subpanel_editor.set_move_cursor
              (Public_data.position_of_refined_influence_node node)
          in
          let origin = Some (Public_data.short_node_of_refined_node node) in
          let origin_label = Some (influence_node_label node) in
          { m with origin; origin_label }))

let total_input =
  Html.input
    ~a:
      [
        Html.a_id total_input_id;
        Html.a_input_type `Number;
        Html.a_value "1";
        Html.a_class [ "form-control" ];
        Html.a_size 1;
      ]
    ()

let fwd_input =
  Html.input
    ~a:
      [
        Html.a_id fwd_input_id;
        Html.a_input_type `Number;
        Html.a_class [ "form-control" ];
        Html.a_size 1;
      ]
    ()

let bwd_input =
  Html.input
    ~a:
      [
        Html.a_id bwd_input_id;
        Html.a_input_type `Number;
        Html.a_class [ "form-control" ];
        Html.a_size 1;
      ]
    ()

let next_node =
  Html.button
    ~a:
      [
        Html.a_button_type `Button;
        Html.a_class [ "form-control"; "btn"; "btn-default" ];
      ]
    [ Html.txt "Next" ]

let prev_node =
  Html.button
    ~a:
      [
        Html.a_button_type `Button;
        Html.a_class [ "form-control"; "btn"; "btn-default" ];
      ]
    [ Html.txt "Previous" ]

let recenter =
  Html.button
    ~a:
      [
        Html.a_button_type `Button;
        Html.a_class [ "form-control"; "btn"; "btn-default" ];
      ]
    [ Html.txt "Reset" ]

let track_cursor_switch =
  Html.button
    ~a:
      [
        Html.a_button_type `Button;
        Tyxml_js.R.Html5.a_class
          (React.S.map
             (fun tc ->
               "form-control" :: "btn" :: "btn-default"
               ::
               (if tc then
                  [ "active" ]
                else
                  []))
             track_cursor);
        Html.a_onclick (fun _ ->
            let () = set_track_cursor (not (React.S.value track_cursor)) in
            true);
      ]
    [ Html.txt "Track cursor" ]

let export_config =
  {
    Widget_export.id = "influence-export";
    Widget_export.handlers =
      [
        {
          Widget_export.suffix = "json";
          Widget_export.label = "json";
          Widget_export.export =
            (fun filename ->
              Lwt.ignore_result
                ( State_error.wrap "influence_map_export"
                    (State_project.with_project ~label:__LOC__ (fun manager ->
                         let { accuracy; _ } = React.S.value model in
                         manager#get_influence_map_raw accuracy
                         >|= Result_util.map (fun influences_string ->
                                 let data = Js.string influences_string in
                                 let () =
                                   Common.saveFile ~data
                                     ~mime:"application/json" ~filename
                                 in
                                 ())))
                >>= fun _ -> Lwt.return_unit ));
        };
      ];
    Widget_export.show = React.S.const true;
  }

let rendering_chooser_id = "influence-rendering"

let rendering_chooser =
  let { rendering; _ } = React.S.value model in
  Html.select
    ~a:[ Html.a_class [ "form-control" ]; Html.a_id rendering_chooser_id ]
    [
      Html.option
        ~a:
          ((fun l ->
             match rendering with
             | DrawTabular _ -> Html.a_selected () :: l
             | DrawGraph _ -> l)
             [ Html.a_value "tabular" ])
        (Html.txt "Tabular");
      Html.option
        ~a:
          ((fun l ->
             match rendering with
             | DrawGraph _ -> Html.a_selected () :: l
             | DrawTabular _ -> l)
             [ Html.a_value "graph" ])
        (Html.txt "Graph");
    ]

let accuracy_chooser_id = "influence-accuracy"

let accuracy_chooser =
  let { accuracy; _ } = React.S.value model in
  let option_gen x =
    Html.option
      ~a:
        ((fun l ->
           if accuracy = Some x then
             Html.a_selected () :: l
           else
             l)
           [ Html.a_value (Public_data.accuracy_to_string x) ])
      (Html.txt (Public_data.accuracy_to_string x))
  in
  Html.select
    ~a:[ Html.a_class [ "form-control" ]; Html.a_id accuracy_chooser_id ]
    (List.map option_gen Public_data.influence_map_accuracy_levels)

let is_center origin_short_opt node =
  match origin_short_opt with
  | None ->
    (match node with
    | Public_data.Rule r -> r.Public_data.rule_id = 0
    | Public_data.Var _ -> false)
  | Some origin ->
    (match origin, node with
    | Public_data.Rule id, Public_data.Rule a -> a.Public_data.rule_id = id
    | Public_data.Var id, Public_data.Var a -> a.Public_data.var_id = id
    | Public_data.Var _, Public_data.Rule _
    | Public_data.Rule _, Public_data.Var _ ->
      false)

let json_to_graph logger (_, _, _, _, origin, influence_map) =
  let origin_short_opt =
    Option_util.map Public_data.short_node_of_refined_node origin
  in
  let () = Graph_loggers.print_graph_preamble logger "" in
  let nodes = influence_map.Public_data.nodes in
  let directives_of_node node =
    let json = Public_data.refined_influence_node_to_json node in
    let label = influence_node_label node in
    match node with
    | Public_data.Rule r ->
      let pos = r.Public_data.rule_position in
      let contextual_help =
        Locality.to_string pos ^ " " ^ r.Public_data.rule_ast
      in
      let fillcolor =
        if is_center origin_short_opt node then
          !Config.center_color
        else
          !Config.rule_color
      in
      [
        Graph_loggers_sig.Label label;
        Graph_loggers_sig.Shape !Config.rule_shape;
        Graph_loggers_sig.FillColor fillcolor;
        Graph_loggers_sig.Color fillcolor;
        Graph_loggers_sig.Position [ pos ];
        Graph_loggers_sig.OnClick json;
        Graph_loggers_sig.Contextual_help contextual_help;
      ]
    | Public_data.Var r ->
      let pos = r.Public_data.var_position in
      let contextual_help = Locality.to_string pos ^ r.Public_data.var_ast in
      let fillcolor =
        if is_center origin_short_opt node then
          !Config.center_color
        else
          !Config.variable_color
      in
      [
        Graph_loggers_sig.Label label;
        Graph_loggers_sig.Shape !Config.variable_shape;
        Graph_loggers_sig.FillColor fillcolor;
        Graph_loggers_sig.Color fillcolor;
        Graph_loggers_sig.Position [ pos ];
        Graph_loggers_sig.OnClick json;
        Graph_loggers_sig.Contextual_help contextual_help;
      ]
  in
  let max_rule_id =
    List.fold_left
      (fun biggest_id n ->
        match n with
        | Public_data.Rule r -> max biggest_id (1 + r.Public_data.rule_id)
        | Public_data.Var _ -> biggest_id)
      (-1) nodes
  in
  let get_id_of_node_id node_id =
    match node_id with
    | Public_data.Rule id -> id
    | Public_data.Var id -> id + max_rule_id
  in
  let get_id_of_node node =
    get_id_of_node_id (Public_data.short_node_of_refined_node node)
  in
  let () =
    List.iter
      (fun node ->
        let directives = directives_of_node node in
        Graph_loggers.print_node logger ~directives
          (string_of_int (get_id_of_node node)))
      nodes
  in
  let print_maps ?(directives = []) logger map =
    Public_data.InfluenceNodeMap.iter
      (fun source map ->
        let source_id = string_of_int (get_id_of_node_id source) in
        Public_data.InfluenceNodeMap.iter
          (fun target label_list ->
            let target_id = string_of_int (get_id_of_node_id target) in
            (*let label_string = "todo"
              in*)
            let label_string = Public_data.string_of_label_list label_list in
            let directives =
              Graph_loggers_sig.Label label_string :: directives
            in
            let () =
              Graph_loggers.print_edge logger ~directives source_id target_id
            in
            ())
          map)
      map
  in
  let directives =
    [
      Graph_loggers_sig.Color !Config.wake_up_color;
      Graph_loggers_sig.ArrowHead !Config.wake_up_arrow;
    ]
  in
  let () = print_maps ~directives logger influence_map.Public_data.positive in
  let directives =
    [
      Graph_loggers_sig.Color !Config.inhibition_color;
      Graph_loggers_sig.ArrowHead !Config.inhibition_arrow;
    ]
  in
  let () = print_maps ~directives logger influence_map.Public_data.negative in
  let () = Graph_loggers.print_graph_foot logger in
  ()

let table_of_influences_json (_, _, _, _, origin, influence_map) =
  let namer =
    List.fold_left
      (fun acc e ->
        Public_data.InfluenceNodeMap.add
          (Public_data.short_node_of_refined_node e)
          e acc)
      Public_data.InfluenceNodeMap.empty influence_map.Public_data.nodes
  in
  let origin_id_opt =
    Option_util.map Public_data.short_node_of_refined_node origin
  in
  match origin_id_opt with
  | None -> empty_sphere
  | Some origin_id ->
    let positive_on, positive_by =
      Public_data.InfluenceNodeMap.fold
        (fun src ->
          Public_data.InfluenceNodeMap.fold (fun dst data ((on, by) as acc) ->
              if src = origin_id then (
                match Public_data.InfluenceNodeMap.find_option dst namer with
                | None -> acc
                | Some v -> (v, data) :: on, by
              ) else if dst = origin_id then (
                match Public_data.InfluenceNodeMap.find_option src namer with
                | None -> acc
                | Some v -> on, (v, data) :: by
              ) else
                acc))
        influence_map.Public_data.positive ([], [])
    in
    let negative_on, negative_by =
      Public_data.InfluenceNodeMap.fold
        (fun src ->
          Public_data.InfluenceNodeMap.fold (fun dst data ((on, by) as acc) ->
              if src = origin_id then (
                match Public_data.InfluenceNodeMap.find_option dst namer with
                | None -> acc
                | Some v -> (v, data) :: on, by
              ) else if dst = origin_id then (
                match Public_data.InfluenceNodeMap.find_option src namer with
                | None -> acc
                | Some v -> on, (v, data) :: by
              ) else
                acc))
        influence_map.Public_data.negative ([], [])
    in
    { positive_on; positive_by; negative_on; negative_by }

let pop_cell = function
  | [] -> Html.td [], []
  | ((node, _mappings), positive) :: t ->
    ( Html.td
        ~a:
          [
            Html.a_onclick (fun _ ->
                let () =
                  Subpanel_editor.set_move_cursor
                    (Public_data.position_of_refined_influence_node node)
                in
                let origin =
                  Some (Public_data.short_node_of_refined_node node)
                in
                let origin_label = Some (influence_node_label node) in
                let () =
                  update_model (fun m -> { m with origin; origin_label })
                in
                true);
            Html.a_class
              [
                (if positive then
                   "success"
                 else
                   "danger");
              ];
          ]
        [ Html.cdata (influence_node_label node) ],
      t )

let rec fill_table acc by on =
  if on = [] && by = [] then
    List.rev acc
  else (
    let b, by' = pop_cell by in
    let o, on' = pop_cell on in
    let line = Html.tr [ b; o ] in
    fill_table (line :: acc) by' on'
  )

let draw_table origin_label
    { positive_on; positive_by; negative_on; negative_by } =
  let by =
    List_util.rev_map_append
      (fun x -> x, false)
      negative_by
      (List.rev_map (fun x -> x, true) positive_by)
  in
  let on =
    List_util.rev_map_append
      (fun x -> x, false)
      negative_on
      (List.rev_map (fun x -> x, true) positive_on)
  in
  let outs = fill_table [] by on in
  Html.tablex
    ~a:[ Html.a_class [ "table" ] ]
    ~thead:
      (Html.thead
         [
           Html.tr
             [
               Html.th
                 ~a:[ Html.a_colspan 2 ]
                 [ Html.cdata (Option_util.unsome "origin" origin_label) ];
             ];
           Html.tr
             [
               Html.th [ Html.cdata "is influenced by" ];
               Html.th [ Html.cdata "influences" ];
             ];
         ])
    [ Html.tbody outs ]

let influence_sphere =
  State_project.on_project_change_async ~on:tab_is_active dummy_model model
    (Result_util.ok empty_sphere)
    (fun manager { rendering; accuracy; origin; origin_label = _ } ->
      match rendering with
      | DrawTabular _ ->
        manager#get_local_influence_map ?fwd:None ?bwd:None ?origin ~total:1
          accuracy
        >|= Result_util.map table_of_influences_json
      | DrawGraph _ -> Lwt.return (Result_util.ok empty_sphere))

let content () =
  let accuracy_form =
    Html.form
      ~a:[ Html.a_class [ "form-horizontal" ] ]
      [
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label
              ~a:
                [
                  Html.a_class [ "col-md-2" ];
                  Html.a_label_for rendering_chooser_id;
                ]
              [ Html.txt "Rendering" ];
            Html.span ~a:[ Html.a_class [ "col-md-4" ] ] [ rendering_chooser ];
            Html.label
              ~a:
                [
                  Html.a_class [ "col-md-2" ];
                  Html.a_label_for accuracy_chooser_id;
                ]
              [ Html.txt "Accuracy" ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ accuracy_chooser ];
          ];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label
              ~a:
                [ Html.a_class [ "col-md-3" ]; Html.a_label_for total_input_id ]
              [ Html.txt "Navigate" ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ prev_node ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ next_node ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ recenter ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ track_cursor_switch ];
          ];
      ]
  in
  let graph_form =
    Html.form
      ~a:[ Html.a_class [ "form-horizontal" ] ]
      [
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label
              ~a:[ Html.a_class [ "col-md-3" ] ]
              [ Html.txt "Size Radius" ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ total_input ];
            Html.label ~a:[ Html.a_class [ "col-md-1" ] ] [ Html.txt "fwd" ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ fwd_input ];
            Html.label ~a:[ Html.a_class [ "col-md-2" ] ] [ Html.txt "bwd" ];
            Html.span ~a:[ Html.a_class [ "col-md-2" ] ] [ bwd_input ];
          ];
      ]
  in
  [
    accuracy_form;
    Html.div
      ~a:
        [
          Tyxml_js.R.Html5.a_class
            (React.S.map
               (fun { rendering; _ } ->
                 match rendering with
                 | DrawGraph _ -> [ "flex-content" ]
                 | DrawTabular _ -> [])
               model);
          Tyxml_js.R.filter_attrib (Html.a_hidden ())
            (React.S.map
               (fun { rendering; _ } ->
                 match rendering with
                 | DrawGraph _ -> false
                 | DrawTabular _ -> true)
               model);
        ]
      [
        graph_form;
        Html.div ~a:[ Html.a_id display_id; Html.a_class [ "flex-content" ] ] [];
      ];
    Tyxml_js.R.Html5.div
      ~a:[ Html.a_class [ "panel-scroll" ] ]
      (ReactiveData.RList.from_signal
         (React.S.l2
            (fun { rendering; origin_label; _ } sphere ->
              match rendering with
              | DrawGraph _ -> []
              | DrawTabular () ->
                Result_util.fold sphere
                  ~ok:(fun sphere -> [ draw_table origin_label sphere ])
                  ~error:(fun error ->
                    List.map
                      (fun m ->
                        Html.p
                          [
                            Html.txt
                              (Format.asprintf "@[%a@]"
                                 Result_util.print_message m);
                          ])
                      error))
            model influence_sphere));
    Widget_export.content export_config;
  ]

let neither_gc_me =
  React.S.l2
    (fun _ { rendering; accuracy; origin; origin_label = _ } ->
      match rendering with
      | DrawTabular _ -> Lwt.return (Result_util.ok ())
      | DrawGraph { fwd; bwd; total } ->
        State_error.wrap ~append:true "influence_map"
          (State_project.with_project ~label:__LOC__
             (fun (manager : Api.concrete_manager) ->
               manager#get_local_influence_map ?fwd ?bwd ?origin ~total accuracy
               >|= Result_util.fold
                     ~ok:(fun influences ->
                       let buf = Buffer.create 1000 in
                       let fmt = Format.formatter_of_buffer buf in
                       let logger =
                         Loggers.open_logger_from_formatter
                           ~mode:Loggers.Js_Graph fmt
                       in
                       let logger_graph =
                         Graph_loggers_sig.extend_logger logger
                       in
                       let () = json_to_graph logger_graph influences in
                       let graph =
                         Graph_loggers_sig.graph_of_logger logger_graph
                       in
                       let graph_json = Graph_json.to_json graph in
                       let () = Loggers.flush_logger logger in
                       let () = Loggers.close_logger logger in
                       let () =
                         influencemap##setData
                           (Js.string (Yojson.Basic.to_string graph_json))
                       in
                       Result_util.ok ())
                     ~error:(fun e ->
                       let () = influencemap##clearData in
                       Result_util.error e))))
    (React.S.on ~eq:State_project.model_equal tab_is_active
       State_project.dummy_model State_project.model)
    model

let nor_gc_me =
  State_file.with_current_pos
    ~on:(React.S.Bool.( && ) tab_is_active track_cursor)
    (fun filename cursor_pos ->
      Some
        (State_project.with_project ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
             manager#get_influence_map_node_at ~filename cursor_pos
             >|= Result_util.map (fun origin' ->
                     update_model (fun m ->
                         { m with origin = origin'; origin_label = None })))))
    (Lwt.return (Result_util.ok ()))

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active
let dont_gc_me = ref []

let onload () =
  let () = dont_gc_me := [ neither_gc_me; nor_gc_me ] in
  let () = Widget_export.onload export_config in
  let () =
    (Tyxml_js.To_dom.of_select rendering_chooser)##.onchange
    := Dom_html.full_handler (fun va _ ->
           let () =
             update_model (fun m ->
                 {
                   m with
                   rendering =
                     (if Js.to_string va##.value = "graph" then
                        DrawGraph { fwd = None; bwd = None; total = 1 }
                      else
                        DrawTabular ());
                 })
           in
           Js._true)
  in
  let () =
    (Tyxml_js.To_dom.of_select accuracy_chooser)##.onchange
    := Dom_html.full_handler (fun va _ ->
           let accuracy =
             Public_data.accuracy_of_string (Js.to_string va##.value)
           in
           let () = update_model (fun m -> { m with accuracy }) in
           Js._true)
  in
  let () =
    (Tyxml_js.To_dom.of_input total_input)##.onchange
    := Dom_html.full_handler (fun va _ ->
           let va = Js.to_string va##.value in
           try
             let () =
               update_model_graph (fun m -> { m with total = int_of_string va })
             in
             Js._true
           with _ -> Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_input fwd_input)##.onchange
    := Dom_html.full_handler (fun va _ ->
           let va = Js.to_string va##.value in
           try
             let fwd =
               if va = "" then
                 None
               else
                 Some (int_of_string va)
             in
             let () = update_model_graph (fun m -> { m with fwd }) in
             Js._true
           with _ -> Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_input bwd_input)##.onchange
    := Dom_html.full_handler (fun va _ ->
           let va = Js.to_string va##.value in
           try
             let bwd =
               if va = "" then
                 None
               else
                 Some (int_of_string va)
             in
             let () = update_model_graph (fun m -> { m with bwd }) in
             Js._true
           with _ -> Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_button recenter)##.onclick
    := Dom_html.full_handler (fun _ _ ->
           let _ =
             State_error.wrap "influence_map_recenter"
               (State_project.with_project ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                    manager#get_initial_node
                    >|= Result_util.map (fun origin_refined ->
                            let origin =
                              Option_util.map
                                Public_data.short_node_of_refined_node
                                origin_refined
                            in
                            let origin_label =
                              Option_util.map influence_node_label
                                origin_refined
                            in
                            update_model (fun m ->
                                { m with origin; origin_label }))))
           in
           Js._true)
  in
  let () =
    (Tyxml_js.To_dom.of_button next_node)##.onclick
    := Dom_html.full_handler (fun _ _ ->
           let { origin; _ } = React.S.value model in
           let _ =
             State_error.wrap "influence_map_next_node"
               (State_project.with_project ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                    manager#get_next_node origin
                    >|= Result_util.map (fun origin_refined ->
                            let origin =
                              Option_util.map
                                Public_data.short_node_of_refined_node
                                origin_refined
                            in
                            let origin_label =
                              Option_util.map influence_node_label
                                origin_refined
                            in
                            update_model (fun m ->
                                { m with origin; origin_label }))))
           in
           Js._true)
  in
  let () =
    (Tyxml_js.To_dom.of_button prev_node)##.onclick
    := Dom_html.full_handler (fun _ _ ->
           let { origin; _ } = React.S.value model in
           let _ =
             State_error.wrap "influence_map_prev_node"
               (State_project.with_project ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                    manager#get_previous_node origin
                    >|= Result_util.map (fun origin_refined ->
                            let origin =
                              Option_util.map
                                Public_data.short_node_of_refined_node
                                origin_refined
                            in
                            let origin_label =
                              Option_util.map influence_node_label
                                origin_refined
                            in
                            update_model (fun m ->
                                { m with origin; origin_label }))))
           in
           Js._true)
  in
  let () =
    Common.jquery_on "#navinfluences" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navinfluences" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        set_tab_is_active true)
  in
  ()

let onresize () : unit = ()
