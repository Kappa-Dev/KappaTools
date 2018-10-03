(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

type model_graph = {
  fwd : int option;
  bwd : int option;
  total : int;
}

type influence_sphere = {
  positive_on :
    ((Public_data.rule,Public_data.var) Public_data.influence_node *
     Public_data.location Public_data.pair list) list;
  negative_on :
    ((Public_data.rule,Public_data.var) Public_data.influence_node *
     Public_data.location Public_data.pair list) list;
  positive_by :
    ((Public_data.rule,Public_data.var) Public_data.influence_node *
     Public_data.location Public_data.pair list) list;
  negative_by :
    ((Public_data.rule,Public_data.var) Public_data.influence_node *
     Public_data.location Public_data.pair list) list;
}

let empty_sphere =
  { positive_on = []; positive_by = []; negative_on = []; negative_by = [] }

type model_rendering =
  | DrawGraph of model_graph
  | DrawTabular of unit

type model = {
  rendering : model_rendering;
  accuracy : Public_data.accuracy_level option;
  origin : (Public_data.rule, Public_data.var) Public_data.influence_node option;
}

let navli () = ReactiveData.RList.empty

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let node = ref None

let dummy_model = {
  rendering = DrawTabular ();
  accuracy = Some Public_data.Low;
  origin = None;
}

let model, set_model = React.S.create dummy_model

let total_input_id = "total_input"
let fwd_input_id = "fwd_input"
let bwd_input_id = "bwd_input"
let recenter_id = "reset"
let next_node_id = "next"
let prev_node_id = "previous"

let update_model_graph f =
  let m = React.S.value model in
  match m.rendering with
  | DrawTabular _ -> ()
  | DrawGraph g -> set_model { m with rendering = DrawGraph (f g) }

let update_model f =
  set_model (f (React.S.value model))

let display_id = "influence_map_display"
let influencemap =
  Js_graphlogger.create_graph_logger
    display_id
    (fun x -> update_model
      (fun m ->
        { m with origin = Some (Public_data.refined_influence_node_of_json
                                  (Yojson.Basic.from_string (Js.to_string x))) }))

let total_input =
  Html.input ~a:[
                  Html.a_id total_input_id ;
                  Html.a_input_type `Number;
                  Html.a_value "1";
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let fwd_input =
  Html.input ~a:[ Html.a_id fwd_input_id ;
                  Html.a_input_type `Number;
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let bwd_input =
  Html.input ~a:[ Html.a_id bwd_input_id ;
                  Html.a_input_type `Number;
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let next_node =
  Html.input ~a:[ Html.a_id next_node_id ;
                  Html.a_input_type `Button;
                  Html.a_value "Next";
                  Html.a_title "Next";
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let prev_node =
  Html.input ~a:[ Html.a_id prev_node_id ;
                  Html.a_title "Previous";
                  Html.a_value "Previous";
                  Html.a_input_type `Button;
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let recenter =
  Html.input ~a:[
    Html.a_id recenter_id ;
                  Html.a_title "Reset";
                  Html.a_value "Reset";
                  Html.a_input_type `Button;
                  Html.a_class ["form-control"];
                  Html.a_size 1;]
    ()

let export_config = {
  Widget_export.id = "influence-export";
  Widget_export.handlers =
    [ {
      Widget_export.suffix = "json";
      Widget_export.label = "json";
      Widget_export.export = (fun filename ->
          Lwt.ignore_result
            (State_project.with_project
               ~label:__LOC__
               (fun manager ->
                  let { accuracy; _ } = React.S.value model in
                  (manager#get_influence_map accuracy) >>= function
                  | Result.Ok influences_json ->
                    let data =
                      Js.string (Yojson.Basic.to_string influences_json) in
                    let () =
                      Common.saveFile ~data ~mime:"application/json" ~filename in
                    Lwt.return (Api_common.result_ok ())
                  | Result.Error _err ->
                    Lwt.return (Api_common.result_ok ()))
             >>= fun _ -> Lwt.return_unit));
    } ];
  Widget_export.show = React.S.const true;
}

let rendering_chooser_id = "influence-rendering"

let rendering_chooser =
  let { rendering; _ } = React.S.value model in
  Html.select
    ~a:[Html.a_class [ "form-control" ]; Html.a_id rendering_chooser_id ]
    [
      Html.option
        ~a:((fun l -> match rendering with
            | DrawTabular _ -> Html.a_selected () :: l
            | DrawGraph _ -> l)
             [ Html.a_value "tabular" ])
        (Html.pcdata "Tabular");
      Html.option
        ~a:((fun l -> match rendering with
            | DrawGraph _ -> Html.a_selected () :: l
            | DrawTabular _ -> l)
             [ Html.a_value "graph" ])
        (Html.pcdata "Graph");
    ]

let accuracy_chooser_id = "influence-accuracy"

let accuracy_chooser =
  let { accuracy; _ } = React.S.value model in
  let option_gen x =
    Html.option
      ~a:
        ((fun l -> if accuracy = Some x then Html.a_selected () :: l else l)
           [ Html.a_value (Public_data.accuracy_to_string x) ])
      (Html.pcdata
         (Public_data.accuracy_to_string x)) in
  Html.select
    ~a:[Html.a_class [ "form-control" ]; Html.a_id accuracy_chooser_id ]
    (List.map option_gen Public_data.influence_map_accuracy_levels)

let is_center origin_short_opt node =
  match origin_short_opt with
  | None ->
    begin
      match node with
      | Public_data.Rule r -> r.Public_data.rule_id = 0
      | Public_data.Var _ -> false
    end
  | Some origin  ->
    match origin, node with
    | Public_data.Rule id, Public_data.Rule a ->
      a.Public_data.rule_id = id
    | Public_data.Var id, Public_data.Var a ->
      a.Public_data.var_id = id
    | Public_data.Var _, Public_data.Rule _
    | Public_data.Rule _, Public_data.Var _ -> false

let influence_node_label = function
  | Public_data.Rule r ->
    if r.Public_data.rule_label = ""
    then r.Public_data.rule_ast
    else r.Public_data.rule_label
  | Public_data.Var r ->
    if r.Public_data.var_label = ""
    then r.Public_data.var_ast
    else r.Public_data.var_label

let json_to_graph logger origin_short_opt influences_json =
  let () = Graph_loggers.print_graph_preamble logger "" in
  let _,_,_,_,influence_map =
    Public_data.local_influence_map_of_json
      influences_json
  in
  let nodes = influence_map.Public_data.nodes in
  let directives_of_node node =
    let json =
      Public_data.refined_influence_node_to_json node
    in
    let label = influence_node_label node in
    match node
    with
    | Public_data.Rule r ->
      let pos = r.Public_data.rule_position in
      let contextual_help =
        (Locality.to_string pos)^" "^(r.Public_data.rule_ast)
      in
      let fillcolor =
        if is_center origin_short_opt node then !Config.center_color
        else !Config.rule_color in
      [
          Graph_loggers_sig.Label label;
          Graph_loggers_sig.Shape !Config.rule_shape;
          Graph_loggers_sig.FillColor fillcolor;
          Graph_loggers_sig.Color fillcolor;
          Graph_loggers_sig.Position [pos] ;
          Graph_loggers_sig.OnClick json ;
          Graph_loggers_sig.Contextual_help contextual_help
        ]
    | Public_data.Var r ->
      let pos = r.Public_data.var_position in
      let contextual_help =
        (Locality.to_string pos)^(r.Public_data.var_ast)
      in
      let fillcolor =
        if is_center origin_short_opt node then !Config.center_color
        else !Config.variable_color in
      [
        Graph_loggers_sig.Label label;
        Graph_loggers_sig.Shape !Config.variable_shape;
        Graph_loggers_sig.FillColor fillcolor;
        Graph_loggers_sig.Color fillcolor;
        Graph_loggers_sig.Position [pos] ;
        Graph_loggers_sig.OnClick json ;
        Graph_loggers_sig.Contextual_help contextual_help]
  in
  let max_rule_id =
    List.fold_left
      (fun biggest_id n ->
         match n with
         | Public_data.Rule r ->
           max biggest_id (1+(r.Public_data.rule_id))
         | Public_data.Var _ -> biggest_id)
      (-1) nodes
  in
  let get_id_of_node_id node_id =
    match node_id with
    | Public_data.Rule id -> id
    | Public_data.Var id -> id + max_rule_id
  in
  let get_id_of_node node =
    get_id_of_node_id (Public_data.short_node_of_refined_node node) in
  let () =
    List.iter
      (fun node ->
         let directives =
           directives_of_node node
         in
         Graph_loggers.print_node
           logger
           ~directives
           (string_of_int (get_id_of_node node))
      )
      nodes
  in
  let print_maps ?directives:(directives=[]) logger map =
    Public_data.InfluenceNodeMap.iter
      (fun source map ->
         let source_id =
           string_of_int (get_id_of_node_id source)
         in
         Public_data.InfluenceNodeMap.iter
           (fun target label_list ->
              let target_id =
                string_of_int (get_id_of_node_id target)
              in
              (*let label_string = "todo"
                in*)
              let label_string =
                Public_data.string_of_label_list label_list
              in
              let directives = (Graph_loggers_sig.Label
                                  label_string)::directives in
              let () =
                Graph_loggers.print_edge logger ~directives
                  source_id target_id in
              ())
           map)
      map
  in
  let directives =
    [Graph_loggers_sig.Color !Config.wake_up_color;
     Graph_loggers_sig.ArrowHead !Config.wake_up_arrow]
  in
  let () =
    print_maps
      ~directives
      logger
      influence_map.Public_data.positive
  in
  let directives =
    [Graph_loggers_sig.Color !Config.inhibition_color;
     Graph_loggers_sig.ArrowHead !Config.inhibition_arrow]
  in
  let () =
    print_maps
      ~directives
      logger
      influence_map.Public_data.negative
  in
  let () = Graph_loggers.print_graph_foot logger in
  ()

let table_of_influences_json origin influences_json =
  let _,_,_,_,influence_map =
    Public_data.local_influence_map_of_json influences_json in
  let namer =
    List.fold_left
      (fun acc e -> Public_data.InfluenceNodeMap.add
          (Public_data.short_node_of_refined_node e) e acc)
      Public_data.InfluenceNodeMap.empty
      influence_map.Public_data.nodes in
  match origin with
  | None -> empty_sphere
  | Some origin ->
    let positive_on,positive_by =
      Public_data.InfluenceNodeMap.fold
        (fun src ->
           Public_data.InfluenceNodeMap.fold
             (fun dst data (on,by as acc) ->
                if src = origin then
                  match Public_data.InfluenceNodeMap.find_option dst namer with
                  | None -> acc
                  | Some v -> ((v,data)::on,by)
                else if dst = origin then
                  match Public_data.InfluenceNodeMap.find_option src namer with
                  | None -> acc
                  | Some v -> (on,(v,data)::by)
                else acc))
        influence_map.Public_data.positive ([],[]) in
    let negative_on,negative_by =
      Public_data.InfluenceNodeMap.fold
        (fun src ->
           Public_data.InfluenceNodeMap.fold
             (fun dst data (on,by as acc) ->
                if src = origin then
                  match Public_data.InfluenceNodeMap.find_option dst namer with
                  | None -> acc
                  | Some v -> ((v,data)::on,by)
                else if dst = origin then
                  match Public_data.InfluenceNodeMap.find_option src namer with
                  | None -> acc
                  | Some v -> (on,(v,data)::by)
                else acc))
        influence_map.Public_data.negative ([],[]) in
    { positive_on; positive_by; negative_on; negative_by }

let pop_cell = function
  | [] -> (Html.td [],[])
  | ((node,_mappings),positive)::t ->
    (Html.td ~a:[ Html.a_class [if positive then "success" else "danger"] ]
       [Html.cdata (influence_node_label node)],t)

let rec fill_table acc by on =
  if on = [] && by = [] then
    List.rev acc
  else
    let b,by' = pop_cell by in
    let o,on' = pop_cell on in
    let line = Html.tr [ b; o ] in
    fill_table (line::acc) by' on'

let draw_table origin { positive_on; positive_by; negative_on; negative_by } =
    let by =
    List_util.rev_map_append (fun x -> (x,false)) negative_by
      (List.rev_map (fun x -> (x,true)) positive_by) in
  let on =
    List_util.rev_map_append (fun x -> (x,false)) negative_on
      (List.rev_map (fun x -> (x,true)) positive_on) in
  let outs = fill_table [] by on in
  Html.tablex
    ~a:[ Html.a_class ["table"]]
    ~thead:(Html.thead [
        Html.tr
          [Html.th ~a:[Html.a_colspan 2]
             [Html.cdata
                (match origin with None -> "origin"|Some n -> influence_node_label n)
             ]];
        Html.tr [
          Html.th [Html.cdata "is influenced by"];
          Html.th [Html.cdata "influences"]];
      ])
    [Html.tbody outs]

let influence_sphere =
  State_project.on_project_change_async
    ~on:tab_is_active dummy_model model empty_sphere
    (fun manager { rendering; accuracy; origin = origin_refined } ->
       match rendering with
       | DrawTabular _ ->
         let origin =
           Option_util.map
             Public_data.short_node_of_refined_node origin_refined in
         (manager#get_local_influence_map
            ?fwd:None ?bwd:None ?origin ~total:1 accuracy >>= function
          | Result.Ok influences_json ->
            Lwt.return
              (table_of_influences_json origin influences_json)
          | Result.Error _e -> Lwt.return empty_sphere)
       | DrawGraph _ -> Lwt.return empty_sphere)

let content () =
  let accuracy_form =
    Html.form ~a:[ Html.a_class [ "form-horizontal" ] ]
      [ Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label
              ~a:[ Html.a_class ["col-md-2"];
                   Html.a_label_for rendering_chooser_id ]
              [Html.pcdata "Rendering"];
            Html.span ~a:[Html.a_class ["col-md-4"] ] [rendering_chooser];
            Html.label
              ~a:[ Html.a_class ["col-md-2"];
                   Html.a_label_for accuracy_chooser_id ]
              [Html.pcdata "Accuracy"];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [accuracy_chooser] ];
              Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[Html.a_class ["col-md-3"];
                  Html.a_label_for total_input_id]
              [Html.pcdata "Navigate"];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [prev_node];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [next_node];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [recenter]]] in
  let graph_form =
    Html.form ~a:[ Html.a_class [ "form-horizontal" ] ]
      [ Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label ~a:[Html.a_class ["col-md-3"]]
              [Html.pcdata "Size Radius"];
            Html.span  ~a:[Html.a_class ["col-md-2"]] [total_input];
            Html.label ~a:[Html.a_class ["col-md-1"]] [Html.pcdata "fwd"];
            Html.span ~a:[Html.a_class ["col-md-2"]] [fwd_input];
            Html.label ~a:[Html.a_class ["col-md-2"]] [Html.pcdata "bwd"];
            Html.span ~a:[Html.a_class ["col-md-2"]] [bwd_input];
          ]] in
  [ accuracy_form;
    Html.div
      ~a:[ Tyxml_js.R.Html5.a_class
             (React.S.map
                (fun { rendering; _ } ->
                   match rendering with
                   | DrawGraph _ -> ["flex-content"]
                   | DrawTabular _ -> [])
                model);
           Tyxml_js.R.filter_attrib
             (Html.a_hidden ())
             (React.S.map
                (fun { rendering; _ } ->
                   match rendering with
                   | DrawGraph _ -> false
                   | DrawTabular _ -> true)
                model)
         ]
      [ graph_form;
        Html.div ~a:[Html.a_id display_id; Html.a_class ["flex-content"] ] []];
    Tyxml_js.R.Html5.div
      ~a:[ Html.a_class ["panel-scroll"] ]
      (ReactiveData.RList.from_signal
         (React.S.l2
            (fun { rendering; origin; _ } sphere ->
               match rendering with
               | DrawGraph _ -> []
               | DrawTabular () -> [ draw_table origin sphere ])
            model influence_sphere));
    Widget_export.content export_config;
  ]

let _ =
  React.S.l2
    (fun _ { rendering; accuracy; origin = origin_refined } ->
       State_project.with_project
         ~label:__LOC__
         (fun (manager : Api.concrete_manager) ->
            (match rendering with
             | DrawTabular _ -> Lwt_result.return ()
             | DrawGraph { fwd; bwd; total } ->
               let origin =
                 Option_util.map
                   Public_data.short_node_of_refined_node origin_refined in
               (manager#get_local_influence_map
                  ?fwd ?bwd ?origin ~total accuracy) >>= function
               | Result.Ok influences_json ->
                 let buf = Buffer.create 1000 in
                 let fmt = Format.formatter_of_buffer buf in
                 let logger =
                   Loggers.open_logger_from_formatter
                     ~mode:Loggers.Js_Graph fmt in
                 let () =
                   json_to_graph logger origin influences_json in
                 let graph = Loggers.graph_of_logger logger in
                 let graph_json = Graph_json.to_json graph in
                 let () = Loggers.flush_logger logger in
                 let () = Loggers.close_logger logger in
                 let () =
                   influencemap##setData
                     (Js.string (Yojson.Basic.to_string graph_json)) in
                 Lwt_result.return ()
               | Result.Error e ->
                 let () = influencemap##clearData in
                 Lwt_result.fail e) >>=
            fun out -> Lwt.return (Api_common.result_lift out)
         ))
    (React.S.on ~eq:State_project.model_equal tab_is_active
       State_project.dummy_model State_project.model)
    model

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Widget_export.onload export_config in
  let () = (Tyxml_js.To_dom.of_select rendering_chooser)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let () = update_model (fun m ->
               { m with
                 rendering =
                   if Js.to_string va##.value = "graph" then
                     DrawGraph { fwd = None; bwd = None; total = 1 }
                   else DrawTabular () }) in
           Js._true) in
  let () = (Tyxml_js.To_dom.of_select accuracy_chooser)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let accuracy =
             Public_data.accuracy_of_string (Js.to_string va##.value) in
           let () = update_model (fun m -> { m with accuracy }) in
           Js._true) in
  let () = (Tyxml_js.To_dom.of_input total_input)##.onchange :=
               Dom_html.full_handler
                 (fun va _ ->
                    let va = Js.to_string va##.value in
                    try
                      let () = update_model_graph
                          (fun m -> { m with total = int_of_string va }) in
                      Js._true
                    with _ -> Js._false)
  in
  let () = (Tyxml_js.To_dom.of_input fwd_input)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let va = Js.to_string va##.value in
           try
             let fwd =
               if va = "" then None
               else Some (int_of_string va)
             in
             let () = update_model_graph (fun m -> { m with fwd }) in
             Js._true
           with _ -> Js._false)
  in
  let () = (Tyxml_js.To_dom.of_input bwd_input )##.onchange :=
               Dom_html.full_handler
                 (fun va _ ->
                    let va = Js.to_string va##.value in
                    try
                      let bwd =
                        if va = "" then None
                        else  Some (int_of_string va)
                      in
                      let () = update_model_graph (fun m -> { m with bwd }) in
                      Js._true
                    with _ -> Js._false) in
  let () =
    (Tyxml_js.To_dom.of_input recenter )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
           let _ =
             State_project.with_project
               ~label:__LOC__
               (fun (manager : Api.concrete_manager) ->
                  (Lwt_result.map
                     (fun origin ->
                        let origin =
                          JsonUtil.to_option
                            Public_data.refined_influence_node_of_json
                            origin in
                        let () = update_model (fun m -> { m with origin }) in
                        ())
                     manager#get_initial_node >>=
                   fun out ->
                   Lwt.return (Api_common.result_lift out)
                  ))
           in Js._true
        )
  in
  let () =
    (Tyxml_js.To_dom.of_input next_node )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
           let origin =
             let { origin; _ } = React.S.value model in
             Option_util.map Public_data.short_node_of_refined_node origin in
           let _ =
             State_project.with_project
               ~label:__LOC__
               (fun (manager : Api.concrete_manager) ->
                  (Lwt_result.map
                     (fun origin' ->
                        let origin' =
                          JsonUtil.to_option
                            Public_data.refined_influence_node_of_json
                            origin'
                        in
                        let () = update_model
                            (fun m -> { m with origin = origin' }) in
                        ())
                     (manager#get_next_node origin) >>=
                   fun out -> Lwt.return (Api_common.result_lift out)
                  ))
           in Js._true
        )
  in
  let () =
    (Tyxml_js.To_dom.of_input prev_node )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
           let origin =
             let { origin; _ } = React.S.value model in
             Option_util.map Public_data.short_node_of_refined_node origin in
           let _ =
             State_project.with_project
               ~label:__LOC__
               (fun (manager : Api.concrete_manager) ->
                  (Lwt_result.map
                     (fun origin' ->
                        let origin' =
                          JsonUtil.to_option
                            Public_data.refined_influence_node_of_json
                            origin'
                        in
                        let () = update_model
                            (fun m -> { m with origin = origin' }) in
                        ())
                     (manager#get_previous_node origin) >>=
                   fun out -> Lwt.return (Api_common.result_lift out)
                  ))
           in Js._true
        )
  in
  let () = Common.jquery_on
      "#navinfluences" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navinfluences" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
