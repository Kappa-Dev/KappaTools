(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let navli () = ReactiveData.RList.empty

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let node = ref None

let accuracy, set_accuracy = React.S.create (Some Public_data.Low)
let fwd, set_fwd = React.S.create None
let bwd, set_bwd = React.S.create None
let total, set_total = React.S.create 1
let origin, set_origin = React.S.create None

let total_input_id = "total_input"
let fwd_input_id = "fwd_input"
let bwd_input_id = "bwd_input"
let recenter_id = "reset"
let next_node_id = "next"
let prev_node_id = "previous"

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



let accuracy_chooser_id = "influence-accuracy"

let accuracy_chooser =
  let option_gen x =
    Html.option
      ~a:
        ((fun l -> if React.S.value accuracy = Some x
           then Html.a_selected () :: l else l)
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
    match node
    with
    | Public_data.Rule r ->
      let label,_ast =
        if r.Public_data.rule_label = ""
        then r.Public_data.rule_ast,""
        else r.Public_data.rule_label,r.Public_data.rule_ast
      in
      let pos = r.Public_data.rule_position in
      let contextual_help =
        (Locality.to_string pos)^" "^(r.Public_data.rule_ast)
      in
      [
          Graph_loggers_sig.Label label;
          Graph_loggers_sig.Shape !Config.rule_shape;
          Graph_loggers_sig.FillColor !Config.rule_color;
          Graph_loggers_sig.Color
            (if is_center origin_short_opt node then
               !Config.center_color
             else
               !Config.rule_color);
          Graph_loggers_sig.Position [pos] ;
          Graph_loggers_sig.OnClick json ;
          Graph_loggers_sig.Contextual_help contextual_help
        ]
    | Public_data.Var r ->
      let label,_ast =
        if r.Public_data.var_label = ""
        then r.Public_data.var_ast,""
        else r.Public_data.var_label,r.Public_data.var_ast
      in
      let pos = r.Public_data.var_position in
      let contextual_help =
        (Locality.to_string pos)^(r.Public_data.var_ast)
      in

      [
        Graph_loggers_sig.Label label;
        Graph_loggers_sig.Shape !Config.variable_shape;
        Graph_loggers_sig.FillColor !Config.variable_color;
        Graph_loggers_sig.Color
          (if is_center origin_short_opt node then
             !Config.center_color
           else
             !Config.variable_color);
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
    get_id_of_node_id
      (
        match node with
        | Public_data.Rule rule ->
          Public_data.Rule rule.Public_data.rule_id
        | Public_data.Var var ->
          Public_data.Var var.Public_data.var_id
      )
  in
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
           (fun target _label ->
              let target_id =
                string_of_int (get_id_of_node_id target)
              in
              let label_string = "todo"
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

let content () =
  let accuracy_form =
    Html.form ~a:[ Html.a_class [ "form-horizontal" ] ]
      [ Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-2"];
                   Html.a_label_for accuracy_chooser_id ]
              [Html.pcdata "Accuracy"];
            Html.div
              ~a:[Html.a_class ["col-md-2"] ]
              [accuracy_chooser] ];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.label ~a:[Html.a_class ["col-md-3"]]
              [Html.pcdata "Size         Radius"];
            Html.span  ~a:[Html.a_class ["col-md-2"]] [total_input];
            Html.label ~a:[Html.a_class ["col-md-1"]] [Html.pcdata "fwd"];
            Html.span ~a:[Html.a_class ["col-md-2"]] [fwd_input];
            Html.label ~a:[Html.a_class ["col-md-2"]] [Html.pcdata "bwd"];
            Html.span ~a:[Html.a_class ["col-md-2"]] [bwd_input];
          ];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
             ~a:[Html.a_class ["col-md-3"];
                 Html.a_label_for total_input_id]
             [Html.pcdata "Navigate"];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [prev_node];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [next_node];
            Html.span ~a:[Html.a_class ["col-md-2"] ] [recenter]]]
  in
  let influences,set_influences = ReactiveData.RList.create [] in
  let _ =
    React.S.l6
      (fun _ acc fwd bwd total origin_refined ->
         (*let origin_json =
             JsonUtil.of_option
               Public_data.refined_influence_node_to_json origin_refined
           in*)
         let origin =
           Public_data.get_short_node_opt_of_refined_node_opt origin_refined
         in
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun influences_json ->
                    let buf = Buffer.create 1000 in
                    let fmt = Format.formatter_of_buffer buf in
                    let logger =
                      Loggers.open_logger_from_formatter
                        ~mode:Loggers.Js_Graph fmt
                    in
                    let () =
                      json_to_graph logger origin influences_json in
                    let graph = Loggers.graph_of_logger logger in
                    let graph_json = Graph_json.to_json graph in
                    let () = Loggers.flush_logger logger in
                    let () = Loggers.close_logger logger in
                    let () =
                      ReactiveData.RList.set
                        set_influences
                        [
                          (*Html.pcdata (Yojson.Basic.to_string origin_json) ;*)
                          Html.pcdata (Yojson.Basic.to_string graph_json)
                        ]  in
                    ())
                 (manager#get_local_influence_map
                    ?fwd ?bwd ?origin ~total acc)) >>=
              fun out -> Lwt.return (Api_common.result_lift out)
           ))
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model)
      accuracy fwd bwd total origin in
  [ Html.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll"]]
      [ accuracy_form; Tyxml_js.R.Html5.p influences ]
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = (Tyxml_js.To_dom.of_select accuracy_chooser)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let va = Js.to_string va##.value in
           let () = set_accuracy (Public_data.accuracy_of_string va) in
           Js._true) in
  let () = (Tyxml_js.To_dom.of_input total_input)##.onchange :=
               Dom_html.full_handler
                 (fun va _ ->
                    let va = Js.to_string va##.value in
                    try
                      let () = set_total (int_of_string va) in
                      Js._true
                    with _ -> Js._false)
  in
  let () = (Tyxml_js.To_dom.of_input fwd_input)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let va = Js.to_string va##.value in
           try
             let va_opt =
               if va = "" then None
               else Some (int_of_string va)
             in
             let () = set_fwd va_opt in
             Js._true
           with _ -> Js._false)
  in
  let () = (Tyxml_js.To_dom.of_input bwd_input )##.onchange :=
               Dom_html.full_handler
                 (fun va _ ->
                    let va = Js.to_string va##.value in
                    try
                      let va_opt =
                        if va = "" then None
                        else  Some (int_of_string va)
                      in
                      let () = set_bwd va_opt in
                      Js._true
                    with _ -> Js._false) in
  let () =
    (Tyxml_js.To_dom.of_input recenter )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
           let _ =
             React.S.l1
               (fun _  ->
                  State_project.with_project
                  ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                     (Lwt_result.map
                        (fun origin ->
                           let origin =
                             JsonUtil.to_option
                               Public_data.refined_influence_node_of_json
                               origin
                           in
                           let () = set_origin origin in
                           ())
                        manager#get_initial_node >>=
                      fun out ->
                      Lwt.return (Api_common.result_lift out)
                     )))
              (React.S.on tab_is_active
                         State_project.dummy_model State_project.model)
          in Js._true
        )
  in
  let () =
    (Tyxml_js.To_dom.of_input next_node )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
          let _ =
            React.S.l2
              (fun _  origin ->
                 let origin =
                     Public_data.get_short_node_opt_of_refined_node_opt origin
                 in
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
                            let () = set_origin origin' in
                            ())
                         (manager#get_next_node origin) >>=
                       fun out -> Lwt.return (Api_common.result_lift out)
                      ))
              )
              (React.S.on tab_is_active
                 State_project.dummy_model State_project.model)
              origin
          in Js._true
        )
  in
  let () =
    (Tyxml_js.To_dom.of_input prev_node )##.onclick :=
      Dom_html.full_handler
        (fun _ _ ->
          let _ =
            React.S.l2
              (fun _  origin ->
                let origin =
                    Public_data.get_short_node_opt_of_refined_node_opt origin
                in
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
                           let () = set_origin origin' in
                           ())
                        (manager#get_previous_node origin) >>=
                      fun out -> Lwt.return (Api_common.result_lift out)
                        ))
                  )
              (React.S.on tab_is_active
                 State_project.dummy_model State_project.model)
              origin
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
