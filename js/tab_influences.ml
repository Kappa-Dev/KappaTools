(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let navli () = []

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let node = ref None

let accuracy, set_accuracy = React.S.create (Some Public_data.Low)
let fwd, set_fwd = React.S.create None
let bwd, set_bwd = React.S.create None
let total, set_total = React.S.create 1
let origin, set_origin = React.S.create `Null

let total_input_id = "total_input"
let fwd_input_id = "fwd_input"
let bwd_input_id = "bwd_input"
let recenter_id = "reset"
let next_node_id = "next"
let prev_node_id = "previous"

let total_input =
  Html.input ~a:[ Html.a_id total_input_id ;
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
                  Html.a_title "Next";
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let prev_node =
  Html.input ~a:[ Html.a_id prev_node_id ;
                  Html.a_title "Previous";
                  Html.a_input_type `Button;
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()

let recenter =
  Html.input ~a:[ Html.a_id recenter_id ;
                  Html.a_title "Reset";
                  Html.a_input_type `Button;
                  Html.a_class ["form-control"];
                  Html.a_size 1;] ()


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
          [ Html.label
              ~a:[ Html.a_class ["col-md-2"];
                   Html.a_label_for total_input_id ]
              [Html.pcdata "Radius"];
            Html.div
              ~a:[Html.a_class ["col-md-2"] ]
              [total_input] ];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-3"];
                   Html.a_label_for fwd_input_id ]
              [Html.pcdata "Max_forward"];
            Html.div
              ~a:[Html.a_class ["col-md-2"] ]
              [fwd_input]];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-3"];
                   Html.a_label_for bwd_input_id ]
              [Html.pcdata "Max_backward"];
            Html.div ~a:[Html.a_class ["col-md-2"] ] [bwd_input]];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-3"];
                   Html.a_label_for next_node_id]
              [Html.pcdata "Next"];
            Html.div ~a:[Html.a_class ["col-md-2"] ] [next_node]];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-3"];
                   Html.a_label_for prev_node_id ]
              [Html.pcdata "Previous"];
            Html.div ~a:[Html.a_class ["col-md-2"] ] [prev_node]];
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label
              ~a:[ Html.a_class ["col-md-3"];
                   Html.a_label_for recenter_id ]
              [Html.pcdata "Reset"];
            Html.div ~a:[Html.a_class ["col-md-2"] ] [recenter]];
      ]
 in
  let influences,set_influences = ReactiveData.RList.create [] in
  let _ =
    React.S.l6
      (fun _ acc fwd bwd total origin ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun influences_json ->
                    let () =
                      ReactiveData.RList.set
                        set_influences
                        [Html.pcdata
                           (Yojson.Basic.to_string origin);
                          Html.pcdata
                           (Yojson.Basic.to_string influences_json) ] in
                    ())
                 (manager#get_local_influence_map
                    ?fwd ?bwd ~total ~origin acc)) >>=
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
                           let () = set_origin origin in
                           ())
                        (manager#get_initial_node ()) >>=
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
              (fun _  (origin:Yojson.Basic.json) ->
                State_project.with_project
                  ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                     (Lwt_result.map
                        (fun origin' ->
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
              (fun _  (origin:Yojson.Basic.json) ->
                State_project.with_project
                  ~label:__LOC__
                  (fun (manager : Api.concrete_manager) ->
                     (Lwt_result.map
                        (fun origin' ->
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
