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
      [ Html.div ~a:[ Html.a_class [ "form-group" ] ]
          [ Html.label ~a:[ Html.a_class ["col-md-2"]; Html.a_label_for accuracy_chooser_id ]
              [Html.pcdata "Accuracy"];
            Html.div ~a:[Html.a_class ["col-md-10"] ] [accuracy_chooser] ]
      ] in
  let influences,set_influences = ReactiveData.RList.create [] in
  let _ =
    React.S.l1
      (fun _ ->
         React.S.l1
           (fun acc ->
              React.S.l1
                (fun fwd ->
                   React.S.l1
                   (fun bwd ->
                     React.S.l1
                       (fun total ->
                          React.S.l1
                            (fun origin ->
                               State_project.with_project
                                 ~label:__LOC__
                                 (fun (manager : Api.concrete_manager) ->
                                    (Lwt_result.map
                                       (fun influences_json ->
                                          let () =
                                            ReactiveData.RList.set
                                              set_influences
                                              [Html.pcdata
                                                 (Yojson.Basic.to_string influences_json) ] in
                                          ())
                                       (

                                         (* old version *)   manager#get_influence_map acc

                                         (* new version*)
                                         (*let fwd = None in
                                         let bwd = None in
                                           manager#get_local_influence_map  ?fwd ?bwd ~total ~origin acc*)

                                           (* when I use the new version instead of the old version, well KaSa_client dies *))) >>=
                   fun out -> Lwt.return (Api_common.result_lift out)
                                 ))
                            origin)
                       total)
                   bwd)
                fwd)
           accuracy)
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model) in
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
  let () = Common.jquery_on
      "#navinfluences" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navinfluences" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
