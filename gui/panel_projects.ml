(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let project_id_modal_id = "menu-editor-project-id-modal"

let project_id_input =
  Html.input
    ~a:
      [
        Html.a_input_type `Text;
        Html.a_class [ "form-control" ];
        Html.a_placeholder "project new";
        Html.a_size 40;
      ]
    ()

let li_new = Html.li [ Html.a [ Html.cdata "New project" ] ]
let li_prefs = Html.li (Modal_preferences.content ())
let project_id_input_dom = Tyxml_js.To_dom.of_input project_id_input

let content () =
  Html.div
    [
      Tyxml_js.R.Html5.ul
        ~a:[ Html.a_class [ "nav"; "nav-tabs"; "nav-justified" ] ]
        (ReactiveData.RList.from_signal
           (React.S.map
              (fun model ->
                let acc =
                  List.rev_map
                    (fun {
                           State_project.model_project_id;
                           State_project.model_project_is_computing;
                         } ->
                      let li_class =
                        if
                          match model.State_project.model_current_id with
                          | Some current_project_id ->
                            current_project_id = model_project_id
                          | None -> false
                        then
                          [ "active" ]
                        else
                          []
                      in
                      let span_close =
                        Html.button
                          ~a:[ Html.a_class [ "close" ] ]
                          [ Html.entity "times" ]
                      in
                      let () =
                        (Tyxml_js.To_dom.of_button span_close)##.onclick
                        := Dom.handler (fun event ->
                               let () =
                                 Panel_projects_controller.close_project
                                   model_project_id
                               in
                               let () = Dom_html.stopPropagation event in
                               Js._false)
                      in
                      let computing =
                        let classes =
                          React.S.map
                            (fun b ->
                              if b then
                                [ "glyphicon"; "glyphicon-refresh" ]
                              else
                                [ "glyphicon"; "glyphicon-ok" ])
                            model_project_is_computing
                        in
                        Html.span ~a:[ Tyxml_js.R.Html5.a_class classes ] []
                      in
                      let a_project =
                        Html.a
                          [
                            computing;
                            Html.cdata (" " ^ model_project_id);
                            span_close;
                          ]
                      in
                      let () =
                        (Tyxml_js.To_dom.of_a a_project)##.onclick
                        := Dom.handler (fun _ ->
                               let () =
                                 Panel_projects_controller.set_project
                                   model_project_id
                               in
                               Js._true)
                      in
                      Html.li ~a:[ Html.a_class li_class ] [ a_project ])
                    model.State_project.model_catalog
                in
                List.rev_append acc [ li_new; li_prefs ])
              State_project.model));
      Ui_common.create_modal ~id:project_id_modal_id ~title_label:"New Project"
        ~body:
          [
            [%html
              {|<div class="input-group">|} [ project_id_input ] {|</div>|}];
          ]
        ~submit_label:"Create Project"
        ~submit:
          (Dom.handler (fun _ ->
               let settings_client_id : string =
                 Js.to_string project_id_input_dom##.value
               in
               let () =
                 Panel_projects_controller.create_project settings_client_id
               in
               let () =
                 Common.modal ~id:("#" ^ project_id_modal_id) ~action:"hide"
               in
               Js._false));
    ]

let onload () =
  let () = Modal_preferences.onload () in
  let () =
    Common.jquery_on
      ("#" ^ project_id_modal_id)
      "shown.bs.modal"
      (Dom_html.handler (fun _ ->
           let () = project_id_input_dom##focus in
           Js._false))
  in
  let () =
    (Tyxml_js.To_dom.of_span li_new)##.onclick
    := Dom.handler (fun _ ->
           let () =
             Common.modal ~id:("#" ^ project_id_modal_id) ~action:"show"
           in
           Js._false)
  in
  ()

let onresize () = ()
