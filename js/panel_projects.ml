(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let project_id_modal_id = "menu-editor-project-id-modal"

let project_id_input =
  Html.input
    ~a:[ Html.a_input_type `Text ;
         Html.a_class [ "form-control" ];
         Html.a_placeholder "project new" ;
         Html.a_size 40;
       ] ()

let new_project_button =
  Html.button
    ~a:[ Html.a_class [ "btn" ; "btn-primary"; ] ]
    [ Html.pcdata "Create Project" ; ]

let li_new =
  Html.li [ Html.a [ Html.cdata "New" ] ]

let content () =
  Html.div
    [Tyxml_js.R.Html5.ul
       ~a:[Html.a_class [ "nav"; "nav-tabs"; "nav-justified"]]
       (ReactiveData.RList.from_signal
          (React.S.map
             (fun model ->
                let acc =
                  List.rev_map
                    (fun project_id ->
                       let li_class =
                         if match model.State_project.model_project_id with
                           | Some current_project_id ->
                             current_project_id = project_id
                           | None -> false then
                           [ "active" ]
                         else
                           [] in
                       let span_close = Html.button
                           ~a:[Html.a_class ["close"]] [ Html.cdata "×" ] in
                       let () = (Tyxml_js.To_dom.of_button span_close)##.onclick :=
                           Dom.handler
                             (fun _ ->
                                let () = Panel_projects_controller.close_project
                                    project_id in
                                Js._true) in
                       let a_project =
                         Html.a [ Html.cdata project_id ; span_close] in
                       let () = (Tyxml_js.To_dom.of_a a_project)##.onclick :=
                           Dom.handler
                             (fun _ ->
                                let () = Panel_projects_controller.set_project
                                    project_id in
                                Js._true) in
                       Html.li ~a:[ Html.a_class li_class ] [a_project])
                    model.State_project.model_project_ids in
                List.rev_append acc [li_new])
             State_project.model));
     Ui_common.create_modal
       ~id:project_id_modal_id
       ~title_label:"New Project"
       ~buttons:[new_project_button]
       ~body:[[%html
               {|<div class="input-group">|}[project_id_input]{|</div>|}] ;
             ]
    ]

let onload () =
  let project_id_input_dom =
    Tyxml_js.To_dom.of_input project_id_input in
  let () =
    (Tyxml_js.To_dom.of_button new_project_button)##.onclick :=
      Dom.handler
        (fun _ ->
           let settings_client_id : string =
             Js.to_string project_id_input_dom##.value in
           let () =
             Panel_projects_controller.create_project settings_client_id in
           let () =
             Common.modal
               ~id:("#"^project_id_modal_id) ~action:"hide" in
           Js._false) in
  let () = (Tyxml_js.To_dom.of_span li_new)##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.modal ~id:("#"^project_id_modal_id) ~action:"show" in
           Js._false) in
  ()

let onresize () = ()
