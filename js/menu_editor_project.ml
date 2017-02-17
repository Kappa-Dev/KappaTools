(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

module Html = Tyxml_js.Html5

let project_id_modal_id = "menu-editor-project-id-modal"
let project_id_input_id = "menu-editor-project-id-input"
let project_id_button_id = "menu-editor-project-id-button"

let project_new_li_id = "menu-editor-project-new-li"
let project_close_li_id = "menu-editor-project-close-li"

let project_id_input =
  Html.input
    ~a:[ Html.a_id project_id_input_id ;
         Html.a_input_type `Text ;
         Html.a_class [ "form-control" ];
         Html.a_placeholder "project new" ;
         Html.a_size 40;
       ] ()

let project_button =
  Html.button
    ~a:[ Html.a_id project_id_button_id ;
         Html.a_class [ "btn" ; "btn-primary"; ] ]
    [ Html.pcdata "Create Project" ; ]


let dropdown (model : State_project.model) =
  (* directories *)
  let hide_on_empty l =
    (match model.State_project.model_project_ids with
     | [] ->  []
     | _::_ -> l)
  in
  let project_li =
    List.map
      (fun (project_id : Api_types_j.project_id) ->
         let current_project_id = model.State_project.model_current in
         let li_class =
           if current_project_id = Some project_id  then
             [ "active" ]
           else
             []
         in
         Html.li
           ~a:[ Html.a_class li_class ;
                Html.Unsafe.string_attrib "data-project-id" project_id ; ]
           [ Html.a
               ~a:[ Html.Unsafe.string_attrib "data-project-id" project_id ; ]
               [ Html.cdata project_id ] ]
      )
      model.State_project.model_project_ids
  in
  let separator_li =
    hide_on_empty
      [ Html.li
          ~a:[ Html.Unsafe.string_attrib "role" "separator" ;
               Html.a_class [ "divider" ] ;
             ] [ ] ]
  in
  let new_li =
    [ Html.li
        [ Html.a
            ~a:[ Html.a_id project_new_li_id ]
            [ Html.cdata "New" ] ] ]

  in
  let close_li =
    hide_on_empty
      [ Html.li
          [ Html.a
              ~a:[ Html.a_id project_close_li_id ]
              [ Html.cdata "Close" ] ] ]
  in
  project_li @ separator_li @ new_li @ close_li

let content () : [> Html_types.div ] Tyxml_js.Html5.elt list =
  let li_list, li_handle = ReactiveData.RList.create [] in
  let _ =
    React.S.bind
      State_project.model
      (fun model ->
         let () =
           ReactiveData.RList.set
             li_handle
             (dropdown model)
         in
         React.S.const ())
  in
  [ Html.div
      ~a:[ Html.a_class [ "btn-group" ] ;
           Html.Unsafe.string_attrib "role" "group" ; ]
      [ Html.button
          ~a:[ Html.Unsafe.string_attrib "type" "button" ;
               Html.a_class [ "btn btn-default"; "dropdown-toggle" ] ;
               Html.Unsafe.string_attrib "data-toggle" "dropdown" ;
               Html.Unsafe.string_attrib "aria-haspopup" "true" ;
               Html.Unsafe.string_attrib "aria-expanded" "false" ;
             ]
          [ Html.pcdata "Project" ;
            Html.span ~a:[ Html.a_class ["caret"]] [ ]
          ] ;
        Tyxml_js.R.Html.ul
          ~a:[ Html.a_class [ "dropdown-menu" ] ]
          li_list ;
        Ui_common.create_modal
          ~id:project_id_modal_id
          ~title_label:"New Project"
          ~buttons:[project_button]
          ~body:[[%html
                  {|<div class="input-group">|}[project_id_input]{|</div>
                                                                 |}] ;
                ]
      ]
  ]


let onload () =
  let project_id_input_dom =
    Tyxml_js.To_dom.of_input project_id_input
  in
  let () =
    Common.jquery_on
      ("#"^project_new_li_id)
      "click"
      (Dom_html.handler
         (fun _ ->
            let () =
              Common.modal
                ~id:("#"^project_id_modal_id)
                ~action:"show"
            in
            Js._false)) in
  let () =
    Common.jquery_on
      ("#"^project_close_li_id)
      "click"
      (Dom_html.handler
         (fun _ ->
            let () = Subpanel_editor_controller.close_project () in
            Js._false)) in
  let () =
    Common.jquery_on
      ("#"^project_id_button_id)
      "click"
      (Dom_html.handler
         (fun _ ->
            let settings_client_id : string =
              Js.to_string project_id_input_dom##.value
            in
            let () =
              Subpanel_editor_controller.create_project settings_client_id
            in
            let () =
              Common.modal
                ~id:("#"^project_id_modal_id)
                ~action:"hide"
            in
            Js._false)) in
  let () =
    Common.jquery_on
      ("li[data-project-id]")
      "click"
      (Dom_html.handler
         (fun (event : Dom_html.event Js.t)  ->
            let target : Dom_html.element Js.t Js.opt = event##.target in
            let runtime_id : Js.js_string Js.t Js.opt =
              Js.Opt.bind
                target
                (fun (element : Dom_html.element Js.t) ->
                   Common.element_data element "project-id")
            in
            let () =
              Js.Opt.case
                runtime_id
                (fun _ -> ())
                (fun runtime_id ->
                   Subpanel_editor_controller.set_project
                     (Js.to_string runtime_id))
            in
            Js._false))
  in
  ()
