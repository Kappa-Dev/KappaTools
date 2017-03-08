(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let simulation_new_modal_id = "menu-editor-simulation-new-modal"
let simulation_new_input_id = "menu-editor-simulation-new-input"
let simulation_new_button_id = "menu-editor-simulation-new-button"
let simulation_dropdown_menu_id = "menu-editor-simulation-dropdown-menu"

  let simulation_new_li_id = "menu-editor-simulation-new-li"
  let simulation_close_li_id = "menu-editor-simulation-close-li"

  (* list simulation_id annotation *)
  let element_get_simulation_id
      (element : Dom_html.element Js.t) : Js.js_string Js.t Js.opt=
    Common.element_data
      (element : Dom_html.element Js.t)
      "simulation-id"
  let element_set_simulation_id
      (name : string) =
    Html.Unsafe.string_attrib
      "data-simulation-id"
      name

  let simulation_new_input =
    Html.input
      ~a:[ Html.a_id simulation_new_input_id ;
           Html.a_input_type `Text ;
           Html.a_class [ "form-control" ];
           Html.a_placeholder "simulation name" ;
           Html.a_size 40;
         ] ()

  let simulation_button =
    Html.button
      ~a:[ Html.a_id simulation_new_button_id ;
           Html.a_class [ "btn" ; "btn-primary"; ] ]
      [ Html.pcdata "Create Simulation" ; ]

  let dropdown (model : State_simulation.model) =
    (* directories *)
    let hide_on_empty l =
      (match model.State_simulation.model_simulations with
       | [] ->  []
       | _::_ -> l)
    in
    let simulation_li =
      List.map
        (fun (simulation_id : Api_types_j.simulation_id) ->
           let current_simulation_id : Api_types_j.simulation_id option =
             Option_util.map
               State_simulation.t_simulation_id
               model.State_simulation.model_current in
           let li_class =
             if current_simulation_id = Some simulation_id  then
               [ "active" ]
             else
               []
           in
           Html.li
             ~a:[ Html.a_class li_class ;
                  element_set_simulation_id simulation_id ; ]
             [ Html.a ~a:[ element_set_simulation_id simulation_id ; ]
                 [ Html.cdata simulation_id ] ] ;
        )
       model.State_simulation.model_simulations
    in
    let separator_li =
      hide_on_empty
        [ Html.li
            ~a:[ Html.Unsafe.string_attrib "role" "separator" ;
                 Html.a_class [ "divider" ;
                                "ui-sort-disabled" ;
                                "ui-sort-bottom-anchor" ; ] ;
               ] [ ] ]
    in
    let new_li =
      [ Html.li
          ~a:[ Html.a_class [ "ui-sort-disabled" ;
                              "ui-sort-bottom-anchor" ; ] ; ]
          [ Html.a
              ~a:[ Html.a_id simulation_new_li_id ; ]
              [ Html.cdata "New" ] ] ]

    in
    let close_li =
     hide_on_empty
       [ Html.li
           ~a:[ Html.a_class [ "ui-sort-disabled" ;
                               "ui-sort-bottom-anchor" ; ] ; ]
           [ Html.a
               ~a:[ Html.a_id simulation_close_li_id ; ]
               [ Html.cdata "Close" ] ] ]
    in
    simulation_li @ separator_li @ new_li @ close_li

  let content () =
    let li_list, li_handle = ReactiveData.RList.create [] in
    let _ =
      React.S.bind
        State_simulation.model
        (fun model ->
           let () = ReactiveData.RList.set li_handle (dropdown model) in
           React.S.const ())
    in
    [ Html.button
        ~a:[ Html.Unsafe.string_attrib "type" "button" ;
             Html.a_class [ "btn btn-default"; "dropdown-toggle" ] ;
             Html.Unsafe.string_attrib "data-toggle" "dropdown" ;
             Html.Unsafe.string_attrib "aria-haspopup" "true" ;
             Html.Unsafe.string_attrib "aria-expanded" "false" ;
             (Tyxml_js.R.filter_attrib
                (Html.a_disabled ())
                (React.S.map
                   (fun model ->
                      match model.State_project.model_project_id with
                      | Some _ -> false
                      | None -> true)
                   State_project.model
                )
             );
           ]
        [ Html.pcdata "Simulation" ;
          Html.span ~a:[ Html.a_class ["caret"]] [ ]
        ] ;
      Tyxml_js.R.Html.ul
        ~a:[ Html.a_id simulation_dropdown_menu_id ;
             Html.a_class [ "dropdown-menu" ] ]
        li_list ;
      Ui_common.create_modal
        ~id:simulation_new_modal_id
        ~title_label:"New Simulation"
        ~buttons:[simulation_button]
        ~body:[[%html
                {|<div class="input-group">|}[simulation_new_input]{|</div>|}] ;
              ] ]

  let onload () =
    let simulation_new_input_dom =
    Tyxml_js.To_dom.of_input simulation_new_input
    in
    let () =
      Common.jquery_on
        ("#"^simulation_new_li_id)
        "click"
        (Dom_html.handler
           (fun _ ->
              let () =
                Common.modal
                  ~id:("#"^simulation_new_modal_id)
                  ~action:"show"
              in
              Js._false)) in
    let () =
      Common.jquery_on
        ("#"^simulation_close_li_id)
        "click"
        (Dom_html.handler
           (fun _ ->
              let () = Menu_editor_simulation_controller.close_simulation () in
              Js._false)) in
    let () =
      Common.jquery_on
        ("#"^simulation_new_button_id)
        "click"
        (Dom_html.handler
           (fun _ ->
              let simulation_id : string =
                Js.to_string simulation_new_input_dom##.value
              in
              let () =
                Menu_editor_simulation_controller.create_simulation simulation_id
              in
              let () =
                Common.modal
                  ~id:("#"^simulation_new_modal_id)
                  ~action:"hide"
              in
              Js._false)) in
    let () =
      Common.jquery_on
        ("li[data-simulation-id]")
        "click"
        (Dom_html.handler
           (fun (event : Dom_html.event Js.t)  ->
              let target : Dom_html.element Js.t Js.opt = event##.target in
              let simulation_id : Js.js_string Js.t Js.opt =
                Js.Opt.bind
                  target
                  (fun (element : Dom_html.element Js.t) ->
                     element_get_simulation_id element)
              in
              let () =
                Js.Opt.case
                  simulation_id
                  (fun _ -> ())
                  (fun simulation_id ->
                     Menu_editor_simulation_controller.set_simulation
                       (Js.to_string simulation_id))
              in
              Js._false))
    in
    ()
