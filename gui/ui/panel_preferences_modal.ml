(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let configuration_seed_input_id = "simulation_seed_input"
let preferences_modal_id = "preferences_modal"
let preferences_button = Html.a [ Html.txt "Preferences" ]

let option_seed_input =
  Html.input
    ~a:
      [
        Html.a_id configuration_seed_input_id;
        Html.a_input_type `Number;
        Html.a_class [ "form-control" ];
      ]
    ()

let option_withtrace = Html.input ~a:[ Html.a_input_type `Checkbox ] ()
let option_withdeadrules = Html.input ~a:[ Html.a_input_type `Checkbox ] ()
let option_withdeadagents = Html.input ~a:[ Html.a_input_type `Checkbox ] ()
let option_withirreversible = Html.input ~a:[ Html.a_input_type `Checkbox ] ()

let decrease_font =
  Html.button
    ~a:
      [
        Html.a_button_type `Button; Html.a_class [ "btn"; "btn-default"; "btn" ];
      ]
    [ Html.txt "-" ]

let increase_font =
  Html.button
    ~a:
      [
        Html.a_button_type `Button;
        Html.a_class [ "btn"; "btn-default"; "btn-lg" ];
      ]
    [ Html.txt "+" ]

let dropdown (model : State_runtime.model) =
  let current_id = State_runtime.spec_id model.State_runtime.model_current in
  List.map
    (fun (spec : State_runtime.spec) ->
      let spec_id = State_runtime.spec_id spec in
      Html.option
        ~a:
          (Html.a_value spec_id
          ::
          (if current_id = spec_id then
             [ Html.a_selected () ]
           else
             []))
        (Html.txt (State_runtime.spec_label spec)))
    model.State_runtime.model_runtimes

let backend_options =
  ReactiveData.RList.from_signal
    (React.S.map (fun list_t -> dropdown list_t) State_runtime.model)

let backend_select =
  Tyxml_js.R.Html.select ~a:[ Html.a_class [ "form-control" ] ] backend_options

let%html bodies =
  {|
    <h4>Application</h4>
    <div class="form-group">
    <label class="col-md-2">Font size</label>
    <div class="col-md-5">|}
    [ decrease_font; Html.txt "  "; increase_font ]
    {|</div>
    </div>
    <div class="form-group">
    <label class="col-md-2">Backend for new projects</label>
    <div class="col-md-5">|}
    [ backend_select ]
    {|</div>
    </div>
    <h4>Project</h4>
    <div class="form-group">
    <label class="col-md-2" for="|}
    configuration_seed_input_id
    {|">Seed</label>
    <div class="col-md-5">|}
    [ option_seed_input ]
    {|</div>
    </div>
    <h4>Stories</h4>
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_withtrace ]
    {|Store trace
    </label></div>
    </div>
    <h4>Static analyses</h4>
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_withdeadrules ]
    {|Show dead rules
    </label></div>
    </div>
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_withdeadagents ]
    {|Show dead agents
    </label></div>
    </div>
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_withirreversible ]
    {|Show non weakly reversible transitions
    </label></div>
    </div>
|}

let set_button =
  Html.button
    ~a:[ Html.a_button_type `Submit; Html.a_class [ "btn"; "btn-primary" ] ]
    [ Html.txt "Set" ]

let save_button =
  Html.button
    ~a:[ Html.a_button_type `Button; Html.a_class [ "btn"; "btn-default" ] ]
    [ Html.txt "Save as default" ]

let modal =
  let head =
    Html.div
      ~a:[ Html.a_class [ "modal-header" ] ]
      [
        Html.button
          ~a:
            [
              Html.a_button_type `Button;
              Html.a_class [ "close" ];
              Html.a_user_data "dismiss" "modal";
            ]
          [ Html.entity "times" ];
        Html.h4 ~a:[ Html.a_class [ "modal-title" ] ] [ Html.txt "Preferences" ];
      ]
  in
  let body = Html.div ~a:[ Html.a_class [ "modal-body" ] ] bodies in
  let foot =
    Html.div
      ~a:[ Html.a_class [ "modal-footer" ] ]
      [
        set_button;
        save_button;
        Html.button
          ~a:
            [
              Html.a_button_type `Button;
              Html.a_class [ "btn"; "btn-default" ];
              Html.a_user_data "dismiss" "modal";
            ]
          [ Html.txt "Close" ];
      ]
  in
  Html.form
    ~a:[ Html.a_class [ "modal-content"; "form-horizontal" ] ]
    [ head; body; foot ]

let content () =
  [
    preferences_button;
    Html.div
      ~a:
        [
          Html.a_class [ "modal"; "fade" ];
          Html.a_id preferences_modal_id;
          Html.a_role [ "dialog" ];
          Html.a_tabindex (-1);
        ]
      [
        Html.div
          ~a:[ Html.a_class [ "modal-dialog" ]; Html.a_role [ "document" ] ]
          [ modal ];
      ];
  ]

let set_action () =
  let input = Tyxml_js.To_dom.of_input option_seed_input in
  let input_value : string = Js.to_string input##.value in
  let model_seed =
    try Some (int_of_string input_value) with Failure _ -> None
  in
  State_project.set_seed model_seed;
  State_project.set_store_trace
    (Js.to_bool (Tyxml_js.To_dom.of_input option_withtrace)##.checked);
  State_project.set_show_dead_rules
    (Js.to_bool (Tyxml_js.To_dom.of_input option_withdeadrules)##.checked);
  State_project.set_show_dead_agents
    (Js.to_bool (Tyxml_js.To_dom.of_input option_withdeadagents)##.checked);
  State_project.set_show_non_weakly_reversible_transitions
    (Js.to_bool (Tyxml_js.To_dom.of_input option_withirreversible)##.checked);

  Panel_projects_controller.set_manager
    (Js.to_string (Tyxml_js.To_dom.of_select backend_select)##.value)

let set_and_save_action () =
  set_action ();
  State_preferences.set_parameters_as_default ();
  State_project.set_parameters_as_default ()

let onload () =
  (Tyxml_js.To_dom.of_form modal)##.onsubmit
  := Dom_html.handler (fun (_ : _ Js.t) ->
         let () =
           Common.modal ~id:("#" ^ preferences_modal_id) ~action:"hide"
         in
         let () = set_action () in
         Js._false);
  (Tyxml_js.To_dom.of_button save_button)##.onclick
  := Dom_html.handler (fun _ ->
         let () = set_and_save_action () in
         Js._false);
  (Tyxml_js.To_dom.of_a preferences_button)##.onclick
  := Dom_html.handler (fun _ ->
         let sp = React.S.value State_project.model in
         let input = Tyxml_js.To_dom.of_input option_seed_input in
         input##.value :=
           Js.string
             (match sp.State_project.model_parameters.State_project.seed with
             | None -> ""
             | Some model_seed -> string_of_int model_seed);

         (Tyxml_js.To_dom.of_input option_withtrace)##.checked
         := Js.bool sp.State_project.model_parameters.State_project.store_trace;
         (Tyxml_js.To_dom.of_input option_withdeadagents)##.checked
         := Js.bool
              sp.State_project.model_parameters.State_project.show_dead_agents;
         (Tyxml_js.To_dom.of_input option_withdeadrules)##.checked
         := Js.bool
              sp.State_project.model_parameters.State_project.show_dead_rules;
         (Tyxml_js.To_dom.of_input option_withirreversible)##.checked
         := Js.bool
              sp.State_project.model_parameters
                .State_project.show_non_weakly_reversible_transitions;

         (Tyxml_js.To_dom.of_select backend_select)##.value
         := Js.string
              (State_runtime.spec_id
                 (React.S.value State_runtime.model).State_runtime.model_current);

         Common.modal ~id:("#" ^ preferences_modal_id) ~action:"show";

         Js._false);

  let () = State_preferences.updateFontSize ~delta:0. in
  (Tyxml_js.To_dom.of_button increase_font)##.onclick
  := Dom_html.handler (fun _ ->
         let () = State_preferences.updateFontSize ~delta:0.2 in
         Js._false);
  (Tyxml_js.To_dom.of_button decrease_font)##.onclick
  := Dom_html.handler (fun _ ->
         let () = State_preferences.updateFontSize ~delta:(-0.2) in
         Js._false)
