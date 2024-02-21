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
let settings_client_id_input_id = "settings-client-id-input"
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
        Html.a_button_type `Button;
        Html.a_class [ "btn"; "btn-default"; "btn-sm" ];
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

let settings_client_id_input =
  Html.input
    ~a:
      [
        Html.a_id settings_client_id_input_id;
        Html.a_input_type `Text;
        Html.a_class [ "form-control" ];
        Html.a_placeholder "client id";
        Html.a_size 40;
      ]
    ()

let settings_client_id_input_dom =
  Tyxml_js.To_dom.of_input settings_client_id_input

let option_http_synch = Html.input ~a:[ Html.a_input_type `Checkbox ] ()

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
    [ decrease_font; increase_font ]
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
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_withtrace ]
    {|Store trace
    </label></div>
    </div>
    <h4>HTTPS backend</h4>
    <div class="form-group">
    <label class="col-md-2" for="|}
    settings_client_id_input_id
    {|">Client id</label>
    <div class="col-md-5">|}
    [ settings_client_id_input ]
    {|</div>
    </div>
    <div class="form-group">
    <div class="col-md-offset-2 col-md-5 checkbox"><label>|}
    [ option_http_synch ]
    {|Auto synch
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
  let settings_client_id = Js.to_string settings_client_id_input_dom##.value in
  let () = State_settings.set_client_id settings_client_id in

  let synch_checkbox_dom = Tyxml_js.To_dom.of_input option_http_synch in
  let is_checked = Js.to_bool synch_checkbox_dom##.checked in
  let () = State_settings.set_synch is_checked in

  let input = Tyxml_js.To_dom.of_input option_seed_input in
  let value : string = Js.to_string input##.value in
  let model_seed = try Some (int_of_string value) with Failure _ -> None in
  let () = State_project.set_seed model_seed in

  let () =
    State_project.set_store_trace
      (Js.to_bool (Tyxml_js.To_dom.of_input option_withtrace)##.checked)
  in
  let () =
    State_project.set_show_dead_rules
      (Js.to_bool (Tyxml_js.To_dom.of_input option_withdeadrules)##.checked)
  in
  let () =
    State_project.set_show_dead_agents
      (Js.to_bool (Tyxml_js.To_dom.of_input option_withdeadagents)##.checked)
  in
  let () =
    State_project.set_show_non_weakly_reversible_transitions
      (Js.to_bool (Tyxml_js.To_dom.of_input option_withirreversible)##.checked)
  in

  let () =
    Panel_projects_controller.set_manager
      (Js.to_string (Tyxml_js.To_dom.of_select backend_select)##.value)
  in
  ()

let set_and_save_action () =
  let () = set_action () in

  let () = State_settings.set_parameters_as_default () in
  let () = State_project.set_parameters_as_default () in

  ()

let onload () =
  let () =
    (Tyxml_js.To_dom.of_form modal)##.onsubmit
    := Dom_html.handler (fun (_ : _ Js.t) ->
           let () =
             Common.modal ~id:("#" ^ preferences_modal_id) ~action:"hide"
           in
           let () = set_action () in
           Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_button save_button)##.onclick
    := Dom_html.handler (fun _ ->
           let () = set_and_save_action () in
           Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_a preferences_button)##.onclick
    := Dom_html.handler (fun _ ->
           let sp = React.S.value State_project.model in
           let () =
             settings_client_id_input_dom##.value
             := Js.string (State_settings.get_client_id ())
           in

           let input = Tyxml_js.To_dom.of_input option_seed_input in
           let () =
             input##.value :=
               Js.string
                 (match
                    sp.State_project.model_parameters.State_project.seed
                  with
                 | None -> ""
                 | Some model_seed -> string_of_int model_seed)
           in

           let () =
             (Tyxml_js.To_dom.of_input option_withtrace)##.checked
             := Js.bool
                  sp.State_project.model_parameters.State_project.store_trace
           in
           let () =
             (Tyxml_js.To_dom.of_input option_withdeadagents)##.checked
             := Js.bool
                  sp.State_project.model_parameters
                    .State_project.show_dead_agents
           in
           let () =
             (Tyxml_js.To_dom.of_input option_withdeadrules)##.checked
             := Js.bool
                  sp.State_project.model_parameters
                    .State_project.show_dead_rules
           in
           let () =
             (Tyxml_js.To_dom.of_input option_withirreversible)##.checked
             := Js.bool
                  sp.State_project.model_parameters
                    .State_project.show_non_weakly_reversible_transitions
           in

           let () =
             (Tyxml_js.To_dom.of_input option_http_synch)##.checked
             := Js.bool (React.S.value State_settings.synch)
           in
           let () =
             (Tyxml_js.To_dom.of_select backend_select)##.value
             := Js.string
                  (State_runtime.spec_id
                     (React.S.value State_runtime.model)
                       .State_runtime.model_current)
           in

           let () =
             Common.modal ~id:("#" ^ preferences_modal_id) ~action:"show"
           in

           Js._false)
  in

  let () = State_settings.updateFontSize ~delta:0. in
  let () =
    (Tyxml_js.To_dom.of_button increase_font)##.onclick
    := Dom_html.handler (fun _ ->
           let () = State_settings.updateFontSize ~delta:0.2 in
           Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_button decrease_font)##.onclick
    := Dom_html.handler (fun _ ->
           let () = State_settings.updateFontSize ~delta:(-0.2) in
           Js._false)
  in
  ()
