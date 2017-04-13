(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let simulation_options_modal_id = "simulation_options_modal"

let configuration_seed_input_id = "simulation_seed_input"

let option_seed_input =
  Html.input ~a:[
    Html.a_id configuration_seed_input_id;
    Html.a_input_type `Number;
    Html.a_class ["form-control"];
  ] ()
let option_withtrace =
  Html.input ~a:[
    Html.a_input_type `Checkbox;
    Tyxml_js.R.filter_attrib (Html.a_checked ())
      (React.S.map
         (fun s -> s.State_project.model_parameters.State_project.store_trace)
         State_project.model);
  ] ()

let options_modal =
  Ui_common.create_modal
    ~id:simulation_options_modal_id
    ~title_label:"Simulation Configuration"
    ~body:[%html
            {|<div class="row">
                   <div class="col-md-1"><label for="|}configuration_seed_input_id{|">Seed</label></div>
                   <div class="col-md-5">|}[option_seed_input]{|</div>
              </div>
              <div class="row">
                <div class="col-md-offset-1 col-md-5 checkbox">
                  <label>|}[option_withtrace]{|Store trace</label>
                </div></div>|}]
    ~submit_label:"Save"
    ~submit:
      (Dom_html.handler
         (fun (_ : Dom_html.event Js.t)  ->
            let input : Dom_html.inputElement Js.t =
              Tyxml_js.To_dom.of_input option_seed_input in
            let value : string = Js.to_string input##.value in
            let model_seed =
              try Some (int_of_string value) with Failure _ -> None in
            let () = State_project.set_seed model_seed in
            let () = State_project.set_store_trace
                (Js.to_bool
                   (Tyxml_js.To_dom.of_input option_withtrace)##.checked) in
            let () =
              Common.modal
                ~id:("#"^simulation_options_modal_id)
                ~action:"hide"
            in
            Js._false))

let options =
  Html.button
    ~a:[ Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn"; "btn-default" ] ]
    [ Html.cdata "Simulation options" ]

let content () =
  [ options ; options_modal]

let onload () =
  let () =
    (Tyxml_js.To_dom.of_button options)##.onclick :=
      Dom_html.handler
        (fun _  ->
           let input : Dom_html.inputElement Js.t =
             Tyxml_js.To_dom.of_input option_seed_input in
           let () = input##.value := Js.string
                 (match (React.S.value State_project.model).
                          State_project.model_parameters.State_project.seed with
                 | None -> ""
                 | Some model_seed -> string_of_int model_seed) in
           let () =
             Common.modal
               ~id:("#"^simulation_options_modal_id)
               ~action:"show"
           in
           Js._false) in
  ()
