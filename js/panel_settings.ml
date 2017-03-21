(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
module R = Tyxml_js.R

let visible_on_states
    ?(a_class=[])
    (state : State_simulation.model_state list) : string list React.signal =
  let hidden_class = ["hidden"] in
  let visible_class = ["visible"] in
  React.S.bind
    State_simulation.model
    (fun model ->
       let current_state = State_simulation.model_simulation_state model.State_simulation.model_current in
       React.S.const
         (match current_state with
          | None -> a_class@hidden_class
          | Some current_state ->
            if List.mem current_state state then
              a_class@visible_class
            else
              a_class@hidden_class))

module FormPerturbation : Ui_common.Div = struct
  let id = "panel_settings_perturbation"
  let input =
    Html.input
      ~a:[Html.a_input_type `Text;
          Html.a_class ["form-control"];
          Html.a_placeholder "Simulation Perturbation";]
      ()
  let button =
    Html.button
      ~a:[ Html.a_button_type `Submit
         ; Html.a_class ["btn"; "btn-default" ] ]
      [ Html.cdata "perturbation" ]
  let form = Html.form ~a:
      [Tyxml_js.R.Html.a_class
         (visible_on_states
            ~a_class:[ "form-horizontal" ]
            [ State_simulation.PAUSED ; ])]
      [ Html.div ~a:[ Html.a_class [ "form-group" ]]
          [ Html.div ~a:[ Html.a_class ["col-md-10"; "col-xs-9"]] [input];
            Html.div ~a:[ Html.a_class ["col-md-2"; "col-xs-3"]] [button] ] ]

  let content () = [ form ]
   let onload () : unit =
     let form_dom = Tyxml_js.To_dom.of_form form in
     let input_dom = Tyxml_js.To_dom.of_input input in
     let handler =
       (fun _ ->
          let model_perturbation : string = Js.to_string input_dom##.value in
          let () =
            State_perturbation.set_model_perturbation model_perturbation in
          Js._true)
     in

     let () = form_dom##.onsubmit :=
         Dom.handler (fun _ ->
             let () = Panel_settings_controller.perturb_simulation () in
             Js._false) in
     let () = input_dom##.onchange := Dom.handler handler in
     ()

end

let signal_change input_dom signal_handler =
  input_dom##.onchange :=
    Dom_html.handler
      (fun _ -> let () = signal_handler (Js.to_string (input_dom##.value)) in
        Js._true)

module InputPauseCondition : Ui_common.Div = struct
  let id = "panel_settings_pause_condition"
  let input =
    Html.input
      ~a:[Html.a_id id ;
        Html.a_input_type `Text;
          Html.a_class ["form-control"];
          Html.a_placeholder "[T] > 100" ;
          Tyxml_js.R.Html.a_value State_parameter.model_pause_condition ]
    ()
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [input]

  let dom = Tyxml_js.To_dom.of_input input

  let onload () =
    let () = signal_change dom
        (fun value ->
           let v' = if value = "" then "[false]" else value in
           State_parameter.set_model_pause_condition v') in
    ()
end

module InputPlotPeriod : Ui_common.Div = struct
  let id = "panel_settings_plot_period"
let format_float_string value =
  let n = string_of_float value in
  let length = String.length n in
  if length > 0 && String.get n (length - 1) = '.' then
    n^"0"
  else
    n

let input =
  Html.input
    ~a:[Html.a_input_type `Number;
        Html.a_id id;
        Html.a_class [ "form-control"];
        Html.a_placeholder "time units";
        Html.Unsafe.string_attrib "min" (string_of_float epsilon_float);
        Tyxml_js.R.Html.a_value
          (React.S.l1 format_float_string State_parameter.model_plot_period)]
    ()
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [input]

  let onload () =
    let input_dom = Tyxml_js.To_dom.of_input input in
    let () = signal_change input_dom
        (fun value ->
           let old_value = React.S.value State_parameter.model_plot_period in
           let reset_value () = input_dom##.value := Js.string (string_of_float old_value) in
           try
             let new_value = (float_of_string value) in
             if new_value > 0.0 then
               State_parameter.set_model_plot_period new_value
             else
               reset_value ()
         with | Not_found | Failure _ -> reset_value ()) in
    ()

end

module DivErrorMessage : Ui_common.Div = struct
  let id = "configuration_error_div"
  (* TODO : [%html {|<div class="alert-sm alert alert-danger"> « 1/2 » [abc.ka] Malformed agent 'adfsa' </div>|}] *)
  let message_label (message : Api_types_j.message) (index : int) (length : int) : string =
    (Format.sprintf  "%d/%d %s %s" index length
       (match message.Api_types_j.message_range with
        | None -> ""
        | Some range -> Format.sprintf "[ %s ]" range.Api_types_j.file)
       message.Api_types_j.message_text)
  let alert_messages =
  Html.div
    ~a:[Html.a_id id;
        Tyxml_js.R.Html.a_class
          (React.S.bind
             State_error.errors
             (fun error ->
                React.S.const
                  (match error with
                   | None -> [ "alert-sm" ; "alert" ; ]
                   | Some _ -> [ "alert-sm" ; "alert" ; "alert-danger" ; ]
                  )
             )
          );
       ]
    [Tyxml_js.R.Html.pcdata
       (React.S.bind
          State_error.errors
          (fun error ->
             React.S.const
               (match error with
                | None -> ""
                | Some errors ->
                  (match errors with
                   | [] -> ""
                   | h::_ -> message_label h 1 (List.length errors))
               )
          )
       )
    ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ alert_messages ]

  let onload () = ()
end

module ButtonStart : Ui_common.Div = struct
  let id = "panel_settings_start_button"
  let button =
    Html.button
      ~a:([ Html.a_id id ;
            Html.Unsafe.string_attrib "type" "button" ;
            Html.a_class [ "btn" ; "btn-default" ; ];
            (Tyxml_js.R.filter_attrib
               (Html.a_disabled ())
               Subpanel_editor.is_paused
            );

          ]
         )
      [ Html.cdata "start" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [button]

  let onload () =
    let start_button_dom = Tyxml_js.To_dom.of_button button in
    let () = start_button_dom##.onclick :=
        Dom.handler
          (fun _ ->
             let () = Panel_settings_controller.start_simulation () in
             Js._true)
    in

    ()
end

module ButtonClear : Ui_common.Div = struct
  let id = "panel_settings_clear_button"
  let button =
  Html.button
    ~a:[ Html.a_id id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "clear" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [button]

  let onload () =
    let dom = Tyxml_js.To_dom.of_button button in
    let () = dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () = Panel_settings_controller.stop_simulation () in
           Js._true)
    in
    ()

end

module ButtonPause : Ui_common.Div = struct
  let id = "panel_settings_pause_button"
  let button =
  Html.button
    ~a:[ Html.a_id id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "pause" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [button]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () = button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () = Panel_settings_controller.pause_simulation () in
           Js._true)
  in
    ()

end

module ButtonContinue : Ui_common.Div = struct
  let id = "panel_settings_continue_button"
  let button =
  Html.button
    ~a:[ Html.a_id id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "continue" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [button]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () = button_dom##.onclick :=
        Dom.handler
          (fun _ ->
             let () = Panel_settings_controller.continue_simulation () in
             Js._true)
    in
    ()

end

module DivStatusIndicator : Ui_common.Div = struct
  let id = "setting_status_indicator"
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    let debug =
      Html.div
        [ Tyxml_js.R.Html.pcdata
            (React.S.bind
               State_simulation.model
               (fun model ->
                  let option =
                    Option_util.map
                      State_simulation.model_state_to_string
                      (State_simulation.model_simulation_state
                     model.State_simulation.model_current)
                  in
                  let label = match option with None -> "None" | Some l -> l in
                  React.S.const label
               )
            );
          Tyxml_js.R.Html.pcdata
            (React.S.bind
               State_simulation.model
               (function model ->
                React.S.const
                  (match model.State_simulation.model_current with
                   | None -> "None"
                   | Some _ -> "Some"
                  )
               )
            )
        ]
    in
    [ Html.div
        ~a:[ Html.a_id id ]
        (Ui_common.level ~debug ()) ]

  let onload () = ()
end

module RunningPanelLayout : Ui_common.Div = struct
  let id = "settings_runetime_layout"
  let lift f x = match x with | None -> None | Some x -> f x
  let progress_bar
      (percent_signal : int Tyxml_js.R.Html.wrap)
      (value_signal : string React.signal) =
    Html.div
      ~a:[ Html.Unsafe.string_attrib "role" "progressbar" ;
           Tyxml_js.R.Html.Unsafe.int_attrib "aria-valuenow" percent_signal ;
           Html.Unsafe.int_attrib "aria-valuemin" 0 ;
           Html.Unsafe.int_attrib "aria-valuemax" 100 ;
           Tyxml_js.R.Html.Unsafe.string_attrib
           "style"
           (React.S.map
              (fun s -> Format.sprintf "width: %d%%;" s)
              percent_signal) ;
           Html.a_class ["progress-bar"] ]
    [ Tyxml_js.R.Html.pcdata
        (React.S.bind
           value_signal
           (fun value -> React.S.const value)
        )
    ]

  let time_progress_bar  () =
    progress_bar
      (React.S.map
         (fun model ->
            let simulation_info = State_simulation.model_simulation_info model in
            let time_percent : int option =
              lift
                (fun (status : Api_types_j.simulation_info) ->
                   status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time_percentage )
                simulation_info
            in
            let time_percent : int = Option_util.unsome 100 time_percent in
            time_percent
         )
         State_simulation.model)
      (React.S.map (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let time : float option =
             lift (fun (status : Api_types_j.simulation_info) ->
                 Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time) simulation_info in
           let time : float = Option_util.unsome 0.0 time in
           string_of_float time
         )
          State_simulation.model)

  let event_progress_bar () =
    progress_bar
      (React.S.map (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let event_percentage : int option =
             lift (fun (status : Api_types_j.simulation_info) ->
                 status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event_percentage) simulation_info in
           let event_percentage : int = Option_util.unsome 100 event_percentage in
           event_percentage
         )
          State_simulation.model)
      (React.S.map (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let event : int option =
             lift (fun (status : Api_types_j.simulation_info) ->
                 Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event)
               simulation_info
           in
           let event : int = Option_util.unsome 0 event in
           string_of_int event
         )
          State_simulation.model)

  let tracked_events state =
    let tracked_events : int option =
      lift (fun (status : Api_types_j.simulation_info) ->
        status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_tracked_events)
        state
    in
    match tracked_events with
      None -> None
  | Some tracked_events ->
    if tracked_events > 0 then
      Some tracked_events
    else
      None

  let tracked_events_count () =
    Tyxml_js.R.Html.pcdata
      (React.S.map
         (fun model ->
            let simulation_info = State_simulation.model_simulation_info model in
            match tracked_events simulation_info with
            | Some tracked_events -> string_of_int tracked_events
            | None -> " "
         )
         State_simulation.model)

  let tracked_events_label () =
    Tyxml_js.R.Html.pcdata
      (React.S.map
         (fun model ->
            let simulation_info = State_simulation.model_simulation_info model in
            match tracked_events simulation_info with
              Some _ -> "tracked events"
            | None -> " "
         )
         State_simulation.model)

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ [%html {|
     <div class="row" id="|}id{|">
        <div class="col-md-4 col-xs-10">
            <div class="progress">
            |}[ event_progress_bar () ]{|
            </div>
        </div>
        <div class="col-md-2 col-xs-2">events</div>
     </div>|}] ;
     [%html {|
     <div class="row">
        <div class="col-md-4 col-xs-10">
            <div class="progress">
            |}[ time_progress_bar () ]{|
            </div>
        </div>
        <div class="col-md-2 col-xs-2">time</div>
     </div>|}] ;
     [%html {|
     <div class="row">
        <div class="col-md-4 col-xs-10">
           |}[ tracked_events_count () ]{|
        </div>
        <div class="col-md-2 col-xs-2">
           |}[ tracked_events_label () ]{|
        </div>
     </div>
   |}] ; ]

  let onload () = ()

end

let stopped_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
  let stopped_row =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "form-group"; "form-group-sm" ]
                [ State_simulation.STOPPED ; ]) ]
    [%html {|
            <label class="col-lg-1 col-md-2 col-xs-2 control-label" for="|}InputPlotPeriod.id{|">Plot period</label>
            <div class="col-md-2 col-xs-3">|}(InputPlotPeriod.content ()){|</div>|}] in
    let paused_row = FormPerturbation.content () in
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ State_simulation.STOPPED ;
                  State_simulation.PAUSED ;]) ]
      ([%html {|
         <form class="form-horizontal">
          <div class="form-group">
            <label class="col-lg-1 col-sm-2 hidden-xs control-label" for="|}InputPauseCondition.id{|">Pause if</label>
            <div class="col-md-2 col-sm-3 col-xs-5">|}(InputPauseCondition.content ()){|</div>
            <div class="col-lg-9 col-md-8 col-xs-7">|}(DivErrorMessage.content ()){|</div>
          </div>|}
          [stopped_row]
          {|</form>|}]::paused_row)

  let initializing_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ State_simulation.INITALIZING ; ]) ]
      [ Html.entity "nbsp" ]

  let running_body () =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ State_simulation.RUNNING ; ]) ]
      (RunningPanelLayout.content ())
let footer () =
  [%html {|
         <div class="panel-footer">
            <div class="row">
         |}[ Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ State_simulation.STOPPED ; ]) ]
               (ButtonStart.content ());
             Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ State_simulation.PAUSED ; ]) ]
               (ButtonContinue.content ());
             Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ State_simulation.RUNNING ; ]) ]
               (ButtonPause.content ());
             Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-3" ]
                    [ State_simulation.PAUSED ;
                      State_simulation.RUNNING ; ]) ]
               (ButtonClear.content ());
             Html.div
               ~a:[ Html.a_class [ "col-md-1"; "col-xs-5" ] ]
               ((DivStatusIndicator.content ())
                @
                [ Html.entity "nbsp" ; ]) ]{|
            </div>
         </div>
  |}]
let content () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (React.S.bind
              State_project.model
              (fun model ->
                 match model.State_project.model_project_id with
                 | None -> React.S.const [ "hide" ]
                 | Some _ -> React.S.const [ "panel"; "panel-default" ]
              )
           )
       ]
    [(stopped_body ());
     (initializing_body ());
     (running_body ());
     (footer ()); ]

let onload () : unit =
  let () = FormPerturbation.onload () in
  let () = InputPauseCondition.onload () in
  let () = InputPlotPeriod.onload () in
  let () = DivErrorMessage.onload () in
  let () = ButtonStart.onload () in
  let () = ButtonPause.onload () in
  let () = ButtonContinue.onload () in
  let () = ButtonClear.onload () in
  let () = DivStatusIndicator.onload() in
  ()
let onresize () : unit = ()
