(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix
open List_util.Infix

let visible_on_states ?(a_class = [])
    (state : State_simulation.model_state list) : string list React.signal =
  let hidden_class = [ "hidden" ] in
  let visible_class = [ "visible" ] in
  React.S.bind State_simulation.model (fun model ->
      let current_state = State_simulation.model_simulation_state model in
      React.S.const
        (if List.mem current_state state then
           a_class @ visible_class
         else
           a_class @ hidden_class))

module FormPerturbation : Ui_common.Div = struct
  let id = "panel_settings_perturbation"

  let input =
    Html.input
      ~a:
        [
          Html.a_input_type `Text;
          Html.a_class [ "form-control" ];
          Html.a_placeholder "Simulation Perturbation";
        ]
      ()

  let button =
    Html.button
      ~a:[ Html.a_button_type `Submit; Html.a_class [ "btn"; "btn-default" ] ]
      [ Html.cdata "intervention" ]

  let form =
    Html.form
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (visible_on_states ~a_class:[ "form-horizontal" ]
               [ State_simulation.PAUSED ]);
        ]
      [
        Html.div
          ~a:[ Html.a_class [ "form-group" ] ]
          [
            Html.div ~a:[ Html.a_class [ "col-md-10"; "col-xs-9" ] ] [ input ];
            Html.div ~a:[ Html.a_class [ "col-md-2"; "col-xs-3" ] ] [ button ];
          ];
      ]

  let content () = [ form ]

  let onload () : unit =
    let form_dom = Tyxml_js.To_dom.of_form form in
    let input_dom = Tyxml_js.To_dom.of_input input in
    let handler _ =
      let model_perturbation : string = Js.to_string input_dom##.value in
      let () = State_perturbation.set_model_intervention model_perturbation in
      Js._true
    in

    let () =
      form_dom##.onsubmit :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.intervene_simulation () in
            Js._false)
    in
    let () = input_dom##.onchange := Dom.handler handler in
    ()
end

let signal_change input_dom signal_handler =
  input_dom##.onchange :=
    Dom_html.handler (fun _ ->
        let () = signal_handler (Js.to_string input_dom##.value) in
        Js._true)

module InputPauseCondition : Ui_common.Div = struct
  let id = "panel_settings_pause_condition"

  let input =
    Html.input
      ~a:
        [
          Html.a_id id;
          Html.a_input_type `Text;
          Html.a_class [ "form-control" ];
          Html.a_placeholder "[T] > 100";
          Tyxml_js.R.Html.a_value
            (React.S.map
               (fun m ->
                 m.State_project.model_parameters.State_project.pause_condition)
               State_project.model);
        ]
      ()

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ input ]
  let dom = Tyxml_js.To_dom.of_input input

  let onload () =
    let () =
      signal_change dom (fun value ->
          let v' =
            if value = "" then
              "[false]"
            else
              value
          in
          State_project.set_pause_condition v')
    in
    ()
end

module InputPlotPeriod : Ui_common.Div = struct
  let id = "panel_settings_plot_period"

  let format_float_string value =
    let n = string_of_float value in
    let length = String.length n in
    if length > 0 && String.get n (length - 1) = '.' then
      n ^ "0"
    else
      n

  let input =
    Html.input
      ~a:
        [
          Html.a_input_type `Number;
          Html.a_id id;
          Html.a_class [ "form-control" ];
          Html.a_placeholder "time units";
          Html.a_input_min (`Number 0);
          Tyxml_js.R.Html.a_value
            (React.S.map
               (fun m ->
                 format_float_string
                   m.State_project.model_parameters.State_project.plot_period)
               State_project.model);
        ]
      ()

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ input ]

  let onload () =
    let input_dom = Tyxml_js.To_dom.of_input input in
    let () =
      signal_change input_dom (fun value ->
          let reset_value () =
            let old_value =
              (React.S.value State_project.model).State_project.model_parameters
                .State_project.plot_period
            in
            input_dom##.value := Js.string (string_of_float old_value)
          in
          try
            let new_value = float_of_string value in
            if new_value < 0. then
              reset_value ()
            else
              State_project.set_plot_period new_value
          with Not_found | Failure _ -> reset_value ())
    in
    ()
end

module DivErrorMessage : Ui_common.Div = struct
  let id = "configuration_error_div"
  let message_nav_inc_id = "panel_settings_message_nav_inc_id"
  let message_nav_dec_id = "panel_settings_message_nav_dec_id"
  let message_file_label_id = "panel_settings_message_file_label"
  let error_index, set_error_index = React.S.create None

  let dont_gc_me =
    React.S.l1
      (function
        | [] -> ()
        | _ :: _ ->
          (match React.S.value error_index with
          | None -> set_error_index (Some 0)
          | Some _ -> ()))
      State_error.errors

  (* if there are less or no errors the index needs to be updated *)
  let sanitize_index (index : int option) errors : int option =
    let () = ignore dont_gc_me in
    match index, errors with
    | None, [] -> None
    | None, _ :: _ -> Some 0
    | Some _, [] -> None
    | Some index, error ->
      let length = List.length error in
      if index > length then (
        let () = set_error_index (Some 0) in
        Some 0
      ) else if 0 > index then (
        let index = Some (List.length error - 1) in
        let () = set_error_index index in
        index
      ) else
        Some index

  let get_message (index : int option) errors : Api_types_t.message option =
    Option_util.bind
      (fun n -> Some (List.nth errors n))
      (sanitize_index index errors)

  let mesage_nav_text =
    React.S.l2
      (fun index error ->
        match index, error with
        | None, [] -> ""
        | Some _, [] -> ""
        | None, _ :: _ -> ""
        | Some index, (_ :: _ as errors) ->
          Format.sprintf "%d/%d" (index + 1) (List.length errors))
      error_index State_error.errors

  let a_class =
    Tyxml_js.R.Html.a_class
      (React.S.bind State_error.errors (fun error ->
           React.S.const
             (match error with
             | [] | [ _ ] -> [ "hide" ]
             | _ :: _ :: _ -> [ "error-span"; "clickable" ])))

  let message_nav_dec =
    Html.span ~a:[ Html.a_id message_nav_dec_id; a_class ] [ Html.txt " « " ]

  let message_nav_inc =
    Html.span ~a:[ Html.a_id message_nav_inc_id; a_class ] [ Html.txt " » " ]

  let message_nav =
    [ message_nav_dec; Tyxml_js.R.Html.txt mesage_nav_text; message_nav_inc ]

  let file_label_text =
    React.S.l2
      (fun index error ->
        let range =
          Option_util.bind
            (fun message -> message.Result_util.range)
            (get_message index error)
        in
        match range with
        | None -> ""
        | Some range -> Format.sprintf "[%s]" range.Locality.file)
      error_index State_error.errors

  let file_label =
    Html.span
      ~a:
        [
          Html.a_id message_file_label_id;
          Html.a_class [ "error-span"; "clickable" ];
        ]
      [ Tyxml_js.R.Html.txt file_label_text ]

  let error_message_text =
    React.S.l2
      (fun index error ->
        match get_message index error with
        | None -> ""
        | Some message -> Format.sprintf " %s " message.Result_util.text)
      error_index State_error.errors

  let error_message =
    Html.span
      ~a:[ Html.a_id id; Html.a_class [ "error-span" ] ]
      [ Tyxml_js.R.Html.txt error_message_text ]

  let alert_messages =
    Html.div
      ~a:
        [
          Html.a_id id;
          Tyxml_js.R.Html.a_class
            (React.S.bind State_error.errors (fun error ->
                 React.S.const
                   (match error with
                   | [] -> [ "alert-sm"; "alert" ]
                   | _ :: _ -> [ "alert-sm"; "alert"; "alert-danger" ])));
        ]
      (message_nav @ [ file_label; error_message ])

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ alert_messages ]

  let file_click_handler () =
    let dom = Tyxml_js.To_dom.of_span file_label in
    let () =
      dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Common.debug (Js.string "file_click_handler") in
            let message : Api_types_t.message option =
              get_message
                (React.S.value error_index)
                (React.S.value State_error.errors)
            in
            let range =
              Option_util.bind
                (fun message -> message.Result_util.range)
                message
            in
            let () =
              match range with
              | Some range -> Panel_settings_controller.focus_range range
              | None -> ()
            in
            Js._true)
    in
    ()

  let index_click_handler dom delta =
    let () =
      dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Common.debug (Js.string "index_click_handler") in
            let index : int option =
              sanitize_index
                (React.S.value error_index)
                (React.S.value State_error.errors)
            in
            let index = Option_util.map delta index in
            let index : int option =
              sanitize_index index (React.S.value State_error.errors)
            in
            let () = set_error_index index in
            Js._true)
    in
    ()

  let inc_click_handler () =
    let dom = Tyxml_js.To_dom.of_span message_nav_dec in
    let () = index_click_handler dom (fun index -> index + 1) in
    ()

  let dec_click_handler () =
    let dom = Tyxml_js.To_dom.of_span message_nav_inc in
    let () = index_click_handler dom (fun index -> index - 1) in
    ()

  let onload () =
    let () = file_click_handler () in
    let () = inc_click_handler () in
    let () = dec_click_handler () in
    ()
end

module ButtonStart : Ui_common.Div = struct
  let id = "panel_settings_start_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn"; "btn-default" ];
          Tyxml_js.R.filter_attrib (Html.a_disabled ())
            (React.S.map
               (function
                 | {
                     State_file.current = Some { State_file.out_of_sync; _ };
                     _;
                   } ->
                   out_of_sync
                 | _ -> false)
               State_file.model);
        ]
      [ Html.cdata "start" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let start_button_dom = Tyxml_js.To_dom.of_button button in
    let () =
      start_button_dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.start_simulation () in
            Js._true)
    in

    ()
end

module ButtonClear : Ui_common.Div = struct
  let id = "panel_settings_clear_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn"; "btn-default" ];
        ]
      [ Html.cdata "clear" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let dom = Tyxml_js.To_dom.of_button button in
    let () =
      dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.stop_simulation () in
            Js._true)
    in
    ()
end

module ButtonPause : Ui_common.Div = struct
  let id = "panel_settings_pause_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn"; "btn-default" ];
        ]
      [ Html.cdata "pause" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () =
      button_dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.pause_simulation () in
            Js._true)
    in
    ()
end

module ButtonTrace : Ui_common.Div = struct
  let id = "panel_settings_get_trace_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Tyxml_js.R.Html5.a_class
            (React.S.map
               (fun model ->
                 (if
                    model.State_project.model_parameters
                      .State_project.store_trace
                  then
                    []
                  else
                    [ "disabled" ])
                 @ [ "btn"; "btn-default" ])
               State_project.model);
        ]
      [ Html.cdata "get trace" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () =
      button_dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.simulation_trace () in
            Js._true)
    in
    ()
end

module ButtonOutputs : Ui_common.Div = struct
  let id = "panel_settings_outputs_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn"; "btn-default" ];
        ]
      [ Html.cdata "All outputs" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () =
      button_dom##.onclick :=
        Dom.handler (fun _ ->
            let () = Panel_settings_controller.simulation_outputs () in
            Js._true)
    in
    ()
end

module ButtonContinue : Ui_common.Div = struct
  let id = "panel_settings_continue_button"

  let button =
    Html.button
      ~a:
        [
          Html.a_id id;
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn"; "btn-default" ];
        ]
      [ Html.cdata "continue" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ button ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () =
      button_dom##.onclick :=
        Dom.handler (fun _ ->
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
        [
          Tyxml_js.R.Html.txt
            (React.S.bind State_simulation.model (fun model ->
                 let label =
                   State_simulation.model_state_to_string
                     (State_simulation.model_simulation_state model)
                 in
                 React.S.const label));
        ]
    in
    [ Html.div ~a:[ Html.a_id id ] (Ui_common.level ~debug ()) ]

  let onload () = ()
end

module RunningPanelLayout : Ui_common.Div = struct
  let id = "settings_runetime_layout"

  let progress_bar (percent_signal : int Tyxml_js.R.Html.wrap)
      (value_signal : string React.signal) =
    Html.div
      ~a:
        [
          Html.Unsafe.string_attrib "role" "progressbar";
          Tyxml_js.R.Html.Unsafe.int_attrib "aria-valuenow" percent_signal;
          Html.Unsafe.int_attrib "aria-valuemin" 0;
          Html.Unsafe.int_attrib "aria-valuemax" 100;
          Tyxml_js.R.Html.Unsafe.string_attrib "style"
            (React.S.map
               (fun s -> Format.sprintf "width: %d%%;" s)
               percent_signal);
          Html.a_class [ "progress-bar" ];
        ]
      [
        Tyxml_js.R.Html.txt
          (React.S.bind value_signal (fun value -> React.S.const value));
      ]

  let time_progress_bar () =
    progress_bar
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let time_percent : int option =
             Option_util.bind
               (fun (status : Api_types_j.simulation_info) ->
                 status.Api_types_j.simulation_info_progress
                   .Api_types_j.simulation_progress_time_percentage)
               simulation_info
           in
           let time_percent : int = Option_util.unsome 100 time_percent in
           time_percent)
         State_simulation.model)
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let time : float option =
             Option_util.map
               (fun (status : Api_types_j.simulation_info) ->
                 status.Api_types_j.simulation_info_progress
                   .Api_types_j.simulation_progress_time)
               simulation_info
           in
           let time : float = Option_util.unsome 0.0 time in
           string_of_float time)
         State_simulation.model)

  let event_progress_bar () =
    progress_bar
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let event_percentage : int option =
             Option_util.bind
               (fun (status : Api_types_j.simulation_info) ->
                 status.Api_types_j.simulation_info_progress
                   .Api_types_j.simulation_progress_event_percentage)
               simulation_info
           in
           let event_percentage : int =
             Option_util.unsome 100 event_percentage
           in
           event_percentage)
         State_simulation.model)
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           let event : int option =
             Option_util.map
               (fun (status : Api_types_j.simulation_info) ->
                 status.Api_types_j.simulation_info_progress
                   .Api_types_j.simulation_progress_event)
               simulation_info
           in
           let event : int = Option_util.unsome 0 event in
           string_of_int event)
         State_simulation.model)

  let tracked_events state =
    let tracked_events : int option =
      Option_util.bind
        (fun (status : Api_types_j.simulation_info) ->
          status.Api_types_j.simulation_info_progress
            .Api_types_j.simulation_progress_tracked_events)
        state
    in
    match tracked_events with
    | None -> None
    | Some tracked_events ->
      if tracked_events > 0 then
        Some tracked_events
      else
        None

  let tracked_events_count () =
    Tyxml_js.R.Html.txt
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           match tracked_events simulation_info with
           | Some tracked_events -> string_of_int tracked_events
           | None -> " ")
         State_simulation.model)

  let tracked_events_label () =
    Tyxml_js.R.Html.txt
      (React.S.map
         (fun model ->
           let simulation_info = State_simulation.model_simulation_info model in
           match tracked_events simulation_info with
           | Some _ -> "tracked events"
           | None -> " ")
         State_simulation.model)

  let efficiency_detail ~current_event t =
    let all =
      float_of_int
        (t.Counter.Efficiency.no_more_binary
       + t.Counter.Efficiency.no_more_unary
       + t.Counter.Efficiency.clashing_instance
       + t.Counter.Efficiency.time_correction)
    in
    let events = float_of_int current_event in
    Html.p
      [
        Html.txt
          (Format.asprintf "@[%.2f%% of event loops were productive.%t@]"
             (100. *. events /. (all +. events))
             (fun f -> if all > 0. then Format.fprintf f "@ Null event cause:"));
      ]
    :: ((if t.Counter.Efficiency.no_more_unary > 0 then
           Some
             (Html.p
                [
                  Html.txt
                    (Format.asprintf
                       "Valid embedding but no longer unary when required: \
                        %.2f%%"
                       (100.
                       *. float_of_int t.Counter.Efficiency.no_more_unary
                       /. all));
                ])
         else
           None)
       $$ ((if t.Counter.Efficiency.no_more_binary > 0 then
              Some
                (Html.p
                   [
                     Html.txt
                       (Format.asprintf
                          "Valid embedding but not binary when required: %.2f%%"
                          (100.
                          *. float_of_int t.Counter.Efficiency.no_more_binary
                          /. all));
                   ])
            else
              None)
          $$ ((if t.Counter.Efficiency.clashing_instance > 0 then
                 Some
                   (Html.p
                      [
                        Html.txt
                          (Format.asprintf "Clashing instance: %.2f%%"
                             (100.
                             *. float_of_int
                                  t.Counter.Efficiency.clashing_instance
                             /. all));
                      ])
               else
                 None)
             $$ ((if t.Counter.Efficiency.time_correction > 0 then
                    Some
                      (Html.p
                         [
                           Html.txt
                             (Format.asprintf
                                "Perturbation interrupting time advance: %.2f%%"
                                (100.
                                *. float_of_int
                                     t.Counter.Efficiency.time_correction
                                /. all));
                         ])
                  else
                    None)
                $$ []))))

  let dont_gc_me = ref []

  let content () : Html_types.div_content Tyxml_js.Html.elt list =
    let state_log, set_state_log = ReactiveData.RList.create [] in
    let () =
      dont_gc_me :=
        [
          Lwt_react.S.map_s
            (fun _ ->
              State_simulation.with_simulation_info ~label:__LOC__
                ~ready:(fun manager status ->
                  manager#simulation_efficiency
                  >>= Api_common.result_bind_lwt ~ok:(fun eff ->
                          let current_event =
                            status.Api_types_j.simulation_info_progress
                              .Api_types_j.simulation_progress_event
                          in
                          let () =
                            ReactiveData.RList.set set_state_log
                              (efficiency_detail ~current_event eff)
                          in
                          Lwt.return (Result_util.ok ())))
                ~stopped:(fun _ ->
                  let () = ReactiveData.RList.set set_state_log [] in
                  Lwt.return (Result_util.ok ()))
                ())
            State_simulation.model;
        ]
    in

    [
      [%html
        {|
     <div class="row" id="|}
          id
          {|">
        <div class="col-md-5">
     <div class="row">
        <div class="col-xs-9">
            <div class="progress">
            |}
          [ event_progress_bar () ]
          {|
            </div>
        </div>
        <div class="col-xs-3">events</div>
     </div>
     <div class="row">
        <div class="col-xs-9">
            <div class="progress">
            |}
          [ time_progress_bar () ]
          {|
            </div>
        </div>
        <div class="col-xs-3">time</div>
     </div>
     <div class="row">
        <div class="col-xs-9">
           |}
          [ tracked_events_count () ]
          {|
        </div>
        <div class="col-xs-3">
           |}
          [ tracked_events_label () ]
          {|
        </div>
     </div>
</div>
<div class="visible-md-block visible-lg-block">
|}
          [ Tyxml_js.R.Html.div state_log ]
          {|
</div>
</div>
   |}];
    ]

  let onload () = ()
end

let stopped_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
  let stopped_row =
    Html.div
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (visible_on_states
               ~a_class:[ "form-group"; "form-group-sm" ]
               [ State_simulation.STOPPED ]);
        ]
      [%html
        {|
            <label class="col-lg-1 col-md-2 col-xs-2 control-label" for="|}
          InputPlotPeriod.id
          {|">Plot period</label>
            <div class="col-md-2 col-xs-3">|}
          (InputPlotPeriod.content ())
          {|</div>|}]
  in
  let paused_row = FormPerturbation.content () in
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (visible_on_states
             ~a_class:[ "panel-body"; "panel-controls" ]
             [ State_simulation.STOPPED; State_simulation.PAUSED ]);
      ]
    ([%html
       {|
         <form class="form-horizontal">
          <div class="form-group">
            <label class="col-lg-1 col-sm-2 hidden-xs control-label" for="|}
         InputPauseCondition.id
         {|">Pause if</label>
            <div class="col-md-2 col-sm-3 col-xs-5">|}
         (InputPauseCondition.content ())
         {|</div>
            <div class="col-lg-9 col-md-8 col-xs-7">|}
         (DivErrorMessage.content ())
         {|</div>
          </div>|}
         [ stopped_row ] {|</form>|}]
    :: paused_row)

let initializing_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (visible_on_states
             ~a_class:[ "panel-body"; "panel-controls" ]
             [ State_simulation.INITALIZING ]);
      ]
    [ Html.entity "nbsp" ]

let running_body () =
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (visible_on_states
             ~a_class:[ "panel-body"; "panel-controls" ]
             [ State_simulation.RUNNING ]);
      ]
    (RunningPanelLayout.content ())

let footer () =
  [%html
    {|
         <div class="panel-footer">
            <div class="row">
         |}
      [
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-md-2"; "col-xs-4" ]
                   [ State_simulation.STOPPED ]);
            ]
          (ButtonStart.content ());
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-md-2"; "col-xs-3" ]
                   [ State_simulation.PAUSED ]);
            ]
          (ButtonContinue.content ());
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-md-2"; "col-xs-3" ]
                   [ State_simulation.PAUSED ]);
            ]
          (ButtonOutputs.content ());
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-md-2"; "col-xs-3" ]
                   [ State_simulation.PAUSED ]);
            ]
          (ButtonTrace.content ());
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-md-2"; "col-xs-4" ]
                   [ State_simulation.RUNNING ]);
            ]
          (ButtonPause.content ());
        Html.div
          ~a:
            [
              Tyxml_js.R.Html.a_class
                (visible_on_states ~a_class:[ "col-xs-2"; "col-sm-1" ]
                   [ State_simulation.PAUSED; State_simulation.RUNNING ]);
            ]
          (ButtonClear.content ());
        Html.div
          ~a:[ Html.a_class [ "col-md-1"; "col-xs-3" ] ]
          (DivStatusIndicator.content () @ [ Html.entity "nbsp" ]);
      ]
      {|
            </div>
         </div>
  |}]

let content () =
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (React.S.bind State_project.model (fun model ->
               match model.State_project.model_current_id with
               | None -> React.S.const [ "hide" ]
               | Some _ -> React.S.const [ "panel"; "panel-default" ]));
      ]
    [ stopped_body (); initializing_body (); running_body (); footer () ]

let onload () : unit =
  let () = FormPerturbation.onload () in
  let () = InputPauseCondition.onload () in
  let () = InputPlotPeriod.onload () in
  let () = DivErrorMessage.onload () in
  let () = ButtonStart.onload () in
  let () = ButtonPause.onload () in
  let () = ButtonContinue.onload () in
  let () = ButtonTrace.onload () in
  let () = ButtonOutputs.onload () in
  let () = ButtonClear.onload () in
  let () = DivStatusIndicator.onload () in
  ()

let onresize () : unit = ()
