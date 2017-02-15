(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module UIState = Ui_state
module Html = Tyxml_js.Html5
module R = Tyxml_js.R

module ButtonPerturbation : Ui_common.Div = struct
  let button_id = "panel_settings_perturbation_button"
  let button =
    Html.button
      ~a:[ Html.a_id button_id
         ; Html.Unsafe.string_attrib "type" "button"
         ; Html.a_class ["btn" ; "btn-default" ; ] ]
      [ Html.cdata "perturbation" ]
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ Html.div [ button ] ]

  let run_perturbation () : unit =
    Common.async
      (fun _ ->
         let code = React.S.value State_perturbation.model_perturbation in
         Ui_simulation.perturb_simulation ~code:code)

  let onload () : unit =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let handler = (fun _ -> let () = run_perturbation () in Js._true) in
    let () = button_dom##.onclick := Dom.handler handler in
    ()
end

module InputPerturbation : Ui_common.Div = struct
  let id = "panel_settings_perturbation_code"
  let input =
    Html.input
      ~a:[Html.a_id id;
          Html.a_input_type `Text;
          Html.a_class ["form-control"];
          Html.a_placeholder "Simulation Perturbation";]
      ()

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ Html.div [ input ] ]
   let onload () : unit =
     let input_dom = Tyxml_js.To_dom.of_input input in
     let handler =
             (fun (event : Dom_html.event Js.t)  ->
                let target : Dom_html.element Js.t =
                  Js.Opt.get
                    event##.target
                    (fun () ->
                   Common.toss
                     "Panel_settings.InputPerturbation.onload input")
                in
                let input : Dom_html.inputElement Js.t = Js.Unsafe.coerce target in
                let model_perturbation : string = Js.to_string input##.value in
                let () = State_perturbation.set_model_perturbation model_perturbation in
                Js._true)
     in
     let () = input_dom##.onchange := Dom.handler handler in
     ()

end

let signal_change input_dom signal_handler =
  input_dom##.onchange :=
    Dom_html.handler
      (fun _ -> let () = signal_handler (Js.to_string (input_dom##.value)) in
        Js._true)

module InputPauseCondition : Ui_common.Div = struct
  let id = "panel_settings_puase_condition"
  let input =
    Html.input
      ~a:[Html.a_id id ;
        Html.a_input_type `Text;
          Html.a_class ["form-control"];
          Html.a_placeholder "[T] > 100" ;
          Tyxml_js.R.Html.a_value State_parameter.model_pause_condition ]
    ()
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ Html.div [ input ] ]

  let dom = Tyxml_js.To_dom.of_input input

  let onload () =
    let () = signal_change dom
        (fun value ->
           let v' = if value = "" then "[false]" else value in
           State_parameter.set_model_pause_condition v') in
    ()
end

module InputPlotPeriod : Ui_common.Div = struct
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
        Html.a_class [ "form-control"];
        Html.a_placeholder "time units";
        Html.Unsafe.string_attrib "min" (string_of_float epsilon_float);
        Tyxml_js.R.Html.a_value
          (React.S.l1 format_float_string State_parameter.model_plot_period)]
    ()
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ Html.div [ input ] ]

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

module ButtonConfiguration : Ui_common.Div = struct
  let configuration_seed_input_id = "configuration_input"
  let configuration_seed_input =
    Html.input ~a:[ Html.a_id configuration_seed_input_id ;
                    Html.a_input_type `Number;
                    Html.a_class ["form-control"];
                  ] ()
  let configuration_save_button_id = "configuration_save_button"
  let configuration_save_button =
    Html.button
      ~a:[ Html.a_class [ "btn" ; "btn-default" ] ;
           Html.a_id configuration_save_button_id ;
         ]
      [ Html.cdata "Save" ]
  let simulation_configuration_modal_id = "simulation_configuration_modal"
  let configuration_modal = Ui_common.create_modal
      ~id:simulation_configuration_modal_id
      ~title_label:"Simulation Configuration"
      ~buttons:[ configuration_save_button ]
      ~body:[[%html
              {|<div class="row">
                   <div class="col-md-1"><label for={[configuration_seed_input_id]}>Seed</label></div>
                   <div class="col-md-5">|}
                     [configuration_seed_input]{|</div>
                </div>|}] ; ]

  let configuration_button_id = "configuration_button"
  let configuration_button =
    Html.button
      ~a:[ Html.a_class [ "btn" ; "btn-default" ] ;
           Html.a_id configuration_button_id ;
         ]
      [ Html.cdata "Options" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ [%html {|<div class="input-group input-group-offset-5">
              |}[configuration_button]{|
              |}[configuration_modal]{|
             </div>|}] ]
  let onload () =
    let () = Common.jquery_on
      (Format.sprintf "#%s" configuration_save_button_id)
      ("click")
      (Dom_html.handler
         (fun (_ : Dom_html.event Js.t)  ->
            let input : Dom_html.inputElement Js.t = Tyxml_js.To_dom.of_input configuration_seed_input in
            let value : string = Js.to_string input##.value in
            let model_seed = try Some (int_of_string value) with Failure _ -> None in
            let () = State_parameter.set_model_seed model_seed in
            let () =
              Common.modal
                ~id:("#"^simulation_configuration_modal_id)
                ~action:"hide"
            in

            Js._true))
    in
    let () = Common.jquery_on
      ("#"^configuration_button_id)
      ("click")
      (Dom_html.handler
         (fun (_ : Dom_html.event Js.t)  ->
            let input : Dom_html.inputElement Js.t = Tyxml_js.To_dom.of_input configuration_seed_input in
            let () = input##.value := Js.string
                  (match React.S.value State_parameter.model_seed with
                   | None -> ""
                   | Some model_seed -> string_of_int model_seed) in
            let () =
              Common.modal
                ~id:("#"^simulation_configuration_modal_id)
                ~action:"show"
            in
            Js._false)) in
    ()
end

module DivErrorMessage : Ui_common.Div = struct
  let alert_messages =
  Html.div
    ~a:[Tyxml_js.R.Html.a_class
          (React.S.bind
             UIState.model_error
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
          UIState.model_error
          (fun error ->
             React.S.const
               (match error with
                | None -> ""
                | Some localized_errors ->
                  (match localized_errors.Ui_state.model_error_messages with
                   | [] -> ""
                   | h::_ -> h.Api_types_j.message_text)
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
            Html.a_class [ "btn" ;
                           "btn-default" ; ] ; ])
      [ Html.cdata "start" ]

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ Html.div [button] ]

  let onload () =
    let start_button_dom = Tyxml_js.To_dom.of_button button in
    let () = start_button_dom##.onclick :=
        Dom.handler
          (fun _ ->
             let () = Common.async (fun _ -> Ui_simulation.start_simulation ()) in
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

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ Html.div [button] ]

  let onload () =
    let dom = Tyxml_js.To_dom.of_button button in
    let () = dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.async
               (fun _ -> Ui_simulation.stop_simulation ())
           in
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

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ Html.div [button] ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () = button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.async
               (fun _ -> Ui_simulation.pause_simulation ()) in
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

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ Html.div [button] ]

  let onload () =
    let button_dom = Tyxml_js.To_dom.of_button button in
    let () = button_dom##.onclick :=
        Dom.handler
          (fun _ ->
             let () =
               Common.async
                 (fun _ -> Ui_simulation.continue_simulation ()) in
             Js._true)
    in
    ()

end

module SelectRuntime : Ui_common.Div = struct

  let select_default_runtime = [ UIState.WebWorker ;
                                 UIState.Embedded ; ]
  let select_runtime_options, select_runtime_options_handle =
    ReactiveData.RList.create select_default_runtime
  let select =
    Tyxml_js.R.Html.select
      (ReactiveData.RList.map
         (fun runtime -> Html.option
             ~a:[Html.a_value
                   (UIState.runtime_value runtime)]
             (Html.pcdata (UIState.runtime_label runtime)))
         select_runtime_options)

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list = [ Html.div [ select] ]

  let onload () =
    let args = Url.Current.arguments in
    let select_dom = Tyxml_js.To_dom.of_select select in
    let set_runtime (runtime : Ui_state.runtime) (continuation : unit -> unit) =
      let r_val = Ui_state.runtime_value runtime in
      Ui_state.set_runtime_url
        r_val
        (fun success ->
           if success then
             select_dom##.value := Js.string r_val
           else
             continuation ())
    in
    let default_runtime () =
      set_runtime  UIState.default_runtime (fun _ -> ()) in
    let () =
      try
        let hosts = List.filter (fun (key,_) -> key = "host") args in
        let hosts : Ui_state.remote option list =
          List.map (fun (_,url) -> Ui_state.parse_remote url) hosts in
        let () = List.iter
            (fun value ->
               match value
               with
               | Some remote ->
                 ReactiveData.RList.cons
                   (Ui_state.Remote remote)
                   select_runtime_options_handle
               | None -> ())
            hosts
        in
        match ReactiveData.RList.value select_runtime_options
        with
        | head::_ -> set_runtime head (default_runtime)
        | _ -> default_runtime ()
      with _ -> default_runtime () in
    let () = select_dom##.onchange :=
        Dom.handler
          (fun _ ->
             let () = UIState.set_runtime_url
                 (Js.to_string select_dom##.value)
                 (fun success ->
                    if success then
                      ()
                    else
                      select_dom##.value :=
                        Js.string
                          (UIState.runtime_value UIState.default_runtime)
                 ) in
             Js._true
          )
    in
    ()
end

module DivStatusIndicator : Ui_common.Div = struct
  (*
  let label () =
    Tyxml_js.R.Html.pcdata
      (React.S.bind
         (Ui_simulation.simulation_status ())
         (fun status ->
            React.S.const
              (match status with
               | Ui_simulation.STOPPED -> "stopped"
               | Ui_simulation.INITALIZING -> "initalizing"
               | Ui_simulation.RUNNING -> "running"
               | Ui_simulation.PAUSED -> "paused"
              )
         )
      )

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    let debug = label () in
    [ Html.div (Ui_common.level ~debug ()) ]
*)
  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [  Html.div
         (Ui_common.level
            ~debug:(Tyxml_js.R.Html.pcdata
                      (React.S.bind
                         (Ui_simulation.simulation_status ())
                         (fun status ->
                            React.S.const
                              (match status with
                               | Ui_simulation.STOPPED -> "stopped"
                               | Ui_simulation.INITALIZING -> "initalizing"
                               | Ui_simulation.RUNNING -> "running"
                               | Ui_simulation.PAUSED -> "paused"
                              )
                         )
                      )) ())]
  let onload () = ()
end

module RunningPanelLayout : Ui_common.Div = struct
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
    let simulation_output = (Ui_simulation.simulation_output ()) in
    progress_bar
      (React.S.map (fun state ->
           let time_percent : int option =
             lift
             (fun (status : Api_types_j.simulation_info) ->
               status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time_percentage )
             state
           in
           let time_percent : int = Tools.unsome 100 time_percent in
           time_percent
         )
          simulation_output)
    (React.S.map (fun state ->
          let time : float option =
            lift (fun (status : Api_types_j.simulation_info) ->
                Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time) state in
         let time : float = Tools.unsome 0.0 time in
          Printf.sprintf "%.4g" time
        )
       simulation_output)

  let event_progress_bar () =
    let simulation_output = (Ui_simulation.simulation_output ()) in
    progress_bar
    (React.S.map (fun state ->
          let event_percentage : int option =
            lift (fun (status : Api_types_j.simulation_info) ->
                status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event_percentage) state in
         let event_percentage : int = Tools.unsome 100 event_percentage in
          event_percentage
        )
       simulation_output)
    (React.S.map (fun status ->
         let event : int option =
           lift (fun (status : Api_types_j.simulation_info) ->
               Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event)
             status
         in
         let event : int = Tools.unsome 0 event in
         string_of_int event
       )
        simulation_output)

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
    let simulation_output = (Ui_simulation.simulation_output ()) in
    Tyxml_js.R.Html.pcdata
    (React.S.map (fun state -> match tracked_events state with
            Some tracked_events -> string_of_int tracked_events
          | None -> " "
        )
        simulation_output)

  let tracked_events_label () =
    let simulation_output = (Ui_simulation.simulation_output ()) in
    Tyxml_js.R.Html.pcdata
      (React.S.map (fun state -> match tracked_events state with
             Some _ -> "tracked events"
           | None -> " "
       )
        simulation_output)

  let content () : [> Html_types.div ] Tyxml_js.Html.elt list =
    [ [%html {|
     <div class="row">
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

let hidden_class = ["hidden"]
let visible_class = ["visible"]
let visible_on_states
    ?(a_class=[])
    (state : Ui_simulation.ui_status list) : string list React.signal =
  (React.S.bind
     (Ui_simulation.simulation_status ())
     (fun run_state ->
        React.S.const
          (if List.mem run_state state then
             a_class@visible_class
           else
             a_class@hidden_class)
     )
  )

let stopped_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
  let stopped_row =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "row" ]
                [ Ui_simulation.STOPPED ; ]) ]
      [ Html.div ~a:[ Html.a_class ["col-md-3"; "col-xs-5" ] ] (InputPlotPeriod.content ()) ;
        Html.div ~a:[ Html.a_class ["col-md-1"; "col-xs-1" ] ] (ButtonConfiguration.content ())]
    in
    let paused_row =
      Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "row" ]
                [ Ui_simulation.PAUSED ; ]) ]
      [ Html.div ~a:[ Html.a_class ["col-md-10"; "col-xs-8" ] ] (InputPerturbation.content ()) ;
        Html.div ~a:[ Html.a_class ["col-md-2"; "col-xs-4" ] ] (ButtonPerturbation.content ()) ]
    in
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ Ui_simulation.STOPPED ;
                  Ui_simulation.PAUSED ;]) ]

      [ Html.div ~a:[ Html.a_class ["row" ] ]
          [ Html.div
              ~a:[ Html.a_class ["col-md-3" ; "col-xs-5" ] ]
              (InputPauseCondition.content ()) ;
            Html.div ~a:[ Html.a_class ["col-md-9"; "col-xs-7" ] ] (DivErrorMessage.content ())  ] ;
        stopped_row ;
        paused_row; ]


  let initializing_body () : [> Html_types.div ] Tyxml_js.Html5.elt =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ Ui_simulation.INITALIZING ; ]) ]
      [ Html.entity "nbsp" ]

  let running_body () =
    Html.div
      ~a:[ Tyxml_js.R.Html.a_class
             (visible_on_states
                ~a_class:[ "panel-body" ; "panel-controls" ]
                [ Ui_simulation.RUNNING ; ]) ]
      (RunningPanelLayout.content ())
let footer () =
  [%html {|
         <div class="panel-footer">
            <div class="row">
         |}[ Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ Ui_simulation.STOPPED ; ]) ]
               (ButtonStart.content ()) ]{|
         |}[ Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ Ui_simulation.PAUSED ; ]) ]
               (ButtonContinue.content ()) ]{|
         |}[ Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-4" ]
                     [ Ui_simulation.RUNNING ; ]) ]
               (ButtonPause.content ()) ]{|
         |}[ Html.div
               ~a:[ Tyxml_js.R.Html.a_class
                    (visible_on_states
                    ~a_class:[ "col-md-2"; "col-xs-3" ]
                    [ Ui_simulation.PAUSED ;
                      Ui_simulation.RUNNING ; ]) ]
               (ButtonClear.content ()) ]{|
         |}[ Html.div
               ~a:[ Html.a_class [ "col-md-1"; "col-xs-5" ] ]
               ((DivStatusIndicator.content ())
                @
                [ Html.entity "nbsp" ; ]) ]{|
            </div>
         </div>
  |}]
let content () =
  [[%html {|
      <div class="panel panel-default">
         |}[stopped_body ()]{|
         |}[initializing_body ()]{|
         |}[running_body ()]{|
         |}[footer ()]{|
     </div>
  |}]]

let onload () : unit =
  let () = ButtonPerturbation.onload () in
  let () = InputPerturbation.onload () in
  let () = InputPauseCondition.onload () in
  let () = InputPlotPeriod.onload () in
  let () = ButtonConfiguration.onload () in
  let () = DivErrorMessage.onload () in
  let () = ButtonStart.onload () in
  let () = ButtonPause.onload () in
  let () = ButtonContinue.onload () in
  let () = ButtonClear.onload () in
  let () = SelectRuntime.onload () in
  let () = DivStatusIndicator.onload() in
  ()
let onresize () : unit = ()
