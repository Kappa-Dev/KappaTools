module UIState = Ui_state
module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5

open ApiTypes

let document = Dom_html.window##document
let number_events_id = "number_events"
let number_events =
  Html.input
    ~a:[Html.a_id number_events_id ;
        Html.a_input_type `Number;
        Html.a_class ["form-control"];
        Html.a_placeholder "Max number";
        Tyxml_js.R.Html.a_value
          (React.S.l1 (fun x -> match x with
               | Some va -> string_of_int va
               | None -> "") UIState.model_max_events)]
    ()
let time_limit_id = "time_limit"
let time_limit =
  Html.input
    ~a:[Html.a_id time_limit_id ;
        Html.a_input_type `Number;
        Html.a_class ["form-control"];
        Html.a_placeholder "Time limit";
        Tyxml_js.R.Html.a_value
          (React.S.l1
             (fun x -> match x with
                | Some value ->
                  (* string_of_float returns integers with a trailing "."
                     which is not recognized by javascript.  this is an
                     attempt at fixing this.
                  *)
                  let value = string_of_float value in
                  let format_float n =
                    let length = String.length n in
                    if length > 0 && String.get n (length - 1) == '.' then
                      n^"0"
                    else
                      n
                  in
                  let value = format_float value in
               value
                | None -> "") UIState.model_max_time)
       ]
    ()
let plot_points_id = "plot_points"
let plot_points =
  Html.input
    ~a:[Html.a_id plot_points_id;
        Html.a_input_type `Number;
        Html.Unsafe.int_attrib "min" 1 ;
        Html.a_class ["form-control"];
        Html.a_placeholder "Expected number";
        Tyxml_js.R.Html.a_value
          (React.S.l1 string_of_int UIState.model_nb_plot)]
    ()
let signal_change id signal_handler =
  let input_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get (document##getElementById (Js.string id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  input_dom##onchange <-
    Dom_html.handler
      (fun _ ->
         let () = signal_handler (Js.to_string (input_dom##value))
         in Js._true)

let error_messages signal =
  Html.div
    ~a:[Tyxml_js.R.Html.a_class
          (React.S.bind
             signal
             (fun e -> React.S.const
                 (match e with
                  | [] ->
                    ["panel-footer";"panel-pre"]
                  | { severity = `Error ; _ }::_ ->
                    ["panel-footer";"error-footer"]
                  | { severity = `Warning ; _ }::_ ->
                    ["panel-footer";"warning-footer"]
                 ))
          )
       ]
    [Tyxml_js.R.Html.pcdata
       (React.S.bind
          signal
          (fun error ->
             React.S.const
               (match error with
                | [] -> ""
                | h::_ -> h.ApiTypes.message
               )
          )
       )
    ]

let code_messages = error_messages UIState.model_error
let start_button = Html.button ~a:([ Html.Unsafe.string_attrib "type" "button"
                                   ; Html.a_class ["btn";"btn-default"] ])
    [ Html.cdata "start" ]
let configuration_settings =
  <:html<<div class="panel-footer panel-footer-white">
            <div class="row">
                        <div class="col-md-4">
                           $number_events$
                        </div>
                        <div class="col-md-2">
                           events
                        </div>
                     </div>

                     <div class="row">
                        <div class="col-md-4">
                           $time_limit$
                        </div>
                        <div class="col-md-2">
                           sec
                        </div>
                     </div>

                     <div class="row">
                        <div class="col-md-4">
                           $plot_points$
                        </div>
                        <div class="col-md-2">
                           points
                        </div>
                     </div>
          </div> >>
let select_default_runtime = [ UIState.WebWorker ; UIState.Embedded ]
let select_runtime_options, select_runtime_options_handle =
  ReactiveData.RList.create select_default_runtime
let select_runtime =
  Tyxml_js.R.Html.select
    (ReactiveData.RList.map
       (fun runtime -> Html.option
           ~a:[Html.a_value (UIState.runtime_value runtime)]
           (Html.pcdata (UIState.runtime_label runtime)))
       select_runtime_options)

let configuration_button =
    <:html<<div class="panel-footer">
                 <div class="row">
                    <div class="col-md-4">
                       $start_button$
                    </div>
                    <div class="col-md-2">
                       $select_runtime$
                    </div>
                 </div>
            </div> >>

let configuration_id = "configuration-id"
let configuration_xml =
  Html.div
    ~a:[ Html.a_id configuration_id
       ; Tyxml_js.R.Html.a_class
           (React.S.bind
              UIState.model_is_running
              (fun is_running -> React.S.const (if is_running then
                                                  ["hidden"]
                                                else
                                                  ["visible"]
                                               )
              )
           )
       ]
    [code_messages; configuration_settings; configuration_button ]

let map_events f (format : 'a -> 'b) (identity : 'a) : 'b React.signal =
  React.S.map
    (fun state ->
       let v : 'a = match state with
           None -> identity
         | Some va -> f va in
       (format v : 'b))
    (UIState.model_runtime_state)

let progress_bar percent_signal value_signal =
  Html.div ~a:[ Html.Unsafe.string_attrib "role" "progressbar"
              ; Tyxml_js.R.Html.Unsafe.int_attrib
                  "aria-valuenow"
                  percent_signal
              ; Html.Unsafe.int_attrib "aria-valuemin" 0
              ; Html.Unsafe.int_attrib "aria-valuemax" 100
              ; Tyxml_js.R.Html.Unsafe.string_attrib
                  "style"
                  (React.S.map (fun s -> Format.sprintf
                                   "width: %d%%;" s)
                     percent_signal)
              ; Html.a_class ["progress-bar"] ]
    [ Tyxml_js.R.Html.pcdata
        (React.S.bind
           value_signal
           (fun value -> React.S.const value)
        )
    ]
let lift f x = match x with None -> None | Some x -> f x
let default x d = match x with None -> d | Some x -> x
let time_progress_bar =
  progress_bar
    (React.S.map (fun state ->
         let time_percent : int option =
           lift
             (fun (state : ApiTypes.state) -> state.time_percentage)
             state
         in
         let time_percent : int = default time_percent 0 in
         time_percent
       )
        UIState.model_runtime_state)
    (React.S.map (fun state ->
         let time : float option = lift (fun (state : ApiTypes.state) ->
             Some state.time) state in
         let time : float = default time 0.0 in
         string_of_float time
       )
        UIState.model_runtime_state)

let event_progress_bar =
  progress_bar
    (React.S.map (fun state ->
         let event_percentage : int option =
           lift (fun (state: ApiTypes.state) -> state.event_percentage) state in
         let event_percentage : int = default event_percentage 0 in
         event_percentage
       )
        UIState.model_runtime_state)
    (React.S.map (fun state ->
         let event : int option =
           lift (fun (state : ApiTypes.state) -> Some state.event)
             state
         in
         let event : int = default event 0 in
         string_of_int event
       )
        UIState.model_runtime_state)
let stop_button =
  Html.button
    ~a:[ Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn";"btn-default"] ] [ Html.cdata "stop" ]
let tracked_events state =
  let tracked_events : int option =
    lift (fun (state : ApiTypes.state) -> state.tracked_events)
      state
  in
  match tracked_events with
    None -> None
  | Some tracked_events -> if tracked_events > 0 then
      Some tracked_events
    else
      None
let tracked_events_count =
  Tyxml_js.R.Html.pcdata
    (React.S.map (fun state -> match tracked_events state with
           Some tracked_events -> string_of_int tracked_events
         | None -> " "
       )
        UIState.model_runtime_state)

let tracked_events_label = Tyxml_js.R.Html.pcdata
    (React.S.map (fun state -> match tracked_events state with
           Some _ -> "tracked events"
         | None -> " "
       )
        UIState.model_runtime_state)

let simulation_messages = error_messages UIState.model_error
let simulation_progress =
  <:html<<div class="panel-footer panel-footer-white">
                <div class="row">
                  <div class="col-md-4">
                    <div class="progress">
                       $event_progress_bar$
                    </div>
                  </div>
                  <div class="col-md-2">
                    events
                  </div>
                </div>

                <div class="row">
                  <div class="col-md-4">
                    <div class="progress">
                       $time_progress_bar$
                    </div>
                  </div>
                  <div class="col-md-4">
                    time
                  </div>
                </div>

                <div class="row">
                  <div class="col-md-4">
                    $tracked_events_count$
                  </div>
                  <div class="col-md-4">
                    $tracked_events_label$
                  </div>
                  <div class="col-md-1 panel-pre"> </div>
                </div>
              </div> >>

let simulation_buttons =
              <:html<<div class="panel-footer">
                      $stop_button$
                      </div> >>
let simulation_id : string = "simulation-panel"
let simulation_xml =
  Html.div
    ~a:[ Html.a_id simulation_id
       ; Tyxml_js.R.Html.a_class
           (React.S.bind
              UIState.model_is_running
              (fun model_is_running -> React.S.const (if model_is_running then
                                                        ["visible"]
                                                      else
                                                        ["hidden"]
             ))
           )
       ]
    [simulation_messages; simulation_progress ; simulation_buttons ]

let xml = <:html<<div>
                  $configuration_xml$
                  $simulation_xml$
                  </div> >>

let onload () : unit =
  let select_runtime_dom = Tyxml_js.To_dom.of_select select_runtime in
  let start_button_dom = Tyxml_js.To_dom.of_button start_button in
  let stop_button_dom = Tyxml_js.To_dom.of_button stop_button in
  let args = Url.Current.arguments in
  let set_runtime runtime continuation =
    let r_val = Ui_state.runtime_value runtime in
    Ui_state.set_runtime_url
      r_val
      (fun success -> if success then
          select_runtime_dom##value <- Js.string r_val
        else
          continuation ())
  in
  let default_runtime () = set_runtime UIState.default_runtime (fun _ -> ()) in
  let () =
    try
      let hosts = List.filter (fun (key,value) -> key = "host") args in
      let hosts =
        List.map
          (fun (_,url) -> Ui_state.compute_remote url) hosts in
      let () = List.iter
          (fun value ->
             match value with
             | Some remote ->
               ReactiveData.RList.cons
                 (Ui_state.Remote remote) select_runtime_options_handle
             | None -> ())
          hosts
      in
      match ReactiveData.RList.value select_runtime_options with
      | head::_ -> set_runtime head (default_runtime)
      | _ -> default_runtime ()
    with _ -> default_runtime () in

  let init_ui () =
    let _ = start_button_dom##disabled <- Js._true in
    let _ = stop_button_dom##disabled <- Js._true in
    ()
  in
  let stop_ui () =
    let _ = start_button_dom##disabled <- Js._true in
    let _ = stop_button_dom##disabled <- Js._false in
    ()
  in
  let start_ui () =
    let _ = start_button_dom##disabled <- Js._false in
    let _ = stop_button_dom##disabled <- Js._true in
    ()
  in
  let () = start_button_dom##onclick <-
      Dom.handler
        (fun _ ->
           let () = UIState.set_model_error [] in
           let () = UIState.set_model_is_running true in
           let () = init_ui () in
           let _ = UIState.start_model
               ~start_continuation:(fun stop_process ->
                   stop_ui ();
                   Lwt_js_events.async
                     (fun _ -> Lwt_js_events.clicks
                         stop_button_dom
                         (fun _ _ ->
                            let _ = init_ui () in
                            stop_process ()
                         ))
                 )
               ~stop_continuation:start_ui
           in Js._true)
  in
  let () = select_runtime_dom##onchange <-
      Dom.handler
        (fun _ ->
           let () = UIState.set_runtime_url
               (Js.to_string select_runtime_dom##value)
               (fun success ->
                  if success then
                    ()
                  else
                    select_runtime_dom##value <-
                      Js.string
                        (UIState.runtime_value UIState.default_runtime)
               ) in
           Js._true
        )
  in
  let () = signal_change number_events_id
      (fun value -> UIState.set_model_max_events
          (try Some (int_of_string value)
           with Failure _ -> None)
      ) in
  let () = signal_change time_limit_id
      (fun value ->
         UIState.set_model_max_time
           (try Some (float_of_string value)
            with Failure _ -> None)) in
  let () = signal_change plot_points_id
      (fun value ->
         try UIState.set_model_nb_plot
               (int_of_string value)
         with Not_found
            | Failure _ -> ()) in
  ()
