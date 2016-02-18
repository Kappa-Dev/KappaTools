module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5
module R = Tyxml_js.R

open Js
open ApiTypes
open Lwt
open Visualization
open Codemirror
open Counter
open UIState

let document = Dom_html.window##document
let number_events_id = "number_events"
let number_events =
  Html5.input
    ~a:[Html5.a_id number_events_id ;
        Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Max number";
        Tyxml_js.R.Html5.a_value
          (React.S.l1 (fun x -> match x with
                                | Some va -> string_of_int va
                                | None -> "") UIState.model_max_events)]
    ()
let time_limit_id = "time_limit"
let time_limit =
  Html5.input
    ~a:[Html5.a_id time_limit_id ;
        Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Time limit";
        Tyxml_js.R.Html5.a_value
          (React.S.l1 (fun x -> match x with
                                | Some va -> string_of_float va
                                | None -> "") UIState.model_max_time)]
    ()
let plot_points_id = "plot_points"
let plot_points =
  Html5.input
    ~a:[Html5.a_id plot_points_id;
        Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Expected number";
        Tyxml_js.R.Html5.a_value
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
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class
          (React.S.bind
             signal
             (fun e -> React.S.const (match e with
                                        [] -> ["panel-footer";"panel-pre"]
                                      | _ -> ["panel-footer";"error-footer"]
                                     ))
          )
       ]
    [Tyxml_js.R.Html5.pcdata
       (React.S.bind
          signal
          (fun error -> React.S.const (match error with
                                         [] -> ""
                                       | h::t -> h
                                      ))
       )
    ]

let code_messages = error_messages UIState.model_error
let start_button_id = "start-button"
let start_button = Html5.button ~a:([ Html5.a_id start_button_id
                                    ; Html5.Unsafe.string_attrib "type" "button"
                                    ; Html5.a_class ["btn";"btn-default"] ])
                                [ Html5.cdata "start" ]
let configuration_settings =
  <:html5<<div class="panel-footer panel-footer-white">
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
let select_runtime_id = "select_runtime_id"
let select_runtime_options, select_runtime_options_handle = ReactiveData.RList.create select_default_runtime
let select_runtime = Tyxml_js.R.Html5.select
                       ~a:[Html5.a_id select_runtime_id ]
                       (ReactiveData.RList.map
                          (fun runtime -> Html5.option
                                            ~a:[Html5.a_value (UIState.runtime_value runtime)]
                                            (Html5.pcdata (UIState.runtime_label runtime)))
                          select_runtime_options)

let configuration_button =
    <:html5<<div class="panel-footer">
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
  Html5.div
    ~a:[ Html5.a_id configuration_id
       ; Tyxml_js.R.Html5.a_class (React.S.bind
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

let map_events (f)
               (format : 'a -> 'b)
               (identity : 'a)
    : 'b React.signal
    = React.S.map
        (fun state ->
         let v : 'a = match state with
                          None -> identity
                        | Some va -> f va in
         (format v : 'b))
      (UIState.model_runtime_state)

let progress_bar percent_signal value_signal =
  Html5.div ~a:[ Html5.a_id start_button_id
               ; Html5.Unsafe.string_attrib "role" "progressbar"
               ; Tyxml_js.R.Html5.Unsafe.int_attrib "aria-valuenow" percent_signal
               ; Html5.Unsafe.int_attrib "aria-valuemin" 0
               ; Html5.Unsafe.int_attrib "aria-valuemax" 100
               ; Tyxml_js.R.Html5.Unsafe.string_attrib "style"
                                                       (React.S.map (fun s -> Format.sprintf
                                                                                "width: %d%%;" s)
                                                                    percent_signal)
               ; Html5.a_class ["progress-bar"] ]
            [ Tyxml_js.R.Html5.pcdata
                (React.S.bind
                   value_signal
                   (fun value -> React.S.const value)
                )
            ]
let lift f x = match x with None -> None | Some x -> f x
let default x d = match x with None -> d | Some x -> x
let time_progress_bar = progress_bar
                          (React.S.map (fun state ->
                                        let time_percent : int option = lift (fun (state : ApiTypes.state) -> state.time_percentage) state in
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

let event_progress_bar = progress_bar
                           (React.S.map (fun state ->
                                         let event_percentage : int option = lift (fun (state : ApiTypes.state) ->
                                                                                   let () = Visualization.update_plot state.plot in
                                                                                   state.event_percentage) state in
                                         let event_percentage : int = default event_percentage 0 in
                                         event_percentage
                                        )
                                       UIState.model_runtime_state)
                           (React.S.map (fun state ->
                                         let event : int option = lift (fun (state : ApiTypes.state) -> Some state.event) state in
                                         let event : int = default event 0 in
                                         string_of_int event
                                        )
                                       UIState.model_runtime_state)
let stop_button_id = "stop_button"
let stop_button = Html5.button ~a:[ Html5.a_id stop_button_id
                                   ; Html5.Unsafe.string_attrib "type" "button"
                                   ; Html5.a_class ["btn";"btn-default"] ] [ Html5.cdata "stop" ]
let tracked_events state =  let tracked_events : int option = lift (fun (state : ApiTypes.state) -> state.tracked_events) state in
                            match tracked_events with
                              None -> None
                            | Some tracked_events -> if tracked_events > 0 then
                                                       Some tracked_events
                                                     else
                                                       None
let tracked_events_count = Tyxml_js.R.Html5.pcdata
                             (React.S.map (fun state -> match tracked_events state with
                                                          Some tracked_events -> string_of_int tracked_events
                                                        | None -> " "
                                          )
                                       UIState.model_runtime_state)

let tracked_events_label = Tyxml_js.R.Html5.pcdata
                             (React.S.map (fun state -> match tracked_events state with
                                                          Some _ -> "tracked events"
                                                        | None -> " "
                                          )
                                       UIState.model_runtime_state)

let simulation_messages = error_messages UIState.model_error
let simulation_progress =
    <:html5<<div class="panel-footer panel-footer-white">
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
              <:html5<<div class="panel-footer">
                      $stop_button$
                      </div> >>
let simulation_id : string = "simulation-panel"
let simulation_xml =
  Html5.div
    ~a:[ Html5.a_id simulation_id
       ; Tyxml_js.R.Html5.a_class (React.S.bind
                                     UIState.model_is_running
                                     (fun model_is_running -> React.S.const (if model_is_running then
                                                                               ["visible"]
                                                                             else
                                                                               ["hidden"]
                                                                 ))
                                  )
       ]
    [simulation_messages; simulation_progress ; simulation_buttons ]

let xml = <:html5<<div>
                  $configuration_xml$
                  $simulation_xml$
                  </div> >>

let onload () : unit =
  let select_runtime_dom : Dom_html.selectElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string select_runtime_id))
                  (fun () -> assert false)) in
  let start_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string start_button_id))
                  (fun () -> assert false)) in
  let stop_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string stop_button_id))
                  (fun () -> assert false)) in
  let args = Url.Current.arguments in
  let set_runtime runtime continuation =
    set_runtime runtime
                (fun success -> if success then
                                  select_runtime_dom##value <- Js.string (UIState.runtime_value runtime)
                                else
                                  continuation ())
  in
  let default_embedded () = set_runtime Embedded (fun _ -> ()) in
  let () = try let hosts = args in
               let hosts = List.filter (fun (key,value) -> key = "host") hosts in
               let hosts = List.map snd hosts in
               let hosts = List.map (fun h -> (h,Url.url_of_string h)) hosts in
               let hosts = List.map (fun x -> match x with
                                                (_,None) -> None
                                              | (url,Some parsed) ->
                                                 let label = (match parsed with
                                                                Url.Http http -> http.Url.hu_host                                                                                                                                                           | Url.Https https -> https.Url.hu_host                                                                                                                                                        | Url.File file -> url
                                                             ) in
                                                 Some (Remote { label = label; url = url })
                                    ) hosts in
               let hosts = List.fold_left (fun acc value -> match value with
                                                              Some remote -> remote::acc
                                                            | None -> acc)
                                          []
                                          hosts
               in
               let hosts = List.concat [select_default_runtime;hosts] in
               let () = ReactiveData.RList.set select_runtime_options_handle hosts in
               let selected_runtime : runtime = (match hosts with
                                                   head::_ -> head
                                                 | _ -> Embedded) in
               let () = set_runtime selected_runtime (default_embedded) in
               ()
           with _ -> default_embedded () in

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
                let () = UIState.set_model_is_running true in
                let _ = init_ui () in
                let _ = UIState.start_model
                          ~start_continuation:(fun thread_is_running ->
                                               stop_ui ();
                                               Lwt_js_events.async
                                                 (fun _ -> Lwt_js_events.clicks
                                                             stop_button_dom
                                                             (fun _ _ ->
                                                              let _ = init_ui () in
                                                              Lwt_switch.turn_off thread_is_running))
                          )
                          ~stop_continuation:start_ui
                in Js._true)
  in
  let () = select_runtime_dom##onchange <-
             Dom.handler
               (fun _ -> let () = UIState.set_runtime_url
                                    (Js.to_string select_runtime_dom##value)
                                    (fun success -> if success then
                                                      ()
                                                    else
                                                      select_runtime_dom##value <- Js.string (UIState.runtime_value UIState.default_runtime)
                                    ) in
                         Js._true
               )
  in
  let () = signal_change number_events_id (fun value -> UIState.set_model_max_events
                                                          (try Some (int_of_string value)
                                                           with Failure _ -> None)
                                          ) in
  let () = signal_change time_limit_id    (fun value -> UIState.set_model_max_time
                                                          (try Some (float_of_string value)
                                                           with Failure _ -> None)) in
  let () = signal_change plot_points_id   (fun value -> try UIState.set_model_nb_plot
                                                              (int_of_string value)
                                                        with Not_found
                                                           | Failure "int_of_string" -> ()) in
  ()
