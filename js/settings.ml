open Js

open Lwt
open Visualization
open Codemirror
open Storage
open Counter

module Html5 = Tyxml_js.Html5
module R = Tyxml_js.R
let document = Dom_html.window##document
let number_events =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Max number";
        Tyxml_js.R.Html5.a_value
          (React.S.l1 (fun x -> match x with
                                | Some va -> string_of_int va
                                | None -> "") Storage.model_max_events)]
    ()
let time_limit =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Time limit";
        Tyxml_js.R.Html5.a_value
          (React.S.l1 (fun x -> match x with
                                | Some va -> string_of_float va
                                | None -> "") Storage.model_max_time)]
    ()
let plot_points =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
        Html5.a_placeholder "Expected number";
        Tyxml_js.R.Html5.a_value
          (React.S.l1 string_of_int Storage.model_nb_plot)]
    ()

let error_messages signal formatter =
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class
          (React.S.bind
             signal
             (fun e -> React.S.const (match e with
                                        Some _ -> ["panel-footer";"error-footer"]
                                      | None -> ["panel-footer";"panel-pre"]
                                     ))
          )
       ]
    [Tyxml_js.R.Html5.pcdata
       (React.S.bind
          signal
          (fun error -> React.S.const (match error with
                                         Some error -> formatter error
                                       | None -> ""
                                      ))
       )
    ]

let code_messages = error_messages model_syntax_error Storage.format_error_message
let start_button_id = "start-button"
let start_button = Html5.button ~a:(R.filter_attrib (Html5.a_disabled `Disabled)
                                                    (React.S.map
                                                       (function None -> false
                                                            | Some _ -> true)
                                                       model_syntax_error
                                                    )
                                    ::
                                    [ Html5.a_id start_button_id
                                    ; Html5.Unsafe.string_attrib "type" "button"
                                    ; Tyxml_js.R.Html5.a_class
                                        (React.S.bind
                                           model_syntax_error
                                           (fun e -> React.S.const (match e with
                                                                      Some (message,location) -> ["btn";"btn-default";"disabled"]
                                                                    | None -> ["btn";"btn-default"]
                                                                   ))
                                        )])
                                [ Html5.cdata "start" ]
let configuration_xml =
  <:html5<<div id="configuration-panel">
                  $code_messages$
                  <div class="panel-footer panel-footer-white">
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
                  </div>
                  <div class="panel-footer">
                    $start_button$
                  </div>
          </div> >>


let events_progress value =
  Html5.div ~a:[ Html5.a_id start_button_id
                                   ; Html5.Unsafe.string_attrib "role"          "progressbar"
                                   ; Html5.Unsafe.int_attrib    "aria-valuenow" value
                                   ; Html5.Unsafe.int_attrib    "aria-valuemin" 0
                                   ; Html5.Unsafe.int_attrib    "aria-valuemax" 100
                                   ; Html5.Unsafe.string_attrib "style"         (Format.sprintf "width: %d;" value)
                                   ; Html5.a_class ["progress-bar"] ]
            [ Html5.span ~a:[ Html5.a_class ["sr-only"] ] [ Html5.cdata (Format.sprintf "%d%% " value) ] ]

let map_events (f : Counter.t -> int option) = React.S.map
                                                 (fun (counter : Counter.t) ->
                                                  match f counter with
                                                  | None -> 0
                                                  | Some va -> va)
                                                 (model_counter : Counter.t React.signal)

let progress_bar signal =
  Html5.div ~a:[ Html5.a_id start_button_id
                                   ; Html5.Unsafe.string_attrib "role" "progressbar"
                                   ; Tyxml_js.R.Html5.Unsafe.int_attrib "aria-valuenow" signal
                                   ; Html5.Unsafe.int_attrib "aria-valuemin" 0
                                   ; Html5.Unsafe.int_attrib "aria-valuemax" 100
                                   ; Tyxml_js.R.Html5.Unsafe.string_attrib "style" (React.S.map (fun s -> Format.sprintf "width: %d;" s) signal)
                                   ; Html5.a_class ["progress-bar"] ] []

let time_progress_bar = progress_bar (map_events Counter.time_percentage)
let event_progress_bar = progress_bar (map_events Counter.event_percentage)

let stop_button_id = "stop_button"
let stop_button = Html5.button ~a:[ Html5.a_id stop_button_id
                                   ; Html5.Unsafe.string_attrib "type" "button"
                                   ; Html5.a_class ["btn";"btn-default"] ] [ Html5.cdata "stop" ]

let counter_stories = Tyxml_js.R.Html5.pcdata
                        (React.S.bind
                           (model_counter : Counter.t React.signal)
                           (fun counter ->
                            let c = Counter.counter_stories counter in
                            if c >= 0 then
                              React.S.const (string_of_int (Counter.counter_stories counter))
                            else
                              React.S.const ""
                           )

                        )

let simulation_messages = error_messages Storage.model_runtime_error_message (fun i -> i)
let simulation_xml =
  <:html5<<div>
            <!-- simulation panel -->
            <div id="simulation-panel">
              $simulation_messages$
              <div class="panel-footer panel-footer-white">
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
                    $counter_stories$
                  </div>
                  <div class="col-md-4">
                    tracked events
                  </div>
                </div>
              </div>
              <div class="panel-footer">
                 $stop_button$
              </div>

            </div> <!-- simulation panel -->
          </div> >>

let xml = <:html5<<div>
                  $configuration_xml$
                  $simulation_xml$
                  </div> >>

let onload () : unit Lwt.t =
  let start_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string start_button_id))
                  (fun () -> assert false)) in
  let stop_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string stop_button_id))
                  (fun () -> assert false)) in
  let () = start_button_dom##onclick <-
             Dom.handler
               (fun _ -> let _ = Storage.run () in
                         Js._true) in
  let () = stop_button_dom##onclick <-
             Dom.handler
               (fun _ ->
                Lwt.wakeup Storage.stopper ();
                Js._true) in
  Lwt.return_unit
