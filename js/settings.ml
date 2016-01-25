open Js

open Lwt
open Visualization
open Codemirror
open Storage

module Html5 = Tyxml_js.Html5
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

let start_button_id = "start-button"
let start_button = Html5.button ~a:[ Html5.a_id start_button_id
                                   ; Html5.Unsafe.string_attrib "type" "button"
                                   ; Html5.a_class ["btn";"btn-default"] ] [ Html5.cdata "start" ]
let configuration_xml =
  <:html5<<div id="configuration-panel">
                  <div class="panel-footer panel-pre"> </div>
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
let events_progress = Html5.div ~a:[ Html5.a_id start_button_id
                                   ; Html5.Unsafe.string_attrib "role"          "progressbar"
                                   ; Html5.Unsafe.int_attrib    "aria-valuenow" 80
                                   ; Html5.Unsafe.int_attrib    "aria-valuemin" 0
                                   ; Html5.Unsafe.int_attrib    "aria-valuemax" 10
                                   ; Html5.Unsafe.string_attrib "style"         "width: 80%;"
                                   ; Html5.a_class ["progress-bar"] ]
                                [ Html5.span ~a:[ Html5.a_class ["sr-only"] ] [ Html5.cdata "80% Complete" ] ]
let stop_button_id = "stop_button"
let stop_button = Html5.button ~a:[ Html5.a_id stop_button_id
                                   ; Html5.Unsafe.string_attrib "type" "button"
                                   ; Html5.a_class ["btn";"btn-default"] ] [ Html5.cdata "stop" ]
let simulation_xml =
  <:html5<<div>
            <!-- simulation panel -->
            <div id="simulation-panel">
              <div class="panel-footer error-footer">
                line 0, characters -1--1: There is no way for the simulation to stop.
              </div>

              <div class="panel-footer panel-footer-white">
                <div class="row">
                  <div class="col-md-4">
                    <div class="progress">
                       $events_progress$
                    </div>
                  </div>
                  <div class="col-md-2">
                    events
                  </div>
                </div>

                <div class="row">
                  <div class="col-md-4">
                    <div class="progress">
                       $events_progress$
                    </div>
                  </div>
                  <div class="col-md-4">
                    time
                  </div>
                </div>

                <div class="row">
                  <div class="col-md-4">
                    0
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
               (fun _ -> Js._true) in
  let () = stop_button_dom##onclick <-
             Dom.handler
               (fun _ -> Js._true) in
  Lwt.return_unit
