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

let xml = <:html5<<div id="configuration-panel">
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
