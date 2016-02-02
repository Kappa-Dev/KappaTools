open Js;;
open Codemirror

let rec list_last = function
  | [] -> failwith "list_last"
  | [ x ] -> x
  | _ :: l -> list_last l

open Lwt
let document = Dom_html.window##document
let has_been_modified = ref (false)

module Html5 = Tyxml_js.Html5
let file_selector_id = "file-selector"
let file_selector = Html5.input ~a:[ Html5.a_id file_selector_id ;
                                     Html5.Unsafe.string_attrib "type" "file" ;
                                     Html5.Unsafe.string_attrib "accept" ".ka" ]
let file_label_signal, set_file_label = React.S.create ""
let file_label =
       Tyxml_js.R.Html5.pcdata
            (React.S.bind
               file_label_signal
               (fun env ->
                React.S.const env))

let save_button_id = "save_button"
let save_button =  Html5.a ~a:[ Html5.a_id save_button_id
                              ; Tyxml_js.R.Html5.Unsafe.string_attrib "download" Storage.opened_filename
                              ; Html5.Unsafe.string_attrib "role" "button"
                              ; Html5.a_class ["btn";"btn-default";"pull-right"]
                              ]
                           [ Html5.cdata "save" ]

let file_selector = Html5.input ~a:[ Html5.a_id "file-selector" ; Html5.Unsafe.string_attrib "type" "file" ; Html5.Unsafe.string_attrib "accept" ".ka" ] ()
let panel_heading = <:html5<<div class="row">
                            <div class="col-md-2">
                            <label class="btn btn-default" for="file-selector">
                            $file_selector$
                            Load
                            </label>
                            </div>
                            <div class="col-md-4">
                            <label id="$file_label_id$" class="filename">
                            $file_label$
                            </label>
                            </div>
                            <div class="col-md-2 col-sm-offset-4 pull-right">
                            $save_button$
                            </div>
                            </div> >>

let xml = <:html5<<div class="col-md-6">
                             <div class="panel panel-default">

                                <div class="panel-heading">
                                   $panel_heading$
                                </div> <!-- panel heading -->

                                <div class="panel-body">
                                   <textarea id="code-mirror"></textarea>
                                </div> <!-- panel body -->

                                <!-- footers -->

                                <!-- configuration panel -->
                                <div id="configuration-panel">
                                   $Settings.xml$
                                </div> <!-- configuration panel -->

                                <!-- simulation panel -->
                                <div id="simulation-panel">
                                </div> <!-- simulation panel -->

                             </div>
                  </div> >>

let initialize codemirror () =
  let args = Url.Current.arguments in
  let () =
    try Storage.set_model_max_events
          (Some (int_of_string (List.assoc "nb_events" args)))
    with Not_found | Failure "int_of_string" -> () in
  let () =
    try Storage.set_model_nb_plot
          (int_of_string (List.assoc "plot_points" args))
    with Not_found | Failure "int_of_string" -> () in
  let () =
    try Storage.set_model_max_time
          (Some (float_of_string (List.assoc "time_limit" args)))
    with Not_found | Failure "float_of_string" -> () in
  try
    let url = List.assoc "model" args in
    XmlHttpRequest.get url >>=
      (fun content ->
       if content.XmlHttpRequest.code <> 200 then return_unit
       else
         let () = match Url.url_of_string content.XmlHttpRequest.url with
           | None -> ()
           | Some u ->
              let filename =
                Tools.list_last (match u with
                                 | (Url.Http h | Url.Https h) -> h.Url.hu_path
                                 | Url.File f -> f.Url.fu_path) in
              Storage.set_opened_filename filename in
         let () = codemirror##setValue(Js.string content.XmlHttpRequest.content) in
         return_unit)
  with Not_found ->
    try
         let text = List.assoc "model_text" args in
         let () = codemirror##setValue(Js.string text) in
         return_unit
       with Not_found ->
         return_unit

let onload () =
  let configuration : configuration Js.t = Codemirror.create_configuration () in
  let gutter_option : Js.string_array Js.t = (Js.string "CodeMirror-linenumbers,breakpoints")##split(Js.string ",") in
  let textarea : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string "code-mirror"))
               (fun () -> assert false) in
  let () = (Js.Unsafe.coerce configuration)##lineNumbers <- Js._true;
           (Js.Unsafe.coerce configuration)##gutters <- gutter_option;
           (Js.Unsafe.coerce configuration)##mode <- (Js.string "Kappa");
           Js.Unsafe.fun_call
             (Js.Unsafe.js_expr "id")
             [|Js.Unsafe.inject configuration |] in
  let codemirror : codemirror Js.t = Codemirror.fromTextArea textarea configuration in
  let _ = Lwt_js_events.async (initialize codemirror) in
  let codemirror_handler _ =
    has_been_modified := true;
    Storage.set_model_text (Js.to_string codemirror##getValue()) in
  let () = codemirror##on((Js.string "change"),
                          (codemirror_handler)) in
  let _ = Js.Unsafe.fun_call
            (Js.Unsafe.js_expr "id")
            [|Js.Unsafe.inject codemirror |] in
  let file_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
    ((Js.Opt.get (document##getElementById (Js.string file_selector_id))
                (fun () -> assert false))
     : Dom_html.element Js.t) in
  let save_button_dom  : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string save_button_id))
                  (fun () -> assert false)) in

  let () = save_button_dom##onclick <-
             Dom.handler
               (fun _ ->
                let header = Js.string "data:text/plain;charset=utf-8," in
                let editor_text :  Js.js_string Js.t = codemirror##getValue() in
                let () = save_button_dom##href <- header##concat((Js.escape editor_text)) in
                Js._true) in
  let file_select_handler () = let files = Js.Optdef.get (file_select_dom##files)
                                                         (fun () -> assert false)
                               in
                               let file = Js.Opt.get (files##item (0))
                                                     (fun () -> assert false)
                               in
                               let filename = file##name in
                               let () = set_file_label (to_string filename) ;
                                        Lwt_js_events.async (fun _ -> File.readAsText file >>=
                                                                        (fun (va : Js.js_string Js.t) ->
                                                                         codemirror##setValue(va);
                                                                         return_unit
                                                                 ));
                                        ()
                               in
                               let () = has_been_modified := false in
                               return_unit
  in
  let _  = Lwt_js_events.changes
             file_select_dom
             (fun _ _ ->
              if not !has_been_modified ||
                   Js.to_bool
                     (Dom_html.window##confirm
                                     (Js.string "Modifications will be lost, do you wish to continue?"))
              then file_select_handler ()
              else return_unit)
      in
      Settings.onload ()
