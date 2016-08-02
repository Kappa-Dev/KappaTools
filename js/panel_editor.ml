module UIState = Ui_state
module Api_types = ApiTypes_j

open Js
open Codemirror

let rec list_last =
  function
  | [] -> failwith "list_last"
  | [ x ] -> x
  | _ :: l -> list_last l

open Lwt
let document = Dom_html.window##.document
let has_been_modified = ref (false)

module Html = Tyxml_js.Html5
let file_selector_id = "file-selector"
let file_selector =
  Html.input
    ~a:[ Html.a_id file_selector_id ;
         Html.Unsafe.string_attrib "type" "file" ;
         Html.Unsafe.string_attrib "accept" ".ka" ]

let file_label_signal, set_file_label = React.S.create ""
let file_label =
  Tyxml_js.R.Html.pcdata
    (React.S.bind
       file_label_signal
       (fun env ->
          React.S.const env))

let save_button_id = "save_button"
let save_button =
  Html.a
    ~a:[ Html.a_id save_button_id
       ; Tyxml_js.R.Html.Unsafe.string_attrib
           "download"
           UIState.opened_filename
       ; Html.Unsafe.string_attrib "role" "button"
       ; Html.a_class ["btn";"btn-default";"pull-right"]
       ]
    [ Html.cdata "save" ]
let file_selector =
  Html.input
    ~a:[ Html.a_id "file-selector" ;
         Html.Unsafe.string_attrib "type" "file" ;
         Html.Unsafe.string_attrib "accept" ".ka" ] ()
let panel_heading =
  [%html {|<div class="row">
             <div class="col-md-2">
                <label class="btn btn-default" for="file-selector">
         |}[file_selector]{|
                   Load
                </label>
             </div>
             <div class="col-md-4">
                <label class="filename">|}[file_label]{|</label>
							</div>
							<div class="col-md-2 col-sm-offset-4 pull-right">
							|}[save_button]{|
              </div>
            </div>|}]

let xml =
  [%html {|<div class="col-md-6">
             <div class="panel panel-default">
                <div class="panel-heading">
         |}[panel_heading]{|
                </div>

                <div class="panel-body">
                   <textarea id="code-mirror"> </textarea>
                </div>

                <div id="configuration-panel">
                   |}[Settings.xml]{|
                </div>


                <div id="simulation-panel"></div>

              </div>
           </div>|}]

let setup_lint codemirror update_linting =
  let error_lint errors : Codemirror.lint Js.t Js.js_array Js.t =
    let position p =
      Codemirror.create_position
        ~ch:p.Location.chr
        ~line:(p.Location.line-1)
    in
    let hydrate (error  : Api_types.error) : lint Js.t option =
      match error.Api_types.range with
      | None ->
        None
      | Some range ->
        Some (Codemirror.create_lint
                ~message:error.Api_types.message
                (* This is a bit of a hack ... i am trying to keep
                   the code mirror code independent of the api code.
                *)
                ~severity:( match error.Api_types.severity with
                    | `Error -> Codemirror.Error
                    | `Warning -> Codemirror.Warning
                  )
                ~from:(position range.Location.from_position)
                ~to_:(position range.Location.to_position))
    in
    Js.array
      (Array.of_list
         (List.fold_left
            (fun acc value ->
               match hydrate value with
               | None -> acc
               | Some value -> value::acc)
            []
            errors
         ))
  in
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "CodeMirror.registerHelper")
      [| Js.Unsafe.inject (Js.string "lint") ;
         Js.Unsafe.inject (Js.string "Kappa") ;
         Js.Unsafe.inject (fun _ -> ())
      |]
  in
  let _ =
    React.S.l1
      (fun (e : Api_types.errors)->
         update_linting
           codemirror
           (error_lint e)
      )
      UIState.model_error
  in
  ()

let initialize codemirror () =
  let args = Url.Current.arguments in
  let () =
    try UIState.set_model_max_events
          (Some (int_of_string (List.assoc "nb_events" args)))
    with Not_found | Failure _ -> () in
  let () =
    try UIState.set_model_nb_plot
          (int_of_string (List.assoc "plot_points" args))
    with Not_found | Failure _ -> () in
  let () =
    try UIState.set_model_max_time
          (Some (float_of_string (List.assoc "time_limit" args)))
    with Not_found | Failure _ -> () in
  try
    let url = List.assoc "model" args in
    XmlHttpRequest.get url >>=
    (fun content ->
       if content.XmlHttpRequest.code <> 200 then return_unit
       else
         let () =
           match Url.url_of_string content.XmlHttpRequest.url with
           | None -> ()
           | Some u ->
             let filename =
               Tools.list_last
                 (match u with
                  | (Url.Http h | Url.Https h) -> h.Url.hu_path
                  | Url.File f -> f.Url.fu_path) in
             UIState.set_opened_filename filename in
         let () =
           codemirror##setValue(Js.string content.XmlHttpRequest.content) in
         return_unit)
  with Not_found ->
  try
    let text = List.assoc "model_text" args in
    let () = codemirror##setValue(Js.string text) in
    return_unit
  with Not_found ->
    return_unit

let onload () : unit =
  (* this needs to be called before code mirror is created *)
  let update_linting : codemirror Js.t
    -> Codemirror.lint Js.t Js.js_array Js.t
    -> Codemirror.lint Js.t Js.js_array Js.t =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "load_lint")
      [| |]
  in
  let configuration : configuration Js.t = Codemirror.create_configuration
      () in
  let gutter_options =
    Js.string "breakpoints,CodeMirror-lint-markers,CodeMirror-linenumbers" in
  let gutter_option : Js.string_array Js.t =
    gutter_options##split(Js.string ",")
  in
  let textarea : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string "code-mirror"))
      (fun () -> assert false) in
  let () =
    (Js.Unsafe.coerce configuration)##.lineNumbers := Js._true;
    (Js.Unsafe.coerce configuration)##.autofocus := Js._true;
    (Js.Unsafe.coerce configuration)##.gutters := gutter_option;
    (Js.Unsafe.coerce configuration)##.lint := Js._true;
    (Js.Unsafe.coerce configuration)##.mode := (Js.string "Kappa")
  in
  let codemirror : codemirror Js.t =
    Codemirror.fromTextArea
      textarea
      configuration in
  let () = codemirror##setValue(Js.string "") in
  let () = setup_lint codemirror update_linting in
  let _ = Lwt_js_events.async (initialize codemirror) in
  let timeout : Dom_html.timeout_id option ref = ref None in
  let handler = fun codemirror change ->
    let () = has_been_modified := true in
    let text : string = Js.to_string codemirror##getValue in
    let () = UIState.set_text text in
    let () = match !timeout with
        None -> ()
      | Some timeout -> Dom_html.window ##
			  clearTimeout (timeout) in
    let delay : float =
      if (((Js.str_array (change##.text ))##.length) > 1)
         ||
         (List.length (React.S.value UIState.model_error) > 0)
      then
        1.0 *. 1000.0
      else
        5.0 *. 1000.0
    in
    let handle_timeout () =
      let () = Common.info "handle_timeout" in
      let () = Common.info text in
      UIState.parse_text (Js.to_string codemirror##getValue) in
    let () = timeout := Some
	  (Dom_html.window ## setTimeout
             (Js.wrap_callback (fun _ -> handle_timeout ())) delay) in
    ()
  in
  let () = codemirror##onChange(handler)
  in


  let file_select_dom = Tyxml_js.To_dom.of_input file_selector in
  let save_button_dom  : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string save_button_id))
         (fun () -> assert false)) in

  let () =
    save_button_dom##.onclick :=
      Dom.handler
	(fun _ ->
           let header = Js.string "data:text/plain;charset=utf-8," in
           let editor_text :  Js.js_string Js.t = codemirror##getValue in
           let () =
             save_button_dom##.href := header##concat((Js.escape editor_text)) in
           Js._true) in
  let file_select_handler () =
    let files = Js.Optdef.get (file_select_dom##.files)
	(fun () -> assert false)
    in
    let file = Js.Opt.get (files##item (0))
	(fun () -> assert false)
    in
    let filename = file##.name in
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
  let ()  =
    Lwt.async (fun () -> Lwt_js_events.changes
		  file_select_dom
		  (fun _ _ ->
		     if not !has_been_modified ||
			Js.to_bool
			  (Dom_html.window##confirm
			     (Js.string "Modifications will be lost, do you wish to continue?"))
		     then file_select_handler ()
		     else return_unit))
  in
  let () = Settings.onload () in
  ()

let onunload () = ()
