open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document

let raw_editor = Html5.div ~a:[Html5.a_id "editor"] []
let editor = Tyxml_js.To_dom.of_div raw_editor

let has_been_modified = ref (false)
(*let _ = Lwt_js_events.changes
	  program (fun _ _ -> let () = has_been_modified := true in return_unit)
 *)
let load_file = Dom_html.createInput ~_type:(Js.string "file") document
let () = load_file##placeholder <- Js.string "Load file"
let () = load_file##className <- Js.string "form-control"

let raw_save_file =
  Html5.a
    ~a:[Tyxml_js.R.Html5.Unsafe.string_attrib "download" Storage.opened_filename;
	Html5.Unsafe.string_attrib "role" "button";
	Html5.a_class ["btn";"btn-default"]]
    [Html5.pcdata "Save file"]
let save_file = Tyxml_js.To_dom.of_a raw_save_file

let raw_event_number =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
	Html5.a_placeholder "Max number";
	Tyxml_js.R.Html5.a_value
	  (React.S.l1 (fun x -> match x with
				| Some va -> string_of_int va
				| None -> "") Storage.model_max_events)]
    ()
let raw_time_limit =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
	Html5.a_placeholder "Time limit";
	Tyxml_js.R.Html5.a_value
	  (React.S.l1 (fun x -> match x with
				| Some va -> string_of_float va
				| None -> "") Storage.model_max_time)]
    ()
let raw_plot_points =
  Html5.input
    ~a:[Html5.a_input_type `Number; Html5.a_class ["form-control"];
	Html5.a_placeholder "Expected number";
	Tyxml_js.R.Html5.a_value
	  (React.S.l1 string_of_int Storage.model_nb_plot)]
    ()

let get_initial_content editor_obj () =
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
    with Not_found | Failure "int_of_string" -> () in
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
	 let () = Ace.set_editor_value editor_obj
		  @@ Js.string content.XmlHttpRequest.content in
	 return_unit)
  with Not_found -> return_unit

let raw_html =
  let event_number = Tyxml_js.To_dom.of_input raw_event_number in
  let () =
    event_number##onchange <-
      Dom_html.handler
	(fun _ ->
	 let () =
           Storage.set_model_max_events
	     (try Some (int_of_string (Js.to_string (event_number##value)))
              with Failure _ -> None) in
	 Js._false) in
  let time_limit = Tyxml_js.To_dom.of_input raw_time_limit in
  let () =
    time_limit##onchange <-
      Dom_html.handler
	(fun _ ->
	 let () =
           Storage.set_model_max_time
	     (try Some (float_of_string (Js.to_string (time_limit##value)))
              with Failure _ -> None) in
	 Js._false) in
  let plot_points = Tyxml_js.To_dom.of_input raw_plot_points in
  let () =
    plot_points##onchange <-
      Dom_html.handler
	(fun _ ->
	 let () =
	   try Storage.set_model_nb_plot
		 (int_of_string (Js.to_string (plot_points##value)))
           with Failure _ -> () in
	 Js._false) in
  <:html5list<<form class="form-inline">
	      <div class="input-group">
	      <span class="input-group-addon">Load File</span>
	      $Tyxml_js.Of_dom.of_input load_file$
	      </div>$raw_save_file$</form>
	      $raw_editor$
	      <div class="form-group"><label class="sr-only">Maximum number of events</label>
	      <div class="input-group">$raw_event_number$
	      <span class="input-group-addon">events</span></div>
	      <label class="sr-only">Maximum simulation time</label>
	      <div class="input-group">$raw_time_limit$
	      <span class="input-group-addon">sec</span></div>
	      <label class="sr-only">Number of plotted points</label>
	      <div class="input-group">$raw_plot_points$
	      <span class="input-group-addon">points</span></div></div> >>

let setup_editor () =
  let editor_obj = Ace.create (Js.string "editor") in
  let _ = Lwt_js_events.changes
	    load_file
	    (fun _ _ ->
	     let files =
	       Js.Optdef.get (load_file##files) (fun () -> assert false) in
	     let file = Js.Opt.get (files##item (0)) (fun () -> assert false) in
	     let () = Storage.set_opened_filename (Js.to_string file##name) in
	     File.readAsText file >>=
	       (fun va ->
		let () =
		  if not !has_been_modified ||
		       Js.to_bool
			 (Dom_html.window##confirm
			    (Js.string "Load with loosing modifications?"))
		  then Ace.set_editor_value editor_obj va in
		let () = has_been_modified := false in
		return_unit)) in
  let () = save_file##onclick <-
	     Dom.handler
	       (fun _ ->
		let header = Js.string "data:text/plain;charset=utf-8," in
		let () =
		  save_file##href <-
		    header##concat (Js.escape (Ace.get_editor_value editor_obj)) in
		let () = has_been_modified := false in
		Js._true) in
  editor_obj
