open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document

let opened_filename, set_opened_filename = React.S.create "model.ka"
let max_events, set_max_events = React.S.create None

let raw_editor = Html5.div ~a:[Html5.a_id "editor"] []
let editor = Tyxml_js.To_dom.of_div raw_editor

let has_been_modified = ref (false)
(*let _ = Lwt_js_events.changes
	  program (fun _ _ -> let () = has_been_modified := true in return_unit)
 *)
let load_file = Dom_html.createInput ~_type:(Js.string "file") document
let () = load_file##placeholder <- Js.string "Load file"
let () = load_file##className <- Js.string "form-control"
let _ = Lwt_js_events.changes
	  load_file
	  (fun _ _ ->
	   let files =
	     Js.Optdef.get (load_file##files) (fun () -> assert false) in
	   let file = Js.Opt.get (files##item (0)) (fun () -> assert false) in
	   let () = set_opened_filename (Js.to_string file##name) in
	   File.readAsText file >>=
	     (fun va ->
	      let () =
		if not !has_been_modified ||
		     Js.to_bool
		       (Dom_html.window##confirm
				       (Js.string "Load with loosing modifications?"))
		then Ace.set_editor_value va in
	      let () = has_been_modified := false in
	      return_unit))

let raw_save_file =
  Html5.a ~a:[Tyxml_js.R.Html5.Unsafe.string_attrib "download" opened_filename;
	      Html5.Unsafe.string_attrib "role" "button";
	      Html5.a_class ["btn";"btn-default"]]
	  [Html5.pcdata "Save file"]
let save_file = Tyxml_js.To_dom.of_a raw_save_file
let () = save_file##onclick <-
	   Dom.handler (fun _ ->
			let header =
			  Js.string "data:text/plain;charset=utf-8," in
			let () =
			  save_file##href <-
			    header##concat (Js.escape (Ace.get_editor_value ())) in
			let () = has_been_modified := false in
			Js._true)
let raw_event_number =
  Html5.input ~a:[Html5.a_input_type `Number;
		  Html5.a_class ["form-control"];
		  Html5.a_placeholder "Max number";
		  Tyxml_js.R.Html5.a_value
		    (React.S.l1 (fun x -> match x with
					     | Some va -> string_of_int va
					     | None -> "") max_events)]
	      ()

let get_initial_content () =
  let args = Url.Current.arguments in
  let () =
    try
	set_max_events (Some (int_of_string (List.assoc "nb_events" args)))
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
	      set_opened_filename filename in
	 let () = Ace.set_editor_value
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
           set_max_events
	     (try
	       Some (int_of_string (Js.to_string (event_number##value)))
             with Failure _ -> None) in
	 Js._false) in
  <:html5list<<form class="form-inline">
	      <div class="input-group">
	      <span class="input-group-addon">Load File</span>
	      $Tyxml_js.Of_dom.of_input load_file$
	      </div>$raw_save_file$</form>
	      $raw_editor$
	      <div class="form-group"><label class="sr-only">Maximum number of events</label>
	      <div class="input-group">$raw_event_number$
	      <span class="input-group-addon">events</span></div></div> >>
