open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document

let opened_filename, set_opened_filename = React.S.create "model.ka"

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
let event_number =
  Dom_html.createInput ~_type:(Js.string "number") document

let get_initial_content () =
  let args = Url.Current.arguments in
  let () =
    try
      Parameter.maxEventValue :=
	Some (int_of_string (List.assoc "nb_events" args))
    with Not_found | Failure "int_of_string" -> () in
  let () = event_number##value <- Js.string (match !Parameter.maxEventValue with
					     | Some va -> string_of_int va
					     | None -> "") in
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
  let () = event_number##placeholder <- Js.string "Max number" in
  let () = event_number##className <- Js.string "form-control" in
  let () =
    event_number##onchange <-
      Dom_html.handler
	(fun _ ->
	 let () =
           Parameter.maxEventValue :=
	     try
	       Some (int_of_string (Js.to_string (event_number##value)))
             with Invalid_argument _ -> None in
	 event_number##value <- Js.string (match !Parameter.maxEventValue with
					   | Some va -> string_of_int va
					   | None -> "");
	 Js._false) in
  <:html5list<<form class="form-inline">
	      <div class="input-group">
	      <span class="input-group-addon">Load File</span>
	      $Tyxml_js.Of_dom.of_input load_file$
	      </div>$raw_save_file$</form>
	      $raw_editor$
	      <div class="form-group"><label class="sr-only">Maximum number of events</label>
	      <div class="input-group">$Tyxml_js.Of_dom.of_input event_number$
	      <span class="input-group-addon">events</span></div></div> >>
