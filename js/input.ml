open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document

let opened_filename, set_opened_filename = React.S.create "model.ka"

let has_been_modified = ref (false)
let raw_program =
  <:html5<<textarea class="form-control" rows="25"></textarea> >>
let program = Tyxml_js.To_dom.of_textarea raw_program
let _ = Lwt_js_events.changes
	  program (fun _ _ -> let () = has_been_modified := true in return_unit)

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
		then program##value <- va in
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
			    header##concat (Js.escape program##value) in
			let () = has_been_modified := false in
			Js._true)

let raw_html =
  let event_number =
    Dom_html.createInput ~_type:(Js.string "number") document in
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
	      <div class="form-group"><label>Model Code</label>$raw_program$</div>
	      <div class="form-group"><label class="sr-only">Maximum number of events</label>
	      <div class="input-group">$Tyxml_js.Of_dom.of_input event_number$
	      <span class="input-group-addon">events</span></div></div> >>
