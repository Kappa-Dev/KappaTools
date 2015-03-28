module Html5 = Tyxml_js.Html5

let raw_alert_close_button () =
  Html5.button ~a:[Html5.a_class ["close"];
		   Html5.a_button_type `Button;
		   Html5.Unsafe.string_attrib "data-dismiss" "alert"]
	       [Html5.span [Html5.entity "times"]]

let show_warnings div =
  let form_warnings =
    List.map
      (fun (pos,msg) ->
       let pstr =
	   match pos with
	   | Some pos -> Format.asprintf "%a@," Pp.position pos
	   | None -> ""
	 in
       let str = Format.asprintf "%t" msg in
       <:html5<<p>$str:pstr$<strong>Warning</strong>: $str:str$</p> >>)
      !ExceptionDefn.warning_buffer in
  let raw =
    (*<button class="close" data-dismiss="alert" aria-label="Close">*)
    (*<span aria-hidden="true">*)
    <:html5<<div class="alert alert-warning alert-dismissible">
	    $raw_alert_close_button ()$
	    $list:form_warnings$
	    </div> >> in
  if !ExceptionDefn.warning_buffer <> [] then
    let () = ExceptionDefn.warning_buffer := [] in
    Dom.appendChild div (Tyxml_js.To_dom.of_div raw)

let show_error pr div (x,pos) =
  let raw =
  <:html5<<div class="alert alert-danger alert-dismisible">
	  $raw_alert_close_button ()$
	  <p>$str:Format.asprintf "%a %a@." Pp.position pos pr x$</p>
	  </div> >> in
  Dom.appendChild div (Tyxml_js.To_dom.of_div raw)

let show_info pr div =
  let raw =
  <:html5<<div class="alert alert-info alert-dismisible">
	  $raw_alert_close_button ()$
	  <p>$str:Format.asprintf "%t@." pr$</p>
	  </div> >> in
  Dom.appendChild div (Tyxml_js.To_dom.of_div raw)
