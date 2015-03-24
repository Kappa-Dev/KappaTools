module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let log_buffer = Buffer.create 512
let log_form = Format.formatter_of_buffer log_buffer

let parse s =
    KappaParser.start_rule KappaLexer.token (Lexing.from_string s)

let write_out div () =
  let p = Dom_html.createP document in
  let () = Dom.appendChild div p in
  let rec aux old_l =
    let () = Format.pp_print_flush log_form () in
    let va = Buffer.contents log_buffer in
    let va_l = String.length va in
    let () = if va_l <> old_l then p##innerHTML <- (Js.string va) in
    Lwt.bind (Lwt.bind (Lwt_js.sleep 1.) (fun () -> Lwt.return va_l)) aux in
  aux 0

let run stop out_div s =
  try
    let result =
      let () = Parameter.pointNumberValue := 100 in
      let () = Ast.init_compil () in
      let () = parse s in
      !Ast.result in
    let (env,counter,state) = Eval.initialize log_form [] result in
    let () = Plot.create "foo.svg" in
    let () = if !Parameter.pointNumberValue > 0 then
	       Plot.plot_now env counter state in
    let profiling = Compression_main.D.S.PH.B.PB.CI.Po.K.P.init_log_info () in
    let () = Feedback.show_warnings out_div in
    Lwt.bind
      (Run.loop_cps log_form
		    (fun f -> if Lwt.is_sleeping stop
			      then Lwt.bind (Lwt_js.yield ()) f
			      else Lwt.return_unit)
		    (fun _ _ _ _ _ _ -> Lwt.return_unit)
		    state profiling [] counter env)
      (fun () ->
       Lwt.return (Plot.value 555))
  with
  | ExceptionDefn.Syntax_Error er ->
     let () = Feedback.show_error Format.pp_print_string out_div er in
     Lwt.return ""
  | ExceptionDefn.Semantics_Error ((fn,ln,cn), msg) ->
     let () = Format.eprintf "***Error (%s) line %d, char %d: %s***@."
			     fn ln cn msg in
     Lwt.return ""
  | ExceptionDefn.Malformed_Decl er ->
     let () = Feedback.show_error Format.pp_print_string out_div er in
     Lwt.return ""
  | ExceptionDefn.Internal_Error er ->
     let () =
       Feedback.show_error
	 (fun f x -> Format.fprintf f "Internal Error (please report):@ %s" x)
	 out_div er in
     Lwt.return ""
  | Invalid_argument msg ->
      let s = "" (*Printexc.get_backtrace()*) in
      let () =
	Format.eprintf "@.@[<v>***Runtime error %s***@,%s@]@." msg s in
      Lwt.return ""

let launch_simulation go_button stop_button out_div graph program =
  let () = Buffer.reset log_buffer in
  let () = go_button##disabled <- Js._true in
  let () = stop_button##disabled <- Js._false in
  let stoppe, stopper = Lwt.wait () in
  let () =
    stop_button##onclick <- Dom_html.handler
			       (fun _ -> let () = Lwt.wakeup stopper () in
					 Js._false) in
  Lwt.bind (run stoppe out_div (Js.to_string program##value))
	   (fun plot ->
	    let () = Feedback.show_warnings out_div in
	    let () = go_button##disabled <- Js._false in
	    let () = stop_button##disabled <- Js._true in
	    let () =
	      graph##innerHTML <- Js.string plot in
	    Lwt.return ())

let onload _ =
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
	       (fun () -> assert false) in
  let raw_go_button =
    <:html5<<button class="btn btn-primary btn-block">Simulate</button> >> in
  let raw_stop_button =
    <:html5<<button class="btn btn-danger btn-block" disabled="disabled">Stop</button> >> in
  let raw_input =
    <:html5<<div class="col-md-6">
	    $list:Input.raw_html$
	    $raw_go_button$$raw_stop_button$</div> >> in
  let raw_graph = <:html5<<div id="graph"></div> >> in
  let graph = Tyxml_js.To_dom.of_div raw_graph in
  let raw_log = <:html5<<div id="log" class="alert alert-info"></div> >> in
  let log = Tyxml_js.To_dom.of_div raw_log in
  let raw_output =
    <:html5<<div class="col-md-6">$raw_graph$$raw_log$</div> >> in
  let output = Tyxml_js.To_dom.of_div raw_output in
  let go_button = Tyxml_js.To_dom.of_button raw_go_button in
  let stop_button = Tyxml_js.To_dom.of_button raw_stop_button in
  let () = Lwt_js_events.async (write_out log) in
  let _ = Lwt_js_events.clicks go_button
			       (fun _ _ ->
				launch_simulation go_button stop_button output graph Input.program) in
  let skeleton = Tyxml_js.To_dom.of_div
		 <:html5<<div class="row">$raw_input$$raw_output$</div> >> in
  let () = Dom.appendChild main skeleton in
  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload
