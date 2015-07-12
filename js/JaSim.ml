open Lwt

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
    Lwt_js.sleep 1. >>= (fun () -> return va_l) >>= aux in
  aux 0

let run stop out_div s =
  catch
    (fun () ->
     let () = Parameter.pointNumberValue := 100 in
     let () = Ast.init_compil () in
     wrap1 parse s
     >>= fun () ->
     let result = !Ast.result in
     wrap3 Eval.initialize log_form [] result
     >>= fun (env,counter,state) ->
     let () = Plot.create "foo.svg" in
     let () = if !Parameter.pointNumberValue > 0 then
		Plot.plot_now env counter state in
     let profiling = Compression_main.init_secret_log_info () in
     let () = Feedback.show_warnings out_div in
     catch
       (fun () ->
	(Run.loop_cps log_form
		      (fun f -> if Lwt.is_sleeping stop
				then Lwt.bind (Lwt_js.yield ()) f
				else Lwt.return_unit)
		      (fun _ _ _ _ _ _ -> Lwt.return_unit)
		      state profiling [] counter env)
	>>= fun () -> return (Plot.value 555))
       (function
	 | ExceptionDefn.Deadlock ->
	    let () =
	      Feedback.show_info
		(fun f ->
		 Format.fprintf
		   f "A deadlock was reached after %d events and %Es (Activity = %.5f)"
		   (Mods.Counter.event counter) (Mods.Counter.time counter)
		   (State.total_activity state)) out_div in
	    return ""
	 | e -> fail e))
    (function
      | ExceptionDefn.Syntax_Error er ->
	 let () = Feedback.show_error Format.pp_print_string out_div er in
	 return ""
      | ExceptionDefn.Malformed_Decl er ->
	 let () = Feedback.show_error Format.pp_print_string out_div er in
	 return ""
      | ExceptionDefn.Internal_Error er ->
	 let () =
	   Feedback.show_error
	     (fun f x -> Format.fprintf f "Internal Error (please report):@ %s" x)
	     out_div er in
	 return ""
      | Invalid_argument msg ->
	 let () =
	   Feedback.show_error
	     (fun f msg ->
	      Format.fprintf f "Runtime error %s" msg)
	     out_div (Term.with_dummy_pos msg) in
	 return ""
      | Sys_error msg ->
	 let () = Feedback.show_error
		    Format.pp_print_string out_div (Term.with_dummy_pos msg) in
	 return ""
      | e -> fail e)

let launch_simulation go_button stop_button out_div graph =
  let () = Buffer.reset log_buffer in
  let () = go_button##disabled <- Js._true in
  let () = stop_button##disabled <- Js._false in
  let stoppe, stopper = Lwt.wait () in
  let () =
    stop_button##onclick <- Dom_html.handler
			       (fun _ -> let () = Lwt.wakeup stopper () in
					 Js._false) in
  run stoppe out_div (Js.to_string (Ace.get_editor_value ())) >>=
    fun plot ->
    let () = Feedback.show_warnings out_div in
    let () = go_button##disabled <- Js._false in
    let () = stop_button##disabled <- Js._true in
    let () =
      graph##innerHTML <- Js.string plot in
    return ()

let onload _ =
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
	       (fun () -> assert false) in
  let raw_go_button =
    <:html5<<button class="btn btn-primary btn-block">Simulate</button> >> in
  let raw_stop_button =
    <:html5<<button class="btn btn-danger btn-block" disabled="disabled">Stop</button> >> in
  let raw_input =
    <:html5<<div class="col-lg-6">
	    $list:Input.raw_html$
	    $raw_go_button$$raw_stop_button$</div> >> in
  let raw_graph = <:html5<<div id="graph"></div> >> in
  let graph = Tyxml_js.To_dom.of_div raw_graph in
  let raw_log = <:html5<<div id="log" class="alert alert-info"></div> >> in
  let log = Tyxml_js.To_dom.of_div raw_log in
  let raw_output =
    <:html5<<div class="col-lg-6 visible-lg-block">$raw_graph$$raw_log$</div> >> in
  let output = Tyxml_js.To_dom.of_div raw_output in
  let go_button = Tyxml_js.To_dom.of_button raw_go_button in
  let stop_button = Tyxml_js.To_dom.of_button raw_stop_button in
  let () = Lwt_js_events.async (write_out log) in
  let () = Lwt_js_events.async (Input.get_initial_content) in
  let _ = Lwt_js_events.clicks go_button
			       (fun _ _ ->
				launch_simulation go_button stop_button output graph) in
  let skeleton = Tyxml_js.To_dom.of_div
		 <:html5<<div class="row">$raw_input$$raw_output$</div> >> in
  let () = Dom.appendChild main skeleton in
  let () = Dom.appendChild document##body Ace.starter in
  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload
