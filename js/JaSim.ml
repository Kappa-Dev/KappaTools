open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let log_buffer = Buffer.create 512
let log_form = Format.formatter_of_buffer log_buffer

let parse s =
    KappaParser.start_rule KappaLexer.token (Lexing.from_string s)

let write_out stop div counter =
  let () = while Js.Opt.test div##firstChild do
	     Js.Opt.iter (div##firstChild) (Dom.removeChild div)
	   done in
  let p = Dom_html.createP document in
  let stat = Dom_html.createDiv document in
  let () = Dom.appendChild div p in
  let () = Dom.appendChild div stat in
  let rec aux old_l =
    let () = Format.pp_print_flush log_form () in
    let va = Buffer.contents log_buffer in
    let va_l = String.length va in
    let () =
      stat##innerHTML <- (Js.string (Counter.to_bootstrap_html counter)) in
    let () = if va_l <> old_l then p##innerHTML <- (Js.string va) in
    stop <?> (Lwt_js.sleep 2. >>= (fun () -> return va_l) >>= aux) in
  aux 0

let run stop log_div out_div =
  catch
    (fun () ->
     let () = Ast.init_compil () in
     wrap1 parse (React.S.value Storage.model_text)
     >>= fun () ->
     let result = !Ast.result in
     let counter = React.S.value Storage.model_counter in
     wrap4 Eval.initialize log_form [] counter result
     >>= fun (_kasa_state,env,domain,graph,state) ->
     let () = Plot.create "foo.svg" in
     let () =
       if (React.S.value Storage.model_nb_plot) > 0 then
	 Plot.plot_now
	   env (Counter.current_time counter)
	   (State_interpreter.observables_values env counter graph state) in
     let () = Feedback.show_warnings out_div in
     Lwt.pick [write_out stop log_div counter;
     State_interpreter.loop_cps
       log_form
       (fun f -> if Lwt.is_sleeping stop
		 then Lwt.bind (Lwt_js.yield ()) f
		 else Lwt.return_unit)
       (fun f _ _ _ _ ->
	let () = ExceptionDefn.flush_warning f in Lwt.return_unit)
       env domain counter graph state]
     >>= fun () -> return (Plot.value 555))
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
	     out_div (Location.dummy_annot msg) in
	 return ""
      | Sys_error msg ->
	 let () = Feedback.show_error
		    Format.pp_print_string out_div (Location.dummy_annot msg) in
	 return ""
      | e -> fail e)

let launch_simulation go_button stop_button out_div log graph =
  let () = Buffer.reset log_buffer in
  let () = go_button##disabled <- Js._true in
  let () = stop_button##disabled <- Js._false in
  let stoppe, stopper = Lwt.task () in
  let () =
    stop_button##onclick <- Dom_html.handler
			       (fun _ -> let () = Lwt.wakeup stopper () in
					 Js._false) in
  run stoppe log out_div >>=
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
  let _ = Lwt_js_events.clicks go_button
			       (fun _ _ ->
				launch_simulation
				  go_button stop_button output log graph) in
  let skeleton = Tyxml_js.To_dom.of_div
		 <:html5<<div class="row">$Ui.raw_menu$$raw_input$$raw_output$</div> >> in
  let () = Dom.appendChild main skeleton in
  let editor_obj = Input.setup_editor () in
  let () = Lwt_js_events.async (Input.get_initial_content editor_obj) in
  let () =  (Ace.starter editor_obj Input.has_been_modified) in
  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload
