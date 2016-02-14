open Lwt
type runtime_state = { plot : Pp_svg.store;
                       time : float;
                       time_percentage : int option;
                       event : int;
                       event_percentage : int option;
                       tracked_events : int option;
                       log_buffer : string
                     }
let get_time (state : runtime_state option) : float option =
  match state with
    None -> None
  | Some state -> Some state.time

let get_plot size (state : runtime_state option) : string option =
  match state with
    None -> None
  | Some state -> Some (Pp_svg.to_string ~width:(size ()) state.plot)

let get_time_percentage (state : runtime_state option) : int option =
  match state with
    None -> None
  | Some state -> state.time_percentage

let get_event (state : runtime_state option) : int option =
  match state with
    None -> None
  | Some state -> Some state.event

let get_event_percentage (state : runtime_state option) : int option =
  match state with
    None -> None
  | Some state -> state.event_percentage

let get_tracked_events (state : runtime_state option) : int option =
  match state with
    None -> None
  | Some state -> state.tracked_events

let get_log_buffer state = match state with
    None -> None
  | Some state -> Some state.log_buffer


let format_error_message (message,linenumber) =
  Format.sprintf "Error at %s : %s" (Location.to_string linenumber) message
let model_nb_plot, set_model_nb_plot = React.S.create 0
let model_max_events, set_model_max_events = React.S.create None
let model_max_time, set_model_max_time = React.S.create None
let model_text, set_model_text = React.S.create ""
let model_runtime_error_message, set_model_runtime_error_message =
  React.S.create (None : string option)
let model_runtime_state , set_model_runtime_state =
  React.S.create (None : runtime_state option)
let model_is_running , set_model_is_running = React.S.create false
let opened_filename, set_opened_filename = React.S.create "model.ka"

let update_counter counter state log_buffer=
  { state with time_percentage = Counter.time_percentage counter
             ; time = Counter.time counter
             ; event_percentage = Counter.event_percentage counter
             ; event = Counter.event counter
             ; tracked_events = Some (Counter.tracked_events counter)
             ; log_buffer = Buffer.contents log_buffer }
let write_out thread_is_running counter label log_buffer =
  let rec aux () =
    let () = match React.S.value model_runtime_state with
        Some state -> set_model_runtime_state
                        (Some (update_counter counter state log_buffer))
      | None -> ()
    in
    if Lwt_switch.is_on thread_is_running then
      Lwt_js.sleep 2. >>= aux
    else
      return_unit in
  aux ()

let fake_env =
  Environment.init
    (Signature.create []) (NamedDecls.create [||]) (NamedDecls.create [||])
    (Operator.DepSet.empty,Operator.DepSet.empty,[||],[||])
    ([||],[||],Connected_component.Set.empty) [||] [||]

let model_env, set_model_env = React.S.create fake_env
let model_domain, set_model_domain =
  React.S.create Connected_component.Env.empty
let model_counter =
  React.S.l5 Counter.create
             model_nb_plot  (React.S.const 0.) (React.S.const 0)
             model_max_time model_max_events

let model_syntax_error, set_model_syntax_error = React.S.create (None : (string * Location.t) option)
let parse text =
  try
    set_model_syntax_error None;
    Some (KappaParser.start_rule KappaLexer.token text Ast.empty_compil)
  with ExceptionDefn.Syntax_Error e ->
    let () = set_model_syntax_error (Some e) in
    None

let model_ast =
  React.S.fmap parse Ast.empty_compil (React.S.l1 Lexing.from_string model_text)

let start ~start_continuation ~stop_continuation =
  let on_error () = set_model_is_running false;
                    stop_continuation ();
                    return_unit
  in
  match React.S.value model_syntax_error with
    Some error -> set_model_syntax_error (Some error);
                  set_model_runtime_error_message (Some (format_error_message error));
                  on_error ()
  | None ->
     let log_buffer = Buffer.create 512 in
     let log_form = Format.formatter_of_buffer log_buffer in
     let label = string_of_float (Unix.time ()) in
     let thread_is_running = Lwt_switch.create () in
     catch
       (fun () ->
        let result = React.S.value model_ast in
        let (counter : Counter.t) = React.S.value model_counter in
        let () = Counter.reinitialize counter in
        let () = start_continuation thread_is_running in
        let () = set_model_is_running true in
        wrap4 (Eval.initialize ?rescale_init:None) log_form [] counter result
        >>= fun (_kasa_state,env,domain,graph,state) ->
        let () =
	  let head =
	    Environment.map_observables
	      (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
	      env in
          set_model_runtime_state
	    (Some  {
		 plot =
		   {Pp_svg.file = "foo.svg";
		    Pp_svg.title = "KaSim output";
		    Pp_svg.descr = "";
		    Pp_svg.legend = head;
		    Pp_svg.points = [Counter.current_time counter,
				     State_interpreter.observables_values env counter graph state];
		   };
                 time = 0.0;
                 time_percentage = None;
                 event_percentage = None;
                 event = 0;
                 tracked_events = None ;
                 log_buffer = Buffer.contents log_buffer
               }) in
        Lwt.join [ write_out thread_is_running counter label log_buffer;
                           State_interpreter.loop_cps
			     ~outputs:(function
					| Data.Flux _ -> ()
					| Data.Plot (t,v) ->
					   match React.S.value model_runtime_state with
					   | None -> ()
					   | Some s ->
					      s.plot.Pp_svg.points <- (t,v) :: s.plot.Pp_svg.points
				      )
                             log_form
                             (fun f -> if Lwt_switch.is_on thread_is_running
                                       then Lwt.bind (Lwt_js.yield ()) f
                                       else Lwt.return_unit)
                             (fun _ _ ->
                              let () = ExceptionDefn.flush_warning log_form in
                              let () =
                                Format.fprintf log_form "Simulation ended" in
                              let () =
                                if Counter.nb_null_event counter = 0
                                then Format.pp_print_newline log_form ()
                                else
                                  let () =
                                    Format.fprintf
                                      log_form " (eff.: %f, detail below)@."
                                      ((float_of_int (Counter.current_event counter)) /.
                                         (float_of_int
                                            (Counter.nb_null_event counter +
                                               Counter.current_event counter))) in
                                  Counter.print_efficiency log_form counter in
                              Lwt_switch.turn_off thread_is_running)
                             env domain counter graph state]
                >>= (fun _ -> Lwt_js.sleep 1.)
                >>= (fun _ -> stop_continuation ();
                              set_model_is_running false;
                              Lwt.return_unit
                    )
       )
       (function
         | ExceptionDefn.Malformed_Decl error ->
            let () = set_model_runtime_error_message (Some (format_error_message error)) in
            let () = set_model_syntax_error (Some error) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | ExceptionDefn.Internal_Error error ->
            let () = set_model_runtime_error_message (Some (format_error_message error)) in
            let () = set_model_syntax_error (Some error) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | Invalid_argument error ->
            let message = Format.sprintf "Runtime error %s" error in
            let () = set_model_runtime_error_message (Some message) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | Sys_error message ->
            let () = set_model_runtime_error_message (Some message) in
            Lwt_switch.turn_off thread_is_running
            >>=
              (fun _ -> on_error ())
         | e -> on_error ()
                >>= (fun _ -> fail e)
            )
