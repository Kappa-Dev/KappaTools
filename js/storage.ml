open Lwt
open Firebug
type runtime_state = { plot : string option ;
                       time_percentage : int option;
                       event_percentage : int option;
                       tracked_events : int option;
                       log_buffer : string
                     }
let get_plot state = match state with
    None -> None
  | Some state -> state.plot
let get_time_percentage state = match state with
    None -> None
  | Some state -> state.time_percentage

let get_event_percentage state = match state with
    None -> None
  | Some state -> state.event_percentage

let get_tracked_events state = match state with
    None -> None
  | Some state -> state.tracked_events

let get_log_buffer state = match state with
    None -> None
  | Some state -> Some state.log_buffer

let log_buffer = Buffer.create 512
let log_form = Format.formatter_of_buffer log_buffer

let format_error_message (message,linenumber) = Format.sprintf "Error at %s : %s" (Location.to_string linenumber) message
let model_nb_plot, set_model_nb_plot = React.S.create 0
let model_max_events, set_model_max_events = React.S.create None
let model_max_time, set_model_max_time = React.S.create None
let model_text, set_model_text = React.S.create ""
let model_runtime_error_message, set_model_runtime_error_message = React.S.create (None : string option)
let model_runtime_state , set_model_runtime_state = React.S.create (None : runtime_state option)
let stop, stopper = Lwt.task ()
let opened_filename, set_opened_filename = React.S.create "model.ka"

let show_graph stop =
  let rec aux () =
    let () = match React.S.value model_runtime_state with
        Some state -> set_model_runtime_state (Some { state with plot = Some (Plot.value 555) })
      | None -> ()
    in
    stop <?> (Lwt_js.sleep 5. >>= aux) in
  aux ()

let write_out stop counter =
  let rec aux () =
    let () = match React.S.value model_runtime_state with
        Some state -> set_model_runtime_state (Some { state with time_percentage = Counter.time_percentage counter
                                                               ; event_percentage = Counter.event_percentage counter
                                                               ; tracked_events = Some (Counter.tracked_events counter)
                                                               ; log_buffer = Buffer.contents log_buffer })
      | None -> ()
    in
    stop <?> (Lwt_js.sleep 2. >>= aux) in
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

let start () =
  match React.S.value model_syntax_error with
    Some error -> set_model_runtime_error_message (Some (format_error_message error));
                  return_unit
  | None ->
     catch
       (fun () ->
        let result = React.S.value model_ast in
        let (counter : Counter.t) = React.S.value model_counter in
        let () = Counter.reinitialize counter in
        let () = set_model_runtime_state (Some  { plot = None;
                                                  time_percentage = None;
                                                  event_percentage = None;
                                                  tracked_events = None ;
                                                  log_buffer = Buffer.contents log_buffer
                                                }) in
        wrap4 (Eval.initialize ?rescale_init:None) log_form [] counter result
        >>= fun (_kasa_state,env,domain,graph,state) ->
                let () = Plot.create "foo.svg" in
                let () = if (React.S.value model_nb_plot) > 0 then
                           Plot.plot_now
                             env (Counter.current_time counter)
                             (State_interpreter.observables_values env counter graph state) in
                Lwt.join [ show_graph stop;
                           write_out stop counter;
                           State_interpreter.loop_cps
                             log_form
                             (fun f -> if Lwt.is_sleeping stop
                                       then Lwt.bind (Lwt_js.yield ()) f
                                       else Lwt.return_unit)
                             (fun f _ _ _ _ ->
                              let () = Lwt.wakeup stopper () in
                              let () = ExceptionDefn.flush_warning f in Lwt.return_unit)
                   env domain counter graph state]
       )
       (function
         | ExceptionDefn.Malformed_Decl error ->
            let () = set_model_runtime_error_message (Some (format_error_message error)) in
            let () = Lwt.wakeup stopper () in
            return_unit
         | ExceptionDefn.Internal_Error error ->
            let () = set_model_runtime_error_message (Some (format_error_message error)) in
            let () = Lwt.wakeup stopper () in
            return_unit
         | Invalid_argument error ->
            let message = Format.sprintf "Runtime error %s" error in
            let () = set_model_runtime_error_message (Some message) in
            let () = Lwt.wakeup stopper () in
            return_unit
         | Sys_error message ->
            let () = set_model_runtime_error_message (Some message) in
            let () = Lwt.wakeup stopper () in
            return_unit
         | e -> fail e)
let stop () =
  Lwt.wakeup stopper ();
  set_model_runtime_state None
