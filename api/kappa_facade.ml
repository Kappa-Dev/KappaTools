open Lwt.Infix

(** Interface to kappa runtime *)
(* Error messages *)
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_intervention_context =
  "Invalid runtime state: missing intervention context"

(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process =
  object
    method log : ?exn:exn -> string -> unit Lwt.t
    method yield : unit -> unit Lwt.t
    method min_run_duration : unit -> float
  end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) =
      let () = ignore(exn) in
      Lwt.return_unit
    method yield () = Lwt.return_unit
    method min_run_duration() = 0.0
  end;;

type file_index =
  { file_index_file_id : Api_types_t.file_id ;
    file_index_line_offset : int ;
    file_index_char_offset : int ;
    file_line_count : int ; }

type kappa_file =
  { kappa_file_id : Api_types_t.file_id ;
    kappa_file_code : string ;
  }

type kappa_code = kappa_file list

(** State of the running simulation. *)
type t =
  { mutable is_running : bool ;
    mutable run_finalize : bool ;
    mutable pause_condition : (Pattern.id array list,int) Alg_expr.bool ;
    dumpIfDeadlocked : bool;
    maxConsecutiveClash : int;
    counter : Counter.t ;
    log_buffer : Buffer.t ;
    log_form : Format.formatter ;
    mutable plot : Data.plot ;
    mutable snapshots : Data.snapshot list ;
    mutable dins : Data.din list ;
    mutable species : (float*User_graph.connected_component) list Mods.StringMap.t;
    mutable files : string list Mods.StringMap.t ;
    mutable error_messages : Api_types_t.errors ;
    mutable trace : Trace.t ;
    ast : Ast.parsing_compil;
    contact_map : Contact_map.t ;
    mutable env : Model.t ;
    mutable graph : Rule_interpreter.t ;
    mutable state : State_interpreter.t ;
    init_l :
      (Primitives.alg_expr * Primitives.elementary_rule) list ;
    mutable lastyield : float ;
  }

let create_t ~log_form ~log_buffer ~contact_map
    ~dumpIfDeadlocked ~maxConsecutiveClash ~env ~counter ~graph
    ~state ~init_l ~lastyield ~ast : t = {
  is_running = false; run_finalize = false; counter; log_buffer; log_form;
  pause_condition = Alg_expr.FALSE; dumpIfDeadlocked; maxConsecutiveClash;
  plot = Data.init_plot env;
  snapshots = [];
  dins = [];
  species = Mods.StringMap.empty;
  files = Mods.StringMap.empty;
  error_messages = [];
  trace = [];
  ast; contact_map; env; graph; state; init_l;
  lastyield;
}

let reinitialize random_state t =
  let () = Counter.reinitialize t.counter in
(*  let () = Format.pp_print_flush t.log_form () in
    let () = Buffer.reset t.log_buffer in*)
  t.is_running <- false;
  t.run_finalize <- false;
  t.pause_condition <- Alg_expr.FALSE;
  t.plot <- Data.init_plot t.env ;
  t.snapshots <- [];
  t.dins <- [];
  t.files <- Mods.StringMap.empty;
  t.error_messages <- [];
  t.graph <- Rule_interpreter.empty
      ~with_trace:false
      random_state t.env t.counter;
  t.state <- State_interpreter.empty ~with_delta_activities:false t.env []

let catch_error : 'a . (Api_types_t.errors -> 'a) -> exn -> 'a =
  fun handler ->
    (function
      |  ExceptionDefn.Syntax_Error
          ((message,region) : string Locality.annot) ->
        handler [Api_data.api_message_errors ~region message]
      | ExceptionDefn.Malformed_Decl
          ((message,region) : string Locality.annot) ->
        handler [Api_data.api_message_errors ~region message]
      | ExceptionDefn.Internal_Error
          ((message,region) : string Locality.annot) ->
        handler [Api_data.api_message_errors ~region message]
      | Invalid_argument error ->
        handler [Api_data.api_message_errors ("Runtime error "^ error)]
      | exn -> handler (Api_data.api_exception_errors exn))

type file = { file_id : string ; file_content : string }

let rec compile_file
    (yield : unit -> unit Lwt.t)
    (compile : Ast.parsing_compil)
  : file list -> (Ast.parsing_compil, Api_types_t.errors) Result.result Lwt.t =
  function
  | [] -> Lwt_result.return compile
  | file::files ->
    let lexbuf = Lexing.from_string file.file_content in
    let () = lexbuf.Lexing.lex_curr_p <-
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file.file_id }  in
    let compile =
      { compile with Ast.filenames = file.file_id :: compile.Ast.filenames } in
    Lwt.catch
      (fun () ->
         (Lwt.wrap1 Klexer4.model lexbuf) >>=
         (fun (insts,err) ->
            (yield ()) >>=
            (fun () ->
               let new_compile = Cst.append_to_ast_compil insts compile in
               compile_file yield new_compile files >>=
               if err = [] then Lwt.return
               else
                 let err = List.map
                     (fun (m,region) -> Api_data.api_message_errors ~region m)
                     err in
                 Result_util.fold
                   ~ok:(fun _ -> Lwt_result.fail err)
                   ~error:(fun error -> Lwt_result.fail (err@error))))
      )
      (catch_error
         (fun e ->
            (yield ()) >>=
            (fun () -> compile_file yield compile files) >>=
            (fun result ->
               let r =
                 Result_util.fold
                   ~ok:(fun _ -> Result_util.error e)
                   ~error:(fun error -> Result_util.error (e@error))
                   result
                in
                (Lwt.return r)
            )
         )
      )

let build_ast (kappa_files : file list) overwrite (yield : unit -> unit Lwt.t) =
  let log_buffer = Buffer.create 512 in
  let log_form = Format.formatter_of_buffer log_buffer in
  let post_parse ast =
    let (conf,_,_,_,_) =
      Configuration.parse ast.Ast.configurations in
    (Lwt.wrap2
       (LKappa_compiler.compil_of_ast ~syntax_version:Ast.V4) overwrite ast) >>=
    (fun
      (sig_nd,
       contact_map,
       tk_nd,_algs_nd,
       _updated_vars,
       (result :
          (Ast.agent, LKappa.rule_agent list, Raw_mixture.t,
           int, LKappa.rule) Ast.compil)) ->
      (yield ()) >>=
      (fun () ->
         (* The last yield is updated after the last yield.
            It is gotten here for the initial last yeild value. *)
         let lastyield = Sys.time () in
         try (* exception raised by compile must have used Lwt.fail.
                Something is wrong for now *)
           Eval.compile
             ~pause:(fun f -> Lwt.bind (yield ()) f)
             ~return:Lwt.return ?rescale_init:None ~compileModeOn:false
             ~outputs:(function
                 | Data.Log s ->
                   Format.fprintf log_form "%s@." s
                 | Data.Snapshot _
                 | Data.DIN _
                 | Data.Species _
                 | Data.DeltaActivities _
                 | Data.Plot _
                 | Data.TraceStep _
                 | Data.Print _ -> assert false)
             ~max_sharing:false sig_nd tk_nd contact_map result >>=
           (fun (env,with_trace,init_l) ->
              let counter =
                Counter.create
                  ~init_t:(0. : float) ~init_e:(0 : int)
                  ?max_time:None ?max_event:None
                  ~plot_period:(Counter.DT 1.) in
              let () = ExceptionDefn.flush_warning log_form in
              let theSeed =
                match conf.Configuration.seed with
                | None ->
                  let () = Random.self_init () in
                  let out = Random.bits () in
                  let () =
                    Format.fprintf log_form "Random seed used: %i@." out in
                  out
                | Some theSeed -> theSeed in
              let random_state = Random.State.make [|theSeed|] in
              let simulation =
                create_t
                  ~contact_map ~log_form ~log_buffer ~ast ~env ~counter
                  ~dumpIfDeadlocked:conf.Configuration.dumpIfDeadlocked
                  ~maxConsecutiveClash:conf.Configuration.maxConsecutiveClash
                  ~graph:(Rule_interpreter.empty
                            ~with_trace
                            random_state env counter)
                  ~state:(State_interpreter.empty ~with_delta_activities:false env [])
                  ~init_l ~lastyield
              in
              Lwt.return (Result_util.ok simulation))
         with e ->
           (catch_error
              (fun e -> Lwt.return (Result_util.error e))) e
      ))
  in
  Lwt.catch
    (fun () ->
       (compile_file yield Ast.empty_compil kappa_files) >>=
       (Result_util.fold
          ~ok:(fun raw_ast ->
              (yield ()) >>=
              (fun () -> post_parse raw_ast))
         ~error:(fun e -> Lwt.return (Result_util.error e))
       )
    )
    (catch_error (fun e -> Lwt.return (Result_util.error e)))

let outputs (simulation : t) =
  function
  | Data.DIN flux_map ->
    simulation.dins <- flux_map::simulation.dins
  | Data.DeltaActivities _ -> assert false
  | Data.Plot new_observables ->
    simulation.plot <- Data.add_plot_line new_observables simulation.plot
  | Data.Species(file,time,mix) ->
    let p = Mods.StringMap.find_default [] file simulation.species in
    simulation.species <-
      Mods.StringMap.add file ((time,mix)::p) simulation.species
  | Data.Print file_line ->
    begin
      match file_line.Data.file_line_name with
      | None ->
        Format.fprintf simulation.log_form "%s@." file_line.Data.file_line_text
      | Some na ->
        let lines = Mods.StringMap.find_default [] na simulation.files in
        simulation.files <-
          Mods.StringMap.add
            na (file_line.Data.file_line_text::lines) simulation.files
    end
  | Data.Snapshot snapshot ->
    let already_there x =
      List.exists (fun y -> x = y.Data.snapshot_file) simulation.snapshots in
    let snapshot_file =
      Tools.find_available_name
        ~already_there snapshot.Data.snapshot_file
        ~facultative:(string_of_int snapshot.Data.snapshot_event) ~ext:".ka" in
    let snapshot' = { snapshot with Data.snapshot_file } in
    simulation.snapshots <- snapshot'::simulation.snapshots
  | Data.Log s -> Format.fprintf simulation.log_form "%s@." s
  | Data.TraceStep st -> simulation.trace <- st :: simulation.trace

let interactive_outputs formatter t = function
  | Data.Log s -> Format.fprintf formatter "%s@." s
  | Data.Print file_line when file_line.Data.file_line_name = None ->
    Format.fprintf formatter "%s@." file_line.Data.file_line_text
  | Data.DIN _ | Data.DeltaActivities _ | Data.Plot _ | Data.Species _ |
    Data.Print _ | Data.Snapshot _ | Data.TraceStep _ as v -> outputs t v

let parse
    ~(system_process : system_process)
    ~(kappa_files : Api_types_t.file list)
    ~overwrites
  : (t,Api_types_t.errors) Result.result Lwt.t
  =

  let kappa_files =
    List.fold_left
      (fun acc f ->
         if f.Api_types_t.file_metadata.Api_types_t.file_metadata_compile
         then {
           file_id = f.Api_types_t.file_metadata.Api_types_t.file_metadata_id ;
           file_content = f.Api_types_t.file_content
         }::acc
         else acc)
         [] kappa_files in
  Lwt.bind
    (build_ast
       kappa_files overwrites system_process#yield)
    (Result_util.fold
       ~ok:(fun simulation -> Lwt.return (Result_util.ok simulation))
       ~error:(fun e -> Lwt.return (Result_util.error e)))

let time_yield
    ~(system_process : system_process)
    ~(t : t) : unit Lwt.t =
  let time = Sys.time () in
  if time -. t.lastyield > system_process#min_run_duration () then
    let () = t.lastyield <- time in
    system_process#yield ()
  else Lwt.return_unit

let finalize_simulation ~(t : t) : unit =
  State_interpreter.end_of_simulation
    ~outputs:(outputs t) t.log_form t.env t.counter t.graph t.state

let run_simulation
    ~(system_process : system_process) ~(t : t) stopped : unit Lwt.t =
  Lwt.catch
    (fun () ->
       let rstop = ref stopped in
       let () = t.is_running <- true in
       let rec iter () =
         (try
            let () =
              while (not !rstop) &&
                    Sys.time () -. t.lastyield <
                    system_process#min_run_duration ()
              do
                let (stop,graph',state') =
                  State_interpreter.a_loop
                    ~outputs:(outputs t) ~dumpIfDeadlocked:t.dumpIfDeadlocked
                    ~maxConsecutiveClash:t.maxConsecutiveClash
                    t.env t.counter t.graph t.state in
                rstop := stop || Rule_interpreter.value_bool
                           t.counter graph' t.pause_condition;
                t.graph <- graph';
                t.state <- state'
              done in
            Lwt.return_unit
          with e -> Lwt.fail e) >>= fun () ->
         if !rstop then
           let () = t.is_running <- false in
           Lwt.return_unit
         else if t.is_running then
           (system_process#yield ()) >>= (fun () ->
               let () = t.lastyield <- Sys.time () in iter ())
         else
           Lwt.return_unit in
       (iter ()) >>=
       (fun () ->
          let () =
            if t.run_finalize then
              finalize_simulation ~t:t
            else
              ExceptionDefn.flush_warning t.log_form
          in
          Lwt.return_unit))
    (catch_error
       (fun e ->
          let () = t.is_running <- false in
          let () = t.error_messages <- e in
          Lwt.return_unit))

let start
    ~(system_process : system_process)
    ~(parameter : Api_types_t.simulation_parameter)
    ~(t : t)
  : (unit,Api_types_t.errors) Result.result Lwt.t =
  let lexbuf =
    Lexing.from_string parameter.Api_types_t.simulation_pause_condition in
  Lwt.catch (fun () ->
      (*let () =
          Counter.set_max_time
                 t.counter
                 parameter.Api_types_j.simulation_max_time
               in
               let () =
               Counter.set_max_events
                 t.counter
                 parameter.Api_types_j.simulation_max_events
               in*)
      let random_state =
        match parameter.Api_types_t.simulation_seed with
        | None -> Random.State.make_self_init ()
        | Some seed -> Random.State.make [|seed|] in
      let () = reinitialize random_state t in
      try
        let pause = Kparser4.standalone_bool_expr Klexer4.token lexbuf in
        Lwt.wrap4 (Evaluator.get_pause_criteria
                     ~max_sharing:false ~syntax_version:Ast.V4)
          t.contact_map t.env t.graph pause >>=
        fun (env',graph',b'') ->
        let () = t.env <- env' in
        let () = t.graph <- graph' in
        let () = t.pause_condition <- b'' in
        let () =
          Counter.set_plot_period
            t.counter
            (Counter.DT parameter.Api_types_t.simulation_plot_period) in
        let () =
          Lwt.async
            (fun () ->
               try (* exception raised by build_initial_state must have been
                      raised with Lwt.fail. Something is wrong for now... *)
                 Eval.build_initial_state
                   ~bind:(fun x f ->
                       (time_yield ~system_process:system_process ~t:t) >>=
                       (fun () -> x >>= f))
                   ~return:Lwt.return ~outputs:(outputs t)
                   ~with_trace:parameter.Api_types_t.simulation_store_trace
                   ~with_delta_activities:false
                   t.counter
                   t.env
                   random_state
                   t.init_l >>=
                 (fun (stop,graph,state) ->
                    let () = t.graph <- graph; t.state <- state in
                    let () = ExceptionDefn.flush_warning t.log_form in
                    let first_obs =
                      State_interpreter.observables_values
                        t.env graph t.counter in
                    let () =
                      t.plot <- Data.add_plot_line first_obs t.plot in
                    run_simulation ~system_process:system_process ~t:t stop)
               with e ->
                 catch_error
                   (fun e ->
                      let () = t.error_messages <- e in
                      Lwt.return_unit) e
            ) in
        Lwt_result.return ()
      with ExceptionDefn.Syntax_Error (message,region) ->
        Lwt_result.fail [Api_data.api_message_errors ~region message])
    (catch_error
       (fun e ->
          let () = t.error_messages <- e in
          Lwt_result.fail e))

let pause
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_t.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  let () = if t.is_running then
      t.is_running <- false
    else
      ()
  in
  Lwt.return (Result_util.ok ())

let stop
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_t.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  Lwt.catch
    (fun () ->
       let () = t.run_finalize <- true in
       (if t.is_running then
          pause ~system_process:system_process ~t:t
        else
          let () = finalize_simulation ~t:t in
          Lwt.return (Result_util.ok ()))
    )
    (catch_error (fun e -> Lwt.return (Result_util.error e)))

let perturbation
    ~(system_process : system_process)
    ~(t : t)
    ~(perturbation:Api_types_t.simulation_intervention)
  : (string, Api_types_t.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let lexbuf =
    Lexing.from_string perturbation.Api_types_t.intervention_code
  in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt_result.fail [Api_data.api_message_errors msg_process_not_paused]
       else
         try
           let e = Kparser4.standalone_effect_list Klexer4.token lexbuf in
           let log_buffer = Buffer.create 512 in
           let log_form = Format.formatter_of_buffer log_buffer in
           Lwt.wrap6
             (Evaluator.do_interactive_directives
                ~outputs:(interactive_outputs log_form t)
                ~max_sharing:false ~syntax_version:Ast.V4)
             t.contact_map t.env t.counter t.graph t.state e >>=
           fun (_,(env',(_,graph'',state'))) ->
           let () = t.env <- env' in
           let () = t.graph <- graph'' in
           let () = t.state <- state' in
           Lwt.return (Result_util.ok (Buffer.contents log_buffer))
         with ExceptionDefn.Syntax_Error (message,region) ->
           Lwt_result.fail [Api_data.api_message_errors ~region message])
    (catch_error (fun e -> Lwt_result.fail e))

let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(pause_condition : string)
  : (unit,Api_types_t.errors) Result.result Lwt.t =
  let lexbuf =
    Lexing.from_string pause_condition in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt.return (Result_util.ok ())
       else
         try
           let pause = Kparser4.standalone_bool_expr Klexer4.token lexbuf in
           Lwt.wrap4 (Evaluator.get_pause_criteria
                        ~max_sharing:false ~syntax_version:Ast.V4)
             t.contact_map t.env t.graph pause >>=
           fun (env',graph',b'') ->
           let () = t.env <- env' in
           let ()  = t.graph <- graph' in
           let () = t.pause_condition <- b'' in
           (*let () =
             Counter.set_max_time
             t.counter
             parameter.Api_types_t.simulation_max_time
             in
             let () =
             Counter.set_max_events
             t.counter
             parameter.Api_types_t.simulation_max_events
             in*)
           let () =
             Lwt.async
               (fun () ->
                  run_simulation ~system_process:system_process ~t:t false) in
           Lwt.return (Result_util.ok ())
         with ExceptionDefn.Syntax_Error (message,region) ->
           Lwt_result.fail [Api_data.api_message_errors ~region message])
    (catch_error
       (fun e -> Lwt.return (Result_util.error e)))

let progress
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_t.simulation_progress,Api_types_t.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
         Lwt.return (Result_util.ok {
             Api_types_t.simulation_progress_time =
               Counter.current_time t.counter ;
             Api_types_t.simulation_progress_time_percentage =
               Counter.time_percentage t.counter ;
             Api_types_t.simulation_progress_event =
               Counter.current_event t.counter ;
             Api_types_t.simulation_progress_event_percentage =
               Counter.event_percentage t.counter ;
             Api_types_t.simulation_progress_tracked_events =
               Counter.tracked_events t.counter ;
             Api_types_t.simulation_progress_is_running =
               t.is_running ;
           }))
      (catch_error (fun e -> Lwt.return (Result_util.error e)))
  | _ -> Lwt.return (Result_util.error t.error_messages)

let outputs
    ~(system_process : system_process)
    ~(t : t) :
  (Api_data.simulation_detail_output,Api_types_t.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
         Lwt.return (Result_util.ok {
             Api_types_t.simulation_output_plot =
               Some t.plot ;
             Api_types_t.simulation_output_dins =
               t.dins ;
             Api_types_t.simulation_output_file_lines =
               t.files ;
             Api_types_t.simulation_output_snapshots =
               t.snapshots ;
             Api_types_t.simulation_output_log_messages =
               Buffer.contents t.log_buffer ;
           }))
      (catch_error (fun e -> Lwt.return (Result_util.error e)))
  | _ -> Lwt.return (Result_util.error t.error_messages)

let efficiency t = Counter.get_efficiency t.counter

let get_raw_trace t =
  Yojson.Basic.to_string
    (`Assoc [
        "model", Model.to_yojson t.env;
        "trace", `List (List.rev_map Trace.step_to_yojson t.trace);
      ])

let get_raw_ast t =
  Yojson.Basic.to_string (Ast.compil_to_json t.ast)
