(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

(* Error messages *)

(** Interface to kappa runtime *)
let msg_process_not_paused = "process not paused"

(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process = object
  method log : ?exn:exn -> string -> unit Lwt.t
  method yield : unit -> unit Lwt.t
  method min_run_duration : unit -> float
end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) =
      ignore exn;
      Lwt.return_unit

    method yield () = Lwt.return_unit
    method min_run_duration () = 0.0
  end

type t = {
  mutable is_running: bool;
  mutable run_finalize: bool;
  mutable pause_condition: (Pattern.id array list, int) Alg_expr.bool;
  parsed_seed: int option;
  dumpIfDeadlocked: bool;
  maxConsecutiveClash: int;
  patternSharing: Pattern.sharing_level;
  counter: Counter.t;
  log_buffer: Buffer.t;
  log_form: Format.formatter;
  mutable plot: Data.plot;
  mutable snapshots: Data.snapshot Mods.StringMap.t;
  mutable dins: (string * Data.din) list;
  mutable species:
    (float * User_graph.connected_component) list Mods.StringMap.t;
  mutable files: string list Mods.StringMap.t;
  mutable error_messages: Result_util.message list;
  (*mutable*) trace: Buffer.t;
  inputs_buffer: Buffer.t;
  inputs_form: Format.formatter;
  parsing_compil: Ast.parsing_compil;
  contact_map: Contact_map.t;
  mutable env: Model.t;
  mutable graph: Rule_interpreter.t option;
  mutable state: State_interpreter.t;
  init_l: (Primitives.alg_expr * Primitives.elementary_rule) list;
  mutable lastyield: float;
}
(** State of the running simulation. *)

let create_t ~log_form ~log_buffer ~contact_map ~inputs_buffer ~inputs_form
    ~dumpIfDeadlocked ~maxConsecutiveClash ~patternSharing ~env ~counter ~graph
    ~state ~init_l ~lastyield ~parsing_compil ~parsed_seed : t =
  {
    is_running = false;
    run_finalize = false;
    pause_condition = Alg_expr.FALSE;
    parsed_seed;
    counter;
    log_buffer;
    log_form;
    dumpIfDeadlocked;
    maxConsecutiveClash;
    patternSharing;
    plot = Data.init_plot env;
    snapshots = Mods.StringMap.empty;
    dins = [];
    species = Mods.StringMap.empty;
    files = Mods.StringMap.empty;
    error_messages = [];
    trace = Buffer.create 1024;
    inputs_buffer;
    inputs_form;
    parsing_compil;
    contact_map;
    env;
    graph;
    state;
    init_l;
    lastyield;
  }

let reinitialize ~outputs random_state t =
  Counter.reinitialize t.counter;
  (* Format.pp_print_flush t.log_form ();
     Buffer.reset t.log_buffer;*)
  t.is_running <- false;
  t.run_finalize <- false;
  t.pause_condition <- Alg_expr.FALSE;
  t.plot <- Data.init_plot t.env;
  t.snapshots <- Mods.StringMap.empty;
  t.dins <- [];
  t.files <- Mods.StringMap.empty;
  t.error_messages <- [];
  t.graph <-
    Some
      (Rule_interpreter.empty ~outputs ~with_trace:false random_state t.env
         t.counter);
  t.state <-
    State_interpreter.empty ~with_delta_activities:false t.counter t.env

let get_graph (t : t) : Rule_interpreter.t =
  Option_util.unsome_or_raise
    ~excep:
      (Failure "No rule interpreter info ready: simulation not started properly")
    t.graph

let set_graph (t : t) (graph : Rule_interpreter.t) = t.graph <- Some graph

let catch_error handler = function
  | ExceptionDefn.Syntax_Error ((message, range) : string Loc.annoted) ->
    handler (Api_common.error_msg ~range message)
  | ExceptionDefn.Malformed_Decl ((message, range) : string Loc.annoted) ->
    handler (Api_common.error_msg ~range message)
  | ExceptionDefn.Internal_Error ((message, range) : string Loc.annoted) ->
    handler (Api_common.error_msg ~range message)
  | Invalid_argument error ->
    handler (Api_common.error_msg ("Runtime error " ^ error))
  | exn ->
    let message =
      try Printexc.to_string exn with _ -> "unspecified exception thrown"
    in
    handler (Api_common.error_msg message)

let parse ~patternSharing (parsing_compil : Ast.parsing_compil) var_overwrite
    system_process =
  let yield = system_process#yield in
  let log_buffer = Buffer.create 512 in
  let log_form = Format.formatter_of_buffer log_buffer in
  let inputs_buffer = Buffer.create 512 in
  let inputs_form = Format.formatter_of_buffer inputs_buffer in
  let conf, _, _, _ = Configuration.parse parsing_compil.Ast.configurations in
  let warning ~pos msg = Data.print_warning ~pos log_form msg in
  Lwt.catch
    (fun () ->
      Lwt.wrap2
        (fun var_overwrite ->
          LKappa_compiler.compil_of_ast ~warning ~debug_mode:false
            ~syntax_version:Ast.V4 ~var_overwrite)
        var_overwrite parsing_compil
      >>= fun (ast_compiled_data : LKappa_compiler.ast_compiled_data) ->
      yield () >>= fun () ->
      (* The last yield is updated after the last yield.
         It is gotten here for the initial last yeild value. *)
      let lastyield = Sys.time () in
      try
        (* exception raised by compile must have used Lwt.fail.
           Something is wrong for now *)
        let outputs = function
          | Data.Log s -> Format.fprintf log_form "%s@." s
          | Data.Warning (pos, msg) -> Data.print_warning ?pos log_form msg
          | Data.Snapshot _ | Data.DIN _ | Data.Species _
          | Data.DeltaActivities _ | Data.Plot _ | Data.TraceStep _
          | Data.Print _ ->
            assert false
        in
        Eval.compile ~debug_mode:false
          ~pause:(fun f -> Lwt.bind (yield ()) f)
          ~return:Lwt.return ?rescale_init:None ?overwrite_t0:None
          ~compile_mode_on:false ~outputs ~sharing:patternSharing
          ast_compiled_data.agents_sig ast_compiled_data.counters_info
          ast_compiled_data.token_names ast_compiled_data.contact_map
          ast_compiled_data.result
        >>= fun (env, _with_trace, init_l) ->
        let counter =
          Counter.create
            ~init_t:(0. : float)
            ~init_e:(0 : int)
            ?max_time:None ?max_event:None ~plot_period:(Configuration.DT 1.)
            ~nb_rules:(Model.nb_rules env) ()
        in

        let parsed_seed = conf.Configuration.seed in
        Data.print_initial_inputs ?uuid:None
          { conf with Configuration.seed = parsed_seed }
          env inputs_form init_l;
        let simulation : t =
          create_t ~contact_map:ast_compiled_data.contact_map ~log_form
            ~log_buffer ~inputs_buffer ~inputs_form ~parsing_compil ~env
            ~counter ~dumpIfDeadlocked:conf.Configuration.dumpIfDeadlocked
            ~maxConsecutiveClash:conf.Configuration.maxConsecutiveClash
            ~patternSharing ~graph:None
            ~state:
              (State_interpreter.empty ~with_delta_activities:false counter env)
            ~init_l ~lastyield ~parsed_seed
        in
        Lwt.return (Result_util.ok simulation)
      with e ->
        (catch_error (fun x -> Lwt.return (Result_util.error [ x ]))) e)
    (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))

let outputs (simulation : t) = function
  | Data.DIN (flux_name, flux_map) ->
    simulation.dins <- (flux_name, flux_map) :: simulation.dins
  | Data.DeltaActivities _ -> assert false
  | Data.Plot new_observables ->
    simulation.plot <- Data.add_plot_line new_observables simulation.plot
  | Data.Species (file, time, mix) ->
    let p = Mods.StringMap.find_default [] file simulation.species in
    simulation.species <-
      Mods.StringMap.add file ((time, mix) :: p) simulation.species
  | Data.Print file_line ->
    (match file_line.Data.file_line_name with
    | None ->
      Format.fprintf simulation.log_form "%s@." file_line.Data.file_line_text
    | Some na ->
      let lines = Mods.StringMap.find_default [] na simulation.files in
      simulation.files <-
        Mods.StringMap.add na
          (file_line.Data.file_line_text :: lines)
          simulation.files)
  | Data.Snapshot (filename, snapshot) ->
    let already_there x = Mods.StringMap.mem x simulation.snapshots in
    let snapshot_file =
      Tools.find_available_name ~already_there filename
        ~facultative:(string_of_int snapshot.Data.snapshot_event)
        ~ext:None
    in
    simulation.snapshots <-
      Mods.StringMap.add snapshot_file snapshot simulation.snapshots
  | Data.Log s -> Format.fprintf simulation.log_form "%s@." s
  | Data.Warning (pos, msg) -> Data.print_warning ?pos simulation.log_form msg
  | Data.TraceStep st ->
    if Buffer.length simulation.trace <> 0 then
      Buffer.add_char simulation.trace ',';
    Trace.write_step simulation.trace st

let interactive_outputs formatter t = function
  | Data.Log s -> Format.fprintf formatter "%s@." s
  | Data.Warning (pos, msg) -> Data.print_warning ?pos formatter msg
  | Data.Print file_line when file_line.Data.file_line_name = None ->
    Format.fprintf formatter "%s@." file_line.Data.file_line_text
  | ( Data.DIN _ | Data.DeltaActivities _ | Data.Plot _ | Data.Species _
    | Data.Print _ | Data.Snapshot _ | Data.TraceStep _ ) as v ->
    outputs t v

let time_yield ~(system_process : system_process) ~(t : t) : unit Lwt.t =
  let time = Sys.time () in
  if time -. t.lastyield > system_process#min_run_duration () then (
    t.lastyield <- time;
    system_process#yield ()
  ) else
    Lwt.return_unit

let finalize_simulation ~(t : t) : unit =
  State_interpreter.end_of_simulation ~outputs:(outputs t) t.env t.counter
    (get_graph t) t.state

let run_simulation ~(system_process : system_process) ~(t : t) stopped :
    unit Lwt.t =
  Lwt.catch
    (fun () ->
      let rstop = ref stopped in
      t.is_running <- true;
      let rec iter () =
        (try
           while
             (not !rstop)
             && Sys.time () -. t.lastyield < system_process#min_run_duration ()
           do
             let stop, graph', state' =
               State_interpreter.a_loop ~debug_mode:false ~outputs:(outputs t)
                 ~dumpIfDeadlocked:t.dumpIfDeadlocked
                 ~maxConsecutiveClash:t.maxConsecutiveClash t.env t.counter
                 (get_graph t) t.state
             in
             rstop :=
               stop
               || Rule_interpreter.value_bool t.counter graph' t.pause_condition;
             set_graph t graph';
             t.state <- state'
           done;
           Lwt.return_unit
         with e -> Lwt.fail e)
        >>= fun () ->
        if !rstop then (
          t.is_running <- false;
          Lwt.return_unit
        ) else if t.is_running then (
          system_process#yield () >>= fun () ->
          t.lastyield <- Sys.time ();
          iter ()
        ) else
          Lwt.return_unit
      in
      iter () >>= fun () ->
      if t.run_finalize then finalize_simulation ~t;
      Lwt.return_unit)
    (catch_error (fun e ->
         t.is_running <- false;
         t.error_messages <- [ e ];
         Lwt.return_unit))

(** Start the simulation from the parameters and using the parsed state.

        Returns the seed used, as it is defined here if non specified *)
let start ~(system_process : system_process)
    ~(parameter : Api_types_t.simulation_parameter) ~(t : t) :
    (int, Result_util.message list) Result_util.t Lwt.t =
  let lexbuf =
    Lexing.from_string parameter.Api_types_t.simulation_pause_condition
  in
  Lwt.catch
    (fun () ->
      (*let () =
          Counter.set_max_time
                 t.counter
                 parameter.Api_types_j.simulation_max_time
               in
               let () =
               Counter.set_max_events
                 t.counter
                 parameter.Api_types_j.simulation_max_events
               in *)
      let simulation_seed : int =
        match parameter.Api_types_t.simulation_seed with
        | Some user_seed ->
          Format.fprintf t.log_form "User-set seed used for simulation: %i@."
            user_seed;
          (match t.parsed_seed with
          | Some parsed_seed ->
            Format.fprintf t.log_form "WARNING: ignored seed from parsing: %i@."
              parsed_seed
          | None -> ());
          user_seed
        | None ->
          (match t.parsed_seed with
          | Some parsed_seed ->
            Format.fprintf t.log_form "Parsed seed set for simulation: %i@."
              parsed_seed;
            parsed_seed
          | None ->
            (* init random to generate seed *)
            Random.self_init ();
            let random_seed = Random.bits () in
            Format.fprintf t.log_form "Random seed used for simulation: %i@."
              random_seed;
            random_seed)
      in
      let random_state = Random.State.make [| simulation_seed |] in
      reinitialize random_state ~outputs:(outputs t) t;
      try
        let pause = Kparser4.standalone_bool_expr Klexer4.token lexbuf in
        Lwt.wrap4
          (Evaluator.get_pause_criteria ~debug_mode:false ~outputs:(outputs t)
             ~sharing:t.patternSharing ~syntax_version:Ast.V4)
          t.contact_map t.env (get_graph t) pause
        >>= fun (env', graph', b'') ->
        t.env <- env';
        set_graph t graph';
        t.pause_condition <- b'';
        Counter.set_plot_period t.counter
          (Configuration.DT parameter.Api_types_t.simulation_plot_period);
        Lwt.async (fun () ->
            try
              (* exception raised by build_initial_state must have been
                 raised with Lwt.fail. Something is wrong for now... *)
              Eval.build_initial_state
                ~bind:(fun x f ->
                  time_yield ~system_process ~t >>= fun () -> x >>= f)
                ~return:Lwt.return ~debug_mode:false ~outputs:(outputs t)
                ~with_trace:parameter.Api_types_t.simulation_store_trace
                ~with_delta_activities:false t.counter t.env random_state
                t.init_l
              >>= fun (stop, graph, state) ->
              set_graph t graph;
              t.state <- state;
              let first_obs =
                State_interpreter.observables_values t.env (get_graph t)
                  t.counter
              in
              t.plot <- Data.add_plot_line first_obs t.plot;
              run_simulation ~system_process ~t stop
            with e ->
              catch_error
                (fun e ->
                  t.error_messages <- [ e ];
                  Lwt.return_unit)
                e);
        Lwt.return (Result_util.ok simulation_seed)
      with ExceptionDefn.Syntax_Error (message, range) ->
        Lwt.return (Api_common.err_result_of_string ~range message))
    (catch_error (fun e ->
         t.error_messages <- [ e ];
         Lwt.return (Result_util.error [ e ])))

let pause ~(system_process : system_process) ~(t : t) :
    (unit, Result_util.message list) Result_util.t Lwt.t =
  ignore system_process;
  ignore t;
  if t.is_running then
    t.is_running <- false
  else
    ();
  Lwt.return (Result_util.ok ())

let stop ~(system_process : system_process) ~(t : t) :
    (unit, Result_util.message list) Result_util.t Lwt.t =
  ignore system_process;
  ignore t;
  Lwt.catch
    (fun () ->
      t.run_finalize <- true;
      if t.is_running then
        pause ~system_process ~t
      else (
        finalize_simulation ~t;
        Lwt.return (Result_util.ok ())
      ))
    (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))

let perturbation ~(system_process : system_process) ~(t : t)
    ~(perturbation : Api_types_t.simulation_intervention) :
    (string, Result_util.message list) Result_util.t Lwt.t =
  ignore system_process;
  let lexbuf = Lexing.from_string perturbation in
  Lwt.catch
    (fun () ->
      if t.is_running then
        Lwt.return (Api_common.err_result_of_string msg_process_not_paused)
      else (
        try
          let e = Kparser4.standalone_effect_list Klexer4.token lexbuf in
          let log_buffer = Buffer.create 512 in
          let log_form = Format.formatter_of_buffer log_buffer in
          Lwt.wrap6
            (Evaluator.do_interactive_directives ~debug_mode:false
               ~outputs:(interactive_outputs log_form t)
               ~sharing:t.patternSharing ~syntax_version:Ast.V4)
            t.contact_map t.env t.counter (get_graph t) t.state e
          >>= fun (e', (env', (_, graph'', state'))) ->
          t.env <- env';
          set_graph t graph'';
          t.state <- state';
          Format.fprintf t.log_form "%%mod: [E] = %i do %a@."
            (Counter.current_event t.counter)
            (Pp.list ~trailing:Pp.colon Pp.colon
               (Kappa_printer.modification ~noCounters:false ~env:t.env))
            e';
          Format.fprintf t.inputs_form "%%mod: [E] = %i do %a@."
            (Counter.current_event t.counter)
            (Pp.list ~trailing:Pp.colon Pp.colon
               (Kappa_printer.modification ~noCounters:false ~env:t.env))
            e';
          Lwt.return (Result_util.ok (Buffer.contents log_buffer))
        with ExceptionDefn.Syntax_Error (message, range) ->
          Lwt.return (Api_common.err_result_of_string ~range message)
      ))
    (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))

let continue ~(system_process : system_process) ~(t : t)
    ~(pause_condition : string) :
    (unit, Result_util.message list) Result_util.t Lwt.t =
  let lexbuf = Lexing.from_string pause_condition in
  Lwt.catch
    (fun () ->
      if t.is_running then
        Lwt.return (Result_util.ok ())
      else (
        try
          let pause = Kparser4.standalone_bool_expr Klexer4.token lexbuf in
          Lwt.wrap4
            (Evaluator.get_pause_criteria ~debug_mode:false ~outputs:(outputs t)
               ~sharing:t.patternSharing ~syntax_version:Ast.V4)
            t.contact_map t.env (get_graph t) pause
          >>= fun (env', graph', b'') ->
          t.env <- env';
          set_graph t graph';
          t.pause_condition <- b'';
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
            Lwt.async (fun () -> run_simulation ~system_process ~t false)
          in
          Lwt.return (Result_util.ok ())
        with ExceptionDefn.Syntax_Error (message, range) ->
          Lwt.return (Api_common.err_result_of_string ~range message)
      ))
    (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))

let progress ~(system_process : system_process) ~(t : t) :
    (Api_types_t.simulation_progress, Result_util.message list) Result_util.t
    Lwt.t =
  ignore system_process;
  ignore t;
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
        Lwt.return
          (Result_util.ok
             {
               Api_types_t.simulation_progress_time =
                 Counter.current_time t.counter;
               Api_types_t.simulation_progress_time_percentage =
                 Option_util.map
                   (fun x -> int_of_float (x *. 100.))
                   (Counter.time_ratio t.counter);
               Api_types_t.simulation_progress_event =
                 Counter.current_event t.counter;
               Api_types_t.simulation_progress_event_percentage =
                 Option_util.map
                   (fun x -> int_of_float (x *. 100.))
                   (Counter.event_ratio t.counter);
               Api_types_t.simulation_progress_tracked_events =
                 Counter.tracked_events t.counter;
               Api_types_t.simulation_progress_is_running = t.is_running;
             }))
      (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))
  | _ -> Lwt.return (Result_util.error t.error_messages)

let outputs ~(system_process : system_process) ~(t : t) :
    (Api_data.simulation_detail_output, Result_util.message list) Result_util.t
    Lwt.t =
  ignore system_process;
  ignore t;
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
        Lwt.return
          (Result_util.ok
             {
               Api_types_t.simulation_output_plot = Some t.plot;
               Api_types_t.simulation_output_dins = t.dins;
               Api_types_t.simulation_output_file_lines = t.files;
               Api_types_t.simulation_output_snapshots = t.snapshots;
               Api_types_t.simulation_output_inputs =
                 Buffer.contents t.inputs_buffer;
               Api_types_t.simulation_output_log_messages =
                 Buffer.contents t.log_buffer;
             }))
      (catch_error (fun e -> Lwt.return (Result_util.error [ e ])))
  | _ -> Lwt.return (Result_util.error t.error_messages)

let efficiency t = Counter.get_efficiency t.counter

let get_raw_trace (t : t) : string =
  JsonUtil.string_of_write
    (fun ob t ->
      Buffer.add_char ob '{';
      JsonUtil.write_field "dict"
        (fun ob () ->
          Buffer.add_char ob '{';
          Buffer.add_string ob Agent.json_dictionnary;
          JsonUtil.write_comma ob;
          Buffer.add_string ob Instantiation.json_dictionnary;
          JsonUtil.write_comma ob;
          Buffer.add_string ob Trace.Simulation_info.json_dictionnary;
          JsonUtil.write_comma ob;
          Buffer.add_string ob Trace.json_dictionnary;
          Buffer.add_char ob '}')
        ob ();
      JsonUtil.write_comma ob;
      JsonUtil.write_field "model" Yojson.Basic.write_json ob
        (Model.to_yojson t.env);
      JsonUtil.write_comma ob;
      JsonUtil.write_field "trace" Buffer.add_string ob
        ("[" ^ Buffer.contents t.trace);
      Buffer.add_string ob "]}")
    t

let get_raw_ast t = Yojson.Basic.to_string (Ast.compil_to_json t.parsing_compil)
