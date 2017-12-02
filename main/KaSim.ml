(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let usage_msg =
  "KaSim "^Version.version_string^":\n"^
  "Usage is KaSim [-l time] [-p delta_t] [-o output_file] input_files\n"

let tmp_trace = ref(None:string option)
let remove_trace () = match !tmp_trace with None -> () | Some d -> Sys.remove d

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let batch_loop
    ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash ~efficiency
    progressConf env counter graph state =
  let rec iter graph state =
    let stop,graph',state' =
      State_interpreter.a_loop
        ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash
        env counter graph state in
    if stop then (graph',state')
    else
      let () = Counter.tick ~efficiency progressConf counter in
      iter graph' state'
  in iter graph state

let interactive_loop
    ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash ~efficiency progressConf
    pause_criteria env counter graph state =
  let user_interrupted = ref false in
  let old_sigint_behavior =
    Sys.signal Sys.sigint
      (Sys.Signal_handle
         (fun _ -> if !user_interrupted then raise Sys.Break
           else user_interrupted := true))
  in
  let rec iter graph state =
    if !user_interrupted ||
       Rule_interpreter.value_bool counter graph pause_criteria then
      let () = Sys.set_signal Sys.sigint old_sigint_behavior in
      (false,graph,state)
    else
      let stop,graph',state' as out =
        State_interpreter.a_loop ~outputs
          ~dumpIfDeadlocked ~maxConsecutiveClash env counter graph state in
      if stop then
        let () = Sys.set_signal Sys.sigint old_sigint_behavior in
        out
      else
        let () = Counter.tick ~efficiency progressConf counter in
        iter graph' state'
  in iter graph state

let finalize
    ~outputs dotFormat cflow_file trace_file
    env counter graph state stories_compression =
  let () = State_interpreter.end_of_simulation
      ~outputs  Format.err_formatter env counter graph state in
  let () = Counter.complete_progress_bar counter in
  let () = Outputs.close ~event:(Counter.current_event counter) () in
  match trace_file,stories_compression with
  | None,_ -> ()
  | Some _, None -> ()
  | Some dump, Some (none,weak,strong) ->
    let args = ["-d"; Kappa_files.get_dir (); Kappa_files.path dump] in
    let args = if none then "--none" :: args else args in
    let args = if weak then "--weak" :: args else args in
    let args = if strong then "--strong" :: args else args in
    let args = "-format" :: dotFormat :: args in
    let args = match cflow_file with
      | None -> args
      | Some f -> "-o" :: f :: args in
    let args = if !Parameter.time_independent
      then "--time-independent" :: args else args in
    let prog =
      let predir =
        Filename.dirname Sys.executable_name in
      let dir = if Filename.is_implicit Sys.executable_name &&
                   predir = "." then "" else predir^"/" in
      dir^"KaStor" in
    let pid = Unix.create_process prog (Array.of_list (prog::args))
        Unix.stdin Unix.stdout Unix.stderr in
    let _old_sigint_behavior =
      Sys.signal
        Sys.sigint (Sys.Signal_handle (fun si -> Unix.kill pid si)) in
    match waitpid_non_intr pid with
    | _, Unix.WEXITED 127 ->
      raise
        (ExceptionDefn.Malformed_Decl
           (Locality.dummy_annot
              ("Executable '"^prog^"' can not be found to compute stories.")))
    | _, Unix.WEXITED n -> if n <> 0 then exit n
    | _, Unix.WSIGNALED n -> failwith ("Killed with signal "^string_of_int n)
    | _, Unix.WSTOPPED n -> failwith ("Stopped with signal "^string_of_int n)

let () =
  let cli_args = Run_cli_args.default in
  let kasim_args = Kasim_args.default in
  let common_args = Common_args.default in
  let options =
    Run_cli_args.options cli_args @
    Kasim_args.options kasim_args @ Common_args.options common_args in
  try
    Arg.parse
      options
      (fun fic -> cli_args.Run_cli_args.inputKappaFileNames <-
          fic::(cli_args.Run_cli_args.inputKappaFileNames))
      usage_msg;
    let () = Kappa_files.set_dir cli_args.Run_cli_args.outputDirectory in
    let () = match kasim_args.Kasim_args.marshalizeOutFile with
      | None -> ()
      | Some marshalizeOutFile ->
        Kappa_files.set_marshalized marshalizeOutFile
    in
    let () = Parameter.debugModeOn := common_args.Common_args.debug in
    let () =
      Parameter.time_independent := common_args.Common_args.timeIndependent in

    let abort =
      match cli_args.Run_cli_args.inputKappaFileNames with
      | [] -> kasim_args.Kasim_args.marshalizedInFile = ""
      | _ -> false in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in
    Printexc.record_backtrace
      (!Parameter.debugModeOn || common_args.Common_args.backtrace);
    (*Possible backtrace*)

    let cpu_time = Sys.time () in
    let (conf, progressConf, env, contact_map, _, story_compression,
         formatCflows, cflowFile, init_l as init_result),
        counter = Cli_init.get_compilation
        ~unit:kasim_args.Kasim_args.unit
        ~max_sharing:kasim_args.Kasim_args.maxSharing
        ~compileModeOn:kasim_args.Kasim_args.compileMode ~kasim_args cli_args in
    let () = if kasim_args.Kasim_args.showEfficiency then
        Format.printf " All that took %fs@." (Sys.time () -. cpu_time) in

    let theSeed,seed_arg =
      match kasim_args.Kasim_args.seedValue,conf.Configuration.seed with
      | Some seed,_ | None, Some seed -> seed,[||]
      | None, None ->
        let () = Format.printf "+ Self seeding...@." in
        let () = Random.self_init () in
        let out = Random.bits () in
        out,[|"-seed";string_of_int out|] in
    let () = Random.init theSeed (*for reproducible  colors in dot snaphot*) in
    let random_state = Random.State.make [|theSeed|] in

    let () =
      if cli_args.Run_cli_args.batchmode &&
         Counter.max_time counter = None && Counter.max_events counter = None then
        Model.check_if_counter_is_filled_enough env in

    let command_line =
      Format.asprintf "@[<h>%a%t%a@]"
        (Pp.array Pp.space
           (fun i f s ->
              Format.fprintf
                f "'%s'" (if i = 0 then "KaSim" else s)))
        Sys.argv
        (fun f -> if Array.length seed_arg > 0 then Format.pp_print_space f ())
        (Pp.array Pp.space (fun _ -> Format.pp_print_string)) seed_arg in

    let trace_file,user_trace_file =
      match kasim_args.Kasim_args.traceFile,
            conf.Configuration.traceFileName with
      | Some _ as x,_ -> x,x
      | _, (Some _ as x) -> x,x
      | None, None ->
        match story_compression with
        | None -> None,None
        | Some _ ->
          let () = tmp_trace := Some (Filename.temp_file "trace" ".json") in
          !tmp_trace,None in
    let plot_file = Option_util.unsome
        (Option_util.unsome "data.csv" conf.Configuration.outputFileName)
        cli_args.Run_cli_args.outputDataFile in
    let plotPack =
      let head =
        Model.map_observables
          (fun o -> Format.asprintf "@[<h>%a@]" (Kappa_printer.alg_expr ~env) o)
          env in
      if Array.length head > 1 then
        let title = "Output of " ^ command_line in
        Some (plot_file,title,head)
      else None in
    let dumpIfDeadlocked = conf.Configuration.dumpIfDeadlocked in
    let maxConsecutiveClash = conf.Configuration.maxConsecutiveClash in
    let deltaActivitiesFileName =
      conf.Configuration.deltaActivitiesFileName in
    let () =
      if not kasim_args.Kasim_args.compileMode then
        match kasim_args.Kasim_args.logFile with
        | None -> ()
        | Some filename ->
          Outputs.initial_inputs
            {Configuration.seed = Some theSeed;
             Configuration.dumpIfDeadlocked; Configuration.maxConsecutiveClash;
             Configuration.deltaActivitiesFileName;
             Configuration.traceFileName = user_trace_file;
             Configuration.initial =
               if Tools.float_is_zero (Counter.init_time counter) then None
               else Some (Counter.init_time counter);
             Configuration.plotPeriod = Some (Counter.plot_period counter);
             Configuration.outputFileName = Some plot_file;}
            env contact_map init_l ~filename
    in
    Kappa_files.setCheckFileExists
      ~batchmode:cli_args.Run_cli_args.batchmode
      plot_file;
    if not kasim_args.Kasim_args.compileMode then
      Outputs.initialize deltaActivitiesFileName trace_file plotPack env;

    let outputs = Outputs.go in
    let () =
      Kappa_files.with_marshalized
        (fun d -> Marshal.to_channel d init_result []) in
    let cpu_time = Sys.time () in
    let () = Format.printf "+ Building initial state@?" in
    let (stop,graph,state) =
      Eval.build_initial_state
        ~bind:(fun x f -> f x) ~return:(fun x -> x) ~outputs counter env
        ~with_trace:(trace_file<>None)
        ~with_delta_activities:(deltaActivitiesFileName<>None)
        random_state init_l in
    let () = Format.printf " (%a)" Rule_interpreter.print_stats graph in
    let () = if kasim_args.Kasim_args.showEfficiency then
        Format.printf " took %fs" (Sys.time () -. cpu_time) in

    Format.printf "@.Done@.+ Command line to rerun is: %s@." command_line;

    let () =
      if kasim_args.Kasim_args.compileMode || !Parameter.debugModeOn then
        Format.eprintf
          "@[<v>@[<v 2>Environment:@,%a@]@,@[<v 2>Polymers:@,%a@]@,\
@[<v 2>Domain:@,%a@]@,@[<v 2>Intial graph;@,%a@]@]@."
          Kappa_printer.env env
          (Contact_map.print_cycles (Model.signatures env)) contact_map
          Pattern.Env.print (Model.domain env)
          (Rule_interpreter.print env) graph
    in
    (*------------------------------------------------------------*)
    let () = match kasim_args.Kasim_args.domainOutputFile with
      | None -> ()
      | Some domainOutputFile ->
        Yojson.Basic.to_file (Kappa_files.path domainOutputFile)
          (Pattern.Env.to_yojson (Model.domain env)) in
    ExceptionDefn.flush_warning Format.err_formatter ;
    (if kasim_args.Kasim_args.compileMode
     then let () = remove_trace () in exit 0
     else ());

    let () = match plotPack with
      | Some _ ->
        if Counter.positive_plot_period counter then
          Outputs.go
            (Data.Plot
               (State_interpreter.observables_values env graph counter))
      | _ -> ()
    in
    let () =
      if stop then
        finalize
          ~outputs formatCflows cflowFile trace_file
          env counter graph state story_compression
      else if cli_args.Run_cli_args.batchmode then
        let (graph',state') =
          batch_loop
            ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash
            ~efficiency:kasim_args.Kasim_args.showEfficiency
            progressConf env counter graph state in
        finalize
          ~outputs formatCflows cflowFile trace_file
          env counter graph' state' story_compression
      else
        let lexbuf = Lexing.from_channel stdin in
        let rec toplevel env graph state =
          let () = ExceptionDefn.flush_warning Format.err_formatter in
          let () = Format.printf "@.> @?" in
          let env',(stop,graph',state') =
            try
              let command = match cli_args.Run_cli_args.syntaxVersion with
                | Ast.V3 ->
                  KappaParser.interactive_command KappaLexer.token lexbuf
                | Ast.V4 ->
                  try Kparser4.interactive_command Klexer4.token lexbuf
                  with ExceptionDefn.Syntax_Error r ->
                    raise (ExceptionDefn.Malformed_Decl r) in
              match command with
              | Ast.RUN b ->
                let env',graph',b'' = Evaluator.get_pause_criteria
                    ~max_sharing:kasim_args.Kasim_args.maxSharing
                    ~syntax_version:(cli_args.Run_cli_args.syntaxVersion)
                    contact_map env graph b in
                env',interactive_loop
                  ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash
                  ~efficiency:kasim_args.Kasim_args.showEfficiency
                  progressConf b'' env' counter graph' state
              | Ast.QUIT -> env,(true,graph,state)
              | Ast.MODIFY e ->
                let e', (env',_ as o) =
                  Evaluator.do_interactive_directives
                    ~outputs ~max_sharing:kasim_args.Kasim_args.maxSharing
                    ~syntax_version:cli_args.Run_cli_args.syntaxVersion
                    contact_map env counter graph state e in
                let () = Outputs.input_modifications
                    env' (Counter.current_event counter) e' in
                o
            with
            | ExceptionDefn.Syntax_Error (msg,pos) ->
              let () = Pp.error Format.pp_print_string (msg,pos) in
              env,(false,graph,state)
            | ExceptionDefn.Malformed_Decl er ->
              let () = Pp.error Format.pp_print_string er in
              env,(false,graph,state) in
          if stop then
            finalize
              ~outputs formatCflows cflowFile trace_file
              env counter graph' state' story_compression
          else
            toplevel env' graph' state' in
        if cli_args.Run_cli_args.interactive then
          let () =
            Format.printf
              "@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
               @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ intervention\
               @ effect@ to@ perform@ it@]@." in
          toplevel env graph state
        else
          let (stop,graph',state') =
            interactive_loop
              ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash
              ~efficiency:kasim_args.Kasim_args.showEfficiency
              progressConf Alg_expr.FALSE env counter graph state in
          if stop then
            finalize
              ~outputs formatCflows cflowFile trace_file
              env counter graph' state' story_compression
          else
            let () =
              Format.printf
                "@.@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
                 @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ intervention\
                 @ effect@ to@ perform@ it@]@." in
            toplevel env graph' state' in
    Format.printf "Simulation ended@.";
    remove_trace ();
    Counter.print_efficiency Format.std_formatter counter ;
  with
  | ExceptionDefn.Malformed_Decl er ->
    let () = Outputs.close () in
    let () = remove_trace () in
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Pp.error Format.pp_print_string er in
    exit 2
  | ExceptionDefn.Internal_Error er ->
    let () = Outputs.close () in
    let () = remove_trace () in
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () =
      Pp.error
        (fun f x -> Format.fprintf f "Internal Error (please report):@ %s" x)
        er in
    exit 2
  | Sys.Break ->
    let () = Outputs.close () in
    let () = remove_trace () in
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () =
      Format.eprintf "@.***Interrupted by user out of simulation loop***@." in
    exit 1
  | Invalid_argument msg ->
    let () = Outputs.close () in
    let () = remove_trace () in
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let s = Printexc.get_backtrace() in
    let () = Format.eprintf "@.@[<v>***Runtime error %s***@,%s@]@." msg s in
    exit 2
  | e ->
    let () = Outputs.close () in
    let () = remove_trace () in
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let s = Printexc.get_backtrace() in
    let () = Format.eprintf "@.@[<v>%s@,%s@]@." (Printexc.to_string e) s in
    exit 3
