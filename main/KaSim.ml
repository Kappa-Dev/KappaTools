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


let batch_loop ~outputs env counter graph state =
  let rec iter graph state =
    let stop,graph',state' =
      State_interpreter.a_loop ~outputs env counter graph state in
    if stop then (graph',state')
    else let () = Counter.tick counter in iter graph' state'
  in iter graph state

let interactive_loop ~outputs pause_criteria env counter graph state =
  let user_interrupted = ref false in
  let old_sigint_behavior =
    Sys.signal
      Sys.sigint (Sys.Signal_handle
                    (fun _ -> if !user_interrupted then raise Sys.Break
                      else user_interrupted := true)) in
  let rec iter graph state =
    if !user_interrupted ||
       Rule_interpreter.value_bool counter graph pause_criteria then
      let () = Sys.set_signal Sys.sigint old_sigint_behavior in
      (false,graph,state)
    else
      let stop,graph',state' as out =
        State_interpreter.a_loop ~outputs env counter graph state in
      if stop then
        let () = Sys.set_signal Sys.sigint old_sigint_behavior in
        out
      else let () = Counter.tick counter in iter graph' state'
  in iter graph state

let finalize
    ~outputs dotFormat cflow_file trace_file
    env counter graph state stories_compression =
  let () = State_interpreter.end_of_simulation
      ~outputs  Format.err_formatter env counter graph state in
  let () = Counter.complete_progress_bar counter in
  let () = Outputs.close () in
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
    let () = Parameter.eclipseMode := kasim_args.Kasim_args.eclipseMode in
    let () = Parameter.compileModeOn := kasim_args.Kasim_args.compileMode in
    let () =
      Parameter.time_independent := common_args.Common_args.timeIndependent in

    let abort =
      match cli_args.Run_cli_args.inputKappaFileNames with
      | [] -> cli_args.Run_cli_args.marshalizedInFile = ""
      | _ -> false in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in
    Printexc.record_backtrace
      (!Parameter.debugModeOn || common_args.Common_args.backtrace);
    (*Possible backtrace*)

    let theSeed,seed_arg =
      match kasim_args.Kasim_args.seedValue with
      | Some seed -> seed,[||]
      | None ->
        let () = Format.printf "+ Self seeding...@." in
        let () = Random.self_init() in
        let out = Random.bits () in
        out,[|"-seed";string_of_int out|] in
    let () = Random.init theSeed (*for reproducible  colors in dot snaphot*) in
    let random_state = Random.State.make [|theSeed|] in
    let command_line =
      Format.asprintf "@[<h>%a%t%a@]"
        (Pp.array Pp.space
           (fun i f s ->
              Format.fprintf
                f "'%s'" (if i = 0 then "KaSim" else s)))
        Sys.argv
        (fun f -> if Array.length seed_arg > 0 then Format.pp_print_space f ())
        (Pp.array Pp.space (fun _ -> Format.pp_print_string)) seed_arg in
    Format.printf "+ Command line to rerun is: %s@." command_line;

    let (env0, contact_map, updated_vars, story_compression,
         formatCflows, cflowFile, init_l as init_result),
        counter,alg_overwrite = Cli_init.get_compilation
        ~unit:kasim_args.Kasim_args.unit
        ~max_sharing:kasim_args.Kasim_args.maxSharing cli_args in
    let () =
      if cli_args.Run_cli_args.batchmode &&
       Counter.max_time counter = None && Counter.max_events counter = None then
        Model.check_if_counter_is_filled_enough env0 in
    let env = Model.propagate_constant
        ?max_time:(Counter.max_time counter)
        ?max_events:(Counter.max_events counter) updated_vars env0 in

    let outputs = Outputs.go (Model.signatures env) in
    let trace_file =
      match kasim_args.Kasim_args.traceFile with
      | Some _ as x -> x
      | None ->
        match story_compression with
        | None -> None
        | Some _ ->
          let () = tmp_trace := Some (Filename.temp_file "trace" ".json") in
          !tmp_trace in
    let plotPack =
      let head =
        Model.map_observables
          (fun o -> Format.asprintf "%a" (Kappa_printer.alg_expr ~env) o)
          env in
      if Array.length head > 1 then
        let title = "Output of " ^ command_line in
        Some (cli_args.Run_cli_args.outputDataFile,title,head)
      else None in

    Kappa_files.setCheckFileExists
      ~batchmode:cli_args.Run_cli_args.batchmode
      cli_args.Run_cli_args.outputDataFile;
    if not !Parameter.compileModeOn then
      Outputs.initialize trace_file plotPack env;

    let () =
      Kappa_files.with_marshalized
        (fun d -> Marshal.to_channel d init_result []) in
    let () = Format.printf "+ Building initial state@." in
    let (stop,graph,state) =
      Eval.build_initial_state
        ~bind:(fun x f -> f x) ~return:(fun x -> x)
        ~outputs alg_overwrite counter env
        ~with_trace:(trace_file<>None) random_state init_l in
    let () = Format.printf "Done@." in
    let () =
      if !Parameter.compileModeOn || !Parameter.debugModeOn then
        Format.eprintf
          "@[<v>@[<v 2>Environment:@,%a@]@,@[<v 2>Domain:@,%a@]@,@[<v 2>Intial graph;@,%a@]@]@."
          Kappa_printer.env env
          Pattern.Env.print (Model.domain env)
          (Rule_interpreter.print env) graph in
    let () = match kasim_args.Kasim_args.domainOutputFile with
      | None -> ()
      | Some domainOutputFile ->
        Yojson.Basic.to_file (Kappa_files.path domainOutputFile)
          (Pattern.Env.to_yojson (Model.domain env)) in
    ExceptionDefn.flush_warning Format.err_formatter ;
    if !Parameter.compileModeOn then let () = remove_trace () in exit 0 else ();

    let () = match plotPack with
      | Some _ ->
        (*if cli_args.Run_cli_args.plotPeriod > 0. then*)
        Outputs.go (Model.signatures env)
          (Data.Plot
             (State_interpreter.observables_values env graph counter))
      | _ -> () in

    let () =
      if stop then
        finalize
          ~outputs formatCflows cflowFile trace_file
          env counter graph state story_compression
      else if cli_args.Run_cli_args.batchmode then
        let (graph',state') = batch_loop ~outputs env counter graph state in
        finalize
          ~outputs formatCflows cflowFile trace_file
          env counter graph' state' story_compression
      else
        let lexbuf = Lexing.from_channel stdin in
        let rec toplevel env graph state =
          let () = Format.printf "@.> @?" in
          let env',(stop,graph',state') =
            try
              match KappaParser.interactive_command KappaLexer.token lexbuf with
              | Ast.RUN b ->
                let env',graph',b'' = Evaluator.get_pause_criteria
                    ~max_sharing:kasim_args.Kasim_args.maxSharing
                    contact_map env graph b in
                 env',interactive_loop ~outputs b'' env' counter graph' state
              | Ast.QUIT -> env,(true,graph,state)
              | Ast.MODIFY e ->
                Evaluator.do_interactive_directives
                  ~outputs ~max_sharing:kasim_args.Kasim_args.maxSharing
                  contact_map env counter graph state e
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
               @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ perturbation\
               @ effect@ to@ perform@ it@]" in
          toplevel env0 graph state
        else
          let (stop,graph',state') =
            interactive_loop ~outputs Alg_expr.FALSE env counter graph state in
          if stop then
            finalize
              ~outputs formatCflows cflowFile trace_file
              env counter graph' state' story_compression
          else
            let () =
              Format.printf
                "@.@[KaSim@ toplevel:@ type@ $RUN@ (optionally@ followed@ by@ a\
                 @ pause@ criteria)@ to@ launch@ the@ simulation@ or@ a@ perturbation\
                 @ effect@ to@ perform@ it@]" in
            toplevel env0 graph' state' in
    Format.printf "Simulation ended@.";
    remove_trace ();
    if Counter.nb_null_event counter <> 0 then
      let () =
        Format.printf
          "@[%.2f%% of event loops were productive.@ Null event cause:@]@."
          (100. *. (float_of_int (Counter.current_event counter)) /.
           (float_of_int
              (Counter.nb_null_event counter +
               Counter.current_event counter))) in
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
