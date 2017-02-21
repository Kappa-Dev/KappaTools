(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Feb 21 2017>
*)

module A = Odes.Make (Ode_interface)

let lowercase = String.lowercase(*_ascii  : ocaml 4.03*)

let main () =
  let usage_msg =
    "KaDE "^Version.version_string^":\n"^
    "Usage is KaDE [-i] input_file [--ode-backend Matlab | Octave | SBML]
[--rate-convention KaSim | Divide_by_nbr_of_autos_in_lhs | Biochemist] [-t-init time] [-t time] [-p delta_t] [-o output_file] [--matlab-output foo.m] [--octave-output foo.m] [--sbml-output foo.xml]\n"
  in
  let cli_args = Run_cli_args.default in
  let common_args = Common_args.default in
  let ode_args = Ode_args.default in
  let options =
    Run_cli_args.options cli_args
    @ Ode_args.options ode_args
      @ Common_args.options common_args
  in
  try
    Arg.parse
      options
      (fun fic ->
         cli_args.Run_cli_args.inputKappaFileNames <-
           fic::(cli_args.Run_cli_args.inputKappaFileNames))
      usage_msg;
    let () = Kappa_files.set_dir cli_args.Run_cli_args.outputDirectory in
    let () = Parameter.debugModeOn := common_args.Common_args.debug in
    let backend =
      match lowercase ode_args.Ode_args.backend with
      | "octave" -> Loggers.Octave
      | "matlab" -> Loggers.Matlab
      | "sbml" -> Loggers.SBML
      | s ->
        begin
          Arg.usage options usage_msg;
          Debug.tag
            Format.std_formatter
            ("Wrong option "^s^".\nOnly Matlab, Octave, and SBML backends are supported.");
          exit 0
        end
    in
    let rate_convention =
      match lowercase ode_args.Ode_args.rate_convention with
    | "kasim" -> Ode_args.KaSim
    | "divide_by_nbr_of_autos_in_lhs"  ->
      Ode_args.Divide_by_nbr_of_autos_in_lhs
    | "biochemist" -> Ode_args.Biochemist
    | s ->
      begin
        Arg.usage options usage_msg;
        Debug.tag
          Format.std_formatter
          ("Wrong option "^s^".\nOnly KaSim and Biochemist are supported.");
        exit 0
      end
    in
    let () =
      match
        ode_args.Ode_args.matlab_output
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.Matlab
          s
    in
    let () =
      match
        ode_args.Ode_args.octave_output
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.Octave
          s
    in
    let () =
      match
        ode_args.Ode_args.sbml_output
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.SBML
          s
    in
    let count =
      match lowercase ode_args.Ode_args.count with
      | "embedding" | "embeddings" -> Ode_args.Embeddings
      | "occurrences" | "occurrence" | "instances" | "instance"->
        Ode_args.Occurrences
    | s ->
      begin
        Arg.usage options usage_msg;
        Debug.tag
          Format.std_formatter
          ("Wrong option "^s^".\nOnly Embeddings and Occurrences are supported.");
        exit 0
      end
    in
    let show_reactions = ode_args.Ode_args.show_reactions in
    let compute_jacobian = ode_args.Ode_args.compute_jacobian in
    let abort =
      match cli_args.Run_cli_args.inputKappaFileNames with
      | [] -> cli_args.Run_cli_args.marshalizedInFile = ""
      | _ -> false
    in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in
    let () =
      Kappa_files.setCheckFileExistsODE
        ~batchmode:cli_args.Run_cli_args.batchmode
        ~mode:backend
    in
    let command_line =
      Format.asprintf "%a"
        (Pp.array (fun f -> Format.fprintf f " ")
           (fun i f s ->
              Format.fprintf
                f "%s" (if i = 0 then "KaDE" else s)))
        Sys.argv
    in
    let command_line_quotes =
      Format.asprintf "%a"
        (Pp.array Pp.space
           (fun i f s ->
              Format.fprintf
                f "'%s'" (if i = 0 then "KaDE" else s)))
        Sys.argv
    in
    let ignore_obs =
      match backend with
      | Loggers.SBML -> true
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML |
        Loggers.HTML_Tabular
      | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS -> true
      | Loggers.Octave
      | Loggers.Matlab | Loggers.Maple | Loggers.Json -> false
    in
    let compil =
      A.get_compil
        ~rate_convention ~show_reactions ~count ~compute_jacobian cli_args in
    let network = A.network_from_compil ~ignore_obs compil in
    let out_channel =
      Kappa_files.open_out (Kappa_files.get_ode ~mode:backend)
    in
    let logger = Loggers.open_logger_from_channel ~mode:backend out_channel in
    let logger_buffer =
      match backend with
      | Loggers.SBML ->
        Loggers.open_infinite_buffer ~mode:backend ()
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS -> logger
      | Loggers.Octave
      | Loggers.Matlab | Loggers.Maple | Loggers.Json -> logger
    in
    (*********************************************************************)
    (*TEST-symmetries*)
    (*let my_out_channel =
        Kappa_files.open_out "my_logger.txt"
    in
    let my_logger = Loggers.open_logger_from_channel my_out_channel in
    let errors = Exception.empty_error_handler in
    let parameters =
      (* TO DO *)
      (* ADD SOME COMMAND LINES PARAMETERS TO TUNE THE REACHEABILITY ANALYSIS *)
      Remanent_parameters.get_parameters
        ~called_from:Remanent_parameters_sig.Server
        ()
    in
    let module B =
      (val Domain_selection.select_domain
          ~reachability_parameters:(Remanent_parameters.get_reachability_analysis_parameters parameters) ())
    in
    let export_to_kade =
      (module Export_to_KaDE.Export(B) : Export_to_KaDE.Type)
    in
    let module Export_to_kade =
      (val export_to_kade : Export_to_KaDE.Type)
    in
    let kasa_compil =
      List.fold_left
        (KappaLexer.compile Format.std_formatter)
        Ast.empty_compil
        cli_args.Run_cli_args.inputKappaFileNames
    in
    let state = Export_to_kade.init ~compil:kasa_compil () in
    let parameters =
      Export_to_kade.get_parameters state in
    let parameters =
      Remanent_parameters.set_logger parameters my_logger
    in
    let parameters =
      Remanent_parameters.set_trace parameters true
    (* This should be tuned according to a command line option *)
    in
    let state =
      Export_to_kade.set_parameters parameters state
    in
    let state, symmetries =
      Export_to_kade.get_symmetric_sites
        ~accuracy_level:Remanent_state.High state
    in
    let cache, () =
      A.compute_symmetries_from_syntactic_rules my_logger compil
    in
    let () = Loggers.flush_logger my_logger in
      let () = close_out my_out_channel in*)
    (*********************************************************************)
    let () = A.export_network
        ~command_line
        ~command_line_quotes
        ?data_file:cli_args.Run_cli_args.outputDataFile
        ?init_t:cli_args.Run_cli_args.minValue
        ~max_t:(Tools.unsome 1. cli_args.Run_cli_args.maxValue)
        ?plot_period:cli_args.Run_cli_args.plotPeriod logger logger_buffer compil network in
    let () = Loggers.flush_logger logger in
    let () = close_out out_channel in
    ()
  with
  | ExceptionDefn.Malformed_Decl er ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Pp.error Format.pp_print_string er in
    exit 2
  | Sys_error msg ->
    let () = ExceptionDefn.flush_warning Format.err_formatter in
    let () = Format.eprintf "%s@." msg in
    exit 2
  | e ->
    let () = Format.pp_print_flush Format.err_formatter () in raise e

let () = main ()
let _ = Alg_expr_extra.print
