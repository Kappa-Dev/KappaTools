(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Apr 24 2017>
*)

module A = Odes.Make (Ode_interface)

let main ?called_from:(called_from=Remanent_parameters_sig.Server) () =
  let usage_msg =
    "KaDE "^Version.version_string^":\n"^
    "Usage is KaDE input_file [--ode-backend Matlab | Octave | Maple | Mathematica | SBML | DOTNET ] [--rate-convention KaSim | Divide_by_nbr_of_autos_in_lhs | Biochemist] [-t-init time] [-t time] [-p delta_t] [-o output_file] [--matlab-output foo.m] [--octave-output foo.m] [--maple-output foo.mws] [--mathematica foo.nb] [--sbml-output foo.xml] [--dotnet-output foo.net] [--compute-jacobian true | false] [--with-symmetries Ground | Forward | Backward] [--show-symmetres false | true] [--views-domain true | false] [--double-bonds-domain true | false] [--site-across-bonds-domain true | false] [--nonnegative false | true ] [--export-time-advance false | true ] [--initial-step float] [--max-step float]
    [--relative-tolerance float] [--absolute-tolerance float]\n"
  in
  let cli_args = Run_cli_args.default in
  let cli_args_gui = Run_cli_args.default_gui in
  let common_args = Common_args.default in
  let common_args_gui = Common_args.default_gui in
  let ode_args = Ode_args.default in
  let options =
    Run_cli_args.options_gui cli_args_gui
      @ Ode_args.options ode_args
      @ Common_args.options_gui common_args_gui
    in
  try
    let files = Ode_args.get_option options in
    let () = Common_args.copy_from_gui common_args_gui common_args in
    let () = Run_cli_args.copy_from_gui cli_args_gui cli_args in
    let () = cli_args.Run_cli_args.inputKappaFileNames <- files in
    let () = Kappa_files.set_dir cli_args.Run_cli_args.outputDirectory in
    let () = Parameter.debugModeOn := common_args.Common_args.debug in
    let backend =
      match Tools.lowercase !(ode_args.Ode_args.backend) with
      | "octave" -> Loggers.Octave
      | "matlab" -> Loggers.Matlab
      | "mathematica" -> Loggers.Mathematica
      | "maple" -> Loggers.Maple
      | "dotnet" -> Loggers.DOTNET
      | "sbml" -> Loggers.SBML
      | s ->
        begin
          (*Arg.usage options usage_msg;*)
          Debug.tag
            Format.std_formatter
            ("Wrong option "^s^".\nOnly DOTNET, Matlab, Mathematica, Maple, Octave, and SBML backends are supported.");
          exit 0
        end
    in
    let rate_convention =
      match Tools.lowercase !(ode_args.Ode_args.rate_convention) with
    | "kasim" -> Remanent_parameters_sig.No_correction
    | "divide_by_nbr_of_autos_in_lhs" ->
      Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs
    | "biochemist" -> Remanent_parameters_sig.Biochemist
    | s ->
      begin
        (*Arg.usage options usage_msg;*)
        Debug.tag
          Format.std_formatter
          ("Wrong option "^s^".\nOnly KaSim and Biochemist are supported.");
        exit 0
      end
    in
    let propagate_constants = !(ode_args.Ode_args.propagate_constants) in
    let () =
      match
        !(ode_args.Ode_args.matlab_output)
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.Matlab
          s
    in
    let () =
      match
        !(ode_args.Ode_args.octave_output)
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.Octave
          s
    in
    (*smbl*)
    let () =
      match
        !(ode_args.Ode_args.sbml_output)
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.SBML
          s
    in
    (*dotnet*)
    let () =
      match
        !(ode_args.Ode_args.dotnet_output)
      with
      | None -> ()
      | Some s ->
        Kappa_files.set_ode
          ~mode:Loggers.DOTNET
          s
    in
    let count =
      match Tools.lowercase !(ode_args.Ode_args.count) with
      | "embedding" | "embeddings" -> Ode_args.Embeddings
      | "occurrences" | "occurrence" | "instances" | "instance"->
        Ode_args.Occurrences
    | s ->
      begin
        (*Arg.usage options usage_msg;*)
        Debug.tag
          Format.std_formatter
          ("Wrong option "^s^".\nOnly Embeddings and Occurrences are supported.");
        exit 0
      end
    in
    let show_reactions = !(ode_args.Ode_args.show_reactions) in
    let compute_jacobian = !(ode_args.Ode_args.compute_jacobian) in
    let show_time_advance = !(ode_args.Ode_args.show_time_advance) in
    let nonnegative = !(ode_args.Ode_args.nonnegative) in
    let initial_step = !(ode_args.Ode_args.initial_step) in
    let max_step = !(ode_args.Ode_args.max_step) in
    let reltol = !(ode_args.Ode_args.relative_tolerance) in
    let abstol= !(ode_args.Ode_args.absolute_tolerance) in
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
    let ignore_obs,dotnet =
      match backend with
      | Loggers.DOTNET -> true,true
      | Loggers.SBML
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML |
        Loggers.HTML_Tabular
      | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS -> true,false
      | Loggers.Octave | Loggers.Matlab
      | Loggers.Mathematica | Loggers.Maple | Loggers.Json -> false,false
    in
    let compil =
      A.get_compil
        ~rate_convention ~show_reactions ~count ~compute_jacobian
        cli_args
    in
    (*************************************************************)
    (*TEST-symmetries*)
    let network = A.init compil in
    let parameters =
      Ode_args.build_kasa_parameters ~called_from ode_args
        common_args
    in
    let parameters' = parameters in
    let ground, forward, backward = 0,1,2 in
    let reduction =
      match Tools.lowercase !(ode_args.Ode_args.with_symmetries)
      with
      | "none" | "ground" | "false" -> ground
      | "true" | "forward" -> forward
      | "backward" -> backward
      | _ -> ground
    in
    let network =
      if
        (not (reduction = ground))
        || !(ode_args.Ode_args.show_symmetries)
      then
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
        let state =
          Export_to_kade.init ~compil:kasa_compil ()
        in
        let parameters =
          Export_to_kade.get_parameters state
        in
        let state =
          Export_to_kade.set_parameters parameters state
        in
        let _state, contact_map =
          Export_to_kade.get_contact_map
            ~accuracy_level:Remanent_state.High state
        in
        let parameters =
          Remanent_parameters.set_logger
            parameters
            (Remanent_parameters.get_logger parameters')
        in
        let parameters =
          Remanent_parameters.set_trace
            parameters
            (Remanent_parameters.get_trace parameters')
        in
        let network =
          A.compute_symmetries_from_model
            parameters
            compil
            network
            contact_map
        in
        let network =
          if reduction = backward
          then
            A.set_to_backward_symmetries_from_model
              network
          else if reduction = forward
          then
            A.set_to_forward_symmetries_from_model
              network
          else
            network
        in
        let () =
          if !(ode_args.Ode_args.show_symmetries)
          then
            A.print_symmetries
              parameters compil
              network
        in
        network
      else
        network
    in
    let smash_reactions =
      !(ode_args.Ode_args.smash_reactions)
    in
    let network =
      A.network_from_compil ~dotnet ~smash_reactions ~ignore_obs parameters compil network
    in
    (*************************************************************)
    let out_channel =
      Kappa_files.open_out (Kappa_files.get_ode ~mode:backend)
    in
    let logger = Loggers.open_logger_from_channel ~mode:backend
        out_channel
    in
    let logger_buffer =
      match backend with
      | Loggers.DOTNET
      | Loggers.SBML -> Loggers.open_infinite_buffer ~mode:backend ()
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS -> logger
      | Loggers.Octave | Loggers.Matlab
      | Loggers.Mathematica | Loggers.Maple | Loggers.Json -> logger
    in
    let logger_err =
      match backend with
      | Loggers.DOTNET
      | Loggers.SBML -> Loggers.open_infinite_buffer ~mode:backend ()
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
      | Loggers.XLS -> logger
      | Loggers.Octave | Loggers.Matlab
      | Loggers.Mathematica | Loggers.Maple | Loggers.Json -> logger
    in
    let () =
      A.export_network
        ~command_line
        ~command_line_quotes
        ?data_file:cli_args.Run_cli_args.outputDataFile
        ?init_t:cli_args.Run_cli_args.minValue
        ~max_t:(Option_util.unsome 1. cli_args.Run_cli_args.maxValue)
        ~propagate_constants
        ~compute_jacobian
        ~show_time_advance
        ~nonnegative
        ~initial_step
        ~max_step
        ~reltol
        ~abstol
        ?plot_period:cli_args.Run_cli_args.plotPeriod
        parameters
        logger
        logger_buffer
        logger_err
        compil network
    in
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
    let () = Format.pp_print_flush Format.err_formatter () in
    raise e

let () = main ~called_from:Remanent_parameters_sig.KaSa ()
