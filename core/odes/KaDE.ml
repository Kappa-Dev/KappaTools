(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jan 08 2020>
*)

module A = Odes.Make (Symmetry_interface)

let main ?(called_from = Remanent_parameters_sig.Server) () =
  let start_time = Sys.time () in
  let cli_args = Run_cli_args.default in
  let cli_args_gui = Run_cli_args.default_gui in
  let common_args = Common_args.default in
  let common_args_gui = Common_args.default_gui in
  let ode_args = Ode_args.default in
  let options =
    List.rev
      (Run_cli_args.options_gui cli_args_gui
      @ Ode_args.options ode_args
      @ Common_args.options_gui common_args_gui)
  in
  try
    let files = Ode_args.get_option options in
    let files =
      List.fold_left
        (fun list elt -> elt :: list)
        !(cli_args_gui.Run_cli_args.inputKappaFileNames_gui)
        files
    in
    let () =
      match files with
      | [] ->
        Format.printf "No input file has been provided.@.";
        exit 0
      | _ :: _ -> ()
    in
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
        Format.printf
          "Wrong option %s.@.Only DOTNET, Matlab, Mathematica, Maple, Octave, \
           and SBML backends are supported.@."
          s;
        exit 0
    in
    let rule_rate_convention =
      match Tools.lowercase !(ode_args.Ode_args.rule_rate_convention) with
      | "kasim" -> Remanent_parameters_sig.No_correction
      | "divide_by_nbr_of_autos_in_lhs" ->
        Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs
      | "biochemist" -> Remanent_parameters_sig.Biochemist
      | s ->
        Format.printf
          "Wrong option %s.@.Only KaSim and Biochemist are supported.@." s;
        exit 0
    in
    let reaction_rate_convention =
      match
        backend, Tools.lowercase !(ode_args.Ode_args.reaction_rate_convention)
      with
      | ( (Loggers.Octave | Loggers.Matlab | Loggers.Mathematica | Loggers.Maple),
          ("kasim" | "divide_by_nbr_of_autos_in_lhs" | "biochemist") )
      | _, "kasim" ->
        Remanent_parameters_sig.No_correction
      | (Loggers.SBML | Loggers.DOTNET), "divide_by_nbr_of_autos_in_lhs" ->
        Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs
      | (Loggers.SBML | Loggers.DOTNET), "biochemist" ->
        Remanent_parameters_sig.Biochemist
      | _, s ->
        Format.printf
          "Wrong option %s.@.Only KaSim and Biochemist are supported.@." s;
        exit 0
    in
    let propagate_constants = !(ode_args.Ode_args.propagate_constants) in
    let max_size = !(ode_args.Ode_args.max_size_for_species) in
    let () =
      match !(ode_args.Ode_args.matlab_output) with
      | None -> ()
      | Some s -> Ode_loggers_sig.set_ode ~mode:Loggers.Matlab s
    in
    let () =
      match !(ode_args.Ode_args.octave_output) with
      | None -> ()
      | Some s -> Ode_loggers_sig.set_ode ~mode:Loggers.Octave s
    in
    (*smbl*)
    let () =
      match !(ode_args.Ode_args.sbml_output) with
      | None -> ()
      | Some s -> Ode_loggers_sig.set_ode ~mode:Loggers.SBML s
    in
    (*dotnet*)
    let () =
      match !(ode_args.Ode_args.dotnet_output) with
      | None -> ()
      | Some s -> Ode_loggers_sig.set_ode ~mode:Loggers.DOTNET s
    in
    let count =
      match Tools.lowercase !(ode_args.Ode_args.count) with
      | "embedding" | "embeddings" -> Ode_args.Embeddings
      | "occurrences" | "occurrence" | "instances" | "instance" ->
        Ode_args.Occurrences
      | s ->
        Format.printf
          "Wrong option %s.@.Only Embeddings and Occurrences are supported.@." s;
        exit 0
    in
    let internal_meaning =
      match Tools.lowercase !(ode_args.Ode_args.internal_meaning) with
      | "embedding" | "embeddings" -> Ode_args.Embeddings
      | "occurrences" | "occurrence" | "instances" | "instance" ->
        Ode_args.Occurrences
      | s ->
        Format.printf
          "Wrong option %s.@.Only Embeddings and Occurrences are supported.@." s;
        exit 0
    in
    let show_reactions = !(ode_args.Ode_args.show_reactions) in
    let compute_jacobian = !(ode_args.Ode_args.compute_jacobian) in
    let show_time_advance = !(ode_args.Ode_args.show_time_advance) in
    let nonnegative = !(ode_args.Ode_args.nonnegative) in
    let initial_step = !(ode_args.Ode_args.initial_step) in
    let max_step = !(ode_args.Ode_args.max_step) in
    let reltol = !(ode_args.Ode_args.relative_tolerance) in
    let abstol = !(ode_args.Ode_args.absolute_tolerance) in
    let () =
      if not cli_args.Run_cli_args.batchmode then
        Kappa_files.check_not_exists (Ode_loggers_sig.get_ode ~mode:backend)
    in
    let command_line =
      Format.asprintf "%a"
        (Pp.array
           (fun f -> Format.fprintf f " ")
           (fun i f s ->
             Format.fprintf f "%s"
               (if i = 0 then
                  "KaDE"
                else
                  s)))
        Sys.argv
    in
    let command_line_quotes =
      Format.asprintf "%a"
        (Pp.array Pp.space (fun i f s ->
             Format.fprintf f "'%s'"
               (if i = 0 then
                  "KaDE"
                else
                  s)))
        Sys.argv
    in
    let ignore_obs, dotnet =
      match backend with
      | Loggers.DOTNET -> true, true
      | Loggers.SBML | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
      | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
      | Loggers.TXT_Tabular | Loggers.GEPHI | Loggers.XLS ->
        true, false
      | Loggers.Octave | Loggers.Matlab | Loggers.Mathematica | Loggers.Maple
      | Loggers.Json ->
        false, false
    in
    let ast = A.get_ast cli_args in
    let preprocessed_ast = A.preprocess cli_args ast in
    (*************************************************************)
    (*TEST-symmetries*)
    let parameters =
      Ode_args.build_kasa_parameters ~called_from ode_args common_args
    in
    let parameters' = parameters in
    let ground, forward, backward = 0, 1, 2 in
    let reduction =
      match Tools.lowercase !(ode_args.Ode_args.with_symmetries) with
      | "none" | "ground" | "false" -> ground
      | "true" | "forward" -> forward
      | "backward" -> backward
      | _ -> ground
    in
    let network, compil =
      if (not (reduction = ground)) || !(ode_args.Ode_args.show_symmetries) then
        let module B =
          (val Domain_selection.select_domain
                 ~reachability_parameters:
                   (Remanent_parameters.get_reachability_analysis_parameters
                      parameters)
                 ())
        in
        let export_to_kade =
          (module Export_to_KaDE.Export (B) : Export_to_KaDE.Type)
        in
        let module Export_to_kade = (val export_to_kade : Export_to_KaDE.Type)
        in
        let () = Format.printf "+ compute symmetric sites... @." in
        let state = Export_to_kade.init ~compil:(A.to_ast ast) () in
        let parameters = Export_to_kade.get_parameters state in
        let state = Export_to_kade.set_parameters parameters state in
        let _state, contact_map =
          Export_to_kade.get_contact_map ~accuracy_level:Public_data.High state
        in
        let parameters =
          Remanent_parameters.set_logger parameters
            (Remanent_parameters.get_logger parameters')
        in
        let parameters =
          Remanent_parameters.set_trace parameters
            (Remanent_parameters.get_trace parameters')
        in
        let compil =
          A.get_compil ~debugMode:common_args.Common_args.debug ~dotnet
            ~reaction_rate_convention ~rule_rate_convention ~show_reactions
            ~count ~internal_meaning ~compute_jacobian cli_args preprocessed_ast
        in
        let network = A.init compil in
        let network =
          A.compute_symmetries_from_model parameters compil network contact_map
        in
        let network, compil =
          if reduction = backward then (
            let network = A.set_to_backward_symmetries_from_model network in
            let bwd_bisim = A.init_bwd_bisim_info network in
            let () =
              Format.printf
                "+ restart compilation to account for ~-equivalent patterns in \
                 algebraic expressions... @."
            in
            let compil =
              A.get_compil ~debugMode:common_args.Common_args.debug ~dotnet
                ?bwd_bisim ~reaction_rate_convention ~rule_rate_convention
                ~show_reactions ~count ~internal_meaning ~compute_jacobian
                cli_args preprocessed_ast
            in
            let network = A.reset compil network in
            let network = A.set_to_backward_symmetries_from_model network in
            network, compil
          ) else if reduction = forward then
            A.set_to_forward_symmetries_from_model network, compil
          else
            network, compil
        in
        let () =
          if !(ode_args.Ode_args.show_symmetries) then
            A.print_symmetries parameters compil network
        in
        network, compil
      else (
        let compil =
          A.get_compil ~debugMode:common_args.Common_args.debug ~dotnet
            ~reaction_rate_convention ~rule_rate_convention ~show_reactions
            ~count ~internal_meaning ~compute_jacobian cli_args preprocessed_ast
        in
        let network = A.init compil in
        network, compil
      )
    in
    let smash_reactions = !(ode_args.Ode_args.smash_reactions) in
    let network =
      A.network_from_compil ?max_size ~smash_reactions ~ignore_obs parameters
        compil network
    in
    (*************************************************************)
    let out_channel =
      Kappa_files.open_out (Ode_loggers_sig.get_ode ~mode:backend)
    in
    let pre_logger =
      Loggers.open_logger_from_channel ~mode:backend out_channel
    in
    let csv_sep = !(ode_args.Ode_args.csv_sep) in
    let logger = Ode_loggers_sig.extend_logger ~csv_sep pre_logger in
    let logger_buffer =
      match backend with
      | Loggers.SBML ->
        Ode_loggers_sig.extend_logger ~csv_sep
          (Loggers.open_infinite_buffer ~mode:backend ())
      | Loggers.DOTNET | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
      | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
      | Loggers.TXT_Tabular | Loggers.XLS | Loggers.GEPHI | Loggers.Octave
      | Loggers.Matlab | Loggers.Mathematica | Loggers.Maple | Loggers.Json ->
        logger
    in
    let logger_err =
      match backend with
      | Loggers.DOTNET | Loggers.SBML ->
        Loggers.open_infinite_buffer ~mode:backend ()
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
      | Loggers.HTML_Tabular | Loggers.GEPHI | Loggers.DOT | Loggers.TXT
      | Loggers.TXT_Tabular | Loggers.XLS ->
        pre_logger
      | Loggers.Octave | Loggers.Matlab | Loggers.Mathematica | Loggers.Maple
      | Loggers.Json ->
        pre_logger
    in
    let network =
      A.export_network ~command_line ~command_line_quotes
        ?data_file:cli_args.Run_cli_args.outputDataFile
        ?init_t:cli_args.Run_cli_args.minValue
        ~max_t:(Option_util.unsome 1. cli_args.Run_cli_args.maxValue)
        ~propagate_constants ~compute_jacobian ~show_time_advance ~nonnegative
        ~initial_step ~max_step ~reltol ~abstol
        ?plot_period:cli_args.Run_cli_args.plotPeriod parameters logger
        logger_buffer logger_err compil network
    in
    let () = Ode_loggers_sig.flush_logger logger in
    let () = close_out out_channel in
    let () =
      if !(ode_args.Ode_args.print_efficiency) then (
        let end_time = Sys.time () in
        let cpu_time = end_time -. start_time in
        let nrules, nreactions, nspecies = A.get_data network in
        let () =
          Format.printf "CPU time: %g s.; %i rules; %i species; %i reactions.@."
            cpu_time nrules nspecies nreactions
        in
        ()
      )
    in
    ()
  with
  | ExceptionDefn.Malformed_Decl er ->
    let () = Pp.error Format.pp_print_string er in
    exit 2
  | Sys_error msg ->
    let () = Format.eprintf "%s@." msg in
    exit 2

let () = main ~called_from:Remanent_parameters_sig.KaSa ()
let _ = Affine_combinations.sum
