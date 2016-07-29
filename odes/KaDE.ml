(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jul 29 2016>
*)

module A = Odes.Make (Dummy_interface.Interface)

let unsome opt default =
  match opt with
  | None -> default
  | Some a -> a

let main () =
  let usage_msg =
    "KaDE "^Version.version_string^":\n"^
    "Usage is KaDE [-i] input_file [-t-init time] [-t time] [-p points] [-o output_file]\n"
  in
  let kasim_args = Kasim_args.default in
  let ode_args = Ode_args.default in
  let options =
    Kasim_args.options kasim_args
    @ Ode_args.options ode_args
  in
  try
    Arg.parse
      options
      (fun fic ->
         kasim_args.Kasim_args.inputKappaFileNames <-
           fic::(kasim_args.Kasim_args.inputKappaFileNames))
      usage_msg;
    let () = Kappa_files.set_data kasim_args.Kasim_args.outputDataFile in
    let () = Kappa_files.set_dir kasim_args.Kasim_args.outputDirectory in
    (*  let () = match kasim_args.Kasim_args.marshalizeOutFile with
        | None -> ()
        | Some marshalizeOutFile ->
        Kappa_files.set_marshalized marshalizeOutFile
        in*)
    (*  let () = match kasim_args.Kasim_args.domainOutputFile with
      | None -> ()
      | Some domainOutputFile ->
        Kappa_files.set_ccFile domainOutputFile
        in*)
    (*  let () = match kasim_args.Kasim_args.traceFile with
        | None -> ()
        | Some traceFile ->
        Kappa_files.set_traceFile traceFile
        in*)
    (*  let () = Parameter.debugModeOn := common_args.Common_args.debug in*)
    (*  let () = Parameter.eclipseMode := kasim_args.Kasim_args.eclipseMode in*)
    (*let () = Parameter.emacsMode := kasim_args.Kasim_args.emacsMode in*)
    (*let () = Parameter.compileModeOn := kasim_args.Kasim_args.compileMode in*)
    let () = Parameter.batchmode := kasim_args.Kasim_args.batchmode in
    (*let () = Parameter.time_independent := common_args.Common_args.timeIndependent*)


    let abort =
      match kasim_args.Kasim_args.inputKappaFileNames with
      | [] -> kasim_args.Kasim_args.marshalizedInFile = ""
      | _ -> false
    in
    if abort then (prerr_string usage_msg ; exit 1) ;
    let () = Sys.catch_break true in
    let () = Kappa_files.setCheckFileExistsODE ~batchmode:!Parameter.batchmode in
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
    let compil =
      A.get_compil kasim_args.Kasim_args.inputKappaFileNames
    in
    let network =
      A.network_from_compil compil
    in
    let out_channel = Kappa_files.open_out (Kappa_files.get_ode ()) in
    let logger = Loggers.open_logger_from_channel ~mode:Loggers.Octave out_channel in
    let () = A.export_network
        ~command_line
        ~command_line_quotes
        ~data_file:(Kappa_files.get_data ())
        ~init_t:ode_args.Ode_args.minTimeValue
        ~max_t:(unsome kasim_args.Kasim_args.maxTimeValue 1.)
        ~nb_points:kasim_args.Kasim_args.pointNumberValue
        logger compil network
    in
    let () = Loggers.flush_logger logger in
    let () = close_out out_channel in
    ()
  with
  | ExceptionDefn.Malformed_Decl _ as e -> raise e
  | _exn ->
    Debug.tag
      Format.std_formatter
      "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
    exit 1


let () = main ()
