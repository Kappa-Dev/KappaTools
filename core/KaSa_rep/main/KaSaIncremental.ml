let main () =
  let start_time = Sys.time () in
  let errors = Exception.empty_exceptions_caught_and_uncaught in
  let _, parameters, _ = Get_option.get_option errors in
  let module A =
    (val Domain_selection.select_domain
           ~reachability_parameters:
             (Remanent_parameters.get_reachability_analysis_parameters
                parameters)
           ())
  in
  let export_to_kasa =
    (module Export_to_KaSa.Export (A) : Export_to_KaSa.Type)
  in
  let module Export_to_KaSa = (val export_to_kasa : Export_to_KaSa.Type) in
  let state = Export_to_KaSa.init () in
  let state =
    if Remanent_parameters.get_compute_symmetries parameters then
      fst (Export_to_KaSa.get_env state)
    else
      state
  in
  let module KaSaUtil = KaSaUtil.KaSaUtil (Export_to_KaSa) in
  let state = KaSaUtil.print_analysis_result start_time state in
  let rec loop state =
    print_string "> ";
    flush stdout;
    (* let error_message s =
         "Invalid input: " ^ s ^ ". Please try again. Type 'help' for help."
       in *)
    try
      match String.trim (read_line ()) with
      | "quit" -> ()
      | "help" ->
        print_endline "TODO";
        loop state
      | "print rules" ->
        let state, compilation = Export_to_KaSa.get_compilation state in
        let log = Remanent_parameters.get_logger parameters in
        let () =
          Loggers.fprintf log "%a" Ast.print_parsing_compil_kappa compilation
        in
        print_endline "TODO";
        loop state
      | "print result" ->
        let state = Export_to_KaSa.output_reachability_result state in
        loop state
      | input ->
        let state =
          match input with
          | "add" -> state
          | "disable" | "enable" -> state
          | _ ->
            print_endline "??";
            state
        in
        print_endline "done";
        loop state
    with e ->
      print_endline "error TODO";
      prerr_endline (Printexc.to_string e);
      prerr_endline (Printexc.get_backtrace ())
  in
  loop state

let () = main ()
