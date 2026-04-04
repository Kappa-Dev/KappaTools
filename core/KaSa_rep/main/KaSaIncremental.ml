let help_message =
  "Available commands:\n\
  \    quit/q\n\
  \        Quits the program.\n\
  \    help/h\n\
  \        Prints this help message.\n\
  \    print rules/print working set/print ws\n\
  \        Prints all rules (or just the working set) with the indexes of each \
   rule and an indication of which rules are enabled.\n\
  \    print result\n\
  \        Prints the KaSa reachability analysis result.\n\
  \    enable/disable <index>\n\
  \        Enables (or disables) the rule at index <index> in the working set.\n\
  \    enable/disable <label>\n\
  \        Enables (or disables) the rule with label <label> in the working set.\n\
  \    update file foo.ka\n\
  \        Replace the content of the current version of foo.ka with the new \
   one.\n\
  \    update file foo.ka as foo'.ka\n\
  \        Replace the content of the current verion of the file foo'.ka with \
   the content of the file foo.ka\n"

type parsed_instruction =
  | Enable_index of bool * int list
  | Enable of bool * string
  | Parsing_error of string

let parse_input s =
  let parse_enable_label s bool =
    match s with
    | "" -> Parsing_error "Empty rule name"
    | s ->
      let fst = String.sub s 0 1 in
      (*labeled rule*)
      if fst = "'" then
        if String.sub s (String.length s - 1) 1 = "'" then
          Enable (bool, String.sub s 1 (String.length s - 2))
        else
          Parsing_error "Label quotation marks were not closed"
      else
        List.fold_left
          (fun e i ->
            match e with
            | Enable_index (bool, l) ->
              (match int_of_string_opt (String.trim i) with
              | None ->
                Parsing_error
                  ("Invalid index list: " ^ s
                 ^ ". Please put a label inside of quotation marks, e.g. \
                    'label'.")
              | Some i -> Enable_index (bool, i :: l))
            | Enable _ | Parsing_error _ -> e)
          (Enable_index (bool, []))
          (if String.contains s ',' then
             String.split_on_char ',' s
           else
             String.split_on_char ' ' s)
  in
  if (* ---- ENABLE ---- *) String.starts_with ~prefix:"enable" s then (
    let s = String.trim (String.sub s 6 (String.length s - 6)) in
    parse_enable_label s true
  ) else if (* ---- DISABLE ---- *)
            String.starts_with ~prefix:"disable" s then (
    let s = String.trim (String.sub s 7 (String.length s - 7)) in
    parse_enable_label s false
  ) else
    Parsing_error ("Unknown command: " ^ s)

let main () =
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
  let module KaSaUtil = KaSaUtil.KaSaUtil (Export_to_KaSa) in
  let print_result parameters state print_analysis =
    let state, _ = Export_to_KaSa.get_reachability_analysis state in
    let state =
      if print_analysis then
        Export_to_KaSa.output_reachability_result state
      else
        state
    in
    let error = Export_to_KaSa.get_errors state in
    let () = Exception.print parameters error in
    state
  in
  let log = Remanent_parameters.get_logger parameters in
  let rec loop ?command state start_time =
    let () =
      match start_time with
      | None -> ()
      | Some start_time ->
        let () = if Remanent_parameters.get_print_efficiency parameters then ( Loggers.fprintf log "execution took " ) in
        let () = KaSaUtil.print_only_timing parameters start_time in
        Loggers.print_newline log
    in
    let state =
      match state with
      | None -> None
      | Some state ->
        Some
          (Export_to_KaSa.set_errors
             Exception.empty_exceptions_caught_and_uncaught state)
    in
    let s =
      match command with
      | Some s -> s
      | None ->
        let () = Loggers.fprintf log "> " in
        let () = Loggers.flush_logger log in
        String.trim (read_line ())
    in
    let state =
      match state with
      | None -> None
      | Some state ->
        Some
          (Export_to_KaSa.set_errors
             Exception.empty_exceptions_caught_and_uncaught state)
    in
    try
      let start_time = Some (Sys.time ()) in
      match s, state with
      | ("quit" | "q"), _ -> ()
      | ("help" | "h"), _ ->
        let () = Loggers.fprintf log "%s" help_message in
        loop state None
      | "restart", _ | _, None ->
        let state = Export_to_KaSa.init () in
        let state =
          if Remanent_parameters.get_compute_symmetries parameters then
            fst (Export_to_KaSa.get_env state)
          else
            state
        in
        let state = print_result parameters state false in
        loop (Some state) start_time
      | ("print rules" | "p rules"), Some state ->
        let state, compilation = Export_to_KaSa.get_compilation state in
        let () =
          Loggers.fprintf log "%a" Ast.print_parsing_compil_kappa compilation
        in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop (Some state) None
      | ("print working set" | "print ws" | "p ws"), Some state ->
        let state, compilation = Export_to_KaSa.get_compilation state in
        let () = Loggers.fprintf log "%a" Ast.print_working_set compilation in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop (Some state) None
      | ("print result" | "p result" | "p"), Some state ->
        let state = print_result parameters state true in
        loop (Some state) start_time
      | input, Some state
        when String.length input > 12 && String.sub input 0 12 = "update file "
        ->
        let l = String.split_on_char ' ' input in
        let do_not_restart_fixpoint_computation =
          match !Config.do_restart_fixpoint_iterations with
          | true -> Some false
          | false -> Some true
        in
        let state =
          match l with
          | [ "update"; "file"; patch_file_name ] ->
            let old_file_name = patch_file_name in
            let state =
              Export_to_KaSa.patch ?do_not_restart_fixpoint_computation
                ~patch_file_name ~old_file_name state
            in
            state
          | [ "update"; "file"; patch_file_name; "as"; old_file_name ] ->
            let state =
              Export_to_KaSa.patch ?do_not_restart_fixpoint_computation
                ~patch_file_name ~old_file_name state
            in
            state
          | _ ->
            let error = Export_to_KaSa.get_errors state in
            let error, () =
              Exception.warn parameters error __POS__
                ~message:("Parsing error: " ^ input)
                Exit ()
            in
            Export_to_KaSa.set_errors error state
        in
        let state = print_result parameters state false in
        loop (Some state) start_time
      | input, Some state ->
        let success, state =
          match parse_input input with
          | Enable (false, i) ->
            let () =
              Loggers.fprintf log "Disabling rule with label '%s'...\n" i
            in
            true, Export_to_KaSa.disable_rule i state
          | Enable (true, i) ->
            let () =
              Loggers.fprintf log "Enabling rule with label '%s'...\n" i
            in
            true, Export_to_KaSa.enable_rule i state
          | Enable_index (false, i) ->
            let () =
              Loggers.fprintf log
                "Disabling initial states or rules at index %s...\n"
                (String.concat "," (List.map string_of_int (List.rev i)))
            in
            true, Export_to_KaSa.disable_rule_index i state
          | Enable_index (true, i) ->
            let () =
              Loggers.fprintf log
                "Enabling initial states or rules at index %s...\n"
                (String.concat "," (List.map string_of_int (List.rev i)))
            in
            true, Export_to_KaSa.enable_rule_index i state
          | Parsing_error s ->
            let error = Export_to_KaSa.get_errors state in
            let error, () =
              Exception.warn parameters error __POS__
                ~message:("Parsing error: " ^ s) Exit ()
            in
            false, Export_to_KaSa.set_errors error state
        in
        let state =
          if success then
            print_result parameters state true
          else (
            let error = Export_to_KaSa.get_errors state in
            let () = Exception.print parameters error in
            state
          )
        in
        loop (Some state) start_time
    with End_of_file -> ()
  in
  loop ~command:"restart" None None

let () = main ()
