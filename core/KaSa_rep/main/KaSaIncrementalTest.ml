let help_message =
  "Available commands:\n\
  \    quit/q\n\
  \        Quits the program.\n\
  \    help/h\n\
  \        Prints this help message.\n\
  \    print rules/working set/ws\n\
  \        Prints all rules (or just the working set) with the indexes of each \
   rule and an indication of which rules are enabled.\n\
  \    print result\n\
  \        Prints the KaSa reachability analysis result.\n\
  \    enable/disable <index>\n\
  \        Enables (or disables) the rule at index <index> in the working set.\n\
  \    enable/disable <label>\n\
  \        Enables (or disables) the rule with label <label> in the working set.\n\
  \    update file foo.ka\n\
  \        Replace the content of the current version of foo.ka with the new one.\n\
  \    update file foo.ka as foo'.ka\n\
  \        Replace the content of the current verion of the file foo'.ka with the content of the file foo.ka\n\
  \    add <rule>\n\
  \        (Not implemented yet TODO) Adds a rule to the working set.\n"

type parsed_instruction =
  | Add of string
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
              (match int_of_string_opt i with
              | None ->
                Parsing_error
                  ("Invalid index list: " ^ s
                 ^ ". Please put a label inside of quotation marks, e.g. \
                    'label'.")
              | Some i -> Enable_index (bool, i :: l))
           | Add _ | Enable _ | Parsing_error _ -> e)
          (Enable_index (bool, []))
          (String.split_on_char ',' s)
  in
  (* ---- ADD ---- *)
  if String.starts_with ~prefix:"add" s then (
    let s = String.trim (String.sub s 3 (String.length s - 3)) in
    Add s (* ---- ENABLE ---- *)
  ) else if String.starts_with ~prefix:"enable" s then (
    let s = String.trim (String.sub s 6 (String.length s - 6)) in
    parse_enable_label s true (* ---- DISABLE ---- *)
  ) else if String.starts_with ~prefix:"disable" s then (
    let s = String.trim (String.sub s 7 (String.length s - 7)) in
    parse_enable_label s false
  ) else
    Parsing_error ("Unknown command: " ^ s)

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
    let log = Remanent_parameters.get_logger parameters in
    Loggers.fprintf log "> ";
    Loggers.flush_logger log;
    let state =
      Export_to_KaSa.set_errors Exception.empty_exceptions_caught_and_uncaught
        state
    in
    try
      match String.trim (read_line ()) with
      | "quit" | "q" -> ()
      | "help" | "h" ->
        Loggers.fprintf log "%s" help_message;
        loop state
      | "print rules" ->
        let state, compilation = Export_to_KaSa.get_compilation state in
        let () =
          Loggers.fprintf log "%a" Ast.print_parsing_compil_kappa compilation
        in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop state
      | "print working set" | "print ws" ->
        let state, compilation = Export_to_KaSa.get_compilation state in
        let () = Loggers.fprintf log "%a" Ast.print_working_set compilation in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop state
      | "print result" ->
        let state = Export_to_KaSa.output_reachability_result state in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop state
      | input when String.length input > 12 && String.sub input 0 12 = "update file " -> 
            let l = String.split_on_char ' ' input in 
            let state = 
              match l with 
            | ["update";"file";patch_file_name] -> 
                let old_file_name = patch_file_name in 
                Export_to_KaSa.patch ~debug:true ~patch_file_name ~old_file_name state  
            | ["update";"file";patch_file_name;"as";old_file_name]  -> 
                Export_to_KaSa.patch ~debug:true ~patch_file_name ~old_file_name state 
            | _ -> 
              let error = Export_to_KaSa.get_errors state in
              let error, () =
                Exception.warn parameters error __POS__
                  ~message:("Parsing error: " ^ input) Exit ()
              in
              Export_to_KaSa.set_errors error state
            in 
          loop state 
      | input ->
        let state =
          match parse_input input with
          | Add _ ->
            print_endline "TODO adding a rule was not implemented yet";
            state
          | Enable (false, i) ->
            let () =
              Loggers.fprintf log "Disabling rule with label '%s'...\n" i
            in
            Export_to_KaSa.disable_rule i state
          | Enable (true, i) ->
            let () =
              Loggers.fprintf log "Enabling rule with label '%s'...\n" i
            in
            Export_to_KaSa.enable_rule i state
          | Enable_index (false, i) ->
            let () =
              Loggers.fprintf log "Disabling rules at index %s...\n"
                (String.concat "," (List.map string_of_int (List.rev i)))
            in
            Export_to_KaSa.disable_rule_index i state
          | Enable_index (true, i) ->
            let () =
              Loggers.fprintf log "Enabling rules at index %s...\n"
                (String.concat "," (List.map string_of_int (List.rev i)))
            in
            Export_to_KaSa.enable_rule_index i state
          | Parsing_error s ->
            let error = Export_to_KaSa.get_errors state in
            let error, () =
              Exception.warn parameters error __POS__
                ~message:("Parsing error: " ^ s) Exit ()
            in
            Export_to_KaSa.set_errors error state
        in
        let error = Export_to_KaSa.get_errors state in
        let () = Exception.print parameters error in
        loop state
    with End_of_file -> ()
  in
  loop state

let () = main ()
