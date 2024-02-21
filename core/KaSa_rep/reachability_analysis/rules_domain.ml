(**
   * rule_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 30th of January
   * Last modification: Time-stamp: <Dec 04 2018>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

module Domain = struct
  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
    domain_static_information: unit; (* no domain-specific static information *)
  }

  (*--------------------------------------------------------------------*)
  (* this array indicates whether a rule has already be applied, or not *)

  (* This array is statically allocated *)
  (* Why do you use extensive arrays ? *)
  type local_dynamic_information =
    bool Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static
  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  (** dead rule local dynamic information*)
  let get_dead_rule dynamic = dynamic.local

  let set_dead_rule dead_rule dynamic = { dynamic with local = dead_rule }

  (*--------------------------------------------------------------------*)

  type 'a zeroary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * dynamic_information * 'd

  (**************************************************************************)
  (** [get_scan_rule_set static] *)

  let initialize static dynamic error =
    let init_global_static_information =
      { global_static_information = static; domain_static_information = () }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let parameters = Analyzer_headers.get_parameter static in
    let nrules = Handler.nrules parameters error kappa_handler in
    let error, init_dead_rule_array =
      if nrules = 0 then
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.create parameters error
          0
      else
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.init parameters error
          (nrules - 1) (fun _ error _ -> error, false)
    in
    let init_global_dynamic_information =
      { global = dynamic; local = init_dead_rule_array }
    in
    error, init_global_static_information, init_global_dynamic_information, []

  let complete_wake_up_relation _static error wake_up = error, wake_up

  let add_initial_state _static dynamic error _species =
    let event_list = [] in
    error, dynamic, event_list

  (*only change the precondition*)
  (*check the preconditon of rule, is this the first time this rule apply*)
  (*{
    Analyzer_headers.Sure_value true
    (*if rule_id apply for the first time, false in dynamic.
    otherwise sure_value false. no maybe
   *)
    }
   *)
  let is_enabled static dynamic error (rule_id : Ckappa_sig.c_rule_id)
      precondition =
    let parameters = get_parameter static in
    let bool_array = get_dead_rule dynamic in
    let error, bool =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
        rule_id bool_array
    in
    match bool with
    | Some false | None ->
      let error, precondition =
        Communication.the_rule_is_applied_for_the_first_time
          (get_parameter static) error precondition
      in
      error, dynamic, Some precondition
    | Some true ->
      let error, precondition =
        Communication.the_rule_is_not_applied_for_the_first_time
          (get_parameter static) error precondition
      in
      error, dynamic, Some precondition

  (***********************************************************)

  (* Nothing to do, in this domain *)
  let maybe_reachable _static dynamic error _flag _pattern precondition =
    error, dynamic, Some precondition

  (***********************************************************)

  let apply_rule static dynamic error rule_id precondition =
    (*false -> true: print the information that rule apply for the first
      time, then update array*)
    let event_list = [] in
    let dead_rule_array = get_dead_rule dynamic in
    let parameters = get_parameter static in
    let compil = get_compil static in
    let error, rule_id_string =
      try Handler.string_of_rule parameters error compil rule_id
      with _ ->
        Exception.warn parameters error __POS__ Exit
          (Ckappa_sig.string_of_rule_id rule_id)
    in
    (*print*)
    let error, dynamic =
      match
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
          rule_id dead_rule_array
      with
      | error, Some false ->
        let error, dead_rule_array =
          Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.set parameters error
            rule_id true dead_rule_array
        in
        let dynamic =
          let log = Remanent_parameters.get_logger parameters in
          if
            local_trace
            || Remanent_parameters.get_trace parameters
            || Remanent_parameters.get_dump_reachability_analysis_iteration
                 parameters
          then (
            let () = Loggers.print_newline log in
            let () =
              Loggers.fprintf log "\t\t%s is applied for the first time"
                rule_id_string
            in
            let () = Loggers.print_newline log in
            dynamic
          ) else
            dynamic
        in
        let dynamic = set_dead_rule dead_rule_array dynamic in
        error, dynamic
      | error, Some true -> error, dynamic
      | error, None -> Exception.warn parameters error __POS__ Exit dynamic
    in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_one_side_effect _static dynamic error _ _ precondition =
    error, dynamic, (precondition, [])
  (* this domain ignores side effects *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let stabilize _static dynamic error = error, dynamic, ()

  let export static dynamic error kasa_state =
    let parameters = get_parameter static in
    let hide_reverse_rule =
      Remanent_parameters.get_hide_reverse_rule_without_label_from_dead_rules
        parameters
    in
    let original = hide_reverse_rule in
    let compil = get_compil static in
    let array = get_dead_rule dynamic in
    let error, list =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
        (fun _parameters error i bool list ->
          if bool then
            error, list
          else (
            let error, info =
              Handler.info_of_rule ~original ~with_rates:false parameters error
                compil i
            in
            let error, b1 = Handler.is_reverse parameters error compil i in
            let error, b2 = Handler.has_no_label parameters error compil i in
            let rule = Remanent_state.info_to_rule info in
            let rule =
              if b1 && b2 && hide_reverse_rule then
                Handler.hide rule
              else
                rule
            in
            error, rule :: list
          ))
        array []
    in
    error, dynamic, Remanent_state.set_dead_rules list kasa_state

  (**************************************************************************)

  let print_dead_rule static dynamic error =
    let parameters = get_parameter static in
    let result = get_dead_rule dynamic in
    let compiled = get_compil static in
    if Remanent_parameters.get_dump_reachability_analysis_result parameters then (
      let error, bool =
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
          (fun _parameters error _k bool bool' -> error, bool && bool')
          result true
      in
      if not bool then (
        let parameters = Remanent_parameters.update_prefix parameters "" in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "------------------------------------------------------------"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "* There are some non applyable rules"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "------------------------------------------------------------"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.iter parameters error
          (fun parameters error k bool ->
            if bool then
              error
            else (
              let error', rule_string =
                try
                  Handler.string_of_rule parameters error compiled k
                    ~with_ast:false
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_rule_id k)
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s will never be applied." rule_string
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error
            ))
          result
      ) else (
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "------------------------------------------------------------"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "every rule may be applied"
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error
      )
    ) else
      error

  let print ?dead_rules static dynamic error _loggers =
    let _ = dead_rules in
    let error = print_dead_rule static dynamic error in
    error, dynamic, ()

  let get_dead_rules _static dynamic parameters error r_id =
    match
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error r_id
        (get_dead_rule dynamic)
    with
    | error, None -> Exception.warn parameters error __POS__ Exit false
    | error, Some b -> error, not b

  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
