(**
   * rule_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
   * Last modification: Time-stamp: <Jul 28 2016>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : unit (* no domain-specific static information *)
    }

  (*--------------------------------------------------------------------*)
  (* this array indicates whether a rule has already be applied, or not *)

  type local_dynamic_information =
    bool Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global
  let set_global_dynamic_information gdynamic dynamic = {dynamic with global = gdynamic}
  (** dead rule local dynamic information*)
  let get_dead_rule dynamic = dynamic.local

  let set_dead_rule dead_rule dynamic =
    {
      dynamic with local = dead_rule
    }

  (*--------------------------------------------------------------------*)

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

  (**************************************************************************)
  (** [get_scan_rule_set static] *)

  let initialize static dynamic error =
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = ();
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let parameter = Analyzer_headers.get_parameter static in
    let nrules = Handler.nrules parameter error kappa_handler in
    let error, init_dead_rule_array =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.init
        parameter error (nrules-1)
        (fun _ error _ -> error, false)
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_dead_rule_array;
      }
    in
    error, init_global_static_information, init_global_dynamic_information

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
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    let parameter = get_parameter static in
    let bool_array = get_dead_rule dynamic in
    let error, bool =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameter error
        rule_id bool_array
    in
    match bool with
    | Some false | None ->
      let error, precondition =
        Communication.the_rule_is_applied_for_the_first_time
          (get_parameter static)
          error
          precondition
      in
      error, dynamic, Some precondition
    | Some true ->
      let error, precondition =
        Communication.the_rule_is_not_applied_for_the_first_time
          (get_parameter static)
          error
          precondition
      in
      error, dynamic, Some precondition

  let apply_rule static dynamic error rule_id precondition =
    (*false -> true: print the information that rule apply for the first
      time, then update array*)
    let event_list = [] in
    let dead_rule_array = get_dead_rule dynamic in
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compil = get_compil static in
    let error, rule_id_string =
      try Handler.string_of_rule parameter error kappa_handler compil rule_id
      with
        _ -> warn parameter error (Some "line 165") Exit (Ckappa_sig.string_of_rule_id rule_id)
    in
    (*print*)
    let error, dynamic =
      match
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
          parameter error
          rule_id dead_rule_array
      with
      | error, Some false ->
        let error, dead_rule_array =
          Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.set
            parameter error
            rule_id
            true
            dead_rule_array
        in
        let dynamic =
          let log = Remanent_parameters.get_logger parameter in
          if local_trace
          || Remanent_parameters.get_trace parameter
          || Remanent_parameters.get_dump_reachability_analysis_iteration parameter
          then
            let () = Loggers.print_newline log in
            let () =
              Loggers.fprintf log "\t\t%s is applied for the first time" rule_id_string
            in
            let () = Loggers.print_newline log in
            let () = Loggers.print_newline log in
              dynamic
          else
            dynamic
        in
        let dynamic = set_dead_rule dead_rule_array dynamic in
        error, dynamic
      | error, Some true -> error, dynamic
      | error, None ->
        warn parameter error (Some "line 208") Exit dynamic
    in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let stabilize _static dynamic error =
    error, dynamic, ()

  let export static dynamic error kasa_state =
    let parameter = get_parameter static in
    let array = get_dead_rule dynamic in
    let error, list =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun _parameter error i bool list ->
           error, if not bool then i::list else list
        )
        array []
    in
    error, dynamic, Remanent_state.set_dead_rules list kasa_state

  (**************************************************************************)

  let print_dead_rule static dynamic error =
    let parameter = get_parameter static in
    let result = get_dead_rule dynamic in
    let compiled = get_compil static in
    let handler = get_kappa_handler static in
    if Remanent_parameters.get_dump_reachability_analysis_result parameter
    then
      let parameter =
        Remanent_parameters.update_prefix parameter ""
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------" in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "* Dead rules :"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------" in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.iter
        parameter
        error
        (fun parameter error k bool ->
           if bool
           then
             error
           else
             let error', rule_string =
               try
                 Handler.string_of_rule parameter error handler compiled k
               with
                 _ ->
                 warn parameter error (Some "line 249") Exit (Ckappa_sig.string_of_rule_id k)
             in
             let error =
               Exception.check warn parameter error error' (Some "line 252") Exit
             in
             let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                 "%s will never be applied." rule_string
             in
             let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
             error)
        result
    else
      error

  let print static dynamic error _loggers =
    let error =
      print_dead_rule
        static
        dynamic
        error
    in
    error, dynamic, ()

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
