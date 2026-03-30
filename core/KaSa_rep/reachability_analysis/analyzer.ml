(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Jul 31 2017>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let local_trace = false

module type Analyzer = sig
  type static_information
  type dynamic_information

  val main :
    Remanent_parameters_sig.parameters ->
    StoryProfiling.StoryStats.log_info ->
    Exception.exceptions_caught_and_uncaught ->
    Ckappa_sig.Views_bdu.handler ->
    Cckappa_sig.compil ->
    Cckappa_sig.kappa_handler ->
    Exception.exceptions_caught_and_uncaught
    * StoryProfiling.StoryStats.log_info
    * (Analyzer_headers.global_static_information * static_information)
    * dynamic_information

  val update_main :
    Remanent_parameters_sig.parameters ->
    StoryProfiling.StoryStats.log_info ->
    Exception.exceptions_caught_and_uncaught ->
    Ckappa_sig.Views_bdu.handler ->
    Cckappa_sig.compil ->
    Cckappa_sig.kappa_handler ->
    Diff.new_indexs ->
    ( Analyzer_headers.global_static_information,
      static_information,
      dynamic_information )
    Remanent_state.state ->
    Exception.exceptions_caught_and_uncaught
    * StoryProfiling.StoryStats.log_info
    * (Analyzer_headers.global_static_information * static_information)
    * dynamic_information

  val export :
    Analyzer_headers.global_static_information ->
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    ('static, 'dynamic) Analyzer_headers.kasa_state ->
    Exception.exceptions_caught_and_uncaught
    * dynamic_information
    * ('static, 'dynamic) Analyzer_headers.kasa_state

  val print :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    Loggers.t ->
    Exception.exceptions_caught_and_uncaught * dynamic_information

  val maybe_reachable :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    Analyzer_headers.pattern_matching_flag ->
    Cckappa_sig.mixture ->
    Exception.exceptions_caught_and_uncaught * dynamic_information * bool

  val enable_or_disable_rule :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    Cckappa_sig.compil ->
    Exception.exceptions_caught_and_uncaught
    * dynamic_information
    * static_information

     val get_bdu_handler: dynamic_information -> Ckappa_sig.Views_bdu.handler 
end

(***************************************************************************)
(*Analyzer is a functor takes a module Domain as its parameter.*)

module Make (Domain : Composite_domain.Composite_domain) = struct
  type static_information = Domain.static_information
  type dynamic_information = Domain.dynamic_information

  let get_log_info dynamic =
    Analyzer_headers.get_log_info
      (Domain.get_global_dynamic_information dynamic)

  let set_log_info log_info dynamic =
    let global =
      Analyzer_headers.set_log_info log_info
        (Domain.get_global_dynamic_information dynamic)
    in
    Domain.set_global_dynamic_information global dynamic

  let lift f parameters error title opt dynamic =
    let log_info = get_log_info dynamic in
    let error, log_info = f parameters error title opt log_info in
    let dynamic = set_log_info log_info dynamic in
    error, dynamic

  let add_event = lift StoryProfiling.StoryStats.add_event
  let close_event = lift StoryProfiling.StoryStats.close_event

  let print static dynamic error loggers =
    let parameters = Domain.get_parameters static in
    let error, dynamic =
      add_event parameters error StoryProfiling.Print_reachability_result None
        dynamic
    in
    let error, dynamic, () = Domain.print static dynamic error loggers in
    let error, dynamic =
      close_event parameters error StoryProfiling.Print_reachability_result None
        dynamic
    in
    error, dynamic

  let main ?patch parameters log_info error mvbdu_handler compil kappa_handler =
    let domain_event, global_event, init_event, analysis_event, new_elts =
      match patch with
      | None ->
        ( StoryProfiling.Domains_initialization,
          StoryProfiling.Global_initialization,
          StoryProfiling.Initial_states,
          StoryProfiling.Reachability_analysis,
          Diff.starting_new_elt )
      | Some (_, _, a) ->
        ( StoryProfiling.Domains_initialization_update,
          StoryProfiling.Global_initialization_update,
          StoryProfiling.Initial_state_updates,
          StoryProfiling.Incremental_reachability_analysis,
          a )
    in
    let error, log_info =
      StoryProfiling.StoryStats.add_event parameters error global_event None
        log_info
    in
    let patch_global =
      match patch with
      | None -> None
      | Some (static, _, _) -> Some (fst static, new_elts)
    in
    let error, global_static, dynamic =
      Analyzer_headers.initialize_global_information ?patch:patch_global
        parameters log_info error mvbdu_handler compil kappa_handler
    in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let error, init =
      Analyzer_headers.compute_initial_state error global_static
    in
    let log_info = Analyzer_headers.get_log_info dynamic in
    let error, log_info =
      StoryProfiling.StoryStats.add_event parameters error domain_event None
        log_info
    in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let patch_domain =
      match patch with
      | None -> None
      | Some (static, dynamicd, _) ->
        Some
          ( snd static,
            Domain.set_global_dynamic_information dynamic dynamicd,
            new_elts )
    in
    let error, static, dynamic, _new_elts =
      let error, static, dynamic =
        Domain.initialize ?patch:patch_domain global_static dynamic error
      in
      error, static, dynamic, new_elts
    in
    let error, dynamic =
      close_event parameters error domain_event None dynamic
    in
    let error, dynamic =
      close_event parameters error global_event None dynamic
    in
    let error, dynamic = add_event parameters error init_event None dynamic in
    let error, dynamic, _ =
      List.fold_left
        (fun (error, dynamic, i) chemical_species ->
          let error, dynamic =
            add_event parameters error (StoryProfiling.Initial_state i) None
              dynamic
          in
          let error, dynamic, () =
            Domain.add_initial_state static dynamic error chemical_species
          in
          let error, dynamic =
            close_event parameters error (StoryProfiling.Initial_state i) None
              dynamic
          in
          error, dynamic, i + 1)
        (error, dynamic, 1) init
    in
    let error, dynamic = close_event parameters error init_event None dynamic in
    let log = Remanent_parameters.get_logger parameters in
    let error, dynamic =
      add_event parameters error analysis_event None dynamic
    in
    let error, static, dynamic =
        let rec aux error dynamic =
          let error, dynamic, next_opt =
            Domain.next_rule static dynamic error
          in
          match next_opt with
          | None -> error, static, dynamic
          | Some rule_id ->
            let error =
              if
                local_trace
                || Remanent_parameters.get_dump_reachability_analysis_iteration
                     parameters
                || Remanent_parameters.get_trace parameters
              then (
                let error, rule_id_string =
                  try Handler.string_of_rule parameters error compil rule_id
                  with _ ->
                    Exception.warn parameters error __POS__ Exit
                      (Ckappa_sig.string_of_rule_id rule_id)
                in
                let () = Loggers.print_newline log in
                let () = Loggers.fprintf log "\tApplying %s:" rule_id_string in
                let () = Loggers.print_newline log in
                error
              ) else
                error
            in
            let error, dynamic, is_enabled =
              Domain.is_enabled static dynamic error rule_id
            in
            (match is_enabled with
            | None ->
              let _ =
                if
                  local_trace
                  || Remanent_parameters
                     .get_dump_reachability_analysis_iteration parameters
                  || Remanent_parameters.get_trace parameters
                then (
                  let () =
                    Loggers.fprintf log
                      "\t\tthe precondition is not satisfied yet"
                  in
                  let () = Loggers.print_newline log in
                  ()
                )
              in
              aux error dynamic
            | Some precondition ->
              let _ =
                if
                  local_trace
                  || Remanent_parameters
                     .get_dump_reachability_analysis_iteration parameters
                  || Remanent_parameters.get_trace parameters
                then (
                  let () =
                    Loggers.fprintf log "\t\tthe precondition is satisfied"
                  in
                  let () = Loggers.print_newline log in
                  ()
                )
              in
              let error, dynamic, () =
                Domain.apply_rule static dynamic error rule_id precondition
              in
              aux error dynamic)
        in
        aux error dynamic
    in
    let error, dynamic, () = Domain.stabilize static dynamic error in
    let error, dynamic =
      close_event parameters error analysis_event None dynamic
    in
    let error, dynamic = print static dynamic error log in
    let log_info =
      Analyzer_headers.get_log_info
        (Domain.get_global_dynamic_information dynamic)
    in
    error, log_info, (global_static, static), dynamic

  let update_main parameters log_info error mvbdu_handler compil kappa_handler
      new_indexs
      (state :
        ( Analyzer_headers.global_static_information,
          static_information,
          dynamic_information )
        Remanent_state.state) =
    let patch =
      match Remanent_state.get_reachability_result state with
      | None -> None
      | Some (static, dynamic) -> Some (static, dynamic, new_indexs)
    in
    main ?patch parameters log_info error mvbdu_handler compil kappa_handler

  let main parameters log_info error mvbdu_handler compil kappa_handler =
    main parameters log_info error mvbdu_handler compil kappa_handler

  let export global static dynamic error kasa_state =
    let kasa_state =
      Remanent_state.set_internal_constraint_list [] kasa_state
    in
    let kasa_state =
      Remanent_state.set_global_static_information global kasa_state
    in
    let error, dynamic, kasa_state =
      Domain.export static dynamic error kasa_state
    in
    let kasa_state =
      match Remanent_state.get_constraint_list kasa_state with
      | None -> kasa_state
      | Some l -> Remanent_state.set_constraint_list (List.rev l) kasa_state
    in
    let kasa_state =
      match Remanent_state.get_internal_constraint_list kasa_state with
      | None -> kasa_state
      | Some l ->
        Remanent_state.set_internal_constraint_list (List.rev l) kasa_state
    in
    error, dynamic, kasa_state

  let maybe_reachable static dynamic error flag pattern =
    let error, dynamic, precondition =
      Domain.maybe_reachable static dynamic error flag pattern
    in
    match precondition with
    | None -> error, dynamic, false
    | Some _ -> error, dynamic, true

  let enable_or_disable_rule static dynamic error cc_compil =
    let parameters = Domain.get_parameters static in
    let error, dynamic =
      add_event parameters error StoryProfiling.Enable_or_disable_rule None
        dynamic
    in
    let error, dynamic, static =
      Domain.enable_or_disable_rule static dynamic error cc_compil
    in
    let error, dynamic, () = Domain.stabilize static dynamic error in
    let error, dynamic =
      close_event parameters error StoryProfiling.Enable_or_disable_rule None
        dynamic
    in
    error, dynamic, static

  let get_bdu_handler dynamic = 
    let global = Domain.get_global_dynamic_information dynamic in 
    Analyzer_headers.get_mvbdu_handler global 
  end

