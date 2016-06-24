(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn
    (fun () -> default)

let local_trace = false

module type Analyzer =
sig

  type static_information
  type dynamic_information

  val main:
    Remanent_parameters_sig.parameters ->
    StoryProfiling.StoryStats.log_info ->
    Exception.method_handler ->
    Ckappa_sig.Views_bdu.handler ->
    Cckappa_sig.compil ->
    Cckappa_sig.kappa_handler ->
    Exception.method_handler * StoryProfiling.StoryStats.log_info * static_information * dynamic_information

  val export:
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Analyzer_headers.kasa_state ->
    Exception.method_handler * dynamic_information * Analyzer_headers.kasa_state

  val print:
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Loggers.t list ->
    Exception.method_handler * dynamic_information

end

(*****************************************************************************************)
(*Analyzer is a functor takes a module Domain as its parameter.*)

module Make (Domain:Composite_domain.Composite_domain) =
struct

  type static_information = Domain.static_information

  type dynamic_information = Domain.dynamic_information

  let print static dynamic error loggers =
    let error, dynamic, () = Domain.print static dynamic error loggers in
    error, dynamic

  let get_log_info dynamic = Analyzer_headers.get_log_info (Domain.get_global_dynamic_information dynamic)
  let set_log_info log_info dynamic =
    let global = Analyzer_headers.set_log_info log_info
        (Domain.get_global_dynamic_information dynamic) in
    Domain.set_global_dynamic_information global dynamic
  let lift f parameter error title opt dynamic =
    let log_info = get_log_info dynamic in
    let error, log_info = f parameter error title opt log_info in
    let dynamic = set_log_info log_info dynamic in
    error, dynamic
  let add_event = lift StoryProfiling.StoryStats.add_event
  let close_event = lift StoryProfiling.StoryStats.close_event

  let main parameter log_info error mvbdu_handler compil kappa_handler =
    let error, log_info = StoryProfiling.StoryStats.add_event parameter error StoryProfiling.Global_initialization None log_info in
    let error, static, dynamic =
      Analyzer_headers.initialize_global_information
        parameter log_info error mvbdu_handler compil kappa_handler
    in
    let error, log_info = StoryProfiling.StoryStats.close_event parameter error StoryProfiling.Global_initialization None log_info in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let error, init = Analyzer_headers.compute_initial_state error static in
    let log_info = Analyzer_headers.get_log_info dynamic in
    let error, log_info = StoryProfiling.StoryStats.add_event parameter error StoryProfiling.Domains_initialization None log_info in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let error, static, dynamic =
      Domain.initialize static dynamic error in
    let error, dynamic = close_event parameter error
        StoryProfiling.Domains_initialization None dynamic
    in
    let error, dynamic,_ =
      List.fold_left
        (fun (error, dynamic, i) chemical_species ->
           let error, dynamic = add_event  parameter error (StoryProfiling.Initial_state i) None dynamic in
           let error, dynamic, () =
             Domain.add_initial_state static dynamic error chemical_species
           in
           let error, dynamic = close_event parameter error  (StoryProfiling.Initial_state i) None dynamic in
           error, dynamic, i+1)
        (error, dynamic, 1)
        init
    in
    let log = Remanent_parameters.get_logger parameter in
    let error, static, dynamic =
      let rec aux error dynamic =
        let error, dynamic, next_opt = Domain.next_rule static dynamic error in
        match next_opt with
        | None -> error, static, dynamic
        | Some rule_id ->
          let _ =
            if local_trace
            || Remanent_parameters.get_dump_reachability_analysis_iteration parameter
            || Remanent_parameters.get_trace parameter
            then
              let error, rule_id_string =
                try
                  Handler.string_of_rule parameter error kappa_handler
                    compil rule_id
                with
                  _ -> warn parameter error (Some "line 99") Exit
                         (Ckappa_sig.string_of_rule_id rule_id)
              in
              let () = Loggers.print_newline log in
              let () = Loggers.fprintf log "\tApplying %s:" rule_id_string in
              let () = Loggers.print_newline log in
              ()
          in
          begin
            let error, dynamic, is_enabled =
              Domain.is_enabled static dynamic error rule_id
            in
            match is_enabled with
            | None ->
              let _ =
                if local_trace
                || Remanent_parameters.get_dump_reachability_analysis_iteration parameter
                || Remanent_parameters.get_trace parameter
                then
                  let () =
                    Loggers.fprintf log "\t\tthe precondition is not satisfied yet"
                  in
                  let () = Loggers.print_newline log
                  in
                  ()
              in
              aux error dynamic
            | Some precondition ->
              let _ =
                if local_trace
                || Remanent_parameters.get_dump_reachability_analysis_iteration parameter
                || Remanent_parameters.get_trace parameter
                then
                  let () = Loggers.fprintf log "\t\tthe precondition is satisfied" in
                  let () = Loggers.print_newline log in
                  ()
              in
              let error, dynamic, () =
                Domain.apply_rule static dynamic error rule_id precondition
              in
              aux error dynamic
          end
      in aux error dynamic
    in
    (*print test*)
    let _ = print static dynamic error [] in
    error, log_info, static, dynamic

  let export static dynamic error kasa_state =
    Domain.export static dynamic error kasa_state

end
