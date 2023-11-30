(**
  * propagation_heuristic.ml
  *
  * Creation:                      <2016-09-05 feret>
  * Last modification: Time-stamp: <2016-02-03 20:03:11 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  *
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let debug_mode = false
let look_up_for_better_cut = Parameter.look_up_for_better_cut
let look_down_for_better_cut = Parameter.look_down_for_better_cut

module type Blackboard_with_heuristic = sig
  module B : Blackboard.Blackboard

  type update_order
  type propagation_check

  val dummy_update_order : update_order

  val forced_events :
    ( B.blackboard,
      (update_order list
      * B.PB.step_id list
      * unit Trace.Simulation_info.t option)
      list )
    B.PB.CI.Po.K.H.unary

  val forbidden_events :
    (B.PB.step_id list, update_order list) B.PB.CI.Po.K.H.unary

  val next_choice : (B.blackboard, update_order list) B.PB.CI.Po.K.H.unary

  val apply_instruction :
    ( B.blackboard,
      update_order,
      update_order list,
      propagation_check list,
      B.blackboard
      * update_order list
      * propagation_check list
      * B.assign_result )
    B.PB.CI.Po.K.H.quaternary

  val propagate :
    ( B.blackboard,
      propagation_check,
      update_order list,
      propagation_check list,
      B.blackboard
      * update_order list
      * propagation_check list
      * B.assign_result )
    B.PB.CI.Po.K.H.quaternary
end

module Propagation_heuristic : Blackboard_with_heuristic = struct
  module B : Blackboard.Blackboard = Blackboard.Blackboard

  let warn parameter error pos ?(message = "") exn default =
    Exception.warn
      (B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error pos ~message exn default

  type update_order =
    | Keep_event of B.PB.step_id
    | Discard_event of B.PB.step_id
    | Cut_event of B.PB.step_id
    | Refine_value_after of B.event_case_address * B.PB.predicate_value
    | Refine_value_before of B.event_case_address * B.PB.predicate_value
    | Skip

  let dummy_update_order = Skip

  type propagation_check =
    | Propagate_up of B.event_case_address
    | Propagate_down of B.event_case_address

  let _print_output log x =
    if B.is_failed x then
      Loggers.fprintf log "FAILED"
    else if B.is_ignored x then
      Loggers.fprintf log "IGNORED"
    else
      Loggers.fprintf log "SUCCESS"

  let forced_events _parameter _handler log_info error blackboard =
    let list = B.forced_events blackboard in
    ( error,
      log_info,
      List.rev_map
        (fun (l, info) ->
          List.rev_map (fun x -> Keep_event x) (List.rev l), l, info)
        (List.rev list) )

  let forbidden_events _paramter _handler log_info error list =
    error, log_info, List.rev_map (fun x -> Cut_event x) (List.rev list)

  let get_gen_unresolved_event_on_pid first last succ stop parameter handler
      log_info error blackboard p_id level =
    let k_init = first blackboard p_id in
    let k_end = last blackboard p_id in
    match k_init, k_end with
    | None, _ | _, None -> error, log_info, None
    | Some i, Some j ->
      let rec aux i log_info error =
        if stop i j then
          error, log_info, None
        else (
          let event_case_address =
            B.build_event_case_address p_id (B.build_pointer i)
          in
          let error, log_info, exist =
            B.exist_case parameter handler log_info error blackboard
              event_case_address
          in
          match exist with
          | None ->
            let error, log_info, (_seid, eid, _test, _action) =
              B.get_static parameter handler log_info error blackboard
                event_case_address
            in
            let error, log_info, level_of_event =
              B.level_of_event parameter handler log_info error blackboard eid
            in
            if level_of_event = level then
              error, log_info, Some eid
            else
              aux (succ i) log_info error
          | Some true | Some false -> aux (succ i) log_info error
        )
      in
      aux i log_info error

  let get_last_unresolved_event_on_pid parameter handler log_info error
      blackboard p_id level =
    get_gen_unresolved_event_on_pid B.get_last_linked_event
      B.get_first_linked_event B.PB.dec_step_short_id
      (fun i j -> i < j)
      parameter handler log_info error blackboard p_id level

  let get_first_unresolved_event_on_pid parameter handler log_info error
      blackboard p_id level =
    get_gen_unresolved_event_on_pid B.get_first_linked_event
      B.get_last_linked_event B.PB.inc_step_short_id
      (fun i j -> i > j)
      parameter handler log_info error blackboard p_id level

  let get_gen_unresolved_event first last succ stop parameter handler log_info
      error blackboard level =
    let rec aux i log_info error =
      if stop i last then
        error, log_info, None
      else (
        let error, log_info, exist =
          B.is_selected_event parameter handler log_info error i blackboard
        in
        match exist with
        | None ->
          let error, log_info, level_of_event =
            B.level_of_event parameter handler log_info error blackboard i
          in
          if level_of_event = level then
            error, log_info, Some i
          else
            aux (succ i) log_info error
        | Some true | Some false -> aux (succ i) log_info error
      )
    in
    aux first log_info error

  let get_last_unresolved_event parameter handler log_info error blackboard
      level =
    get_gen_unresolved_event
      (B.PB.step_id_of_int (B.get_n_eid blackboard))
      B.PB.zero_step_id B.PB.dec_step_id
      (fun i j -> i < j)
      parameter handler log_info error blackboard level

  let get_first_unresolved_event parameter handler log_info error blackboard
      level =
    get_gen_unresolved_event B.PB.zero_step_id
      (B.PB.step_id_of_int (B.get_n_eid blackboard))
      B.PB.inc_step_id
      (fun i j -> i > j)
      parameter handler log_info error blackboard level

  let next_choice parameter handler log_info error blackboard =
    let bool, string =
      match parameter.B.PB.CI.Po.K.H.current_compression_mode with
      | None | Some Story_json.Causal -> false, ""
      | Some Story_json.Weak ->
        ( Parameter.dump_grid_after_branching_during_weak_compression,
          Parameter.xlsweakFileName )
      | Some Story_json.Strong ->
        ( Parameter.dump_grid_after_branching_during_strong_compression,
          Parameter.xlsstrongFileName )
    in
    let () = Priority.n_branch := !Priority.n_branch + 1 in
    let error, log_info =
      if bool then (
        let error, log_info, () =
          B.export_blackboard_to_xls parameter handler log_info error string
            !Priority.n_story !Priority.n_branch blackboard
        in
        error, log_info
      ) else
        error, log_info
    in
    let error, priority =
      match B.PB.CI.Po.K.H.get_priorities parameter with
      | Some x -> error, x
      | None ->
        warn parameter error __POS__
          ~message:"Compression mode has to been selected"
          (Failure "Compression mode has not been selected") Priority.causal
    in
    let n_p_id = B.get_npredicate_id blackboard in
    let error, () =
      match priority.Priority.candidate_set_of_events with
      | Priority.All_remaining_events ->
        warn parameter error __POS__
          ~message:"All_remaining_events strategy is not implemented yet"
          (Failure "All remaining events strategy is not implemented yet") ()
      | Priority.Wire_with_the_least_number_of_events -> error, ()
      | Priority.Wire_with_the_most_number_of_events -> error, ()
    in
    let best_pair x a b =
      match x.Priority.candidate_set_of_events with
      | Priority.All_remaining_events
      | Priority.Wire_with_the_most_number_of_events ->
        Tools.max_pos_int_not_zero a b
      | Priority.Wire_with_the_least_number_of_events ->
        Tools.min_pos_int_not_zero a b
    in
    let get_unresolved_event x parameter handler log_info error blackboard p_id
        level =
      match x.Priority.try_to_remove_first with
      | Priority.Late_events ->
        (match x.Priority.candidate_set_of_events with
        | Priority.All_remaining_events ->
          get_last_unresolved_event parameter handler log_info error blackboard
            level
        | Priority.Wire_with_the_most_number_of_events
        | Priority.Wire_with_the_least_number_of_events ->
          get_last_unresolved_event_on_pid parameter handler log_info error
            blackboard p_id level)
      | Priority.Early_events ->
        (match x.Priority.candidate_set_of_events with
        | Priority.All_remaining_events ->
          get_first_unresolved_event parameter handler log_info error blackboard
            level
        | Priority.Wire_with_the_most_number_of_events
        | Priority.Wire_with_the_least_number_of_events ->
          get_first_unresolved_event_on_pid parameter handler log_info error
            blackboard p_id level)
    in
    let error, list =
      if n_p_id = 0 then
        error, []
      else (
        let rec try_level level_opt error =
          match level_opt with
          | None -> error, []
          | Some level ->
            let rec aux level n_p_id step best =
              if step = n_p_id then
                best
              else (
                let grade =
                  B.get_n_unresolved_events_of_pid_by_level blackboard step
                    level
                in
                aux level n_p_id (step + 1)
                  (best_pair priority best (grade, step))
              )
            in
            let n, p_id =
              aux level n_p_id 1
                (B.get_n_unresolved_events_of_pid_by_level blackboard 0 level, 0)
            in
            if n = 0 then
              try_level (Priority.lower level) error
            else (
              let error, _log_info, event_id =
                get_unresolved_event priority parameter handler log_info error
                  blackboard p_id level
              in
              match event_id with
              | None ->
                let log = B.PB.CI.Po.K.H.get_debugging_channel parameter in

                let error, () =
                  warn parameter error __POS__
                    ~message:
                      ("An empty wire has been selected" ^ string_of_int n)
                    (Failure "An empty wire has been selected") ()
                in
                let () =
                  Loggers.fprintf log "ERROR 249: %s\n"
                    (Priority.string_of_level level)
                in
                try_level (Priority.lower level) error
              | Some event_id ->
                error, [ Discard_event event_id; Keep_event event_id ]
            )
        in
        try_level (Some Priority.highest) error
      )
    in
    error, log_info, list

  let print_event_case_address parameter handler log_info error blackboard case
      =
    let error, log_info, (_, eid, _, _) =
      B.get_static parameter handler log_info error blackboard case
    in
    let () =
      Loggers.fprintf
        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
        "Event: %i, Predicate: %i" (B.PB.int_of_step_id eid)
        (B.predicate_id_of_case_address case)
    in
    let () =
      Loggers.print_newline (B.PB.CI.Po.K.H.get_debugging_channel parameter)
    in
    error, log_info, ()

  let propagate_down parameter handler log_info error blackboard
      event_case_address instruction_list propagate_list =
    let error, log_info, bool =
      B.exist_case parameter handler log_info error blackboard
        event_case_address
    in
    match bool with
    | Some false ->
      (* the case has been removed from the blackboard, nothing to be done *)
      error, log_info, (blackboard, instruction_list, propagate_list, B.success)
    | Some true | None ->
      (* we know that the pair (test/action) can been executed *)
      let case_address =
        B.case_address_of_case_event_address event_case_address
      in
      let error, log_info, case_value =
        B.get parameter handler log_info error case_address blackboard
      in
      let error, log_info, predicate_value =
        B.predicate_value_of_case_value parameter handler log_info error
          case_value
      in
      let error, log_info, next_event_case_address =
        B.follow_pointer_down parameter handler log_info error blackboard
          event_case_address
      in
      let error, log_info, bool2 =
        B.exist_case parameter handler log_info error blackboard
          next_event_case_address
      in
      (match bool2 with
      | Some false ->
        (* The blackboard is inconsistent: *)
        (* Pointers should not point to removed events.*)
        let error, () =
          warn parameter error __POS__
            ~message:"inconsistent pointers in blackboard"
            (Failure "inconsistent pointers in blackboard") ()
        in
        ( error,
          log_info,
          (blackboard, instruction_list, propagate_list, B.success) )
      | Some true ->
        (* next event is selected *)
        let error, log_info, (_next_seid, _next_eid, next_test, next_action) =
          B.get_static parameter handler log_info error blackboard
            next_event_case_address
        in
        let case_address =
          B.case_address_of_case_event_address event_case_address
        in
        let error, log_info, case_value =
          B.get parameter handler log_info error case_address blackboard
        in
        let error, log_info, predicate_value =
          B.predicate_value_of_case_value parameter handler log_info error
            case_value
        in
        (match B.PB.is_unknown next_test, B.PB.is_unknown next_action with
        | true, true ->
          (* no test, no action in next event *)
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_down (case 1):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "next event is kept but has no test and no action"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Value is propagated after the next event"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          (* next event is selected *)
          (* no test, no action in next event *)
          (* we propagate the value after the next event*)
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_down 1 log_info
          in
          ( error,
            log_info,
            ( blackboard,
              Refine_value_after (next_event_case_address, predicate_value)
              :: instruction_list,
              propagate_list,
              B.success ) )
        | true, false ->
          (* no test, but an action in next event *)
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_down  (case 2):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "next event is kept, no test, but an action"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Nothing to be done"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          (* next event is selected *)
          (* no test, but an action in next event *)
          (* nothing to propagate downward*)
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_down 2 log_info
          in
          ( error,
            log_info,
            (blackboard, instruction_list, propagate_list, B.success) )
        | false, true ->
          (* no action, but a test in next event *)
          if B.PB.compatible predicate_value next_test then (
            (* the test is compatible with the value *)
            let error, log_info, conj =
              B.PB.conj parameter handler log_info error next_test
                predicate_value
            in
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 3):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event is kept, a test but no action "
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Next event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_test
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate new predicate_value "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    conj
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    " before and after next event"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            (* next event is selected *)
            (* no action, but a test in next event *)
            (* the test is compatible with the value *)
            (* we propagate the meet of the test and the value before and after the next event *)
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 3 log_info
            in
            ( error,
              log_info,
              ( blackboard,
                Refine_value_before (next_event_case_address, conj)
                :: Refine_value_after (next_event_case_address, conj)
                :: instruction_list,
                propagate_list,
                B.success ) )
          ) else (
            (* the test and the value are incompatible *)
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 4):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event is kept, a test but no action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Next event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_test
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Cut"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 4 log_info
            in
            (* next event is selected *)
            (* no action, but a test in next event *)
            (* the test is not compatible with the value *)
            (* we cut the exploration *)
            error, log_info, (blackboard, [], [], B.fail)
          )
        | false, false ->
          (*there is a test and an action in the next event *)
          if B.PB.compatible predicate_value next_test then (
            (* the test and the value are compatible *)
            let error, log_info, conj =
              B.PB.conj parameter handler log_info error next_test
                predicate_value
            in
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 5):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event is kept, a test but no action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_test
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event Action:"
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_action
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate new predicate_value "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    conj
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    " before the next event"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 5 log_info
            in
            (* next event is selected *)
            (* an action and a test in next event *)
            (* the test is compatible with the value *)
            (* we propagate the meet of the test and the value before the next event *)
            ( error,
              log_info,
              ( blackboard,
                Refine_value_before (next_event_case_address, conj)
                :: instruction_list,
                propagate_list,
                B.success ) )
          ) else (
            (* test and value are incompatible *)
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 6):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event is kept, a test, an action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Next event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_test
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Next event Action: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_action
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Cut"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 6 log_info
            in
            (* next event is selected *)
            (* an action and a test in next event *)
            (* the test is not compatible with the value *)
            (* we cut the exploration *)
            error, log_info, (blackboard, [], [], B.fail)
          ))
      | None ->
        (* we do not know whether the event is played or not *)
        let error, log_info, (_next_seid, next_eid, next_test, next_action) =
          B.get_static parameter handler log_info error blackboard
            next_event_case_address
        in
        (match B.PB.is_unknown next_action with
        | true ->
          (* there is no action in the next event *)
          (match B.PB.is_unknown next_test with
          | true ->
            (*there is no test in the next event *)
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 7):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "we do not know if the next event is kept"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "there is no test, no action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "The value is propagated after and before the next event"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***\n"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 7 log_info
            in
            (* we do not know whether the event is played or not *)
            (*there is no test in the next event *)
            (* there is no action in the next event *)
            (* we propagate the value after the next event*)
            ( error,
              log_info,
              ( blackboard,
                Refine_value_after (next_event_case_address, predicate_value)
                :: instruction_list,
                propagate_list,
                B.success ) )
          | false ->
            (* there is a test in the next event *)
            if B.PB.compatible next_test predicate_value then (
              (* test and predicate_value are compatible *)
              let error, log_info =
                if debug_mode then (
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Propagate_down  (case 8):"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let error, log_info, () =
                    print_event_case_address parameter handler log_info error
                      blackboard event_case_address
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "we do not know if the next event is kept"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "there is a test, but no action"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "next event Test: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      next_test
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Wire_state: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      predicate_value
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "The value "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      predicate_value
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      " is propagated after and before the next event"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "***"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  error, log_info
                ) else
                  error, log_info
              in
              let log_info =
                StoryProfiling.StoryStats.add_propagation_case_down 8 log_info
              in
              (* we do not know whether the event is played or not *)
              (* there is a test in the next event *)
              (* there is no action in the next event *)
              (* the test is compatible with the value *)
              (* we propagate the value after the next event*)
              ( error,
                log_info,
                ( blackboard,
                  Refine_value_after (next_event_case_address, predicate_value)
                  :: instruction_list,
                  propagate_list,
                  B.success ) )
            ) else (
              let error, log_info =
                if debug_mode then (
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Propagate_down  (case 9):"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let error, log_info, () =
                    print_event_case_address parameter handler log_info error
                      blackboard event_case_address
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "we do not know if the next event is kept"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "there is a test, but no action"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "next event Test: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      next_test
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Wire_state: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      predicate_value
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "We discard the next event (%i)"
                      (B.PB.int_of_step_id next_eid)
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "***"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  error, log_info
                ) else
                  error, log_info
              in
              let log_info =
                StoryProfiling.StoryStats.add_propagation_case_down 9 log_info
              in
              (* we do not know whether the event is played or not *)
              (* there is a test in the next event *)
              (* there is no action in the next event *)
              (* the test is not compatible with the value *)
              (* we discard the next event *)
              ( error,
                log_info,
                ( blackboard,
                  Discard_event next_eid :: instruction_list,
                  propagate_list,
                  B.success ) )
            ))
        | false ->
          (* there is an action in the next event *)
          if not (B.PB.compatible next_action predicate_value) then (
            (* the action is not compatible with the value *)
            let error, log_info, computed_next_predicate_value =
              B.PB.disjunction parameter handler log_info error predicate_value
                next_action
            in
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_down  (case 10):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "we do not know if the next event is kept"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "there is an action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "next event Action: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    next_action
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "The value "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    computed_next_predicate_value
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    " is propagated after the next event"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_down 10 log_info
            in
            (* we do not know whether the event is played or not *)
            (* there is an action in the next event *)
            (* the action is compatible with the value *)
            (* we propagate the join of the value and the action after the next event*)
            ( error,
              log_info,
              ( blackboard,
                Refine_value_after
                  (next_event_case_address, computed_next_predicate_value)
                :: instruction_list,
                propagate_list,
                B.success ) )
          ) else (
            (*the action is compatible with the value *)
            let error, log_info, computed_next_predicate_value =
              B.PB.disjunction parameter handler log_info error predicate_value
                next_action
            in
            match B.PB.is_unknown next_test with
            | true ->
              (* there is no test in the next event *)
              let error, log_info =
                if debug_mode then (
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Propagate_down  (case 11):"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let error, log_info, () =
                    print_event_case_address parameter handler log_info error
                      blackboard event_case_address
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "we do not know if the next event is kept"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "there is no test, but there is an action"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "next event Action: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      next_action
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Wire_state: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      predicate_value
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "The value "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      computed_next_predicate_value
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      " is propagated after the next event"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "***"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  error, log_info
                ) else
                  error, log_info
              in
              let log_info =
                StoryProfiling.StoryStats.add_propagation_case_down 11 log_info
              in
              (* we do not know whether the event is played or not *)
              (* there is no test in the next event *)
              (* there is an action in the next event *)
              (* the action is compatible with the value *)
              (* we propagate the join of the value and the action after the next event*)
              ( error,
                log_info,
                ( blackboard,
                  Refine_value_after
                    (next_event_case_address, computed_next_predicate_value)
                  :: instruction_list,
                  propagate_list,
                  B.success ) )
            | false ->
              if B.PB.compatible next_test predicate_value then (
                let error, log_info =
                  if debug_mode then (
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Propagate_down  (case 12):"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let error, log_info, () =
                      print_event_case_address parameter handler log_info error
                        blackboard event_case_address
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "we do not know if the next event is kept"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "there is a test, but there is an action"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "next event Test: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        next_test
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "next event Action: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        next_action
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Wire_state: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        predicate_value
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "The value "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        computed_next_predicate_value
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        " is propagated after the next event"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "***"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    error, log_info
                  ) else
                    error, log_info
                in
                let log_info =
                  StoryProfiling.StoryStats.add_propagation_case_down 12
                    log_info
                in
                (* we do not know whether the event is played or not *)
                (* there is a test in the next event *)
                (* there is an action in the next event *)
                (* the test is compatible with the value *)
                (* the action is compatible with the value *)
                (* we propagate the join of the value and the action after the next event*)
                ( error,
                  log_info,
                  ( blackboard,
                    Refine_value_after
                      (next_event_case_address, computed_next_predicate_value)
                    :: instruction_list,
                    propagate_list,
                    B.success ) )
              ) else (
                let error, log_info =
                  if debug_mode then (
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Propagate_down  (case 13):"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let error, log_info, () =
                      print_event_case_address parameter handler log_info error
                        blackboard event_case_address
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "we do not know if the next event is kept"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "there is a test, but there is an action"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "next event Test: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        next_test
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "next event Action: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        next_action
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Wire_state: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        predicate_value
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Next event (%i) is discarded"
                        (B.PB.int_of_step_id next_eid)
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "***"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    error, log_info
                  ) else
                    error, log_info
                in
                let log_info =
                  StoryProfiling.StoryStats.add_propagation_case_down 13
                    log_info
                in
                (* we do not know whether the event is played or not *)
                (* there is a test in the next event *)
                (* there is an action in the next event *)
                (* the test is not compatible with the value *)
                (* we discard the next event *)
                ( error,
                  log_info,
                  ( blackboard,
                    Discard_event next_eid :: instruction_list,
                    propagate_list,
                    B.success ) )
              )
          )))

  let rec last_chance_up parameter handler log_info error blackboard
      predicate_value event_case_address =
    let error, log_info, bool =
      B.exist_case parameter handler log_info error blackboard
        event_case_address
    in
    match bool with
    | Some false -> error, false, log_info, blackboard
    | Some true ->
      let error, log_info, (_seid, _eid, _test, action) =
        B.get_static parameter handler log_info error blackboard
          event_case_address
      in
      if B.PB.is_unknown action then (
        let error, log_info, preview_event_case_address =
          B.follow_pointer_up parameter handler log_info error blackboard
            event_case_address
        in
        let preview_case_address =
          B.case_address_of_case_event_address preview_event_case_address
        in
        let error, log_info, preview_case_value =
          B.get parameter handler log_info error preview_case_address blackboard
        in
        let error, log_info, preview_predicate_value =
          B.predicate_value_of_case_value parameter handler log_info error
            preview_case_value
        in
        if B.PB.compatible preview_predicate_value predicate_value then (
          let error, log_info, bool =
            B.is_boundary parameter handler log_info error blackboard
              event_case_address
          in
          if bool then (
            let log_info =
              StoryProfiling.StoryStats.add_look_up_case 1 log_info
            in
            error, not (B.PB.is_undefined predicate_value), log_info, blackboard
          ) else
            last_chance_up parameter handler log_info error blackboard
              predicate_value preview_event_case_address
        ) else (
          let log_info =
            StoryProfiling.StoryStats.add_look_up_case 2 log_info
          in
          error, true, log_info, blackboard
        )
      ) else if B.PB.more_refined action predicate_value then
        error, false, log_info, blackboard
      else (
        let log_info = StoryProfiling.StoryStats.add_look_up_case 3 log_info in
        error, true, log_info, blackboard
      )
    | None ->
      let error, log_info, (_seid, _eid, _test, action) =
        B.get_static parameter handler log_info error blackboard
          event_case_address
      in
      if B.PB.more_refined action predicate_value then
        error, false, log_info, blackboard
      else (
        let error, log_info, preview_event_case_address =
          B.follow_pointer_up parameter handler log_info error blackboard
            event_case_address
        in
        let preview_case_address =
          B.case_address_of_case_event_address preview_event_case_address
        in
        let error, log_info, preview_case_value =
          B.get parameter handler log_info error preview_case_address blackboard
        in
        let error, log_info, preview_predicate_value =
          B.predicate_value_of_case_value parameter handler log_info error
            preview_case_value
        in
        if B.PB.compatible preview_predicate_value predicate_value then
          last_chance_up parameter handler log_info error blackboard
            predicate_value preview_event_case_address
        else (
          let log_info =
            StoryProfiling.StoryStats.add_look_up_case 4 log_info
          in
          error, true, log_info, blackboard
        )
      )

  let last_chance_up parameter handler log_info error blackboard predicate_value
      address =
    if B.PB.is_unknown predicate_value then
      error, false, log_info, blackboard
    else
      last_chance_up parameter handler log_info error blackboard predicate_value
        address

  let last_chance_up =
    if look_up_for_better_cut then
      last_chance_up
    else
      fun _ _ log_info error blackboard _ _ ->
    error, false, log_info, blackboard

  let propagate_up parameter handler log_info error blackboard
      event_case_address instruction_list propagate_list =
    let error, log_info, bool =
      B.exist_case parameter handler log_info error blackboard
        event_case_address
    in
    match bool with
    | Some false ->
      (* the case has been removed from the blackboard, nothing to be done *)
      error, log_info, (blackboard, instruction_list, propagate_list, B.success)
    | Some true ->
      (* we know that the pair (test/action) has been executed *)
      let error, log_info, (_seid, _eid, test, action) =
        B.get_static parameter handler log_info error blackboard
          event_case_address
      in
      let case_address =
        B.case_address_of_case_event_address event_case_address
      in
      let error, log_info, case_value =
        B.get parameter handler log_info error case_address blackboard
      in
      let error, log_info, predicate_value =
        B.predicate_value_of_case_value parameter handler log_info error
          case_value
      in
      if B.PB.is_unknown action then
        (* no action, we keep on propagating with the conjonction of the test of the value *)
        if B.PB.compatible test predicate_value then (
          let error, log_info, new_value =
            B.PB.conj parameter handler log_info error test predicate_value
          in
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 1):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "The event before is kept, there is no action"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Refine before the event (before) with the state "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  new_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 1 log_info
          in
          ( error,
            log_info,
            ( blackboard,
              Refine_value_before (event_case_address, new_value)
              :: instruction_list,
              propagate_list,
              B.success ) )
        ) else (
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 2):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "The event before is kept, there is no action"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Cut"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 2 log_info
          in
          error, log_info, (blackboard, [], [], B.fail)
        )
      else if B.PB.more_refined action predicate_value then
        if B.PB.is_undefined test then (
          (*the wire has just be created, nothing to be done *)
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 3):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "The event before is kept, there is an action and a test"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Nothing to be done"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 3 log_info
          in
          ( error,
            log_info,
            (blackboard, instruction_list, propagate_list, B.success) )
        ) else if
            (*we know that the wire was defined before*)
            B.PB.compatible test B.PB.defined
          then (
          let error, log_info, state =
            B.PB.conj parameter handler log_info error test B.PB.defined
          in
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 4):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "The event before is kept, there is an action and a test"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Refine before the event (before) with the state "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  state
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  ""
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 4 log_info
          in
          ( error,
            log_info,
            ( blackboard,
              Refine_value_before (event_case_address, state)
              :: instruction_list,
              propagate_list,
              B.success ) )
        ) else (
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 5):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "The event before is kept, there is an action and a test"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Cut"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 5 log_info
          in
          error, log_info, (blackboard, [], [], B.fail)
        )
      else (
        (*The event has to be discarded which is absurd *)
        let error, log_info =
          if debug_mode then (
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "Propagate_up  (case 6):"
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let error, log_info, () =
              print_event_case_address parameter handler log_info error
                blackboard event_case_address
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "The event before is kept, there is an action"
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "before event Action: "
            in
            let () =
              B.PB.print_predicate_value
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                action
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "Wire_state: "
            in
            let () =
              B.PB.print_predicate_value
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                predicate_value
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "Cut"
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            let () =
              Loggers.fprintf
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                "***"
            in
            let () =
              Loggers.print_newline
                (B.PB.CI.Po.K.H.get_debugging_channel parameter)
            in
            error, log_info
          ) else
            error, log_info
        in
        let log_info =
          StoryProfiling.StoryStats.add_propagation_case_up 6 log_info
        in
        error, log_info, (blackboard, [], [], B.fail)
      )
    | None ->
      (* we do not know whether the pair (test/action) has been executed *)
      let error, log_info, (_seid, eid, test, action) =
        B.get_static parameter handler log_info error blackboard
          event_case_address
      in
      let case_address =
        B.case_address_of_case_event_address event_case_address
      in
      let error, log_info, case_value =
        B.get parameter handler log_info error case_address blackboard
      in
      let error, log_info, predicate_value =
        B.predicate_value_of_case_value parameter handler log_info error
          case_value
      in
      (match B.PB.is_unknown action with
      | true ->
        (match B.PB.is_unknown test with
        | true ->
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 7):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "we do not know if the event before is kept,"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "there is neither a test, nor  action "
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Refine before the event (before) with the state "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 7 log_info
          in
          ( error,
            log_info,
            ( blackboard,
              Refine_value_before (event_case_address, predicate_value)
              :: instruction_list,
              propagate_list,
              B.success ) )
        | false ->
          if B.PB.compatible test predicate_value then (
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_up  (case 8):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "we do not know if the event is kept,"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "there is a  test, but no action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "before event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    test
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Refine before the event (before) with the state "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_up 8 log_info
            in
            ( error,
              log_info,
              ( blackboard,
                Refine_value_before (event_case_address, predicate_value)
                :: instruction_list,
                propagate_list,
                B.success ) )
          ) else (
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_up  (case 9):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "we do not know if the event before is kept,"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "there is a  test, but no action"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "before event Test: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    test
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Event before (%i) is discarded" (B.PB.int_of_step_id eid)
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_up 9 log_info
            in
            ( error,
              log_info,
              ( blackboard,
                Discard_event eid :: instruction_list,
                propagate_list,
                B.success ) )
          ))
      | false ->
        let error, log_info, preview_event_case_address =
          B.follow_pointer_up parameter handler log_info error blackboard
            event_case_address
        in
        let preview_case_address =
          B.case_address_of_case_event_address preview_event_case_address
        in
        let error, log_info, preview_case_value =
          B.get parameter handler log_info error preview_case_address blackboard
        in
        let error, log_info, preview_predicate_value =
          B.predicate_value_of_case_value parameter handler log_info error
            preview_case_value
        in
        if B.PB.compatible preview_predicate_value predicate_value then
          if B.PB.more_refined action predicate_value then (
            let error, bool, log_info, blackboard =
              last_chance_up parameter handler log_info error blackboard
                predicate_value preview_event_case_address
            in
            if bool then (
              let error, log_info =
                if debug_mode then (
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Propagate_up  (case 10):"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let error, log_info, () =
                    print_event_case_address parameter handler log_info error
                      blackboard event_case_address
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "we do not know if the event before is kept,"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      " there is an action"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "before event Action: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      action
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "Wire_state: "
                  in
                  let () =
                    B.PB.print_predicate_value
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      predicate_value
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "This is the only opportunity to set up the wire, we \
                       keep the event"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  let () =
                    Loggers.fprintf
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      "***"
                  in
                  let () =
                    Loggers.print_newline
                      (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  in
                  error, log_info
                ) else
                  error, log_info
              in
              let log_info =
                StoryProfiling.StoryStats.add_propagation_case_up 10 log_info
              in
              ( error,
                log_info,
                ( blackboard,
                  Keep_event eid :: instruction_list,
                  propagate_list,
                  B.success ) )
            ) else (
              match B.PB.is_unknown test with
              | true ->
                let error, log_info, new_predicate_value =
                  B.PB.disjunction parameter handler log_info error test
                    predicate_value
                in
                let error, log_info =
                  if debug_mode then (
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Propagate_up  (case 11):"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let error, log_info, () =
                      print_event_case_address parameter handler log_info error
                        blackboard event_case_address
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "we do not know if the event before is kept,"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "there is an action, but no test"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "before event Action: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        action
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Wire_state: "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        predicate_value
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "Refine before the event (before) with the state "
                    in
                    let () =
                      B.PB.print_predicate_value
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        preview_predicate_value
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    let () =
                      Loggers.fprintf
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        "***"
                    in
                    let () =
                      Loggers.print_newline
                        (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    in
                    error, log_info
                  ) else
                    error, log_info
                in
                let log_info =
                  StoryProfiling.StoryStats.add_propagation_case_up 11 log_info
                in
                ( error,
                  log_info,
                  ( blackboard,
                    Refine_value_before (event_case_address, new_predicate_value)
                    :: instruction_list,
                    propagate_list,
                    B.success ) )
              | false ->
                if B.PB.compatible test predicate_value then
                  if B.PB.compatible test preview_predicate_value then (
                    let error, log_info, new_test =
                      B.PB.conj parameter handler log_info error test
                        preview_predicate_value
                    in
                    let error, log_info, new_predicate_value =
                      B.PB.disjunction parameter handler log_info error new_test
                        predicate_value
                    in
                    let error, log_info =
                      if debug_mode then (
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Propagate_up  (case 12):"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let error, log_info, () =
                          print_event_case_address parameter handler log_info
                            error blackboard event_case_address
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "we do not know if the event before is kept,"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            " there is an action and a test"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "before event Test:"
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            test
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "before event Action: "
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            action
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Wire_state: "
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            predicate_value
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Refine before the event (before) with the state "
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            new_predicate_value
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "***"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        error, log_info
                      ) else
                        error, log_info
                    in
                    let log_info =
                      StoryProfiling.StoryStats.add_propagation_case_up 12
                        log_info
                    in
                    ( error,
                      log_info,
                      ( blackboard,
                        Refine_value_before
                          (event_case_address, new_predicate_value)
                        :: instruction_list,
                        propagate_list,
                        B.success ) )
                  ) else (
                    let error, log_info =
                      if debug_mode then (
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Propagate_up  (case 13):"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let error, log_info, () =
                          print_event_case_address parameter handler log_info
                            error blackboard event_case_address
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "we do not know if the event before is kept,"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            " there is an action and a test"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "before event Test:"
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            test
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "before event Action: "
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            action
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Wire_state: "
                        in
                        let () =
                          B.PB.print_predicate_value
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            predicate_value
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "Discard the event before (%i)"
                            (B.PB.int_of_step_id eid)
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        let () =
                          Loggers.fprintf
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                            "***"
                        in
                        let () =
                          Loggers.print_newline
                            (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                        in
                        error, log_info
                      ) else
                        error, log_info
                    in
                    let log_info =
                      StoryProfiling.StoryStats.add_propagation_case_up 13
                        log_info
                    in
                    ( error,
                      log_info,
                      ( blackboard,
                        Discard_event eid :: instruction_list,
                        propagate_list,
                        B.success ) )
                  )
                else (
                  let error, log_info, prev' =
                    B.PB.disjunction parameter handler log_info error
                      predicate_value test
                  in
                  let error, log_info =
                    if debug_mode then (
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "Propagate_up  (case 14):"
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let error, log_info, () =
                        print_event_case_address parameter handler log_info
                          error blackboard event_case_address
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "we do not know if the event before is kept,"
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "there is an action and a test"
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "before event Test:"
                      in
                      let () =
                        B.PB.print_predicate_value
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          test
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "before event Action: "
                      in
                      let () =
                        B.PB.print_predicate_value
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          action
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "Wire_state: "
                      in
                      let () =
                        B.PB.print_predicate_value
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          predicate_value
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "Refine before the event (before) with the state "
                      in
                      let () =
                        B.PB.print_predicate_value
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          prev'
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      let () =
                        Loggers.fprintf
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                          "***"
                      in
                      let () =
                        Loggers.print_newline
                          (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                      in
                      error, log_info
                    ) else
                      error, log_info
                  in
                  let log_info =
                    StoryProfiling.StoryStats.add_propagation_case_up 14
                      log_info
                  in
                  ( error,
                    log_info,
                    ( blackboard,
                      Refine_value_before (event_case_address, prev')
                      :: instruction_list,
                      propagate_list,
                      B.success ) )
                )
            )
          ) else (
            let error, log_info =
              if debug_mode then (
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Propagate_up  (case 15):"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let error, log_info, () =
                  print_event_case_address parameter handler log_info error
                    blackboard event_case_address
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "we do not know if the event before is kept,"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    " there is an action and maybe a test"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "before event Action: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    action
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Wire_state: "
                in
                let () =
                  B.PB.print_predicate_value
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    predicate_value
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "Discard the event before (%i)" (B.PB.int_of_step_id eid)
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                let () =
                  Loggers.fprintf
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                    "***"
                in
                let () =
                  Loggers.print_newline
                    (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                in
                error, log_info
              ) else
                error, log_info
            in
            let log_info =
              StoryProfiling.StoryStats.add_propagation_case_up 15 log_info
            in
            ( error,
              log_info,
              ( blackboard,
                Discard_event eid :: instruction_list,
                propagate_list,
                B.success ) )
          )
        else if B.PB.more_refined action predicate_value then (
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 16):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "we do not know if the event before is kept,"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  " there is an action and a test"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test:"
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Previous wire state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  preview_predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Select the event before (%i)" (B.PB.int_of_step_id eid)
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 16 log_info
          in
          ( error,
            log_info,
            ( blackboard,
              Keep_event eid :: instruction_list,
              propagate_list,
              B.success ) )
        ) else (
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Propagate_up  (case 17):"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let error, log_info, () =
                print_event_case_address parameter handler log_info error
                  blackboard event_case_address
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "we do not know if the event before is kept,"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  " there is an action and a test"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Test:"
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  test
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "before event Action: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  action
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Wire_state: "
              in
              let () =
                B.PB.print_predicate_value
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  predicate_value
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "Cut"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              let () =
                Loggers.fprintf
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
                  "***"
              in
              let () =
                Loggers.print_newline
                  (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              in
              error, log_info
            ) else
              error, log_info
          in
          let log_info =
            StoryProfiling.StoryStats.add_propagation_case_up 17 log_info
          in
          error, log_info, (blackboard, [], [], B.fail)
        ))

  let propagate parameter handler log_info error blackboard check
      instruction_list propagate_list =
    match check with
    | Propagate_up x ->
      propagate_up parameter handler log_info error blackboard x
        instruction_list propagate_list
    | Propagate_down x ->
      propagate_down parameter handler log_info error blackboard x
        instruction_list propagate_list

  let cut_case parameter handler case
      (error, log_info, blackboard, instruction_list, propagate_list) =
    let error, log_info, pointer_next =
      B.follow_pointer_down parameter handler log_info error blackboard case
    in
    let error, log_info, pointer_previous =
      B.follow_pointer_up parameter handler log_info error blackboard case
    in
    (* we remove the case *)
    let error, log_info, (blackboard, result) =
      B.refine parameter handler log_info error (B.exist case)
        (B.boolean (Some false)) blackboard
    in
    if B.is_failed result then
      (error, log_info, blackboard, [], []), result
    else if B.is_ignored result then
      (error, log_info, blackboard, instruction_list, propagate_list), result
    else (
      let error, log_info, blackboard =
        B.dec parameter handler log_info error
          (B.n_unresolved_events_in_column case)
          blackboard
      in
      (* we plug pointer next of the previous event *)
      let error, log_info, blackboard =
        B.overwrite parameter handler log_info error
          (B.pointer_to_next pointer_previous)
          (B.pointer pointer_next) blackboard
      in
      (* we plug pointer previous of the next event *)
      let error, log_info, blackboard =
        B.overwrite parameter handler log_info error
          (B.pointer_to_previous pointer_next)
          (B.pointer pointer_previous)
          blackboard
      in
      (error, log_info, blackboard, instruction_list, propagate_list), result
    )

  let look_down parameter handler log_info error blackboard propagate_list case
      =
    let error, log_info, (_, _, _, action) =
      B.get_static parameter handler log_info error blackboard case
    in
    if B.PB.is_unknown action then
      error, log_info, blackboard, propagate_list
    else (
      let list_values = B.PB.weakening action in
      let propagate_list, error, log_info, blackboard =
        List.fold_left
          (fun (propagate_list, error, log_info, blackboard) value ->
            let rec aux case bool error log_info =
              let ca = B.case_address_of_case_event_address case in
              let error, log_info, case_value =
                B.get parameter handler log_info error ca blackboard
              in
              let error, log_info, predicate_value =
                B.predicate_value_of_case_value parameter handler log_info error
                  case_value
              in
              if B.PB.more_refined predicate_value value then (
                let error, log_info, pointer_next =
                  B.follow_pointer_down parameter handler log_info error
                    blackboard case
                in
                let log_info =
                  StoryProfiling.StoryStats.add_look_down_case 1 log_info
                in
                ( Propagate_up pointer_next :: propagate_list,
                  error,
                  log_info,
                  blackboard )
              ) else (
                let error, log_info, next_case =
                  B.follow_pointer_down parameter handler log_info error
                    blackboard case
                in
                let error, log_info, exist =
                  B.exist_case parameter handler log_info error blackboard
                    next_case
                in
                match exist with
                | Some true ->
                  let log_info =
                    StoryProfiling.StoryStats.add_look_down_case 2 log_info
                  in
                  propagate_list, error, log_info, blackboard
                | Some false -> aux next_case bool error log_info
                | None ->
                  let error, log_info, (_, _, _, next_action) =
                    B.get_static parameter handler log_info error blackboard
                      next_case
                  in
                  if B.PB.more_refined next_action value then
                    if bool then (
                      let log_info =
                        StoryProfiling.StoryStats.add_look_down_case 3 log_info
                      in
                      propagate_list, error, log_info, blackboard
                    ) else
                      aux next_case true error log_info
                  else
                    aux next_case bool error log_info
              )
            in
            aux case false error log_info)
          (propagate_list, error, log_info, blackboard)
          list_values
      in
      error, log_info, blackboard, propagate_list
    )

  let look_down =
    if look_down_for_better_cut then
      look_down
    else
      fun _ _ log_info error blackboard list _ ->
    error, log_info, blackboard, list

  let refine_value_after parameter handler log_info error blackboard address
      value instruction_list propagate_list =
    let case_address = B.value_after address in
    let state = B.state value in
    let error, log_info, (blackboard, result) =
      B.refine parameter handler log_info error case_address state blackboard
    in
    if B.is_ignored result then
      error, log_info, (blackboard, instruction_list, propagate_list, result)
    else if B.is_failed result then
      error, log_info, (blackboard, [], [], result)
    else (
      let propagate_list =
        Propagate_up address :: Propagate_down address :: propagate_list
      in
      error, log_info, (blackboard, instruction_list, propagate_list, result)
    )

  let refine_value_before parameter handler log_info error blackboard address
      value instruction_list propagate_list =
    let error, log_info, pointer_previous =
      B.follow_pointer_up parameter handler log_info error blackboard address
    in
    refine_value_after parameter handler log_info error blackboard
      pointer_previous value instruction_list propagate_list

  let discard_case parameter handler case
      (error, log_info, blackboard, instruction_list, propagate_list) =
    let error, log_info, pointer_next =
      B.follow_pointer_down parameter handler log_info error blackboard case
    in
    let error, log_info, pointer_previous =
      B.follow_pointer_up parameter handler log_info error blackboard case
    in
    (* we remove the case *)
    let error, log_info, (blackboard, result) =
      B.refine parameter handler log_info error (B.exist case)
        (B.boolean (Some false)) blackboard
    in
    if B.is_failed result then
      (error, log_info, blackboard, [], []), result
    else if B.is_ignored result then
      (error, log_info, blackboard, instruction_list, propagate_list), result
    else (
      let ca = B.case_address_of_case_event_address case in
      let error, log_info, case_value =
        B.get parameter handler log_info error ca blackboard
      in
      let error, log_info, predicate_value =
        B.predicate_value_of_case_value parameter handler log_info error
          case_value
      in
      let error, log_info, (blackboard, instruction_list, _, result') =
        refine_value_after parameter handler log_info error blackboard
          pointer_previous predicate_value instruction_list propagate_list
      in
      if B.is_failed result' then
        (error, log_info, blackboard, [], []), result'
      else (
        let error, log_info, blackboard =
          B.dec parameter handler log_info error
            (B.n_unresolved_events_in_column case)
            blackboard
        in
        let error, log_info, (_, event, _, _) =
          B.get_static parameter handler log_info error blackboard case
        in
        let error, log_info, level =
          B.level_of_event parameter handler log_info error blackboard event
        in
        let error, log_info, blackboard =
          B.dec parameter handler log_info error
            (B.n_unresolved_events_in_column_at_level case level)
            blackboard
        in
        (* we plug pointer next of the previous event *)
        let error, log_info, blackboard =
          B.overwrite parameter handler log_info error
            (B.pointer_to_next pointer_previous)
            (B.pointer pointer_next) blackboard
        in
        (* we plug pointer previous of the next event *)
        let error, log_info, blackboard =
          B.overwrite parameter handler log_info error
            (B.pointer_to_previous pointer_next)
            (B.pointer pointer_previous)
            blackboard
        in
        let propagate_list =
          Propagate_up pointer_next :: Propagate_down pointer_previous
          :: Propagate_up pointer_previous :: propagate_list
        in
        let error, log_info, blackboard, propagate_list =
          look_down parameter handler log_info error blackboard propagate_list
            case
        in
        (error, log_info, blackboard, instruction_list, propagate_list), result
      )
    )

  let keep_case parameter handler case
      (error, log_info, blackboard, instruction_list, propagate_list) =
    (* we keep the case *)
    let error, log_info, (blackboard, result) =
      B.refine parameter handler log_info error (B.exist case)
        (B.boolean (Some true)) blackboard
    in
    if B.is_failed result then
      (error, log_info, blackboard, [], []), result
    else if B.is_ignored result then
      (error, log_info, blackboard, instruction_list, propagate_list), result
    else (
      let error, log_info, pointer_previous =
        B.follow_pointer_up parameter handler log_info error blackboard case
      in
      let error, log_info, _pointer_next =
        B.follow_pointer_down parameter handler log_info error blackboard case
      in
      let error, log_info, (_seid, eid, test, action) =
        B.get_static parameter handler log_info error blackboard case
      in
      let error, log_info, (blackboard, instruction_list, _, result') =
        refine_value_before parameter handler log_info error blackboard case
          test instruction_list propagate_list
      in
      let error, log_info, (blackboard, instruction_list, _, result'') =
        refine_value_after parameter handler log_info error blackboard case
          action instruction_list propagate_list
      in
      if B.is_failed result' || B.is_failed result'' then
        (error, log_info, blackboard, [], []), B.fail
      else (
        let error, log_info, blackboard =
          B.dec parameter handler log_info error
            (B.n_unresolved_events_in_column case)
            blackboard
        in
        let error, log_info, level =
          B.level_of_event parameter handler log_info error blackboard eid
        in
        let error, log_info, blackboard =
          B.dec parameter handler log_info error
            (B.n_unresolved_events_in_column_at_level case level)
            blackboard
        in
        let propagate_list =
          Propagate_up case :: Propagate_down case
          :: Propagate_down pointer_previous :: Propagate_up pointer_previous
          :: propagate_list
        in
        (error, log_info, blackboard, instruction_list, propagate_list), result
      )
    )

  let keep_event parameter handler log_info error blackboard step_id
      instruction_list propagate_list =
    let error, log_info, (blackboard, success) =
      B.refine parameter handler log_info error (B.is_exist_event step_id)
        (B.boolean (Some true)) blackboard
    in
    if B.is_failed success then
      error, log_info, (blackboard, [], [], success)
    else if B.is_ignored success then
      error, log_info, (blackboard, instruction_list, propagate_list, success)
    else (
      let log_info = StoryProfiling.StoryStats.inc_selected_events log_info in
      let () =
        if debug_mode then (
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "***"
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "We keep event %i"
              (B.PB.int_of_step_id step_id)
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "***"
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          ()
        )
      in
      let error, log_info, blackboard =
        B.dec parameter handler log_info error B.n_unresolved_events blackboard
      in
      let error, log_info, list =
        B.case_list_of_eid parameter handler log_info error blackboard step_id
      in
      let rec aux l x success =
        match l with
        | [] -> x, success
        | t :: q ->
          let y, success2 = keep_case parameter handler t x in
          if B.is_ignored success2 then
            aux q y success
          else if B.is_succeeded success2 then
            aux q y success2
          else
            y, success2
      in
      let ( (error, log_info, blackboard, instruction_list, propagate_list),
            success ) =
        aux list
          (error, log_info, blackboard, instruction_list, propagate_list)
          B.ignore
      in
      error, log_info, (blackboard, instruction_list, propagate_list, success)
    )

  let gen_event f_case g parameter handler log_info error blackboard step_id
      instruction_list propagate_list =
    let error, log_info, (blackboard, success) =
      B.refine parameter handler log_info error (B.is_exist_event step_id)
        (B.boolean (Some false)) blackboard
    in
    if B.is_failed success then
      error, log_info, (blackboard, [], [], success)
    else if B.is_ignored success then
      error, log_info, (blackboard, instruction_list, propagate_list, success)
    else (
      let () =
        if debug_mode then (
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "***"
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "We remove event %i"
              (B.PB.int_of_step_id step_id)
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          let () =
            Loggers.fprintf
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
              "***"
          in
          let () =
            Loggers.print_newline
              (B.PB.CI.Po.K.H.get_debugging_channel parameter)
          in
          ()
        )
      in
      let log_info = g log_info in
      let error, log_info, blackboard =
        B.dec parameter handler log_info error B.n_unresolved_events blackboard
      in
      let error, log_info, level =
        B.level_of_event parameter handler log_info error blackboard step_id
      in
      let error, log_info, blackboard =
        B.dec parameter handler log_info error
          (B.n_unresolved_events_at_level level)
          blackboard
      in
      let error, log_info, list =
        B.case_list_of_eid parameter handler log_info error blackboard step_id
      in
      let rec aux l x success =
        match l with
        | [] -> x, success
        | t :: q ->
          let y, success2 = f_case parameter handler t x in
          if B.is_ignored success2 then
            aux q y success
          else if B.is_succeeded success2 then
            aux q y success2
          else
            y, success2
      in
      let ( (error, log_info, blackboard, instruction_list, propagate_list),
            success ) =
        aux list
          (error, log_info, blackboard, instruction_list, propagate_list)
          B.ignore
      in
      error, log_info, (blackboard, instruction_list, propagate_list, success)
    )

  let cut_event = gen_event cut_case StoryProfiling.StoryStats.inc_cut_events

  let discard_event =
    gen_event discard_case StoryProfiling.StoryStats.inc_removed_events

  let apply_instruction parameter handler log_info error blackboard instruction
      instruction_list propagate_list =
    match instruction with
    | Keep_event step_id ->
      keep_event parameter handler log_info error blackboard step_id
        instruction_list propagate_list
    | Cut_event step_id ->
      cut_event parameter handler log_info error blackboard step_id
        instruction_list propagate_list
    | Discard_event step_id ->
      discard_event parameter handler log_info error blackboard step_id
        instruction_list propagate_list
    | Refine_value_after (address, value) ->
      refine_value_after parameter handler log_info error blackboard address
        value instruction_list propagate_list
    | Refine_value_before (address, value) ->
      refine_value_before parameter handler log_info error blackboard address
        value instruction_list propagate_list
    | Skip ->
      error, log_info, (blackboard, instruction_list, propagate_list, B.ignore)

  let _keep x = Keep_event x
end
