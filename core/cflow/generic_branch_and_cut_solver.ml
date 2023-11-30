(**
  * generic_branch_and_cup_solver.ml
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 29/08/2011
  * Last modification: 19/07/2013
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let log_steps = false

module type Solver = sig
  module PH : Propagation_heuristics.Blackboard_with_heuristic

  val compress :
    ( PH.B.blackboard,
      PH.update_order list,
      PH.B.blackboard * PH.B.assign_result * PH.B.result list )
    PH.B.PB.CI.Po.K.H.binary

  val detect_independent_events :
    ( PH.B.blackboard,
      PH.B.PB.step_id list,
      PH.B.PB.step_id list )
    PH.B.PB.CI.Po.K.H.binary

  val filter :
    ( PH.B.blackboard,
      PH.B.PB.step_id list,
      PH.B.blackboard )
    PH.B.PB.CI.Po.K.H.binary

  val sub : (Trace.t, PH.B.blackboard) PH.B.PB.CI.Po.K.H.unary
  val clean : (PH.B.blackboard, PH.B.blackboard) PH.B.PB.CI.Po.K.H.unary

  val translate :
    ( PH.B.blackboard,
      PH.B.PB.step_id list,
      Trace.t * PH.B.result )
    PH.B.PB.CI.Po.K.H.binary

  val translate_result : PH.B.result -> Trace.t
end

module Solver = struct
  module PH = Propagation_heuristics.Propagation_heuristic
  (*Blackboard_with_heuristic*)

  let warn parameter error pos ?(message = "") exn default =
    Exception.warn
      (PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error pos ~message exn default

  let _combine_output o1 o2 =
    if PH.B.is_ignored o2 then
      o1
    else
      o2

  let rec propagate parameter handler log_info error instruction_list
      propagate_list blackboard =
    let bool, log_info = PH.B.tick log_info in
    let _ =
      if bool then
        StoryProfiling.StoryStats.dump_complete_log
          (PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
          log_info
    in
    match instruction_list with
    | t :: q ->
      let ( error,
            log_info,
            (blackboard, instruction_list, propagate_list, assign_result) ) =
        PH.apply_instruction parameter handler log_info error blackboard t q
          propagate_list
      in
      if PH.B.is_failed assign_result then
        error, log_info, (blackboard, assign_result)
      else
        propagate parameter handler log_info error instruction_list
          propagate_list blackboard
    | [] ->
      (match propagate_list with
      | t :: q ->
        let ( error,
              log_info,
              (blackboard, instruction_list, propagate_list, assign_result) ) =
          PH.propagate parameter handler log_info error blackboard t
            instruction_list q
        in
        if PH.B.is_failed assign_result then
          error, log_info, (blackboard, assign_result)
        else
          propagate parameter handler log_info error instruction_list
            propagate_list blackboard
      | [] -> error, log_info, (blackboard, PH.B.success))

  type choices = {
    current: PH.update_order list;
    stack: PH.update_order list list;
  }

  let branch_choice_list choice_list =
    { current = []; stack = choice_list.current :: choice_list.stack }

  let update_current choice_list list = { choice_list with current = list }

  let pop_next_choice parameter _handler error stack =
    match stack.current with
    | t :: q -> error, (t, { stack with current = q })
    | [] ->
      warn parameter error __POS__ ~message:"Empty choice stack"
        (Failure "Empty choice list in pop_next_choice")
        (PH.dummy_update_order, stack)

  let no_more_choice stack =
    match stack.current with
    | [] -> true
    | _ -> false

  let backtrack parameter handler log_info error blackboard choice_list =
    let rec backtrack_aux log_info error blackboard choice_list =
      match choice_list.current with
      | [] ->
        (match choice_list.stack with
        | [] -> error, log_info, (blackboard, None)
        | t :: q ->
          let choice_list = { current = t; stack = q } in
          let error, log_info, blackboard =
            PH.B.reset_last_branching parameter handler log_info error
              blackboard
          in
          backtrack_aux log_info error blackboard choice_list)
      | _ -> error, log_info, (blackboard, Some choice_list)
    in
    let error, log_info, blackboard =
      PH.B.reset_last_branching parameter handler log_info error blackboard
    in
    backtrack_aux log_info error blackboard choice_list

  let empty_choice_list = { stack = []; current = [] }

  let rec sublist l l' =
    match l, l' with
    | [], _ -> true
    | _, [] -> false
    | h :: t, h' :: t' when h = h' -> sublist t t'
    | _, _h' :: t' -> sublist l t'

  let sort_stories_according_to_length l =
    List.rev_map fst
      (List.sort
         (fun (_, a) (_, b) -> compare b a)
         (List.rev_map (fun a -> a, List.length a) l))

  let filter_out_non_minimal_story l =
    let rec aux to_visit goodones =
      match to_visit with
      | [] -> List.rev goodones
      | h :: t ->
        aux
          (List.filter (fun story -> not (sublist h story)) t)
          (h :: List.filter (fun story -> not (sublist h story)) goodones)
    in
    aux (sort_stories_according_to_length l) []

  let rec iter parameter handler log_info error blackboard choice_list
      story_list =
    let error, log_info, bool =
      PH.B.is_maximal_solution parameter handler log_info error blackboard
    in
    if bool then (
      (* SUCCESS *)
      let error, log_info, list =
        PH.B.translate_blackboard parameter handler log_info error blackboard
      in
      if PH.B.PB.CI.Po.K.H.get_all_stories_per_obs parameter then (
        let story_list = list :: story_list in
        let error, () =
          if choice_list.current <> [] then
            warn parameter error __POS__
              ~message:
                "In case of success, the current list of choices should be \
                 empty"
              (Failure
                 "In case of success, the current list of choices should be \
                  empty") ()
          else
            error, ()
        in
        let choice_list =
          match choice_list.stack with
          | [] -> choice_list
          | t :: q -> { current = t; stack = q }
        in
        let error, log_info, (blackboard, choice_list) =
          backtrack parameter handler log_info error blackboard choice_list
        in
        match choice_list with
        | Some choice_list ->
          iter parameter handler log_info error blackboard choice_list
            story_list (*(update_first_story first_story list)*)
        | None ->
          let _ =
            PH.B.export_blackboard_to_xls parameter handler log_info error
              "FAIL" !Priority.n_story !Priority.n_branch blackboard
          in
          error, log_info, (blackboard, story_list)
      ) else
        error, log_info, (blackboard, [ list ])
    ) else (
      let error, choice_list =
        if no_more_choice choice_list then (
          let error, _log_info, list =
            PH.next_choice parameter handler log_info error blackboard
          in
          error, update_current choice_list list
        ) else
          error, choice_list
      in
      let error, log_info, blackboard =
        PH.B.branch parameter handler log_info error blackboard
      in
      let error, (choice, choice_list) =
        pop_next_choice parameter handler error choice_list
      in
      let error, log_info, (blackboard, output) =
        propagate parameter handler log_info error [ choice ] [] blackboard
      in
      if PH.B.is_failed output then (
        let error, log_info, (blackboard, choice_list) =
          backtrack parameter handler log_info error blackboard choice_list
        in
        match choice_list with
        | Some choice_list ->
          iter parameter handler log_info error blackboard choice_list
            story_list
        | None -> error, log_info, (blackboard, story_list)
      ) else
        iter parameter handler log_info error blackboard
          (branch_choice_list choice_list)
          story_list
    )

  let detect_independent_events parameter handler log_info error blackboard
      list_eid =
    let error, log_info, (_blackboard, events_to_keep) =
      PH.B.cut parameter handler log_info error blackboard list_eid
    in
    error, log_info, events_to_keep

  let translate _parameter _handler log_info error blackboard list =
    let list' =
      List.rev_map
        (fun k ->
          ( PH.B.get_event blackboard (PH.B.PB.dec_step_id k),
            PH.B.side_effect_of_event blackboard (PH.B.PB.dec_step_id k) ))
        (List.rev list)
    in
    let list = List.rev_map fst (List.rev list') in
    error, log_info, (list, list')

  let translate_result result = List.rev_map fst @@ List.rev result

  let clean parameter handler error log_info blackboard =
    PH.B.reset_init parameter handler error log_info blackboard

  let filter parameter handler log_info error blackboard events_to_keep =
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    let error, log_info, blackboard =
      PH.B.branch parameter handler log_info error blackboard
    in
    let events_to_remove =
      let n_events = PH.B.get_n_eid blackboard in
      let rec aux k list sol =
        if k = n_events then
          List.rev sol
        else (
          match list with
          | t :: q ->
            if PH.B.PB.int_of_step_id t = k then
              aux (k + 1) q sol
            else
              aux (k + 1) list (PH.B.PB.step_id_of_int k :: sol)
          | [] -> aux (k + 1) list (PH.B.PB.step_id_of_int k :: sol)
        )
      in
      aux 0 events_to_keep []
    in
    let error, log_info, forbidden_events =
      PH.forbidden_events parameter handler log_info error events_to_remove
    in
    let () =
      if log_steps then (
        let () =
          Loggers.fprintf
            (PH.B.PB.CI.Po.K.H.get_logger parameter)
            "Start cutting"
        in
        let () =
          Loggers.print_newline (PH.B.PB.CI.Po.K.H.get_logger parameter)
        in
        ()
      )
    in
    let error, log_info, (blackboard, _output) =
      propagate parameter handler log_info error forbidden_events [] blackboard
    in
    let log_info =
      StoryProfiling.StoryStats.set_concurrent_event_deletion_time log_info
    in
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    error, log_info, blackboard

  let sub parameter handler log_info error to_keep =
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    let error, log_info, blackboard =
      PH.B.import parameter handler log_info error to_keep
    in
    let log_info =
      StoryProfiling.StoryStats.set_concurrent_event_deletion_time log_info
    in
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    error, log_info, blackboard

  let compress parameter handler log_info error blackboard list_order =
    let error, log_info, blackboard =
      PH.B.branch parameter handler log_info error blackboard
    in
    let log_info =
      StoryProfiling.StoryStats.set_concurrent_event_deletion_time log_info
    in
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    let () =
      if log_steps then (
        let () =
          Loggers.fprintf
            (PH.B.PB.CI.Po.K.H.get_logger parameter)
            "After Causal Cut  %i"
            (PH.B.get_n_unresolved_events blackboard)
        in
        let () =
          Loggers.print_newline (PH.B.PB.CI.Po.K.H.get_logger parameter)
        in
        ()
      )
    in
    let error, log_info, (blackboard, output) =
      propagate parameter handler log_info error list_order [] blackboard
    in
    if PH.B.is_failed output then (
      let () =
        if log_steps then (
          let () =
            Loggers.fprintf
              (PH.B.PB.CI.Po.K.H.get_logger parameter)
              "After observable propagation  FAIL %i @."
              (PH.B.get_n_unresolved_events blackboard)
          in
          let () =
            Loggers.print_newline (PH.B.PB.CI.Po.K.H.get_logger parameter)
          in
          ()
        )
      in
      error, log_info, (blackboard, output, [])
    ) else (
      let () =
        if log_steps then (
          let () =
            Loggers.fprintf
              (PH.B.PB.CI.Po.K.H.get_logger parameter)
              "After observable propagation  %i @."
              (PH.B.get_n_unresolved_events blackboard)
          in
          let () =
            Loggers.print_newline (PH.B.PB.CI.Po.K.H.get_logger parameter)
          in
          ()
        )
      in
      let bool, string =
        match parameter.PH.B.PB.CI.Po.K.H.current_compression_mode with
        | None | Some Story_json.Causal -> false, ""
        | Some Story_json.Weak ->
          ( Parameter.dump_grid_after_branching_during_weak_compression,
            Parameter.xlsweakFileName )
        | Some Story_json.Strong ->
          ( Parameter.dump_grid_after_branching_during_strong_compression,
            Parameter.xlsstrongFileName )
      in
      let () = Priority.n_branch := 1 + (!Priority.n_branch + 1) in
      let error, log_info =
        if bool then (
          let error, log_info, () =
            PH.B.export_blackboard_to_xls parameter handler log_info error
              string !Priority.n_story !Priority.n_branch blackboard
          in
          error, log_info
        ) else
          error, log_info
      in
      let error, log_info, (blackboard, story_list) =
        iter parameter handler log_info error blackboard empty_choice_list []
      in
      let output =
        match story_list with
        | [] -> PH.B.fail
        | _ -> PH.B.success
      in
      ( error,
        log_info,
        (blackboard, output, filter_out_non_minimal_story (List.rev story_list))
      )
    )
end
