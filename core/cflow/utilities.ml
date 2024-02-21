(**
 * utilities.ml
 *
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2016-02-03 20:53:14 feret>
 *
 * API for causal compression
 * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Jean Krivine, Université Paris-Diderot, CNRS
 *
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.
 * This file is distributed under the terms of the
 * GNU Library General Public License *)

let debug_mode = false

module D = Dag.StoryTable
module S = Generic_branch_and_cut_solver.Solver
module P = StoryProfiling.StoryStats

type error_log = Exception.method_handler
type parameter = S.PH.B.PB.CI.Po.K.H.parameter
type kappa_handler = S.PH.B.PB.CI.Po.K.H.handler
type profiling_info = P.log_info
type shall_we = parameter -> bool
type step = Trace.step
type step_with_side_effects = step * S.PH.B.PB.CI.Po.K.side_effect
type step_id = S.PH.B.PB.step_id

type trace = {
  compressed_trace: step_with_side_effects list option;
  pretrace: step list;
  with_potential_ambiguity: bool;
}

type trace_runtime_info = profiling_info Trace.Simulation_info.t

type 'a with_handlers =
  parameter ->
  ?shall_we_compute:shall_we ->
  ?shall_we_compute_profiling_information:shall_we ->
  ?print_if_zero:shall_we ->
  kappa_handler ->
  profiling_info ->
  error_log ->
  'a

type 'a zeroary = (error_log * profiling_info * 'a) with_handlers
type ('a, 'b) unary = ('a -> error_log * profiling_info * 'b) with_handlers

type ('a, 'b, 'c) binary =
  ('a -> 'b -> error_log * profiling_info * 'c) with_handlers

type ('a, 'b, 'c, 'd) ternary =
  ('a -> 'b -> 'c -> error_log * profiling_info * 'd) with_handlers

type ('a, 'b, 'c, 'd, 'e) quaternary =
  ('a -> 'b -> 'c -> 'd -> error_log * profiling_info * 'e) with_handlers

type ('a, 'b, 'c, 'd, 'e, 'f) quinternary =
  ('a -> 'b -> 'c -> 'd -> 'e -> error_log * profiling_info * 'f) with_handlers

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) sexternary =
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> error_log * profiling_info * 'g)
  with_handlers

let (we_shall : shall_we) = fun _ -> true
let (we_shall_not : shall_we) = fun _ -> false
let get_pretrace_of_trace trace = trace.pretrace
let size_of_pretrace trace = List.length (get_pretrace_of_trace trace)
let may_initial_sites_be_ambiguous trace = trace.with_potential_ambiguity
let set_ambiguity_level trace x = { trace with with_potential_ambiguity = x }
let _set_pretrace trace x = { trace with pretrace = x }
let _set_compressed_trace trace x = { trace with compressed_trace = x }

let get_compressed_trace trace =
  match trace.compressed_trace with
  | Some x -> x
  | None ->
    List.rev_map (fun x -> x, []) (List.rev (get_pretrace_of_trace trace))

let is_compressed_trace trace = trace.compressed_trace != None

let trace_of_pretrace_with_ambiguity with_ambiguity pretrace =
  {
    pretrace;
    compressed_trace = None;
    with_potential_ambiguity = with_ambiguity;
  }

let trace_of_pretrace = trace_of_pretrace_with_ambiguity true

let build_compressed_trace x y =
  { compressed_trace = Some y; pretrace = x; with_potential_ambiguity = false }

(*let get_log_step = S.PH.B.PB.CI.Po.K.H.get_log_step
  let get_debugging_mode = S.PH.B.PB.CI.Po.K.H.get_debugging_mode
  let get_logger = S.PH.B.PB.CI.Po.K.H.get_logger*)
let get_id_of_event = S.PH.B.PB.CI.Po.K.get_id_of_refined_step
let get_simulation_time_of_event = S.PH.B.PB.CI.Po.K.get_time_of_refined_step

let _extend_trace_with_dummy_side_effects l =
  List.rev_map (fun a -> a, []) (List.rev l)

let print_pretrace parameter _handler =
  Loggers.fprintf
    (S.PH.B.PB.CI.Po.K.H.get_out_channel parameter)
    "@[<v>%a@]@."
    (Pp.list Pp.space (Trace.print_step ~compact:true ?env:None))

let print_trace parameter handler trace =
  print_pretrace parameter handler (get_pretrace_of_trace trace)

let transform_trace_gen f log_message debug_message profiling_event parameters
    ?(shall_we_compute = we_shall) ?shall_we_compute_profiling_information:_
    ?(print_if_zero = we_shall) kappa_handler profiling_info error trace =
  if shall_we_compute parameters then (
    let error, profiling_info =
      StoryProfiling.StoryStats.add_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameters)
        error profiling_event
        (Some (fun () -> size_of_pretrace trace))
        profiling_info
    in
    let bool =
      if S.PH.B.PB.CI.Po.K.H.get_log_step parameters then (
        match log_message with
        | Some log_message ->
          let () =
            Loggers.fprintf
              (S.PH.B.PB.CI.Po.K.H.get_logger parameters)
              "%s%s"
              (Remanent_parameters.get_prefix
                 (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameters))
              log_message
          in
          let () =
            Loggers.flush_logger (S.PH.B.PB.CI.Po.K.H.get_logger parameters)
          in
          true
        | None -> false
      ) else
        false
    in
    let pretrace = get_pretrace_of_trace trace in
    let error, profiling_info, (pretrace', n) =
      f parameters kappa_handler profiling_info error pretrace
    in
    let trace' = trace_of_pretrace pretrace' in
    let trace =
      if trace == trace' then
        trace
      else
        set_ambiguity_level trace' (may_initial_sites_be_ambiguous trace)
    in
    let () =
      if bool then
        if n = 0 then
          if print_if_zero parameters then
            Loggers.fprintf
              (S.PH.B.PB.CI.Po.K.H.get_logger parameters)
              ": nothing has changed @."
          else
            Loggers.fprintf (S.PH.B.PB.CI.Po.K.H.get_logger parameters) "@."
        else if n > 0 then
          Loggers.fprintf
            (S.PH.B.PB.CI.Po.K.H.get_logger parameters)
            ": -%i events @." n
        else
          Loggers.fprintf
            (S.PH.B.PB.CI.Po.K.H.get_logger parameters)
            ": +%i events @." (-n)
    in
    let () =
      if S.PH.B.PB.CI.Po.K.H.get_debugging_mode parameters then (
        let _ =
          match
            Loggers.formatter_of_logger
              (S.PH.B.PB.CI.Po.K.H.get_debugging_channel parameters)
          with
          | Some fmt -> Format.fprintf fmt debug_message
          | None -> ()
        in
        print_trace parameters kappa_handler trace
      )
    in

    let error, profiling_info =
      StoryProfiling.StoryStats.close_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameters)
        error profiling_event
        (Some (fun () -> size_of_pretrace trace))
        profiling_info
    in
    error, profiling_info, trace
  ) else
    error, profiling_info, trace

type kind = (*  | Solve_ambiguities*)
  | May_add_ambiguities | Neutral

let handle_ambiguities kind b =
  match kind with
  (*  | Solve_ambiguities -> false*)
  | Neutral -> b
  | May_add_ambiguities -> true

type ambiquities_precondition =
  | Do_not_care
  | Require_the_abscence_of_ambiguity (*| Better_when_no_ambiguity*)

let must_we_solve_ambiguity _parameters x =
  match x with
  | Do_not_care -> false
  | Require_the_abscence_of_ambiguity -> true
(*  | Better_when_no_ambiguity -> S.PH.B.PB.CI.Po.K.H.always_disambiguate parameters*)

let monadic_lift f _ _ log_info error t =
  let t' = f t in
  error, log_info, (t', List.length t - List.length t')

let _dummy_profiling _ p = p

let disambiguate =
  transform_trace_gen
    (monadic_lift S.PH.B.PB.CI.Po.K.disambiguate)
    (Some "\t- alpha-conversion") "Trace after having renames agents:\n"
    StoryProfiling.Agent_ids_disambiguation

let make_unambiguous parameters ?(shall_we_compute = we_shall)
    ?(shall_we_compute_profiling_information = we_shall_not)
    ?(print_if_zero = we_shall_not) kappa_handler profiling_info error trace =
  if may_initial_sites_be_ambiguous trace then (
    let error, profiling_info, trace' =
      disambiguate parameters
        ~shall_we_compute:(fun parameters ->
          may_initial_sites_be_ambiguous trace && shall_we_compute parameters)
        ~shall_we_compute_profiling_information ~print_if_zero kappa_handler
        profiling_info error trace
    in
    ( error,
      profiling_info,
      if trace' == trace then
        set_ambiguity_level trace false
      else
        set_ambiguity_level trace' false )
  ) else
    error, profiling_info, trace

let lift_to_care_about_ambiguities f requirement effect parameters
    ?(shall_we_compute = we_shall)
    ?(shall_we_compute_profiling_information = we_shall)
    ?(print_if_zero = we_shall) kappa_handler profiling_info error trace =
  if shall_we_compute parameters then (
    let error, profiling_info, trace =
      if must_we_solve_ambiguity parameters requirement then
        make_unambiguous parameters ~shall_we_compute kappa_handler
          profiling_info error trace
      else
        error, profiling_info, trace
    in
    let error, log_info, trace =
      (f
        : S.PH.B.PB.CI.Po.K.H.parameter ->
          ?shall_we_compute:shall_we ->
          ?shall_we_compute_profiling_information:shall_we ->
          ?print_if_zero:shall_we ->
          S.PH.B.PB.CI.Po.K.H.handler ->
          StoryProfiling.StoryStats.log_info ->
          Exception.method_handler ->
          trace ->
          Exception.method_handler * StoryProfiling.StoryStats.log_info * trace)
        parameters ~shall_we_compute ~shall_we_compute_profiling_information
        ~print_if_zero kappa_handler profiling_info error trace
    in
    let trace = set_ambiguity_level trace (handle_ambiguities effect true) in
    error, log_info, trace
  ) else
    error, profiling_info, trace

let split_init =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift S.PH.B.PB.CI.Po.K.split_init)
       (Some "\t- splitting initial events")
       "Trace after having split initial events:\n"
       StoryProfiling.Decompose_initial_state)
    Do_not_care Neutral

let cut =
  lift_to_care_about_ambiguities
    (transform_trace_gen S.PH.B.PB.CI.Po.cut
       (Some "\t- cutting concurrent events")
       "Trace after having removed concurrent events:\n"
       StoryProfiling.Partial_order_reduction)
    Require_the_abscence_of_ambiguity Neutral

type on_the_fly_cut_state = S.PH.B.PB.CI.Po.on_the_fly_state

let on_the_fly_cut_init = S.PH.B.PB.CI.Po.init_cut
let on_the_fly_cut_step = S.PH.B.PB.CI.Po.cut_step

let on_the_fly_cut_finalize cut_state =
  fst (S.PH.B.PB.CI.Po.finalize_cut cut_state)

let cut_rev_trace rev_trace =
  on_the_fly_cut_finalize
    (List.fold_left on_the_fly_cut_step on_the_fly_cut_init rev_trace)

let remove_obs_before _parameter _handler log_info error last_eid trace =
  ( error,
    log_info,
    let rec aux l score output =
      match l with
      | [] -> List.rev output, score
      | t :: q ->
        if Trace.step_is_obs t then (
          match Trace.simulation_info_of_step t with
          | None -> aux q score (t :: output)
          | Some x ->
            if Trace.Simulation_info.story_id x >= last_eid then
              ( List.rev (List.fold_left (fun list a -> a :: list) output l),
                score )
            else
              aux q (succ score) output
        ) else
          aux q score (t :: output)
    in
    aux trace 0 [] )

let remove_obs_before parameter handler log_info error last_info trace =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (fun parameter handler log_info error ->
         remove_obs_before parameter handler log_info error last_info)
       (Some "\t- Removing already visited observable hits")
       "Trace after having removed seen observable hits\n"
       StoryProfiling.Partial_order_reduction)
    Do_not_care Neutral parameter handler log_info error trace

let remove_obs_before parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error last_info trace =
  match last_info with
  | None -> error, log_info, trace
  | Some l ->
    let last =
      List.fold_left
        (fun result x ->
          let last_eid = Trace.Simulation_info.story_id x in
          max result last_eid)
        0 l
    in
    remove_obs_before parameter handler log_info error last trace

let fill_siphon =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift S.PH.B.PB.CI.Po.K.fill_siphon)
       (Some "\t- detecting siphons") "Trace after having detected siphons:\n"
       StoryProfiling.Siphon_detection)
    Require_the_abscence_of_ambiguity May_add_ambiguities

let (remove_events_after_last_obs : (trace, trace) unary) =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift
          (List_util.remove_suffix_after_last_occurrence Trace.step_is_obs))
       (Some "\t- removing events occurring after the last observable")
       "Trace after having removed the events after the last observable"
       StoryProfiling.Remove_events_after_last_observable)
    Do_not_care Neutral

let remove_pseudo_inverse_events =
  lift_to_care_about_ambiguities
    (transform_trace_gen S.PH.B.PB.CI.cut
       (Some "\t- detecting pseudo inverse events")
       "Trace after having removed pseudo inverse events:\n"
       StoryProfiling.Pseudo_inverse_deletion)
    Do_not_care Neutral

type cflow_grid = Causal.grid
type enriched_cflow_grid = Causal.enriched_grid
type musical_grid = S.PH.B.blackboard
type story_table = { story_counter: int; story_list: D.table }

let count_stories story_table = D.count_stories story_table.story_list

type observable_hit = {
  list_of_actions: S.PH.update_order list;
  list_of_events: step_id list;
  runtime_info: unit Trace.Simulation_info.t option;
}

let get_event_list_from_observable_hit a = a.list_of_events
let get_runtime_info_from_observable_hit a = a.runtime_info
let _get_list_order a = a.list_of_actions
let error_init = Exception.empty_error_handler

let extract_observable_hits_from_musical_notation a ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ b profiling_info
    c d =
  let error, profiling_info, l = S.PH.forced_events a b profiling_info c d in
  ( error,
    profiling_info,
    List.rev_map
      (fun (a, b, c) ->
        { list_of_actions = a; list_of_events = b; runtime_info = c })
      (List.rev l) )

let extract_observable_hit_from_musical_notation a ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ b profiling_info
    c string d =
  let error, profiling_info, l = S.PH.forced_events a b profiling_info c d in
  match l with
  | [ (a, b, c) ] ->
    ( error,
      profiling_info,
      { list_of_actions = a; list_of_events = b; runtime_info = c } )
  | [] -> failwith (string ^ " no story")
  | _ :: _ -> failwith (string ^ " several stories")

let translate p h profiling_info e b list =
  let error, profiling_info, (list, _) =
    S.translate p h profiling_info e b list
  in
  error, profiling_info, trace_of_pretrace list

let causal_prefix_of_an_observable_hit parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler
    profiling_info error string blackboard (enriched_grid : enriched_cflow_grid)
    observable_id =
  let eid =
    match get_event_list_from_observable_hit observable_id with
    | [ a ] -> a
    | [] -> failwith ("no observable in that story" ^ string)
    | _ -> failwith ("several observables in that story" ^ string)
  in
  let event_id_list =
    Graph_closure.get_list_in_increasing_order_with_last_event
      (S.PH.B.PB.int_of_step_id (S.PH.B.PB.inc_step_id eid))
      enriched_grid.Causal.prec_star
  in
  let error, profiling_info, output =
    translate parameter handler profiling_info error blackboard
      (List.rev_map S.PH.B.PB.step_id_of_int (List.rev event_id_list))
  in
  error, profiling_info, output

let export_musical_grid_to_xls a ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ b
    (p : StoryProfiling.StoryStats.log_info) c d e f g =
  S.PH.B.export_blackboard_to_xls a b p c d e f g

let print_musical_grid a ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ b p c d =
  S.PH.B.print_blackboard a b p c d

let create_story_table parameters ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler
    profiling_info error =
  let error, profiling_info, init =
    D.init_table parameters handler profiling_info error
  in
  error, profiling_info, { story_counter = 1; story_list = init }

let _get_trace_of_story (_, _, _, y, _) = trace_of_pretrace y
let _get_info_of_story (_, _, _, _, t) = t

let tick_opt parameter bar =
  match bar with
  | None -> bar
  | Some (logger, bar) ->
    Some
      ( logger,
        Tick_stories.tick_stories logger Configuration.empty
          (S.PH.B.PB.CI.Po.K.H.save_progress_bar parameter)
          bar )

let close_progress_bar_opt logger = Loggers.print_newline logger

let print_fails logger s n =
  match n with
  | 0 -> ()
  | 1 -> Loggers.fprintf logger "@.\t 1 %s has failed@." s
  | _ -> Loggers.fprintf logger "@.\t %i %ss have failed@." n s

let inc_fails a a' b =
  if a == a' then
    succ b
  else
    b

let fold_story_table_gen logger parameter ?(shall_we_compute = we_shall)
    ?(shall_we_compute_profiling_information = we_shall)
    (handler : kappa_handler) (profiling_info : profiling_info) error s
    (f : (trace, trace_runtime_info list, 'a, 'a) ternary) l a =
  let n_stories_input = count_stories l in
  let progress_bar =
    Some
      ( logger,
        Tick_stories.tick_stories logger Configuration.empty
          (S.PH.B.PB.CI.Po.K.H.save_progress_bar parameter)
          (false, 0, 0, n_stories_input) )
  in
  let g parameter handler profiling_info error story info
      (k, progress_bar, a, n_fails) =
    let event = StoryProfiling.Story k in
    let error, profiling_info =
      P.add_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
        error event None profiling_info
    in
    let error, profiling_info, a' =
      f parameter ~shall_we_compute ~shall_we_compute_profiling_information
        handler profiling_info error
        (trace_of_pretrace_with_ambiguity false story)
        info a
    in
    let progress_bar = tick_opt parameter progress_bar in
    let n_fails = inc_fails a a' n_fails in
    let error, profiling_info =
      P.close_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
        error event None profiling_info
    in
    error, profiling_info, (succ k, progress_bar, a', n_fails)
  in
  let error, profiling_info, (_, _, a, n_fails) =
    D.fold_table parameter handler profiling_info error g l.story_list
      (1, progress_bar, a, 0)
  in
  let () = close_progress_bar_opt logger in
  let () = print_fails parameter.S.PH.B.PB.CI.Po.K.H.logger_err s n_fails in
  error, (profiling_info : profiling_info), a

let fold_story_table_with_progress_bar parameter ?shall_we_compute
    ?shall_we_compute_profiling_information ?print_if_zero:_ handler
    profiling_info error s f l a =
  fold_story_table_gen
    (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
    parameter ?shall_we_compute ?shall_we_compute_profiling_information handler
    profiling_info error s f l a

let fold_story_table_without_progress_bar parameter ?shall_we_compute
    ?shall_we_compute_profiling_information ?print_if_zero:_ handler
    profiling_info error s f l a =
  fold_story_table_gen Loggers.dummy_txt_logger parameter ?shall_we_compute
    ?shall_we_compute_profiling_information handler profiling_info error s f l a

let get_counter story_list = story_list.story_counter
let get_stories story_list = story_list.story_list

let _inc_counter story_list =
  { story_list with story_counter = succ story_list.story_counter }

let build_grid parameter handler computation_info error trace bool =
  let error, computation_info =
    StoryProfiling.StoryStats.add_event
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error StoryProfiling.Build_grid None computation_info
  in
  let grid = S.PH.B.PB.CI.Po.K.build_grid trace bool handler in
  let error, computation_info =
    StoryProfiling.StoryStats.close_event
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error StoryProfiling.Build_grid None computation_info
  in
  error, computation_info, grid

let store_trace (parameter : parameter) ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler
    computation_info error trace obs_info story_table =
  let error, computation_info =
    StoryProfiling.StoryStats.add_event
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error StoryProfiling.Store_trace None computation_info
  in
  let pretrace = get_pretrace_of_trace trace in
  let trace2 = get_compressed_trace trace in
  let bool = not (is_compressed_trace trace) in
  let error, computation_info, grid =
    build_grid parameter handler computation_info error trace2 bool
  in
  let computation_info = P.set_grid_generation computation_info in
  let story_info =
    List.rev_map
      (Trace.Simulation_info.update_profiling_info (P.copy computation_info))
      (List.rev obs_info)
  in
  let error, computation_info, story_list =
    D.add_story parameter handler computation_info error grid pretrace
      story_info story_table.story_list
  in
  let story_table =
    { story_list; story_counter = story_table.story_counter + 1 }
  in
  let error, computation_info =
    StoryProfiling.StoryStats.close_event
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      error StoryProfiling.Store_trace None computation_info
  in

  error, computation_info, story_table

let flatten_story_table parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error story_table =
  let error, log_info, list =
    D.hash_list parameter handler log_info error story_table.story_list
  in
  error, log_info, { story_table with story_list = list }

let compress ?heuristic parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error trace =
  match parameter.S.PH.B.PB.CI.Po.K.H.current_compression_mode with
  | None -> error, log_info, [ trace ]
  | Some Story_json.Causal ->
    let () =
      if
        S.PH.B.PB.CI.Po.K.H.is_server_mode parameter
        && S.PH.B.PB.CI.Po.K.H.is_server_channel_on parameter
      then
        S.PH.B.PB.CI.Po.K.H.push_json parameter
          (Story_json.Phase
             (Story_json.Inprogress, "Start one causal compression"))
    in
    let error, log_info, trace =
      cut parameter ~shall_we_compute:we_shall handler log_info error trace
    in
    error, log_info, [ trace ]
  | Some (Story_json.Weak | Story_json.Strong) ->
    let event, s =
      match parameter.S.PH.B.PB.CI.Po.K.H.current_compression_mode with
      | Some Story_json.Weak ->
        StoryProfiling.Weak_compression, "Start one weak compression"
      | _ -> StoryProfiling.Strong_compression, "Start one strong compression"
    in
    let () =
      if
        S.PH.B.PB.CI.Po.K.H.is_server_mode parameter
        && S.PH.B.PB.CI.Po.K.H.is_server_channel_on parameter
      then
        S.PH.B.PB.CI.Po.K.H.push_json parameter
          (Story_json.Phase (Story_json.Inprogress, s))
    in
    let error, log_info =
      P.add_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
        error event
        (Some (fun () -> size_of_pretrace trace))
        log_info
    in
    let event_list = get_pretrace_of_trace trace in
    let error, log_info, blackboard =
      S.PH.B.import ?heuristic parameter handler log_info error event_list
    in
    let error, log_info, list =
      S.PH.forced_events parameter handler log_info error blackboard
    in
    let list_order =
      match list with
      | (list_order, _, _) :: _ -> list_order
      | _ -> []
    in
    let error, log_info, (blackboard, output, list) =
      S.compress parameter handler log_info error blackboard list_order
    in
    let list =
      List.rev_map
        (fun pretrace ->
          let event_list = S.translate_result pretrace in
          let event_list = S.PH.B.PB.CI.Po.K.clean_events event_list in
          build_compressed_trace event_list pretrace)
        list
    in
    let log_info = P.set_story_research_time log_info in
    let error, log_info =
      P.close_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
        error event
        (Some (fun () -> size_of_pretrace trace))
        log_info
    in
    let error, () =
      if S.PH.B.is_failed output then
        Exception.warn
          (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
          error __POS__ ~message:"One compression has failed" Exit ()
      else
        error, ()
    in
    let error =
      if debug_mode then (
        let () =
          Loggers.fprintf parameter.S.PH.B.PB.CI.Po.K.H.logger_err
            "\t\t * result"
        in
        let error =
          if S.PH.B.is_failed output then (
            let error, _log_info, () =
              S.PH.B.export_blackboard_to_xls parameter handler log_info error
                "FAIL" 0 0 blackboard
            in
            let () =
              Loggers.fprintf parameter.S.PH.B.PB.CI.Po.K.H.logger_err
                "Fail_to_compress"
            in
            error
          ) else (
            let () =
              Loggers.fprintf parameter.S.PH.B.PB.CI.Po.K.H.logger_err
                "Succeed_to_compress"
            in
            error
          )
        in
        error
      ) else
        error
    in
    error, log_info, list

let strongly_compress ?heuristic parameter =
  compress ?heuristic (S.PH.B.PB.CI.Po.K.H.set_compression_strong parameter)

let weakly_compress ?heuristic parameter =
  compress ?heuristic (S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter)

let convert_trace_into_grid trace handler =
  let event_list = get_compressed_trace trace in
  S.PH.B.PB.CI.Po.K.build_grid event_list
    (not (is_compressed_trace trace))
    handler

let convert_trace_into_musical_notation parameters ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ kappa_handler
    profiling_info error trace =
  S.PH.B.import parameters kappa_handler profiling_info error
    (get_pretrace_of_trace trace)

let enrich_grid_with_transitive_closure config logger ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error grid =
  let error, log_info, output =
    Causal.enrich_grid
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters logger)
      handler log_info error config grid
  in
  error, log_info, output

let enrich_grid_with_transitive_past_of_observables_with_a_progress_bar =
  enrich_grid_with_transitive_closure
    Graph_closure.config_big_graph_with_progress_bar

let enrich_grid_with_transitive_past_of_observables_without_a_progress_bar =
  enrich_grid_with_transitive_closure
    Graph_closure.config_big_graph_without_progress_bar

let _enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar =
  enrich_grid_with_transitive_closure
    Graph_closure.config_big_graph_without_progress_bar

let enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar =
  enrich_grid_with_transitive_closure Graph_closure.config_small_graph

let sort_story_list = D.sort_list

let export_story_table parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error x =
  let a, log_info, b =
    sort_story_list parameter handler log_info error (get_stories x)
  in
  a, log_info, b

let has_obs x = List.exists Trace.step_is_obs (get_pretrace_of_trace x)

let fold_left_with_progress_bar ?(event = StoryProfiling.Dummy) parameter
    ?(shall_we_compute = we_shall)
    ?(shall_we_compute_profiling_information = we_shall)
    ?(print_if_zero = we_shall) handler profiling_information error
    (f : ('a, 'b, 'a) binary) a list =
  let n = List.length list in
  let string, (error, profiling_information) =
    if StoryProfiling.StoryStats.is_dummy event then
      "", (error, profiling_information)
    else
      ( StoryProfiling.string_of_step_kind event,
        StoryProfiling.StoryStats.add_event
          (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
          error event
          (Some (fun _ -> List.length list))
          profiling_information )
  in
  let progress_bar =
    Tick_stories.tick_stories
      (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
      Configuration.empty
      (Remanent_parameters.save_progress_bar
         (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
      (false, 0, 0, n)
  in
  let error, profiling_information, _, _, a, n_fail =
    let rec aux list (error, profiling_information, bar, k, a, n_fail) =
      let event = StoryProfiling.Story k in
      match list with
      | [] -> error, profiling_information, bar, k, a, n_fail
      | x :: tail ->
        let error, profiling_information =
          P.add_event
            (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
            error event None profiling_information
        in
        let output_opt =
          try
            let error, profiling_information, a' =
              f parameter ~shall_we_compute
                ~shall_we_compute_profiling_information ~print_if_zero handler
                profiling_information error a x
            in
            let bar =
              Tick_stories.tick_stories
                (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
                Configuration.empty
                (Remanent_parameters.save_progress_bar
                   (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
                bar
            in
            let n_fail = inc_fails a a' n_fail in
            let error, profiling_information =
              P.close_event
                (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
                error event None profiling_information
            in
            Some (error, profiling_information, bar, k + 1, a', n_fail)
          with Sys.Break -> None
        in
        (match output_opt with
        | None ->
          let error, profiling_information =
            P.close_event
              (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
              error event None profiling_information
          in
          error, profiling_information, bar, k + 1, a, n_fail
        | Some remanent -> aux tail remanent)
    in
    aux list (error, profiling_information, progress_bar, 1, a, 0)
  in
  let () = close_progress_bar_opt (S.PH.B.PB.CI.Po.K.H.get_logger parameter) in
  let error, profiling_information =
    if StoryProfiling.StoryStats.is_dummy event then
      error, profiling_information
    else
      StoryProfiling.StoryStats.close_event
        (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
        error event None profiling_information
  in
  let () = print_fails parameter.S.PH.B.PB.CI.Po.K.H.logger_err string n_fail in
  error, profiling_information, a

let _fold_over_the_causal_past_of_observables_through_a_grid_with_a_progress_bar
    parameter handler log_info error f t a =
  let grid = convert_trace_into_grid t handler in
  Causal.fold_over_causal_past_of_obs parameter handler log_info error
    Graph_closure.config_big_graph_with_progress_bar grid f (error, log_info, a)

let fold_over_the_causal_past_of_observables_with_a_progress_bar parameter
    ?shall_we_compute:_ ?shall_we_compute_profiling_information:_
    ?print_if_zero:_ handler log_info error log_step debug_mode counter f t a =
  let () =
    if log_step parameter then
      Loggers.fprintf
        (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
        "%s\t- blackboard generation @."
        (Remanent_parameters.get_prefix
           (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
  in
  let error, log_info, blackboard =
    convert_trace_into_musical_notation parameter handler log_info error t
  in
  let error, log_info, list =
    extract_observable_hits_from_musical_notation parameter handler log_info
      error blackboard
  in
  let n_stories = List.length list in
  let () =
    if log_step parameter then
      Loggers.fprintf
        (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
        "%s\t- computing causal past of each observed events (%i)@."
        (Remanent_parameters.get_prefix
           (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
        n_stories
  in
  (* generation of uncompressed stories *)
  let () =
    if debug_mode parameter then
      Loggers.fprintf
        (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
        "%s\t\t* causal compression "
        (Remanent_parameters.get_prefix
           (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
  in
  let log_info = P.set_start_compression log_info in
  let grid = convert_trace_into_grid t handler in
  let output =
    Causal.fold_over_causal_past_of_obs
      (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
      handler log_info error Graph_closure.config_big_graph_with_progress_bar
      grid
      (fun parameter' handler log_info error observable_hit causal_past
           (counter, list, a) ->
        match list with
        | [] -> error, log_info, Stop.success (counter, list, a)
        | head :: tail ->
          let error, log_info =
            StoryProfiling.StoryStats.add_event parameter' error
              (StoryProfiling.Story counter) None log_info
          in
          let observable_id = head in
          let log_info = P.reset_log log_info in
          let () =
            if debug_mode parameter then
              Loggers.fprintf
                (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
                "%s\t\t* causal compression "
                (Remanent_parameters.get_prefix
                   (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter))
          in
          (* we translate the list of event ids into a trace thanks to the blackboad *)
          let error, log_info, trace =
            translate parameter handler log_info error blackboard
              (List.rev_map S.PH.B.PB.step_id_of_int
                 (observable_hit :: causal_past))
          in
          (* we collect run time info about the observable *)
          let info =
            match get_runtime_info_from_observable_hit observable_id with
            | None -> []
            | Some info ->
              let info =
                { info with Trace.Simulation_info.story_id = counter }
              in
              let info =
                Trace.Simulation_info.update_profiling_info log_info info
              in
              [ info ]
          in
          let error, log_info, a =
            (f : ('a, 'b, 'c, 'd) ternary)
              parameter handler log_info error trace info a
          in
          let error, log_info =
            StoryProfiling.StoryStats.close_event parameter' error
              (StoryProfiling.Story counter) None log_info
          in
          ( error,
            log_info,
            Stop.success_or_stop
              (fun a -> Stop.success (counter + 1, tail, a))
              (fun b -> Stop.stop (b, counter))
              a ))
      (counter, List.rev list, a)
  in
  Stop.success_or_stop
    (fun (error, log_info, (_, _, a)) -> error, log_info, Stop.success a)
    (fun (error, log_info, (a, counter)) ->
      error, log_info, Stop.stop (a, counter))
    output

let copy_log_info = P.copy

type canonical_form = Dag.canonical_form

let compare_canonical_form = Dag.compare_canonic

let compute_canonical_form parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error trace =
  let grid = convert_trace_into_grid trace handler in
  let error, log_info, graph =
    Dag.graph_of_grid parameter handler log_info error grid
  in
  let error, log_info, canonic =
    Dag.canonicalize parameter handler log_info error graph
  in
  (error : error_log), (log_info : profiling_info), (canonic : canonical_form)

module Event : Black_list.Event with type event = step = struct
  type event = step
  type eid = int
  type 'a t = 'a array

  let key_of_event event =
    if
      (Trace.step_is_rule event || Trace.step_is_pert event)
      && not (Trace.has_creation_of_step event)
    then
      get_id_of_event event
    else
      None

  let init eid default = Array.make eid default

  let set t eid value =
    let () = Array.set t eid value in
    t

  let get t eid = Array.get t eid
end

module BlackList : Black_list.Blacklist with type Event.event = step =
Black_list.Make ((Event : Black_list.Event with type event = step))

type black_list = BlackList.t

let create_black_list n = BlackList.init n

let black_list _parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ _handler log_info
    error trace blacklist =
  let blacklist =
    List.fold_left
      (fun blacklist (event, _) -> BlackList.black_list event blacklist)
      blacklist
      (get_compressed_trace trace)
  in
  error, log_info, blacklist

let remove_blacklisted_event _handler log_info error blacklist trace =
  let list, int =
    List.fold_left
      (fun (trace, int) event ->
        if BlackList.is_black_listed event blacklist then
          trace, succ int
        else
          event :: trace, int)
      ([], 0) trace
  in
  error, log_info, (List.rev list, int)

let remove_blacklisted_event parameter ?shall_we_compute:_
    ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler log_info
    error blacklist trace =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (fun _parameter handler log_info error ->
         remove_blacklisted_event handler log_info error blacklist)
       None "" StoryProfiling.Removing_blacklisted_events)
    Do_not_care Neutral parameter handler log_info error trace

let last_eid_in_pretrace trace =
  let l = List.rev (get_pretrace_of_trace trace) in
  let rec aux = function
    | [] -> 0
    | h :: t ->
      (match get_id_of_event h with
      | None -> aux t
      | Some eid -> eid)
  in
  aux l

let pop_json = S.PH.B.PB.CI.Po.K.H.pop_json

let profiling_state_to_json parameters =
  `Assoc
    [
      ( "profiling information",
        Loggers.to_json (S.PH.B.PB.CI.Po.K.H.get_profiling_logger parameters) );
    ]

let error_list_to_json parameters =
  `Assoc
    [
      ( "errors",
        Loggers.to_json (S.PH.B.PB.CI.Po.K.H.get_debugging_channel parameters) );
    ]

let computation_steps_to_json parameters =
  `Assoc
    [
      ( "computation steps",
        Loggers.to_json (S.PH.B.PB.CI.Po.K.H.get_logger parameters) );
    ]
