(*
  * utilities.mli
  *
  * Creation:                      <2015-08-10 09:21:53 feret>
  * Last modification: Time-stamp: <2016-02-03 20:29:44 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS
  *
  * *
  * Some functionalities for story compression
  *
  * Copyright 2011,2012,2013,2014,2015 Institut National de Recherche en
  * Informatique et en Automatique.  All rights reserved.  This file is
  * distributed under the terms of the GNU Library General Public License *)

(** High-level elementary primitives to generate stories *)

module S : Generic_branch_and_cut_solver.Solver

type error_log = Exception.method_handler

val error_init : error_log
(** error_init is an empty log of errors *)

type parameter = S.PH.B.PB.CI.Po.K.H.parameter
type kappa_handler = S.PH.B.PB.CI.Po.K.H.handler
type profiling_info = StoryProfiling.StoryStats.log_info
type shall_we = parameter -> bool

(** enriched types for functions: *)

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

val fold_left_with_progress_bar :
  ?event:StoryProfiling.step_kind ->
  (('a, 'b, 'a) binary, 'a, 'b list, 'a) ternary

(* Json interactions *)

val pop_json :
  parameter -> StoryProfiling.StoryStats.log_info Story_json.message option

val profiling_state_to_json : parameter -> Yojson.Basic.t
val error_list_to_json : parameter -> Yojson.Basic.t
val computation_steps_to_json : parameter -> Yojson.Basic.t

(* on the fly causal compression *)
type on_the_fly_cut_state

val on_the_fly_cut_init : on_the_fly_cut_state

val on_the_fly_cut_step :
  on_the_fly_cut_state -> Trace.step -> on_the_fly_cut_state

val on_the_fly_cut_finalize : on_the_fly_cut_state -> Trace.step list

val cut_rev_trace :
  Trace.step list (*reverse order*) -> Trace.step list (* correct order *)

type trace
(** traces *)

type trace_runtime_info = profiling_info Trace.Simulation_info.t
(** Runtime information about a trace provided by the simulator*)

val size_of_pretrace : trace -> int
val last_eid_in_pretrace : trace -> int
val print_trace : parameter -> kappa_handler -> trace -> unit

val has_obs : trace -> bool
(** check wether there is an observable in a trace *)

val trace_of_pretrace : Trace.t -> trace
(** convert a list of refined steps into a trace *)

val get_pretrace_of_trace : trace -> Trace.t
(** conversely, convert a trace in a list of refined steps *)

val get_simulation_time_of_event : Trace.step -> float option
(** get the date of an event in the simulation
    (only proper events have a time) *)

val get_id_of_event : Trace.step -> int option
(** get the id of an event in the simulation
    (only proper events have an id) *)

val remove_events_after_last_obs : (trace, trace) unary
(** remove the events after the last observable *)

val split_init : (trace, trace) unary
(** split_init split init event agent-wise *)

val fill_siphon : (trace, trace) unary
(** fill_siphon adds spurious init event, to break causal dependences;
    Currently, it inserts an init event when an agent return to its initial state;
    other heuristics may be considered;
    The output has to be  disanbiguated, otherwise it is useless (compression will remove the ficitious init events)
    It should work in quasi linear time (I think)*)

val cut : (trace, trace) unary
(** cut performs partial order reduction and remove orthogonal events *)

val remove_pseudo_inverse_events : (trace, trace) unary
(** remove_pseudo_inverse_events removes pseudo inverse events *)

val remove_obs_before : (trace_runtime_info list option, trace, trace) binary
(** remove_obs_before removes the observable_hits before the simulation info prodided as a first argument*)

val make_unambiguous : (trace, trace) unary
(** reallocate agent id to avoid conflict (implicitly called by cut and fill_siphon) *)

val weakly_compress :
  ?heuristic:Priority.priorities -> (trace, trace list) unary
(** compute the weak compression of a given trace,
    if parameter.compute_all_stories, then each minimal stories is computed,
    otherwise, only the first found one is provided;
    the optional argument heuristic can be used to redefined the heuristic that
    select which event to try to discard first. *)

val strongly_compress :
  ?heuristic:Priority.priorities -> (trace, trace list) unary
(** compute the strong compression of a given trace,
    if parameter.compute_all_stories, then each minimal stories is computed,
    otherwise, only the first found one is provided;
    the optional argument heuristic can be used to redefined the heuristic that
    select which event to try to discard first. *)

val fold_over_the_causal_past_of_observables_with_a_progress_bar :
  ( shall_we,
    shall_we,
    int,
    (trace, trace_runtime_info list, 'a, ('a, 'b) Stop.stop) ternary,
    trace,
    'a,
    ('a, 'b * int) Stop.stop )
  sexternary
(** fold over the causal past of each observable in a given trace,
    the first argument indicates whether we display the current steps on the err output;
    the second arfument indicated whether the function is launched in debug mode or not *)

type black_list

val create_black_list : int -> black_list
val black_list : (trace, black_list, black_list) binary
val remove_blacklisted_event : (black_list, trace, trace) binary

type story_table
(** Story table *)

val create_story_table : story_table zeroary
(** Initialization *)

val count_stories : story_table -> int
(** Give the number of stories (up to isomorphism classes) stored in a table *)

val store_trace :
  (trace, trace_runtime_info list, story_table, story_table) ternary
(** Store trace in story table *)

(** Apply a function on each trace (and each list of runtime information associated to this trace),
    the string contains the message to display in case of faillure of one call of the ternary function*)

val fold_story_table_with_progress_bar :
  ( string,
    (trace, trace_runtime_info list, 'a, 'a) ternary,
    story_table,
    'a,
    'a )
  quaternary

val fold_story_table_without_progress_bar :
  ( string,
    (trace, trace_runtime_info list, 'a, 'a) ternary,
    story_table,
    'a,
    'a )
  quaternary
(** Apply a function on each trace (and each list of runtime information associated to this trace),
    the string contains the message to display in case of faillure of one call of the ternary function*)

val flatten_story_table : (story_table, story_table) unary
(** put together the stories having the same canonic form, this has do be done explicitely on the moment, I will improve this soon*)

val export_story_table :
  (story_table, (Trace.t * Causal.grid * trace_runtime_info list) list) unary
(** convert a table into a list of grid (with runtime information)*)

(** The following functions are for expert only *)

val compress : ?heuristic:Priority.priorities -> (trace, trace list) unary
(** compress a trace with the level of abstraction defined in the argument parameter.
    The optional argument heuristic can be used to tune the heuristic that select which
    event will be tried to be discarded first. *)

val copy_log_info :
  StoryProfiling.StoryStats.log_info -> StoryProfiling.StoryStats.log_info

type cflow_grid = Causal.grid
type enriched_cflow_grid = Causal.enriched_grid

val convert_trace_into_grid : trace -> kappa_handler -> cflow_grid

val enrich_grid_with_transitive_past_of_observables_with_a_progress_bar :
  (cflow_grid, enriched_cflow_grid) unary
(** compute transitive closure with different parameters (progress_bar, gc) *)

val enrich_grid_with_transitive_past_of_observables_without_a_progress_bar :
  (cflow_grid, enriched_cflow_grid) unary

val enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar :
  (cflow_grid, enriched_cflow_grid) unary

type canonical_form
(** Cannonic forms *)

val compare_canonical_form : canonical_form -> canonical_form -> int
val compute_canonical_form : (trace, canonical_form) unary

type musical_grid
(** Blackboard with debugging utilities *)

type observable_hit

val get_runtime_info_from_observable_hit :
  observable_hit -> unit Trace.Simulation_info.t option

(** Musical processing *)

val convert_trace_into_musical_notation : (trace, musical_grid) unary

val extract_observable_hits_from_musical_notation :
  (musical_grid, observable_hit list) unary

val extract_observable_hit_from_musical_notation :
  (string, musical_grid, observable_hit) binary

val causal_prefix_of_an_observable_hit :
  (string, musical_grid, enriched_cflow_grid, observable_hit, trace) quaternary

val export_musical_grid_to_xls :
  (string, int, int, musical_grid, unit) quaternary
(** Show the current status of the branch and cut assumptions in a libreoffice macro file *)

val print_musical_grid : (musical_grid, unit) unary
(** Show the current status of the branch and cut assumptions in ASCII *)

val get_counter : story_table -> int (* to be removed from the interface*)
