(**
  * blackboard.ml
  *
  * Creation:                      <2011-09-05 feret>
  * Last modification: Time-stamp: <2016-02-03 20:48:34 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let debug_mode = false

module type Blackboard = sig
  module PB : Blackboard_generation.PreBlackboard

  type event_case_address
  (** blackboard matrix *)

  type case_info
  type case_value
  type case_address
  type pointer

  (** blackboard*)

  type blackboard (*blackboard, once finalized*)
  type assign_result

  val is_failed : assign_result -> bool
  val is_succeeded : assign_result -> bool
  val is_ignored : assign_result -> bool
  val success : assign_result
  val ignore : assign_result
  val fail : assign_result
  val predicate_id_of_case_address : event_case_address -> PB.predicate_id
  val build_pointer : PB.step_short_id -> pointer
  val is_before_blackboard : pointer -> bool
  val get_event : blackboard -> PB.step_id -> Trace.step
  val get_n_eid : blackboard -> int
  val get_npredicate_id : blackboard -> int

  val get_n_unresolved_events_of_pid_by_level :
    blackboard -> PB.predicate_id -> Priority.level -> int

  val get_n_unresolved_events_of_pid : blackboard -> PB.predicate_id -> int
  val get_n_unresolved_events : blackboard -> int

  val get_first_linked_event :
    blackboard -> PB.predicate_id -> PB.step_short_id option

  val get_last_linked_event :
    blackboard -> PB.predicate_id -> PB.step_short_id option

  val get_stack_depth : blackboard -> int

  val is_selected_event :
    (PB.step_id, blackboard, bool option) PB.CI.Po.K.H.binary

  val case_address_of_case_event_address : event_case_address -> case_address

  val predicate_value_of_case_value :
    (case_value, PB.predicate_value) PB.CI.Po.K.H.unary

  val follow_pointer_up :
    (blackboard, event_case_address, event_case_address) PB.CI.Po.K.H.binary

  val follow_pointer_down :
    (blackboard, event_case_address, event_case_address) PB.CI.Po.K.H.binary

  val is_boundary : (blackboard, event_case_address, bool) PB.CI.Po.K.H.binary

  val build_event_case_address :
    PB.predicate_id -> pointer -> event_case_address

  val exist_case :
    (blackboard, event_case_address, bool option) PB.CI.Po.K.H.binary

  val get_static :
    ( blackboard,
      event_case_address,
      PB.step_short_id * PB.step_id * PB.predicate_value * PB.predicate_value
    )
    PB.CI.Po.K.H.binary

  val set :
    (case_address, case_value, blackboard, blackboard) PB.CI.Po.K.H.ternary

  val get : (case_address, blackboard, case_value) PB.CI.Po.K.H.binary
  val dec : (case_address, blackboard, blackboard) PB.CI.Po.K.H.binary

  val overwrite :
    (case_address, case_value, blackboard, blackboard) PB.CI.Po.K.H.ternary

  val refine :
    ( case_address,
      case_value,
      blackboard,
      blackboard * assign_result )
    PB.CI.Po.K.H.ternary

  val branch : (blackboard, blackboard) PB.CI.Po.K.H.unary
  val reset_last_branching : (blackboard, blackboard) PB.CI.Po.K.H.unary
  val reset_init : (blackboard, blackboard) PB.CI.Po.K.H.unary

  val import :
    ?heuristic:Priority.priorities ->
    (Trace.step list, blackboard) PB.CI.Po.K.H.unary
  (** initialisation*)

  type result = (Trace.step * PB.CI.Po.K.side_effect) list
  (** output result*)

  val is_maximal_solution : (blackboard, bool) PB.CI.Po.K.H.unary
  (** iteration*)

  val translate_blackboard : (blackboard, result) PB.CI.Po.K.H.unary
  (** exporting result*)

  val print_blackboard : (blackboard, unit) PB.CI.Po.K.H.unary
  (**pretty printing*)

  val export_blackboard_to_xls :
    (string, int, int, blackboard, unit) PB.CI.Po.K.H.quaternary

  val print_event_case_address :
    (blackboard, event_case_address, unit) PB.CI.Po.K.H.binary

  val print_stack : (blackboard, unit) PB.CI.Po.K.H.unary
  val exist : event_case_address -> case_address
  val boolean : bool option -> case_value
  val pointer_to_previous : event_case_address -> case_address
  val pointer_to_next : event_case_address -> case_address
  val pointer : event_case_address -> case_value
  val value_after : event_case_address -> case_address

  val case_list_of_eid :
    (blackboard, PB.step_id, event_case_address list) PB.CI.Po.K.H.binary

  val state : PB.predicate_value -> case_value
  val is_exist_event : PB.step_id -> case_address
  val n_unresolved_events_at_level : Priority.level -> case_address
  val n_unresolved_events : case_address

  val n_unresolved_events_in_column_at_level :
    event_case_address -> Priority.level -> case_address

  val n_unresolved_events_in_column : event_case_address -> case_address

  val forced_events :
    blackboard -> (PB.step_id list * unit Trace.Simulation_info.t option) list

  val side_effect_of_event : blackboard -> PB.step_id -> PB.CI.Po.K.side_effect

  val cut :
    ( blackboard,
      PB.step_id list,
      blackboard * PB.step_id list )
    PB.CI.Po.K.H.binary

  val tick :
    StoryProfiling.StoryStats.log_info ->
    bool * StoryProfiling.StoryStats.log_info
  (* to do: move to the module StoryProfiling.StoryStats*)

  val level_of_event :
    (blackboard, PB.step_id, Priority.level) PB.CI.Po.K.H.binary
end

module Blackboard : Blackboard = struct
  module PB = Blackboard_generation.Preblackboard
  (** blackboard matrix*)

  type assign_result = Fail | Success | Ignore
  type pointer = PB.step_short_id

  let warn parameter log_info error pos ?(message = "") exn default =
    let error, a =
      Exception.warn
        (PB.CI.Po.K.H.get_kasa_parameters parameter)
        error pos ~message exn default
    in
    error, log_info, a

  let success = Success
  let ignore = Ignore
  let fail = Fail

  let is_ignored x =
    match x with
    | Ignore -> true
    | _ -> false

  let is_failed x =
    match x with
    | Fail -> true
    | Success | Ignore -> false

  let is_succeeded x =
    match x with
    | Success -> true
    | Fail | Ignore -> false

  let null_pointer = PB.dummy_step_short_id
  let is_null_pointer x = x = null_pointer
  let pointer_before_blackboard = PB.zero_step_short_id
  let is_null_pointer_step_id x = x = PB.dummy_step_id
  let is_before_blackboard x = x = pointer_before_blackboard
  let build_pointer i = i

  type event_case_address = {
    column_predicate_id: PB.predicate_id;
    row_short_event_id: pointer;
  }

  let predicate_id_of_case_address x = x.column_predicate_id

  let is_boundary _parameter _handler log_info error _blackboard event_address =
    error, log_info, is_before_blackboard event_address.row_short_event_id

  let build_event_case_address pid seid =
    { column_predicate_id = pid; row_short_event_id = seid }

  type case_address =
    | N_unresolved_events_in_column_at_level of int * Priority.level
    | N_unresolved_events_in_column of int
    | Pointer_to_next of event_case_address
    | Value_after of event_case_address
    | Value_before of event_case_address
    | Pointer_to_previous of event_case_address
    | N_unresolved_events
    | N_unresolved_events_at_level of Priority.level
    | Exist of event_case_address
    | Keep_event of PB.step_id

  let is_exist_event i = Keep_event i

  let n_unresolved_events_in_column i =
    N_unresolved_events_in_column i.column_predicate_id

  let n_unresolved_events_in_column_at_level i j =
    N_unresolved_events_in_column_at_level (i.column_predicate_id, j)

  let pointer_to_next e = Pointer_to_next e
  let value_after e = Value_after e
  let value_before e = Value_before e
  let pointer_to_previous e = Pointer_to_previous e
  let n_unresolved_events = N_unresolved_events
  let n_unresolved_events_at_level i = N_unresolved_events_at_level i
  let exist e = Exist e

  type case_value =
    | State of PB.predicate_value
    | Counter of int
    | Pointer of pointer
    | Boolean of bool option

  let print_case_value parameter x =
    match x with
    | State x ->
      let () =
        Loggers.fprintf (PB.CI.Po.K.H.get_debugging_channel parameter) "State! "
      in
      let () =
        PB.print_predicate_value
          (PB.CI.Po.K.H.get_debugging_channel parameter)
          x
      in
      let () =
        Loggers.print_newline (PB.CI.Po.K.H.get_debugging_channel parameter)
      in
      ()
    | Counter i ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_debugging_channel parameter)
          "Counter %i" i
      in
      let () =
        Loggers.print_newline (PB.CI.Po.K.H.get_debugging_channel parameter)
      in
      ()
    | Pointer i ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_debugging_channel parameter)
          "Pointer %i"
          (PB.int_of_step_short_id i)
      in
      let () =
        Loggers.print_newline (PB.CI.Po.K.H.get_debugging_channel parameter)
      in
      ()
    | Boolean b ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_debugging_channel parameter)
          "Boolean %s"
          (match b with
          | None -> "?"
          | Some true -> "true"
          | _ -> "false")
      in
      let () =
        Loggers.print_newline (PB.CI.Po.K.H.get_debugging_channel parameter)
      in
      ()

  let string_of_pointer seid =
    "event seid " ^ string_of_int (PB.int_of_step_short_id seid)

  let print_pointer log seid = Loggers.fprintf log "%s" (string_of_pointer seid)
  let state predicate_value = State predicate_value
  let pointer p = Pointer p.row_short_event_id
  let boolean b = Boolean b

  let case_address_of_case_event_address event_address =
    Value_after event_address

  let predicate_value_of_case_value parameter _handler log_info error case_value
      =
    match case_value with
    | State x -> error, log_info, x
    | Counter _ | Pointer _ | Boolean _ ->
      let _ = print_case_value parameter case_value in
      warn parameter log_info error __POS__
        ~message:"wrong kind of case_value in predicate_value_of_case_value"
        (Failure "predicate_value_of_case_value") PB.unknown

  type assignment = case_address * case_value

  let bool_strictly_more_refined x y =
    match x, y with
    | Some _, None -> true
    | _, _ -> false

  let g p p2 pos parameter _handler log_info error x y =
    match x, y with
    | State x, State y -> error, log_info, p x y
    | Boolean x, Boolean y -> error, log_info, p2 x y
    | State _, _ | Boolean _, _ | Counter _, _ | Pointer _, _ ->
      let file, line, _, _ = pos in
      let string = file ^ ", line: " ^ string_of_int line in
      warn parameter log_info error pos
        ~message:"Counters and/or Pointers should not be compared"
        (Failure (string ^ " Comparison between pointers and counters"))
        false

  let strictly_more_refined =
    g PB.strictly_more_refined bool_strictly_more_refined __POS__

  type case_info_static = {
    row_short_id: PB.step_short_id;
    event_id: PB.step_id;
    test: PB.predicate_value;
    action: PB.predicate_value;
  }

  type case_info_dynamic = {
    pointer_previous: pointer;
    pointer_next: pointer;
    state_after: PB.predicate_value;
    selected: bool option;
  }

  type case_info = { static: case_info_static; dynamic: case_info_dynamic }

  let dummy_case_info_static =
    {
      row_short_id = PB.dummy_step_short_id;
      event_id = PB.dummy_step_id;
      test = PB.unknown;
      action = PB.unknown;
    }

  let dummy_case_info_dynamic =
    {
      pointer_previous = null_pointer;
      pointer_next = null_pointer;
      state_after = PB.unknown;
      selected = None;
    }

  let correct_pointer seid size =
    let int_seid = PB.int_of_step_short_id seid in
    if int_seid < 0 then
      pointer_before_blackboard
    else if int_seid >= size then
      PB.step_short_id_of_int size
    else
      seid

  let init_info_dynamic seid size =
    {
      pointer_previous = correct_pointer (PB.dec_step_short_id seid) size;
      pointer_next = correct_pointer (PB.inc_step_short_id seid) size;
      state_after = PB.unknown;
      selected = None;
    }

  let init_info_static _p_id seid (eid, _, test, action) =
    { row_short_id = seid; event_id = eid; test; action }

  let get_eid_of_triple (x, _, _, _) = x

  let dummy_case_info =
    { static = dummy_case_info_static; dynamic = dummy_case_info_dynamic }

  let init_info p_id seid size triple =
    {
      static = init_info_static p_id seid triple;
      dynamic = init_info_dynamic seid size;
    }

  type stack = assignment list
  (** blackboard *)

  type blackboard = {
    event: Trace.step PB.A.t;
    pre_column_map_inv: PB.predicate_info PB.A.t;
        (** maps each wire id to its wire label *)
    forced_events: (PB.step_id list * unit Trace.Simulation_info.t option) list;
    n_predicate_id: int;
    n_eid: int;
    n_seid: int PB.A.t;
    current_stack: stack;
    stack: stack list;
    blackboard: case_info PB.A.t PB.A.t;
    selected_events: bool option PB.A.t;
    weigth_of_predicate_id: int PB.A.t;
    weigth_of_predicate_id_by_level: int PB.A.t Priority.LevelMap.t;
    used_predicate_id: bool PB.A.t;
    n_unresolved_events: int;
    n_unresolved_events_by_level: int Priority.LevelMap.t;
    last_linked_event_of_predicate_id: PB.step_short_id PB.A.t;
    event_case_list: event_case_address list PB.A.t;
    side_effect_of_event: PB.CI.Po.K.side_effect PB.A.t;
    fictitious_observable: PB.step_id option;
    level_of_event: Priority.level PB.A.t;
  }

  let tick profiling_info = StoryProfiling.StoryStats.tick profiling_info

  let level_of_event parameter _handler log_info error blackboard eid =
    try
      ( error,
        log_info,
        PB.A.get blackboard.level_of_event (PB.int_of_step_id eid) )
    with Not_found ->
      warn parameter log_info error __POS__ ~message:"Unknown event id"
        (Failure "Unknown event id") Priority.highest

  let get_event blackboard k = PB.A.get blackboard.event (PB.int_of_step_id k)
  let get_n_eid blackboard = blackboard.n_eid
  let get_stack_depth blackboard = List.length blackboard.stack
  let forced_events blackboard = blackboard.forced_events

  let side_effect_of_event blackboard i =
    PB.A.get blackboard.side_effect_of_event (PB.int_of_step_id i)

  let case_list_of_eid parameter _handler log_info error blackboard eid =
    try
      ( error,
        log_info,
        PB.A.get blackboard.event_case_list (PB.int_of_step_id eid) )
    with _ ->
      warn parameter log_info error __POS__
        ~message:"Dereferencing Null pointer"
        (Failure "Dereferencing null pointer") []

  let get_case parameter _handler log_info error case_address blackboard =
    try
      ( error,
        log_info,
        PB.A.get
          (PB.A.get blackboard.blackboard case_address.column_predicate_id)
          (PB.int_of_step_short_id case_address.row_short_event_id) )
    with _ ->
      warn parameter log_info error __POS__
        ~message:"Dereferencing Null pointer"
        (Failure "Dereferencing null pointer") dummy_case_info

  let get_static parameter handler log_info error blackboard address =
    let error, log_info, case =
      get_case parameter handler log_info error address blackboard
    in
    let static = case.static in
    ( error,
      log_info,
      (static.row_short_id, static.event_id, static.test, static.action) )

  let print_event_case_address parameter handler log_info error blackboard case
      =
    let error, log_info, (_, eid, _, _) =
      get_static parameter handler log_info error blackboard case
    in
    let () =
      Loggers.fprintf
        (PB.CI.Po.K.H.get_logger parameter)
        "Event: %i, Predicate: %i@." (PB.int_of_step_id eid)
        (predicate_id_of_case_address case)
    in
    error, log_info, ()

  let print_case_address parameter handler log_info error blackboard x =
    match x with
    | N_unresolved_events_in_column_at_level (i, j) ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_logger parameter)
          "n_unresolved_events_in_pred %i %s@." i
          (Priority.string_of_level j)
      in
      error, log_info, ()
    | N_unresolved_events_in_column i ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_logger parameter)
          "n_unresolved_events_in_pred %i @." i
      in
      error, log_info, ()
    | Pointer_to_next e ->
      let () = Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Pointer" in
      print_event_case_address parameter handler log_info error blackboard e
    | Value_after e ->
      let () =
        Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Value_after  "
      in
      print_event_case_address parameter handler log_info error blackboard e
    | Value_before e ->
      let () =
        Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Value_before "
      in
      print_event_case_address parameter handler log_info error blackboard e
    | Pointer_to_previous e ->
      let () =
        Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Pointer_before "
      in
      print_event_case_address parameter handler log_info error blackboard e
    | N_unresolved_events_at_level i ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_debugging_channel parameter)
          "Unresolved_events_at_level %s"
          (Priority.string_of_level i)
      in
      error, log_info, ()
    | N_unresolved_events ->
      let _ =
        Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Unresolved_events"
      in
      error, log_info, ()
    | Exist e ->
      let () = Loggers.fprintf (PB.CI.Po.K.H.get_logger parameter) "Exist " in
      print_event_case_address parameter handler log_info error blackboard e
    | Keep_event i ->
      let () =
        Loggers.fprintf
          (PB.CI.Po.K.H.get_logger parameter)
          "Keep %i" (PB.int_of_step_id i)
      in
      error, log_info, ()

  let get_npredicate_id blackboard = blackboard.n_predicate_id

  let get_n_unresolved_events_of_pid_by_level blackboard pid level =
    match
      Priority.LevelMap.find_option level
        blackboard.weigth_of_predicate_id_by_level
    with
    | Some x -> PB.A.get x pid
    | None -> 0

  let get_n_unresolved_events_of_pid blackboard pid =
    PB.A.get blackboard.weigth_of_predicate_id pid

  let get_n_unresolved_events blackboard = blackboard.n_unresolved_events
  let get_pointer_next case = case.dynamic.pointer_next

  let follow_pointer_down parameter handler log_info error blackboard address =
    let error, log_info, case =
      get_case parameter handler log_info error address blackboard
    in
    ( error,
      log_info,
      { address with row_short_event_id = case.dynamic.pointer_next } )

  let follow_pointer_up parameter handler log_info error blackboard address =
    let error, log_info, case =
      get_case parameter handler log_info error address blackboard
    in
    ( error,
      log_info,
      { address with row_short_event_id = case.dynamic.pointer_previous } )

  let get_first_linked_event blackboard pid =
    if pid < 0 || pid >= blackboard.n_predicate_id then
      None
    else
      Some PB.zero_step_short_id

  let get_last_linked_event blackboard pid =
    if pid < 0 || pid >= blackboard.n_predicate_id then
      None
    else
      Some (PB.A.get blackboard.last_linked_event_of_predicate_id pid)

  (**pretty printing*)
  let print_known_case log pref inf suf case =
    let _ = Loggers.fprintf log "%stest:" pref in
    let _ = PB.print_predicate_value log case.static.test in
    let _ =
      Loggers.fprintf log "/eid:%i/action:"
        (PB.int_of_step_id case.static.event_id)
    in
    let _ = PB.print_predicate_value log case.static.action in
    let _ = Loggers.fprintf log "%s" inf in
    let _ = PB.print_predicate_value log case.dynamic.state_after in
    let _ = Loggers.fprintf log "%s" suf in
    ()

  let print_case log case =
    let status = case.dynamic.selected in
    match status with
    | Some false -> ()
    | Some true -> print_known_case log "" " " " " case
    | None -> print_known_case log "?(" ") " " " case

  let print_address parameter handler log_info error blackboard address =
    let log = PB.CI.Po.K.H.get_debugging_channel parameter in
    match address with
    | Keep_event i ->
      let () =
        Loggers.fprintf log "Is the event %i selected ? " (PB.int_of_step_id i)
      in
      error, log_info, ()
    | Exist i ->
      let () = Loggers.fprintf log "Is the case " in
      let error, log_info, () =
        print_event_case_address parameter handler log_info error blackboard i
      in
      let () = Loggers.fprintf log "selected ? " in
      error, log_info, ()
    | N_unresolved_events_in_column_at_level (i, j) ->
      let () =
        Loggers.fprintf log
          "Number of unresolved events for the predicate %i at level %s" i
          (Priority.string_of_level j)
      in
      error, log_info, ()
    | N_unresolved_events_in_column i ->
      let () =
        Loggers.fprintf log "Number of unresolved events for the predicate %i" i
      in
      error, log_info, ()
    | Pointer_to_next i ->
      let () = Loggers.fprintf log "Prochain événement agissant sur " in
      print_event_case_address parameter handler log_info error blackboard i
    | Value_after i ->
      let () = Loggers.fprintf log "Valeur après " in
      print_event_case_address parameter handler log_info error blackboard i
    | Value_before i ->
      let () = Loggers.fprintf log "Valeur avant " in
      print_event_case_address parameter handler log_info error blackboard i
    | Pointer_to_previous i ->
      let () = Loggers.fprintf log "Evenement précésent agissant sur " in
      print_event_case_address parameter handler log_info error blackboard i
    | N_unresolved_events ->
      let () = Loggers.fprintf log "Nombre d'événements non résolu" in
      error, log_info, ()
    | N_unresolved_events_at_level i ->
      let () =
        Loggers.fprintf log "Nombre d'événements non résolu at level %s"
          (Priority.string_of_level i)
      in
      error, log_info, ()

  let string_of_value value =
    match value with
    | State pb -> PB.string_of_predicate_value pb
    | Counter i -> "Counter " ^ string_of_int i
    | Pointer i -> string_of_pointer i
    | Boolean bool ->
      (match bool with
      | None -> "?"
      | Some true -> "Yes"
      | Some false -> "No")

  let print_value log value =
    match value with
    | State pb -> PB.print_predicate_value log pb
    | Counter i -> Loggers.fprintf log "Counter %i" i
    | Pointer i -> print_pointer log i
    | Boolean bool ->
      Loggers.fprintf log "%s"
        (match bool with
        | None -> "?"
        | Some true -> "Yes"
        | Some false -> "No")

  let print_assignment parameter handler log_info error blackboard
      (address, value) =
    let error, log_info, () =
      print_address parameter handler log_info error blackboard address
    in
    let _ = print_value (PB.CI.Po.K.H.get_debugging_channel parameter) value in
    error, log_info

  let print_blackboard parameter handler log_info error blackboard =
    let log = PB.CI.Po.K.H.get_debugging_channel parameter in
    let () = Loggers.fprintf log "**BLACKBOARD**" in
    let () = Loggers.print_newline log in
    let () =
      Loggers.fprintf log "%i wires, %i events" blackboard.n_predicate_id
        blackboard.n_eid
    in
    let () = Loggers.print_newline log in
    let () = Loggers.fprintf log "*wires:*" in
    let () = Loggers.print_newline log in
    let err = ref error in
    let () =
      PB.A.iteri
        (fun i array ->
          let () = Loggers.fprintf log "%i" i in
          let () = Loggers.print_newline log in
          let () =
            if PB.A.get blackboard.used_predicate_id i then (
              let () = Loggers.fprintf log "*wires %i: " i in
              let () = Loggers.print_newline log in
              let rec aux j error =
                let () = Loggers.fprintf log "* %i:" j in
                let () = Loggers.print_newline log in
                let case = PB.A.get array j in
                let () = print_case log case in
                let j' = get_pointer_next case in
                let j' = PB.int_of_step_short_id j' in
                if j = j' then
                  error
                else
                  aux j' error
              in
              let error =
                aux (PB.int_of_step_short_id pointer_before_blackboard) !err
              in
              let _ = err := error in
              ()
            ) else
              ()
          in
          Loggers.print_newline log)
        blackboard.blackboard
    in
    let error = !err in
    let () = Loggers.fprintf log "*stacks*" in
    let () = Loggers.print_newline log in
    let error, log_info =
      List.fold_left
        (fun (error, log_info) ->
          print_assignment parameter handler log_info error blackboard)
        (error, log_info)
        (List.rev blackboard.current_stack)
    in
    let () = Loggers.print_newline log in
    let _ =
      List.fold_left
        (fun (error, log_info) stack ->
          let error, log_info =
            List.fold_left
              (fun (error, log_info) ->
                print_assignment parameter handler log_info error blackboard)
              (error, log_info) stack
          in
          let () = Loggers.print_newline log in
          error, log_info)
        (error, log_info) blackboard.stack
    in
    let () = Loggers.fprintf log "*selected_events*" in
    let () = Loggers.print_newline log in
    let () =
      PB.A.iteri
        (fun i bool ->
          match bool with
          | None -> ()
          | Some b ->
            let () =
              Loggers.fprintf log "  Event:%i (%s)" i
                (if b then
                   "KEPT"
                 else
                   "REMOVED")
            in
            Loggers.print_newline log)
        blackboard.selected_events
    in
    let () = Loggers.fprintf log "*unsolved_events*" in
    let () = Loggers.print_newline log in
    let () = Loggers.fprintf log " %i" blackboard.n_unresolved_events in
    let () = Loggers.print_newline log in
    let () = Loggers.fprintf log "*weight of predicate_id*" in
    let () = Loggers.print_newline log in
    let () =
      PB.A.iteri
        (fun a b ->
          let () = Loggers.fprintf log " %i:%i" a b in
          Loggers.print_newline log)
        blackboard.weigth_of_predicate_id
    in
    let () = Loggers.fprintf log "*weight of predicate_id_by_level*" in
    let () =
      Priority.LevelMap.iter
        (fun l ->
          let () =
            Loggers.fprintf log " Level:%s" (Priority.string_of_level l)
          in
          let () = Loggers.print_newline log in
          PB.A.iteri (fun a b ->
              let () = Loggers.fprintf log " %i:%i" a b in
              Loggers.print_newline log))
        blackboard.weigth_of_predicate_id_by_level
    in
    let () = Loggers.fprintf log "**" in
    let () = Loggers.print_newline log in
    error, log_info, ()

  (** propagation request *)

  let add_event eid (pid, seid) array level unsolved =
    let event_case_address = build_event_case_address pid seid in
    let old = PB.A.get array (PB.int_of_step_id eid) in
    let unsolved =
      Priority.LevelMap.add level
        (Priority.LevelMap.find_default 0 level unsolved + 1)
        unsolved
    in
    PB.A.set array (PB.int_of_step_id eid) (event_case_address :: old), unsolved

  let empty_stack = []

  let import ?heuristic:_ parameter handler log_info error pre_blackboard =
    let error, log_info, n_predicates =
      PB.n_predicates parameter handler log_info error pre_blackboard
    in
    let error, log_info, n_events =
      PB.n_events parameter handler log_info error pre_blackboard
    in
    let stack = [] in
    let current_stack = empty_stack in
    let event_case_list = PB.A.make n_events [] in
    let n_seid = PB.A.make n_predicates 0 in
    let unsolved_by_level = Priority.LevelMap.empty in
    let blackboard = PB.A.make n_predicates (PB.A.make 1 dummy_case_info) in
    let weigth_of_predicate_id_by_level =
      let rec aux level_opt map =
        match level_opt with
        | None -> map
        | Some level ->
          aux (Priority.higher level)
            (Priority.LevelMap.add level (PB.A.make 0 0) map)
      in
      aux (Some Priority.lowest) Priority.LevelMap.empty
    in
    let inc_depth level p_id =
      match
        Priority.LevelMap.find_option level weigth_of_predicate_id_by_level
      with
      | Some a ->
        let old = try PB.A.get a p_id with Not_found -> 0 in
        PB.A.set a p_id (old + 1)
      | None -> ()
    in

    let weigth_of_predicate_id = PB.A.make 0 0 in
    let last_linked_event_of_predicate_id =
      PB.A.make n_predicates PB.zero_step_short_id
    in
    let error, log_info =
      let rec aux1 p_id log_info error =
        if p_id < 0 then
          error, log_info
        else (
          let error, log_info, size =
            PB.n_events_per_predicate parameter handler log_info error
              pre_blackboard p_id
          in
          let size = size + 1 in
          let _ =
            PB.A.set last_linked_event_of_predicate_id p_id
              (PB.step_short_id_of_int (size - 1))
          in
          let _ = PB.A.set weigth_of_predicate_id p_id (size - 2) in
          let _ = PB.A.set n_seid p_id size in
          let error, log_info, list =
            PB.event_list_of_predicate parameter handler log_info error
              pre_blackboard p_id
          in
          let array = PB.A.make size dummy_case_info in
          let _ = PB.A.set blackboard p_id array in
          let rec aux2 seid l log_info =
            match l with
            | [] ->
              let info =
                {
                  dynamic =
                    {
                      pointer_previous = PB.zero_step_short_id;
                      pointer_next = PB.inc_step_short_id PB.zero_step_short_id;
                      state_after = PB.undefined;
                      selected = Some true;
                    };
                  static =
                    {
                      row_short_id = PB.zero_step_short_id;
                      event_id = PB.dummy_step_id;
                      test = PB.unknown;
                      action = PB.unknown;
                    };
                }
              in
              let _ = PB.A.set array 0 info in
              let pred_size =
                PB.dec_step_short_id (PB.step_short_id_of_int size)
              in
              let info =
                {
                  dynamic =
                    {
                      pointer_previous = pred_size;
                      pointer_next = pred_size;
                      state_after = PB.unknown;
                      selected = Some true;
                    };
                  static =
                    {
                      row_short_id = pred_size;
                      event_id = PB.dummy_step_id;
                      test = PB.unknown;
                      action = PB.unknown;
                    };
                }
              in
              let _ = PB.A.set array (size - 1) info in
              log_info
            | triple :: q ->
              let info = init_info p_id seid (size - 1) triple in
              let eid = get_eid_of_triple triple in
              let error, log_info, _events =
                PB.get_pre_event parameter handler log_info error pre_blackboard
              in
              let _error, log_info, level =
                PB.get_level_of_event parameter handler log_info error
                  pre_blackboard eid
              in
              let () = inc_depth level p_id in
              let (), _ =
                add_event eid (p_id, seid) event_case_list level
                  Priority.LevelMap.empty
              in
              let () = PB.A.set array (PB.int_of_step_short_id seid) info in
              aux2 (PB.dec_step_short_id seid) q log_info
          in
          let log_info =
            aux2 (PB.step_short_id_of_int (size - 2)) list log_info
          in
          aux1 (p_id - 1) log_info error
        )
      in
      aux1 (n_predicates - 1) log_info error
    in
    let error, log_info, forced_events =
      PB.mandatory_events parameter handler log_info error pre_blackboard
    in
    let error, log_info, event =
      PB.get_pre_event parameter handler log_info error pre_blackboard
    in
    let error, log_info, unsolved_by_level =
      let rec aux k error log_info map =
        if k = 0 then
          error, log_info, map
        else (
          let error, log_info, level =
            PB.get_level_of_event parameter handler log_info error
              pre_blackboard (PB.step_id_of_int k)
          in
          let map =
            Priority.LevelMap.add level
              (Priority.LevelMap.find_default 0 level map + 1)
              map
          in
          aux (k - 1) error log_info map
        )
      in
      aux n_events error log_info unsolved_by_level
    in
    let error, log_info, side_effects =
      PB.get_side_effect parameter handler log_info error pre_blackboard
    in
    let error, log_info, fictitious_obs =
      PB.get_fictitious_observable parameter handler log_info error
        pre_blackboard
    in
    let b =
      {
        event;
        side_effect_of_event = side_effects;
        pre_column_map_inv = PB.get_pre_column_map_inv pre_blackboard;
        level_of_event = PB.levels pre_blackboard;
        forced_events;
        n_eid = n_events;
        n_seid;
        event_case_list;
        last_linked_event_of_predicate_id;
        n_predicate_id = n_predicates;
        current_stack;
        stack;
        blackboard;
        selected_events = PB.A.make n_events None;
        weigth_of_predicate_id;
        weigth_of_predicate_id_by_level;
        used_predicate_id = PB.A.make n_predicates true;
        n_unresolved_events = n_events;
        n_unresolved_events_by_level = unsolved_by_level;
        fictitious_observable = fictitious_obs;
      }
    in
    error, log_info, b

  let exist_case parameter handler log_info error blackboard case_address =
    let error, log_info, info =
      get_case parameter handler log_info error case_address blackboard
    in
    error, log_info, info.dynamic.selected

  let set_case parameter _handler log_info error case_address case_value
      blackboard =
    try
      let _ =
        PB.A.set
          (PB.A.get blackboard.blackboard case_address.column_predicate_id)
          (PB.int_of_step_short_id case_address.row_short_event_id)
          case_value
      in
      error, log_info, blackboard
    with _ ->
      warn parameter log_info error __POS__
        ~message:"Dereferencing Null pointer"
        (Failure "Dereferencing null pointer") blackboard

  let set parameter handler log_info error case_address case_value blackboard =
    match case_address with
    | N_unresolved_events_in_column_at_level (int, level) ->
      (match case_value with
      | Counter int2 ->
        (match
           Priority.LevelMap.find_option level
             blackboard.weigth_of_predicate_id_by_level
         with
        | Some a ->
          let () = PB.A.set a int int2 in
          error, log_info, blackboard
        | None ->
          warn parameter log_info error __POS__
            ~message:"Incompatible address and value in function set"
            (Failure "Incompatible address and value in function Blackboard.set")
            blackboard)
      | Pointer _ | State _ | Boolean _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | N_unresolved_events_in_column int ->
      (match case_value with
      | Counter int2 ->
        let _ = PB.A.set blackboard.weigth_of_predicate_id int int2 in
        error, log_info, blackboard
      | Pointer _ | Boolean _ | State _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | Pointer_to_next case_address ->
      (match case_value with
      | Pointer int2 ->
        let error, log_info, old =
          get_case parameter handler log_info error case_address blackboard
        in
        let case_value =
          { old with dynamic = { old.dynamic with pointer_next = int2 } }
        in
        let error, log_info, blackboard =
          set_case parameter handler log_info error case_address case_value
            blackboard
        in
        error, log_info, blackboard
      | Counter _ | Boolean _ | State _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | Value_after case_address ->
      (match case_value with
      | State state ->
        let error, log_info, old =
          get_case parameter handler log_info error case_address blackboard
        in
        let case_value =
          { old with dynamic = { old.dynamic with state_after = state } }
        in
        let error, log_info, blackboard =
          set_case parameter handler log_info error case_address case_value
            blackboard
        in
        error, log_info, blackboard
      | Boolean _ | Counter _ | Pointer _ ->
        warn parameter log_info error __POS__
          ~message:"set should not be called with value_after"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | Value_before _case_address ->
      warn parameter log_info error __POS__
        ~message:"set should not be called with value_before"
        (Failure "Incompatible address and value in function Blackboard.set")
        blackboard
    | Pointer_to_previous case_address ->
      (match case_value with
      | Pointer int2 ->
        let error, log_info, old =
          get_case parameter handler log_info error case_address blackboard
        in
        let case_value =
          { old with dynamic = { old.dynamic with pointer_previous = int2 } }
        in
        let error, log_info, blackboard =
          set_case parameter handler log_info error case_address case_value
            blackboard
        in
        error, log_info, blackboard
      | Boolean _ | State _ | Counter _ ->
        warn parameter log_info error __POS__
          ~message:
            "set, line 896, Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | N_unresolved_events ->
      (match case_value with
      | Counter int ->
        error, log_info, { blackboard with n_unresolved_events = int }
      | Boolean _ | Pointer _ | State _ ->
        warn parameter log_info error __POS__
          ~message:
            "set, line 905, Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | N_unresolved_events_at_level level ->
      (match case_value with
      | Counter int ->
        ( error,
          log_info,
          {
            blackboard with
            n_unresolved_events_by_level =
              Priority.LevelMap.add level int
                blackboard.n_unresolved_events_by_level;
          } )
      | Boolean _ | State _ | Pointer _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | Keep_event step_id ->
      (match case_value with
      | Boolean b ->
        let _ =
          PB.A.set blackboard.selected_events (PB.int_of_step_id step_id) b
        in
        error, log_info, blackboard
      | Pointer _ | State _ | Counter _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)
    | Exist case_address ->
      (match case_value with
      | Boolean b ->
        let error, log_info, old =
          get_case parameter handler log_info error case_address blackboard
        in
        let case_value =
          { old with dynamic = { old.dynamic with selected = b } }
        in
        let error, log_info, blackboard =
          set_case parameter handler log_info error case_address case_value
            blackboard
        in
        error, log_info, blackboard
      | Pointer _ | Counter _ | State _ ->
        warn parameter log_info error __POS__
          ~message:"Incompatible address and value in function set"
          (Failure "Incompatible address and value in function Blackboard.set")
          blackboard)

  let is_selected_event _parameter _handler log_info error step_id blackboard =
    ( error,
      log_info,
      PB.A.get blackboard.selected_events (PB.int_of_step_id step_id) )

  let rec get parameter handler log_info error case_address blackboard =
    match case_address with
    | Keep_event step_id ->
      ( error,
        log_info,
        Boolean
          (PB.A.get blackboard.selected_events (PB.int_of_step_id step_id)) )
    | N_unresolved_events_in_column_at_level (int, level) ->
      let n =
        match
          Priority.LevelMap.find_option level
            blackboard.weigth_of_predicate_id_by_level
        with
        | Some a -> PB.A.get a int
        | None -> 0
      in
      error, log_info, Counter n
    | N_unresolved_events_in_column int ->
      error, log_info, Counter (PB.A.get blackboard.weigth_of_predicate_id int)
    | Exist case_address ->
      let error, log_info, case =
        get_case parameter handler log_info error case_address blackboard
      in
      error, log_info, Boolean case.dynamic.selected
    | Pointer_to_next case_address ->
      let error, log_info, case =
        get_case parameter handler log_info error case_address blackboard
      in
      error, log_info, Pointer case.dynamic.pointer_next
    | Value_after case_address ->
      let error, log_info, case =
        get_case parameter handler log_info error case_address blackboard
      in
      error, log_info, State case.dynamic.state_after
    | Value_before case_address ->
      let error, log_info, case =
        get_case parameter handler log_info error case_address blackboard
      in
      let pointer = case.dynamic.pointer_previous in
      if is_null_pointer pointer then
        warn parameter log_info error __POS__
          ~message:"Value before an unexisting element requested"
          (Failure "Value before an unexisting element requested")
          (State PB.undefined)
      else
        get parameter handler log_info error
          (Value_after { case_address with row_short_event_id = pointer })
          blackboard
    | Pointer_to_previous case_address ->
      let error, log_info, case =
        get_case parameter handler log_info error case_address blackboard
      in
      error, log_info, Pointer case.dynamic.pointer_previous
    | N_unresolved_events ->
      error, log_info, Counter blackboard.n_unresolved_events
    | N_unresolved_events_at_level lvl ->
      ( error,
        log_info,
        Counter
          (Priority.LevelMap.find_default 0 lvl
             blackboard.n_unresolved_events_by_level) )

  let export_blackboard_to_xls parameter handler log_info error prefix int int2
      blackboard =
    let file_name =
      prefix ^ "_" ^ string_of_int int ^ "_" ^ string_of_int int2 ^ ".sxw"
    in
    let desc_chan = Kappa_files.open_out file_name in
    let desc = Loggers.open_logger_from_channel ~mode:Loggers.XLS desc_chan in
    let parameter = PB.CI.Po.K.H.set_logger parameter desc in
    let ncolumns_left = 3 in
    let nrows_head = 2 in
    let row_of_precondition eid = nrows_head + (3 * eid) in
    let row_of_postcondition eid = 1 + row_of_precondition eid in
    let column_of_pid pid = pid + ncolumns_left in
    let () = Loggers.fprintf desc "REM  *****  BASIC  *****" in
    let () = Loggers.print_newline desc in
    let colors = PB.A.make blackboard.n_eid None in
    let backcolor log color =
      match color with
      | Some color ->
        let r, g, b = Color.triple_of_color color in
        let () = Loggers.fprintf log "C.CellBackColor = RGB(%i,%i,%i)" r g b in
        let () = Loggers.print_newline log in
        ()
      | None -> ()
    in
    let textcolor log color =
      match color with
      | Some color ->
        let r, g, b = Color.triple_of_color color in
        let () = Loggers.fprintf log "C.CharColor = RGB(%i,%i,%i)" r g b in
        Loggers.print_newline log
      | None -> ()
    in
    let getcell log row col =
      let () = Loggers.fprintf log "C = S.getCellByPosition(%i,%i)" col row in
      Loggers.print_newline log
    in
    let overline_case log row _col _color =
      let () = Loggers.fprintf log "R=S.Rows(%i)" row in
      let () = Loggers.print_newline log in
      let () = Loggers.fprintf log "R.TopBorder = withBord" in
      Loggers.print_newline log
    in
    let print_case log row col color_font color_back string =
      if string <> "" then (
        let () = getcell log row col in
        let () = textcolor log color_font in
        let () = backcolor log color_back in
        let () = Loggers.fprintf log "C.setFormula(\"%s\")" string in
        Loggers.print_newline log
      )
    in
    let print_case_fun log row col color_font color_back f error =
      let () = getcell log row col in
      let () = textcolor log color_font in
      let () = backcolor log color_back in
      let () = Loggers.fprintf log "C.setFormula(\"" in
      let error = f error in
      let () = Loggers.fprintf log "\")" in
      let () = Loggers.print_newline log in
      error
    in
    let () = Loggers.fprintf desc "Sub Main" in
    let () = Loggers.print_newline desc in
    let () = Loggers.print_newline desc in
    let r, g, b = Color.triple_of_color Color.Black in
    let () =
      Loggers.fprintf desc "Dim withBord As New com.sun.star.table.BorderLine"
    in
    let () = Loggers.print_newline desc in
    let () =
      Loggers.fprintf desc "With withBord withBord.Color = RGB(%i,%i,%i)" r g b
    in
    let () = Loggers.print_newline desc in
    let () = Loggers.fprintf desc "withBord.OuterLineWidth = 60" in
    let () = Loggers.print_newline desc in
    let () = Loggers.fprintf desc "End With" in
    let () = Loggers.print_newline desc in
    let () = Loggers.fprintf desc "S = ThisComponent.Sheets(0)" in
    let () = Loggers.print_newline desc in
    let _ =
      match forced_events blackboard with
      | [ (list, _) ] ->
        List.iter
          (fun eid -> PB.A.set colors (PB.int_of_step_id eid) (Some Color.Red))
          list
      | _ -> ()
    in
    let _ =
      PB.A.iteri
        (fun pid p_info ->
          print_case_fun desc 0 (column_of_pid pid) None None
            (fun _error -> PB.print_predicate_info desc p_info)
            error)
        blackboard.pre_column_map_inv
    in
    let rec aux eid error log_info stack =
      if eid >= blackboard.n_eid then
        error, log_info
      else (
        let error, log_info, list =
          case_list_of_eid parameter handler log_info error blackboard
            (PB.step_id_of_int eid)
        in
        let row_precondition = row_of_precondition eid in
        let row_postcondition = row_of_postcondition eid in
        let color, maybekept =
          match PB.A.get blackboard.selected_events eid with
          | None -> PB.A.get colors eid, true
          | Some true -> Some Color.Red, true
          | Some false -> Some Color.Grey, false
        in
        let rec aux2 f g l error log_info =
          match l with
          | [] -> error, log_info
          | t :: q ->
            let _ =
              overline_case desc row_postcondition
                (column_of_pid t.column_predicate_id)
                None
            in
            let _ =
              print_case desc row_precondition
                (column_of_pid t.column_predicate_id)
                None color (f t)
            in
            let _ =
              print_case desc row_postcondition
                (column_of_pid t.column_predicate_id)
                None color (g t)
            in
            let error, log_info =
              if maybekept then (
                let error, log_info, value_before =
                  try
                    let error, log_info, case_value =
                      get parameter handler log_info error (value_before t)
                        blackboard
                    in
                    error, log_info, string_of_value case_value
                  with Not_found -> error, log_info, "Undefined"
                in
                let _ =
                  print_case desc (row_precondition - 1)
                    (column_of_pid t.column_predicate_id)
                    (Some Color.Lightblue) None value_before
                in
                let error, log_info, value_after =
                  try
                    let error, log_info, case_value =
                      get parameter handler log_info error (value_after t)
                        blackboard
                    in
                    error, log_info, string_of_value case_value
                  with Not_found -> error, log_info, "Undefined"
                in
                let _ =
                  print_case desc (row_postcondition + 1)
                    (column_of_pid t.column_predicate_id)
                    (Some Color.Lightblue) None value_after
                in
                error, log_info
              ) else
                error, log_info
            in
            aux2 f g q error log_info
        in
        let print_test t =
          let column = PB.A.get blackboard.blackboard t.column_predicate_id in
          let case =
            PB.A.get column (PB.int_of_step_short_id t.row_short_event_id)
          in
          PB.string_of_predicate_value case.static.test
        in
        let print_action t =
          let column = PB.A.get blackboard.blackboard t.column_predicate_id in
          let case =
            PB.A.get column (PB.int_of_step_short_id t.row_short_event_id)
          in
          PB.string_of_predicate_value case.static.action
        in
        let string_eid error =
          let () =
            try
              Loggers.print_as_logger desc (fun f ->
                  Trace.print_step ~compact:true ~env:handler.PB.CI.Po.K.H.env f
                    (PB.A.get blackboard.event eid))
            with Not_found -> Loggers.fprintf desc "Event:%i" eid
          in
          error
        in
        let error =
          print_case_fun desc row_precondition 1 None color string_eid error
        in
        let error =
          print_case_fun desc row_postcondition 1 None color string_eid error
        in
        let () = print_case desc row_precondition 2 None color "PRECONDITION" in
        let () =
          print_case desc row_postcondition 2 None color "POSTCONDITION"
        in
        let error, log_info =
          aux2 print_test print_action list error log_info
        in
        let bool =
          try
            let cand = PB.A.get blackboard.event eid in
            Trace.step_is_rule cand || Trace.step_is_pert cand
            || Trace.step_is_obs cand || Trace.step_is_init cand
          with Not_found -> false
        in
        let error, stack =
          if bool then (
            let error =
              List.fold_left
                (fun error row ->
                  print_case_fun desc row 0 None color string_eid error)
                error (List.rev stack)
            in
            error, []
          ) else
            error, row_precondition :: row_postcondition :: stack
        in
        aux (eid + 1) error log_info stack
      )
    in
    let error, log_info = aux 0 error log_info [] in
    let () = Loggers.fprintf desc "End Sub" in
    let () = Loggers.print_newline desc in
    let () = close_out desc_chan in
    error, log_info, ()

  let record_modif _parameter _handler error case_address case_value blackboard
      =
    ( error,
      {
        blackboard with
        current_stack = (case_address, case_value) :: blackboard.current_stack;
      } )

  let refine parameter handler log_info error case_address case_value blackboard
      =
    let error, log_info, old =
      get parameter handler log_info error case_address blackboard
    in
    if case_value = old then (
      let error, log_info =
        if debug_mode then (
          let () =
            Loggers.fprintf
              (PB.CI.Po.K.H.get_debugging_channel parameter)
              "@.***@.REFINE_VALUE@.Value before: "
          in
          let error, log_info, () =
            print_case_address parameter handler log_info error blackboard
              case_address
          in
          let () = print_case_value parameter old in
          let () =
            Loggers.fprintf
              (PB.CI.Po.K.H.get_debugging_channel parameter)
              "@.New value: "
          in
          let () = print_case_value parameter case_value in
          let () =
            Loggers.fprintf
              (PB.CI.Po.K.H.get_debugging_channel parameter)
              "@.IGNORED***@."
          in
          error, log_info
        ) else
          error, log_info
      in
      error, log_info, (blackboard, Ignore)
    ) else (
      let error, log_info, bool =
        strictly_more_refined parameter handler log_info error old case_value
      in
      if bool then (
        let error, log_info =
          if debug_mode then (
            let () =
              Loggers.fprintf
                (PB.CI.Po.K.H.get_debugging_channel parameter)
                "@.***@.REFINE_VALUE@.Value before: "
            in
            let error, log_info, () =
              print_case_address parameter handler log_info error blackboard
                case_address
            in
            let () = print_case_value parameter old in
            let () =
              Loggers.fprintf
                (PB.CI.Po.K.H.get_debugging_channel parameter)
                "@.New value: "
            in
            let () = print_case_value parameter case_value in
            let () =
              Loggers.fprintf
                (PB.CI.Po.K.H.get_debugging_channel parameter)
                "@.IGNORED***@."
            in
            error, log_info
          ) else
            error, log_info
        in
        error, log_info, (blackboard, Ignore)
      ) else (
        let error, log_info, bool =
          strictly_more_refined parameter handler log_info error case_value old
        in
        if bool then (
          let error, log_info, blackboard =
            set parameter handler log_info error case_address case_value
              blackboard
          in
          let error, blackboard =
            record_modif parameter handler error case_address old blackboard
          in
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.***@.REFINE_VALUE@.Value before: "
              in
              let error, log_info, () =
                print_case_address parameter handler log_info error blackboard
                  case_address
              in
              let () = print_case_value parameter old in
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.New value: "
              in
              let () = print_case_value parameter case_value in
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.SUCCESS***@."
              in
              error, log_info
            ) else
              error, log_info
          in
          error, log_info, (blackboard, Success)
        ) else (
          let error, log_info =
            if debug_mode then (
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.***@.REFINE_VALUE@.Value before: "
              in
              let error, log_info, () =
                print_case_address parameter handler log_info error blackboard
                  case_address
              in
              let () = print_case_value parameter old in
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.New value: "
              in
              let () = print_case_value parameter case_value in
              let () =
                Loggers.fprintf
                  (PB.CI.Po.K.H.get_debugging_channel parameter)
                  "@.FAIL***@."
              in
              error, log_info
            ) else
              error, log_info
          in
          error, log_info, (blackboard, Fail)
        )
      )
    )

  let overwrite parameter handler log_info error case_address case_value
      blackboard =
    let error, log_info, old =
      get parameter handler log_info error case_address blackboard
    in
    if case_value = old then
      error, log_info, blackboard
    else (
      let error, log_info, blackboard =
        set parameter handler log_info error case_address case_value blackboard
      in
      let error, blackboard =
        record_modif parameter handler error case_address old blackboard
      in
      error, log_info, blackboard
    )

  let dec parameter handler log_info error case_address blackboard =
    let error, log_info, old =
      get parameter handler log_info error case_address blackboard
    in
    match old with
    | Counter k ->
      if k = 0 then
        error, log_info, blackboard
      else (
        let error, log_info, blackboard =
          set parameter handler log_info error case_address
            (Counter (k - 1))
            blackboard
        in
        let error, blackboard =
          record_modif parameter handler error case_address old blackboard
        in
        error, log_info, blackboard
      )
    | Pointer _ | Boolean _ | State _ ->
      warn parameter log_info error __POS__ ~message:"Wrong type of case value"
        (Failure "Wrong type of case value") blackboard

  let branch parameter handler log_info error blackboard =
    let error, log_info =
      if debug_mode then (
        let () =
          Loggers.fprintf
            (PB.CI.Po.K.H.get_debugging_channel parameter)
            "*******@. * BRANCH *@.*******@."
        in
        let error, log_info, () =
          print_blackboard parameter handler log_info error blackboard
        in
        error, log_info
      ) else
        error, log_info
    in
    let log_info = StoryProfiling.StoryStats.inc_branch log_info in
    ( error,
      log_info,
      {
        blackboard with
        stack = blackboard.current_stack :: blackboard.stack;
        current_stack = [];
      } )

  let reset_last_branching parameter handler log_info error blackboard =
    let error, log_info =
      if debug_mode then (
        let () =
          Loggers.fprintf
            (PB.CI.Po.K.H.get_debugging_channel parameter)
            "*******@.* Cut *@.*******"
        in
        let error, log_info, () =
          print_blackboard parameter handler log_info error blackboard
        in
        error, log_info
      ) else
        error, log_info
    in
    let stack = blackboard.current_stack in
    let error, log_info, blackboard =
      List.fold_left
        (fun (error, log_info, blackboard) (case_address, case_value) ->
          set parameter handler log_info error case_address case_value
            blackboard)
        (error, log_info, blackboard)
        stack
    in
    let error, log_info =
      if debug_mode then (
        let () =
          Loggers.fprintf
            (PB.CI.Po.K.H.get_debugging_channel parameter)
            "*******@.* After_Cut *@.*******"
        in
        let error, log_info, () =
          print_blackboard parameter handler log_info error blackboard
        in
        error, log_info
      ) else
        error, log_info
    in
    let log_info = StoryProfiling.StoryStats.inc_cut log_info in
    match blackboard.stack with
    | [] -> error, log_info, { blackboard with current_stack = [] }
    | t :: q ->
      error, log_info, { blackboard with current_stack = t; stack = q }

  let reset_init parameter handler log_info error blackboard =
    let rec aux (error, log_info, blackboard) =
      match blackboard.current_stack with
      | [] -> error, log_info, blackboard
      | _ ->
        aux (reset_last_branching parameter handler log_info error blackboard)
    in
    let error, log_info, blackboard = aux (error, log_info, blackboard) in
    let log_info = StoryProfiling.StoryStats.reset_log log_info in
    error, log_info, blackboard

  type result = (Trace.step * PB.CI.Po.K.side_effect) list
  (** output result*)

  (** iteration*)
  let is_maximal_solution _parameter _handler log_info error blackboard =
    error, log_info, blackboard.n_unresolved_events = 0

  (** exporting result*)

  let translate_blackboard _parameter _handler log_info error blackboard =
    let array = blackboard.selected_events in
    let step_array = blackboard.event in
    let side_array = blackboard.side_effect_of_event in
    let size = PB.A.length array in
    let rec aux k list =
      if k = size then
        List.rev list
      else (
        let bool = PB.A.get array k in
        match bool with
        | None -> aux (k + 1) list
        | Some false -> aux (k + 1) list
        | Some true ->
          let step = PB.A.get step_array k in
          let side = PB.A.get side_array k in
          aux (k + 1) ((step, side) :: list)
      )
    in
    let list = aux 0 [] in
    error, log_info, list

  let print_stack parameter handler log_info error blackboard =
    let stack = blackboard.current_stack in
    let log = PB.CI.Po.K.H.get_debugging_channel parameter in
    let () =
      Loggers.fprintf log "Current_stack_level %i " (List.length stack)
    in
    let error, log_info =
      List.fold_left
        (fun (error, log_info) i ->
          let error =
            print_assignment parameter handler log_info error blackboard i
          in
          let () = Loggers.fprintf log "@." in
          error)
        (error, log_info) (List.rev stack)
    in
    let error, log_info =
      List.fold_left
        (fun (error, log_info) x ->
          let () = Loggers.fprintf log "Other level %i " (List.length x) in
          List.fold_left
            (fun (error, log_info) ->
              print_assignment parameter handler log_info error blackboard)
            (error, log_info) (List.rev x))
        (error, log_info)
        (List.rev blackboard.stack)
    in
    error, log_info, ()

  let is_fictitious_obs blackboard eid =
    Some eid = blackboard.fictitious_observable

  let useless_predicate_id parameter handler log_info error blackboard list =
    let n_events = blackboard.n_eid in
    if Parameter.do_local_cut then (
      let event_array = PB.A.make n_events false in
      let kept_events = [] in
      let kept_events =
        List.fold_left
          (fun kept_events i ->
            let _ = PB.A.set event_array (PB.int_of_step_id i) true in
            i :: kept_events)
          kept_events list
      in
      let rec aux log_info error event_list kept_events =
        match event_list with
        | [] -> error, log_info, kept_events
        | eid :: q ->
          if is_fictitious_obs blackboard eid then
            aux log_info error q kept_events
          else (
            let list =
              PB.A.get blackboard.event_case_list (PB.int_of_step_id eid)
            in
            let error, log_info, q, kept_events =
              List.fold_left
                (fun (error, log_info, q, kept_events) event_case_address ->
                  let error, log_info, case =
                    get_case parameter handler log_info error event_case_address
                      blackboard
                  in
                  if PB.is_undefined case.static.test then
                    error, log_info, q, kept_events
                  else (
                    let pointer = case.dynamic.pointer_previous in
                    let eid =
                      let rec scan_down pointer =
                        let prev_event_case_address =
                          {
                            event_case_address with
                            row_short_event_id = pointer;
                          }
                        in
                        let _error, _log_info, prev_case =
                          get_case parameter handler log_info error
                            prev_event_case_address blackboard
                        in
                        let prev_eid = prev_case.static.event_id in
                        if is_null_pointer_step_id prev_eid then
                          None
                        else if PB.is_unknown prev_case.static.action then (
                          let pointer = prev_case.dynamic.pointer_previous in
                          scan_down pointer
                        ) else
                          Some prev_eid
                      in
                      scan_down pointer
                    in
                    match eid with
                    | None -> error, log_info, q, kept_events
                    | Some prev_eid ->
                      let bool =
                        try PB.A.get event_array (PB.int_of_step_id prev_eid)
                        with _ -> false
                      in
                      let q, kept_events =
                        if bool then
                          q, kept_events
                        else (
                          let _ =
                            PB.A.set event_array
                              (PB.int_of_step_id prev_eid)
                              true
                          in
                          prev_eid :: q, prev_eid :: kept_events
                        )
                      in
                      error, log_info, q, kept_events
                  ))
                (error, log_info, q, kept_events)
                list
            in
            aux log_info error q kept_events
          )
      in
      let error, log_info, rep = aux log_info error list kept_events in
      error, log_info, List.sort compare rep, n_events - List.length rep
    ) else (
      let events_to_keep =
        let rec aux k list =
          if k < 0 then
            list
          else
            aux (k - 1) (PB.step_id_of_int k :: list)
        in
        aux (n_events - 1) []
      in
      error, log_info, events_to_keep, 0
    )

  let cut parameter handler log_info error blackboard list =
    let error, log_info, cut_causal_flow, n_events_removed =
      useless_predicate_id parameter handler log_info error blackboard list
    in
    let log_info =
      StoryProfiling.StoryStats.set_concurrent_event_detection_time log_info
    in
    let log_info = StoryProfiling.StoryStats.set_step_time log_info in
    let log_info =
      StoryProfiling.StoryStats.inc_k_cut_events n_events_removed log_info
    in
    error, log_info, (blackboard, cut_causal_flow)

  let import ?heuristic parameter handler log_info error list =
    let error, log_info, preblackboard =
      PB.init parameter handler log_info error
    in
    let error, log_info, (preblackboard, _step_id, string, to_xls) =
      match parameter.PB.CI.Po.K.H.current_compression_mode with
      | None ->
        warn parameter log_info error __POS__
          ~message:"Compression mode has not been set up"
          (Failure "Compression mode has not been set up.")
          (preblackboard, PB.zero_step_id, "None", false)
      | Some Story_json.Strong ->
        let error, log_info, (preblackboard, int) =
          List.fold_left
            (fun (error, log_info, (preblackboard, int)) refined_event ->
              PB.add_step_up_to_iso parameter handler log_info error
                refined_event preblackboard int)
            (error, log_info, (preblackboard, PB.zero_step_id))
            list
        in
        ( error,
          log_info,
          ( preblackboard,
            int,
            Parameter.xlsstrongFileName,
            Parameter.dump_grid_before_strong_compression ) )
      | Some Story_json.Weak | Some Story_json.Causal ->
        let error, log_info, (preblackboard, int) =
          List.fold_left
            (fun (error, log_info, (preblackboard, int)) refined_event ->
              PB.add_step parameter handler log_info error refined_event
                preblackboard int)
            (error, log_info, (preblackboard, PB.zero_step_id))
            list
        in
        ( error,
          log_info,
          ( preblackboard,
            int,
            Parameter.xlsweakFileName,
            Parameter.dump_grid_before_weak_compression ) )
    in
    let error, log_info, preblackboard =
      PB.finalize heuristic parameter handler log_info error preblackboard
    in
    let error, log_info, blackboard =
      import parameter handler log_info error preblackboard
    in
    let _ = Priority.n_story := !Priority.n_story + 1 in
    let _ = Priority.n_branch := 1 in
    let error, log_info, () =
      if to_xls then
        export_blackboard_to_xls parameter handler log_info error string
          !Priority.n_story 0 blackboard
      else
        error, log_info, ()
    in
    error, log_info, blackboard
end
