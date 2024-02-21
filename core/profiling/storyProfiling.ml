(**
  * storyProfiling.ml
  *
  * Generate profiling information for stories in KaSim
  *
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  *
  * Creation: 09/04/2012
  * Last modification: 15/06/2012
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type step_kind =
  | Dummy
  | Beginning
  | Collect_traces
  | Causal_compression
  | Weak_compression
  | Strong_compression
  | Iteration of int
  | Story of int
  | Partial_order_reduction
  | Siphon_detection
  | Decompose_initial_state
  | Agent_ids_disambiguation
  | Pseudo_inverse_deletion
  | Remove_events_after_last_observable
  | Compression
  | Build_grid
  | Build_configuration
  | Transitive_closure
  | Graph_reduction
  | Graph_conversion
  | Cannonic_form_computation
  | Store_trace
  | Removing_blacklisted_events
  | Blacklisting_events
  | Global_initialization
  | Domains_initialization
  | Domain_initialization of string
  | Apply_rule of int
  | Initial_state of int
  | Scan_rule_static of int
  | Scan_rule_dynamic of int
  | Regular_influences
  | Side_effects_influences
  | Merge_influences
  | KaSim_compilation
  | KaSa_precompilation
  | KaSa_lexing
  | KaSa_linking
  | Influence_map of string
  | Internal_influence_map of string
  | LKappa_signature

let string_of_step_kind x =
  match x with
  | Dummy | Beginning -> ""
  | Build_configuration -> "Build configuration"
  | Collect_traces -> "Collect traces"
  | Causal_compression -> "Causal compression"
  | Weak_compression -> "Weak compression"
  | Strong_compression -> "Strong compression"
  | Iteration int -> Printf.sprintf "Iteration %i" int
  | Story int -> Printf.sprintf "Story %i" int
  | Partial_order_reduction -> "Partial order reduction"
  | Siphon_detection -> "Detection of siphons"
  | Decompose_initial_state -> "Splitting initial states"
  | Agent_ids_disambiguation -> "Renaming agents to avoid conflicts"
  | Pseudo_inverse_deletion -> "Deletion of pseudo inverse events"
  | Remove_events_after_last_observable ->
    "Removing events after the last observables"
  | Compression -> "Compression"
  | Transitive_closure -> "Transitive closure"
  | Build_grid -> "Grid computation"
  | Graph_reduction -> "Transitive reduction"
  | Graph_conversion -> "Graph conversion"
  | Cannonic_form_computation -> "Computing the cannonic form"
  | Store_trace -> "Store trace"
  | Removing_blacklisted_events -> "Removing black-listed events"
  | Blacklisting_events -> "Blaklisting events"
  | Global_initialization -> "Global initialization"
  | Domains_initialization -> "Domains initialization"
  | Domain_initialization string ->
    Printf.sprintf "Domain initialization (%s)" string
  | Apply_rule int -> Printf.sprintf "Apply rule %i" int
  | Initial_state int -> Printf.sprintf "Initial state %i" int
  | Scan_rule_static int -> Printf.sprintf "Scan rule %i (static)" int
  | Scan_rule_dynamic int -> Printf.sprintf "Scan rule %i (dynamic)" int
  | Regular_influences -> Printf.sprintf "Regular influences computation"
  | Side_effects_influences ->
    Printf.sprintf "Side-effects influences computation"
  | Merge_influences -> Printf.sprintf "Merging influences"
  | KaSim_compilation -> Printf.sprintf "KaSim frontend"
  | KaSa_precompilation -> Printf.sprintf "KaSa precompilation"
  | KaSa_lexing -> Printf.sprintf "KaSa Lexing"
  | KaSa_linking -> Printf.sprintf "KaSa Linking"
  | Influence_map string -> Printf.sprintf "Influence map (%s)" string
  | Internal_influence_map string ->
    Printf.sprintf "Influence map (internal %s)" string
  | LKappa_signature -> Printf.sprintf "LKappa signature"

let print_step_kind parameters x =
  Loggers.print_cell
    (Remanent_parameters.get_profiler parameters)
    (string_of_step_kind x)

module type StoryStats = sig
  type log_info

  val inc_removed_events : log_info -> log_info
  val inc_selected_events : log_info -> log_info
  val log_info_to_json : log_info -> Yojson.Basic.t
  val log_info_of_json : Yojson.Basic.t -> log_info
  val inc_cut_events : log_info -> log_info
  val inc_k_cut_events : int -> log_info -> log_info
  val reset_cut_events : log_info -> log_info
  val inc_n_kasim_events : log_info -> log_info
  val inc_n_init_events : log_info -> log_info
  val inc_n_side_events : log_info -> log_info
  val inc_n_obs_events : log_info -> log_info
  val inc_branch : log_info -> log_info
  val inc_cut : log_info -> log_info
  val reset_log : log_info -> log_info
  val dump_complete_log : Remanent_parameters_sig.parameters -> log_info -> unit
  val dump_short_log : Remanent_parameters_sig.parameters -> log_info -> unit
  val add_propagation_case_up : int -> log_info -> log_info
  val add_propagation_case_down : int -> log_info -> log_info
  val add_look_up_case : int -> log_info -> log_info
  val add_look_down_case : int -> log_info -> log_info
  val copy : log_info -> log_info
  val is_dummy : step_kind -> bool

  val add_event :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    step_kind ->
    (unit -> int) option ->
    log_info ->
    Exception.method_handler * log_info

  val close_event :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    step_kind ->
    (unit -> int) option ->
    log_info ->
    Exception.method_handler * log_info

  val add_event_opt :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    step_kind option ->
    (unit -> int) option ->
    log_info ->
    Exception.method_handler * log_info

  val close_event_opt :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    step_kind option ->
    (unit -> int) option ->
    log_info ->
    Exception.method_handler * log_info

  val set_time : log_info -> log_info
  val set_step_time : log_info -> log_info
  val set_global_cut : int -> log_info -> log_info
  val set_pseudo_inv : int -> log_info -> log_info
  val set_start_compression : log_info -> log_info
  val set_grid_generation : log_info -> log_info
  val set_canonicalisation : log_info -> log_info
  val set_concurrent_event_detection_time : log_info -> log_info
  val set_concurrent_event_deletion_time : log_info -> log_info
  val set_story_research_time : log_info -> log_info
  val ellapsed_global_time : log_info -> float
  val ellapsed_time : log_info -> float
  val init_log_info : unit -> log_info
  val tick : log_info -> bool * log_info
  val close_logger : Remanent_parameters_sig.parameters -> unit
  val flush_logger : Remanent_parameters_sig.parameters -> unit
end

module StoryStats : StoryStats = struct
  type stack_head = {
    current_branch: int;
    selected_events: int;
    remaining_events: int;
    removed_events: int;
    stack_size: int;
  }

  type step = {
    tag: step_kind;
    size_before: int option;
    size_after: int option;
    time_start: float;
    duration: float option;
    depth: int;
  }

  let k_first _parameter k l =
    let rec aux k l output =
      if k = 0 then
        [], List.rev output
      else (
        match l with
        | [] ->
          ( (let rec aux k output =
               if k = 0 then
                 output
               else
                 aux (k - 1) ("" :: output)
             in
             aux k []),
            List.rev output )
        | t :: q -> aux (k - 1) q (t :: output)
      )
    in
    aux k l []

  let print_task parameter (a, b) =
    let _ = print_step_kind parameter a.tag in
    let tab, b = k_first parameter 4 b in
    let _ = List.iter (print_step_kind parameter) b in
    let _ =
      List.iter
        (Loggers.print_cell (Remanent_parameters.get_profiler parameter))
        tab
    in
    let _ =
      Loggers.print_cell
        (Remanent_parameters.get_profiler parameter)
        (match a.size_before with
        | None -> ""
        | Some i -> string_of_int i)
    in
    let _ =
      Loggers.print_cell
        (Remanent_parameters.get_profiler parameter)
        (match a.size_after with
        | None -> ""
        | Some i -> string_of_int i)
    in
    let _ =
      Loggers.print_cell
        (Remanent_parameters.get_profiler parameter)
        (match a.duration with
        | None -> ""
        | Some time -> string_of_float time)
    in
    ()

  let close_logger parameter =
    Loggers.close_logger (Remanent_parameters.get_profiler parameter)

  let flush_logger parameter =
    Loggers.flush_logger (Remanent_parameters.get_profiler parameter)

  type log_info = {
    global_time: float;
    story_time: float;
    step_time: float;
    current_task: step list;
    next_depth: int;
    branch: int;
    cut: int;
    stack: stack_head list;
    current_stack: stack_head;
    propagation: int array;
    last_tick: float;
  }

  let is_dummy step_kind =
    match step_kind with
    | Dummy -> true
    | Beginning | Collect_traces | Causal_compression | Weak_compression
    | Strong_compression | Partial_order_reduction | Siphon_detection
    | Decompose_initial_state | Agent_ids_disambiguation
    | Pseudo_inverse_deletion | Remove_events_after_last_observable
    | Compression | Build_grid | Build_configuration | Transitive_closure
    | Graph_reduction | Graph_conversion | Cannonic_form_computation
    | Store_trace | Removing_blacklisted_events | Blacklisting_events
    | Global_initialization | Domains_initialization | Regular_influences
    | Side_effects_influences | Merge_influences | KaSim_compilation
    | KaSa_precompilation | KaSa_lexing | KaSa_linking | LKappa_signature
    | Iteration _ | Story _ | Domain_initialization _ | Apply_rule _
    | Initial_state _ | Scan_rule_static _ | Scan_rule_dynamic _
    | Influence_map _ | Internal_influence_map _ ->
      false

  let add_event parameter error step_kind f log_info =
    if is_dummy step_kind then (
      let error, () =
        Exception.warn parameter error __POS__
          ~message:
            "Inconsistent profiling information, add_event should not be \
             called with a dummy event"
          (Failure "Dummy event in add_event") ()
      in
      error, log_info
    ) else (
      let next_depth = log_info.next_depth in
      let task =
        {
          tag = step_kind;
          size_before =
            (match f with
            | None -> None
            | Some f -> Some (f ()));
          size_after = None;
          time_start = Sys.time ();
          duration = None;
          depth = next_depth;
        }
      in
      let _ = Loggers.open_row (Remanent_parameters.get_profiler parameter) in
      let _ =
        Loggers.print_cell (Remanent_parameters.get_profiler parameter) "Start"
      in
      let terminated_task =
        task, List.rev_map (fun x -> x.tag) (List.rev log_info.current_task)
      in
      let _ = print_task parameter terminated_task in
      let _ = Loggers.close_row (Remanent_parameters.get_profiler parameter) in
      let _ = flush_logger parameter in
      let () =
        Remanent_parameters.save_current_phase_title parameter
          (string_of_step_kind step_kind)
      in
      let current_task = task :: log_info.current_task in
      error, { log_info with next_depth = next_depth + 1; current_task }
    )

  let close_event parameter error step_kind f log_info =
    if is_dummy step_kind then (
      let error, () =
        Exception.warn parameter error __POS__
          ~message:
            "Inconsistent profiling information, close_event should not be \
             called with a dummy event"
          (Failure "Dummy event in close_event") ()
      in
      error, log_info
    ) else (
      let rec aux log_info error interrupted =
        let next_depth = log_info.next_depth in
        let error, () =
          if next_depth = 1 then
            Exception.warn parameter error __POS__
              ~message:
                "Inconsistent profiling information, depth should not be equal \
                 to 1 when closing an event"
              (Failure "Depth=1 in close_event") ()
          else
            error, ()
        in
        match log_info.current_task with
        | [] ->
          Exception.warn parameter error __POS__
            ~message:
              "Inconsistent profiling information, no current task when \
               closing an event"
            (Failure "No current tasks in close_event") log_info
        | current_task :: tail when current_task.tag = step_kind ->
          let size_after =
            match f with
            | Some f -> Some (f ())
            | None -> None
          in
          let time = Sys.time () -. current_task.time_start in
          let task = { current_task with size_after; duration = Some time } in
          let terminated_task =
            task, List.rev_map (fun x -> x.tag) (List.rev tail)
          in
          let () =
            Loggers.open_row (Remanent_parameters.get_profiler parameter)
          in
          let () =
            Loggers.print_cell
              (Remanent_parameters.get_profiler parameter)
              (if interrupted then
                 "Interrupted"
               else
                 "End")
          in
          let () = print_task parameter terminated_task in
          let () =
            Loggers.close_row (Remanent_parameters.get_profiler parameter)
          in
          let () = flush_logger parameter in
          ( error,
            { log_info with next_depth = next_depth - 1; current_task = tail } )
        | current_task :: tail ->
          let () =
            Loggers.open_row (Remanent_parameters.get_profiler parameter)
          in
          let terminated_task =
            current_task, List.rev_map (fun x -> x.tag) (List.rev tail)
          in
          let () =
            Loggers.print_cell
              (Remanent_parameters.get_logger parameter)
              "Interrupted"
          in
          let () = print_task parameter terminated_task in
          let () =
            Loggers.close_row (Remanent_parameters.get_profiler parameter)
          in
          let () = flush_logger parameter in
          aux
            { log_info with next_depth = next_depth - 1; current_task = tail }
            error true
      in
      aux log_info error false
    )

  let gen_opt gen parameter error step_kind f log_info =
    match step_kind with
    | None -> error, log_info
    | Some e -> gen parameter error e f log_info

  let add_event_opt = gen_opt add_event
  let close_event_opt = gen_opt close_event

  let propagation_labels =
    [|
      "None";
      "Up:        case 1 ";
      "Up:        case 2 ";
      "Up:        case 3 ";
      "Up:        case 4 ";
      "Up:        case 5 ";
      "Up:        case 6 ";
      "Up:        case 7 ";
      "Up:        case 8 ";
      "Up:        case 9 ";
      "Up:        case 10";
      "Up:        case 11";
      "Up:        case 12";
      "Up:        case 13";
      "Up:        case 14";
      "Up:        case 15";
      "Up:        case 16";
      "Down:      case 1 ";
      (*17*)
      "Down:      case 2 ";
      (*18*)
      "Down:      case 3 ";
      (*19*)
      "Down:      case 4 ";
      (*20*)
      "Down:      case 5 ";
      (*21*)
      "Down:      case 6 ";
      (*22*)
      "Down:      case 7 ";
      (*23*)
      "Down:      case 8 ";
      (*24*)
      "Down:      case 9 ";
      (*25*)
      "Down:      case 10";
      (*26*)
      "Down:      case 11";
      (*27*)
      "Down:      case 12";
      (*28*)
      "Down:      case 13";
      (*29*)
      "Down:      case 14";
      (*30*)
      "Down:      case 15";
      (*31*)
      "Down:      case 16";
      (*32*)
      "Look_up:   case  1";
      (*33*)
      "Look_up:   case  2";
      (*34*)
      "Look_up:   case  3";
      (*35*)
      "Look_up:   case  4";
      (*36*)
      "Look_down: case  1";
      (*37*)
      "Look_down: case  2";
      (*38*)
      "Look_down: case  3";
      (*39*)
      "Look_down: case  4";
      (*40*)
    |]

  let propagation_cases = Array.length propagation_labels

  let copy log_info =
    { log_info with propagation = Array.copy log_info.propagation }

  let init_log_info () =
    let time = Sys.time () in
    {
      next_depth = 1;
      global_time = time;
      story_time = time;
      step_time = time;
      current_task = [];
      propagation = Array.make propagation_cases 0;
      branch = 0;
      cut = 0;
      current_stack =
        {
          current_branch = 0;
          selected_events = 0;
          remaining_events = 0;
          removed_events = 0;
          stack_size = 0;
        };
      stack = [];
      last_tick = 0.;
    }

  let log_info_to_json log_info =
    `Assoc
      [
        "global_time", `Float log_info.global_time;
        "story_time", `Float log_info.story_time;
        "step_time", `Float log_info.step_time;
        "next_depth", `Int log_info.next_depth;
        "branch", `Int log_info.branch;
        "cut", `Int log_info.cut;
      ]

  let float_of_json = function
    | `Float f -> f
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct float", x))

  let int_of_json = function
    | `Int f -> f
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct float", x))

  let log_info_of_json x =
    let init = init_log_info () in
    match x with
    | `Assoc l when List.length l = 6 ->
      (try
         {
           init with
           global_time = float_of_json (List.assoc "global_time" l);
           story_time = float_of_json (List.assoc "story_time" l);
           step_time = float_of_json (List.assoc "step_time" l);
           next_depth = int_of_json (List.assoc "next_depth" l);
           branch = int_of_json (List.assoc "branch" l);
           cut = int_of_json (List.assoc "cut" l);
         }
       with Not_found ->
         raise (Yojson.Basic.Util.Type_error ("Not a correct log_info", x)))
    | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct log_info", x))

  let dump_short_log parameter log_info =
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_compression_status_logger parameter)
        "Remaining events: %i ; Stack size: %i ; "
        log_info.current_stack.remaining_events
        log_info.current_stack.stack_size
    in
    Loggers.fprintf
      (Remanent_parameters.get_compression_status_logger parameter)
      "Total branch: %i ; Total cut: %i ; Current depth: %i @." log_info.branch
      log_info.cut log_info.current_stack.current_branch

  let reset_log log =
    let t = log.propagation in
    let _ = Array.fill t 0 (Array.length t) 0 in
    let time = Sys.time () in
    { log with step_time = time; story_time = time }

  let propagate_up i = i
  let propagate_down i = i + 16
  let look_up i = i + 32
  let look_down i = i + 36

  let ellapsed_time log =
    let time = Sys.time () in
    time -. log.story_time

  let ellapsed_global_time log =
    let time = Sys.time () in
    time -. log.global_time

  let set_time log =
    { log with story_time = Sys.time (); step_time = Sys.time () }

  let set_step_time log = { log with step_time = Sys.time () }
  let set_start_compression = set_time

  let set_story_research_time log =
    let t = Sys.time () in
    let st = log.step_time in
    { log with story_time = t -. st; step_time = t }

  let set_concurrent_event_detection_time log = log
  let set_concurrent_event_deletion_time log = log
  let set_grid_generation log = log
  let set_canonicalisation log = log

  let add_case i log =
    let t = log.propagation in
    let _ = t.(i) <- t.(i) + 1 in
    log

  let add_look_down_case i = add_case (look_down i)
  let add_look_up_case i = add_case (look_up i)
  let add_propagation_case_down i = add_case (propagate_down i)
  let add_propagation_case_up i = add_case (propagate_up i)

  let inc_cut log =
    match log.stack with
    | [] -> log
    | t :: q -> { log with current_stack = t; stack = q; cut = log.cut + 1 }

  let inc_branch log =
    {
      log with
      stack = log.current_stack :: log.stack;
      branch = log.branch + 1;
      current_stack =
        {
          log.current_stack with
          current_branch = log.current_stack.current_branch + 1;
        };
    }

  let inc_n_kasim_events log = log
  let inc_n_obs_events log = log
  let inc_n_side_events log = log
  let inc_n_init_events log = log
  let inc_cut_events log = log
  let inc_k_cut_events _k log = log
  let reset_cut_events log = log
  let inc_selected_events log = log
  let inc_removed_events log = log

  let dump_complete_log parameter log_info =
    let logger = Remanent_parameters.get_compression_status_logger parameter in
    let () = Loggers.fprintf logger "/*" in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "Story profiling" in
    let () = Loggers.print_newline logger in
    let () =
      Loggers.fprintf logger "Ellapsed_time:                  %f"
        (ellapsed_time log_info)
    in
    let () = Loggers.print_newline logger in
    let () =
      Loggers.fprintf logger "Story research time:            %f"
        log_info.story_time
    in
    let () = Loggers.print_newline logger in
    let () =
      Loggers.fprintf logger "Exploration depth:              %i"
        log_info.current_stack.current_branch
    in
    let () = Loggers.print_newline logger in
    let () =
      Loggers.fprintf logger "Exploration cuts:               %i" log_info.cut
    in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "***" in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "Propagation Hits:" in
    let () = Loggers.print_newline logger in
    let rec aux k =
      if k >= propagation_cases then
        ()
      else (
        let () =
          let () =
            Loggers.fprintf logger "        %s %i" propagation_labels.(k)
              log_info.propagation.(k)
          in
          let () = Loggers.print_newline logger in
          ()
        in
        aux (k + 1)
      )
    in
    let _ = aux 1 in
    let () = Loggers.fprintf logger "*/" in
    let () = Loggers.print_newline logger in
    ()

  let tick log_info =
    let time = Sys.time () in
    if time -. log_info.last_tick > 600. then
      true, { log_info with last_tick = time }
    else
      false, log_info

  let set_global_cut _n log_info = log_info
  let set_pseudo_inv _n log_info = log_info
end
