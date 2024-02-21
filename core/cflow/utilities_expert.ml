(**
 * utilities.ml
 *
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2016-02-19 11:28:05 feret>
 *
 * API for causal compression (for expert)
 * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Jean Krivine, Université Paris-Diderot, CNRS
 *
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.
 * This file is distributed under the terms of the
 * GNU Library General Public License *)

let debug_mode = false
let _ = debug_mode

type parameters = {
  iteration_before_calibrating: int;
  global_time_factor_min: float option;
  global_time_factor_max: float option;
  trace_before_factor_min: int option;
  trace_after_factor_min: int option;
  trace_before_factor_max: int option;
  trace_after_factor_max: int option;
  computation_time_factor_min: float option;
  computation_time_factor_max: float option;
}

let parameters =
  {
    iteration_before_calibrating = 10;
    global_time_factor_min = Some 5.;
    global_time_factor_max = Some 20.;
    trace_before_factor_min = Some 10;
    trace_after_factor_min = Some 10;
    trace_before_factor_max = Some 500;
    trace_after_factor_max = Some 500;
    computation_time_factor_min = Some 10.;
    computation_time_factor_max = Some 500.;
  }

type status_global = {
  counter_min: int;
  global_time_min: float option;
  global_time_max: float option;
  cpu_time_min: float option;
  cpu_time_max: float option;
  cpu_time_ref: float option;
  trace_length_before_min: int option;
  trace_length_after_min: int option;
  trace_length_before_max: int option;
  trace_length_after_max: int option;
  trace_length_before_ref: int option;
  trace_length_after_ref: int option;
}

type status = {
  counter: int;
  global_time: float;
  trace_length_before: int;
  trace_length_after: int;
  cpu_time: float;
}

let get_status_before counter trace =
  {
    counter;
    global_time = Sys.time ();
    trace_length_before = Utilities.size_of_pretrace trace;
    trace_length_after = 0;
    cpu_time = 0.;
  }

let get_status_after status_before trace =
  {
    status_before with
    cpu_time = Sys.time () -. status_before.global_time;
    trace_length_after = Utilities.size_of_pretrace trace;
  }

let cmp_opt cmp a b =
  match a, b with
  | None, _ | _, None -> 0
  | Some a, Some b -> cmp a b

let cmp_float_opt = cmp_opt (compare : float -> float -> int)
let cmp_int_opt = cmp_opt (compare : int -> int -> int)

let gen_bin_opt op a b =
  match a, b with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (op a b)

let add_float_opt = gen_bin_opt ( +. )
let mult_float_opt = gen_bin_opt ( *. )
let mult_int_opt = gen_bin_opt ( * )

let max_opt cmp a b =
  match a with
  | None -> Some b
  | Some a ->
    Some
      (if cmp a b > 0 then
         a
       else
         b)

let stop_before global status =
  status.counter > global.counter_min
  && (cmp_float_opt (Some status.global_time) global.global_time_max > 0
     || cmp_int_opt (Some status.trace_length_before)
          global.trace_length_before_max
        > 0
     || cmp_float_opt (Some status.global_time) global.global_time_min >= 0
        && cmp_int_opt (Some status.trace_length_before)
             global.trace_length_before_min
           >= 0)

let stop_after global status =
  status.counter > global.counter_min
  && (cmp_float_opt (Some status.global_time) global.global_time_max > 0
     || cmp_float_opt (Some status.cpu_time) global.cpu_time_max > 0
     || cmp_int_opt (Some status.trace_length_before)
          global.trace_length_before_max
        > 0
     || cmp_int_opt (Some status.trace_length_after)
          global.trace_length_after_max
        > 0
     || cmp_float_opt (Some status.global_time) global.global_time_min >= 0
        && cmp_float_opt (Some status.cpu_time) global.cpu_time_min >= 0
        && cmp_int_opt (Some status.trace_length_before)
             global.trace_length_before_min
           >= 0
        && cmp_int_opt (Some status.trace_length_after)
             global.trace_length_after_min
           >= 0)

let set_status_init cflow_parameters parameters float1 float2 counter =
  {
    counter_min = counter + parameters.iteration_before_calibrating;
    global_time_min =
      (if Utilities.S.PH.B.PB.CI.Po.K.H.get_is_time_independent cflow_parameters
       then
         None
       else
         add_float_opt (Some float2)
           (mult_float_opt
              (Some (float2 -. float1))
              parameters.global_time_factor_min));
    global_time_max =
      (if Utilities.S.PH.B.PB.CI.Po.K.H.get_is_time_independent cflow_parameters
       then
         None
       else
         add_float_opt (Some float2)
           (mult_float_opt
              (Some (float2 -. float1))
              parameters.global_time_factor_max));
    cpu_time_min = None;
    cpu_time_max = None;
    cpu_time_ref = None;
    trace_length_before_min = None;
    trace_length_after_min = None;
    trace_length_before_max = None;
    trace_length_after_max = None;
    trace_length_before_ref = None;
    trace_length_after_ref = None;
  }

let update_status_before global status =
  if status.counter < global.counter_min then
    {
      global with
      trace_length_before_ref =
        max_opt compare global.trace_length_before_ref
          status.trace_length_before;
    }
  else
    global

let update_status_after global status =
  if status.counter < global.counter_min then
    {
      global with
      trace_length_after_ref =
        max_opt compare global.trace_length_after_ref status.trace_length_after;
      cpu_time_ref = max_opt compare global.cpu_time_ref status.cpu_time;
    }
  else
    global

let compute_status_ranges cflow_parameters parameter global_status =
  {
    global_status with
    cpu_time_min =
      (if Utilities.S.PH.B.PB.CI.Po.K.H.get_is_time_independent cflow_parameters
       then
         None
       else
         mult_float_opt global_status.cpu_time_ref
           parameter.computation_time_factor_min);
    cpu_time_max =
      (if Utilities.S.PH.B.PB.CI.Po.K.H.get_is_time_independent cflow_parameters
       then
         None
       else
         mult_float_opt global_status.cpu_time_ref
           parameter.computation_time_factor_max);
    trace_length_before_min =
      mult_int_opt global_status.trace_length_before_ref
        parameter.trace_before_factor_min;
    trace_length_before_max =
      mult_int_opt global_status.trace_length_before_ref
        parameter.trace_before_factor_max;
    trace_length_after_min =
      mult_int_opt global_status.trace_length_after_ref
        parameter.trace_after_factor_min;
    trace_length_after_max =
      mult_int_opt global_status.trace_length_after_ref
        parameter.trace_after_factor_max;
  }

let fold_over_the_causal_past_of_observables_with_a_progress_bar_while_reshaking_the_trace
    cflow_parameters ~shall_we_compute:_
    ~shall_we_compute_profiling_information:_ handler log_info error we_shall
    never parameters global_trace_simplification f
    (store_result :
      ( Utilities.trace,
        Utilities.trace_runtime_info list,
        'a,
        'a )
      Utilities.ternary) trace (table : 'a) =
  let f cflow_parameters ?shall_we_compute:_
      ?shall_we_compute_profiling_information:_ ?print_if_zero:_ handler
      log_info error trace info
      (last_info, stop_next, global_status, counter, table) =
    if stop_next then
      error, log_info, Stop.stop (last_info, table)
    else (
      let status_before = get_status_before counter trace in
      let stop = stop_before global_status status_before in
      let global_status = update_status_before global_status status_before in
      if stop then
        error, log_info, Stop.stop (last_info, table)
      else (
        let error, log_info, trace =
          f cflow_parameters handler log_info error trace
        in
        let status_after = get_status_after status_before trace in
        let stop = stop_after global_status status_after in
        let error, log_info, table =
          store_result cflow_parameters ~shall_we_compute:we_shall
            ~shall_we_compute_profiling_information:we_shall handler log_info
            error trace info table
        in
        let last_info = Some info in
        let global_status = update_status_after global_status status_after in
        let global_status =
          if counter = global_status.counter_min then
            compute_status_ranges cflow_parameters parameters global_status
          else
            global_status
        in
        ( error,
          log_info,
          Stop.success (last_info, stop, global_status, succ counter, table) )
      )
    )
  in
  let rec aux log_info error counter trace table =
    if Utilities.has_obs trace then (
      let start_iteration = Sys.time () in
      let output =
        try Some (global_trace_simplification 0 (error, log_info, trace))
        with Sys.Break -> None
      in
      match output with
      | None -> error, log_info, table
      | Some (error, log_info, trace) ->
        let end_simplification = Sys.time () in
        let status =
          set_status_init cflow_parameters parameters start_iteration
            end_simplification counter
        in
        let error, log_info, output =
          Utilities.fold_over_the_causal_past_of_observables_with_a_progress_bar
            cflow_parameters ~shall_we_compute:we_shall
            ~shall_we_compute_profiling_information:we_shall handler log_info
            error we_shall never counter f trace
            (None, false, status, counter, table)
        in
        Stop.success_or_stop
          (fun (_, _, _, _, output) -> error, log_info, output)
          (fun ((last_info, table), counter) ->
            let error, log_info, trace =
              Utilities.remove_obs_before cflow_parameters handler log_info
                error last_info trace
            in
            aux log_info error counter trace table)
          output
    ) else
      error, log_info, table
  in
  aux log_info error 1 trace table
