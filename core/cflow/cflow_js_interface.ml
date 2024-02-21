(*
  * cflow_js_interface.ml
  *
  * Creation:                      <2016-04-30 18:34:00 feret>
  * Last modification: Time-stamp: <2016-04-30 18:24:00 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS
  *
  * *
  * This is the interface for the Java-script server
  *
  * Copyright 2011,2012,2013,2014,2015,2016 Institut National de Recherche en
  * Informatique et en Automatique.  All rights reserved.  This file is
  * distributed under the terms of the GNU Library General Public License *)

type cflow_state = {
  std_logger: Loggers.t option;
  err_logger: Loggers.t option;
  profiling_logger: Loggers.t option;
  status_logger: Loggers.t option;
  current_phase: string option;
  progress_bar: (bool * int * int * int) option;
  causal_flows: Utilities.story_table option;
  trivial_compression: Utilities.story_table option;
  weak_compression: Utilities.story_table option;
  strong_compression: Utilities.story_table option;
  error_list: Utilities.error_log option;
}

let init () =
  ref
    {
      std_logger = None;
      err_logger = None;
      profiling_logger = None;
      status_logger = None;
      current_phase = None;
      progress_bar = None;
      causal_flows = None;
      trivial_compression = None;
      weak_compression = None;
      strong_compression = None;
      error_list = None;
    }

let get_std_buffer cflow_state = cflow_state.std_logger
let get_err_buffer cflow_state = cflow_state.err_logger
let get_profiling_buffer cflow_state = cflow_state.profiling_logger
let get_branch_and_cut_status cflow_state = cflow_state.status_logger
let get_progress_bar cflow_state = cflow_state.progress_bar
let get_current_phase_title cflow_state = cflow_state.current_phase
let get_causal_flow_table cflow_state = cflow_state.causal_flows
let get_trivial_compression_table cflow_state = cflow_state.trivial_compression
let get_weak_compression_table cflow_state = cflow_state.weak_compression
let get_strong_compression_table cflow_state = cflow_state.strong_compression
let get_error_list cflow_state = cflow_state.error_list

let save_current_phase_title_aux cflow_state_ptr string =
  { !cflow_state_ptr with current_phase = Some string }

let reset_current_phase_title_aux cflow_state_ptr =
  { !cflow_state_ptr with current_phase = None }

let save_progress_bar_aux cflow_state_ptr bar =
  { !cflow_state_ptr with progress_bar = Some bar }

let reset_progress_bar_aux cflow_state_ptr =
  { !cflow_state_ptr with progress_bar = None }

let save_causal_flow_table_aux cflow_state_ptr table =
  { !cflow_state_ptr with causal_flows = Some table }

let save_trivial_compression_table_aux cflow_state_ptr table =
  { !cflow_state_ptr with trivial_compression = Some table }

let save_weak_compression_table_aux cflow_state_ptr table =
  { !cflow_state_ptr with weak_compression = Some table }

let save_strong_compression_table_aux cflow_state_ptr table =
  { !cflow_state_ptr with strong_compression = Some table }

let save_error_log_aux cflow_state_ptr error =
  { !cflow_state_ptr with error_list = Some error }

let redirect_std_buffer_aux cflow_state_ptr loggers =
  { !cflow_state_ptr with std_logger = loggers }

let redirect_err_buffer_aux cflow_state_ptr loggers =
  { !cflow_state_ptr with err_logger = loggers }

let redirect_profiling_buffer_aux cflow_state_ptr loggers =
  { !cflow_state_ptr with profiling_logger = loggers }

let redirect_branch_and_cut_buffer_aux cflow_state_ptr loggers =
  { !cflow_state_ptr with status_logger = loggers }

let lift f cflow_state_ptr_opt data =
  match cflow_state_ptr_opt with
  | None -> ()
  | Some cflow_state_ptr -> cflow_state_ptr := f cflow_state_ptr data

let lift_reset f cflow_state_ptr_opt =
  lift (fun x () -> f x) cflow_state_ptr_opt ()

let save_current_phase_title = lift save_current_phase_title_aux
let reset_current_phase_title = lift_reset reset_current_phase_title_aux
let save_progress_bar = lift save_progress_bar_aux
let reset_progress_bar = lift_reset reset_progress_bar_aux
let save_causal_flow_table = lift save_causal_flow_table_aux
let save_trivial_compression_table = lift save_trivial_compression_table_aux
let save_weak_compression_table = lift save_weak_compression_table_aux
let save_strong_compression_table = lift save_strong_compression_table_aux
let save_error_list = lift save_error_log_aux
let redirect_std_buffer = lift redirect_std_buffer_aux
let redirect_err_buffer = lift redirect_err_buffer_aux
let redirect_profiling_buffer = lift redirect_profiling_buffer_aux
let redirect_branch_and_cut_buffer = lift redirect_branch_and_cut_buffer_aux
