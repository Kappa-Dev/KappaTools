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

type cflow_state

val init : unit -> cflow_state ref
val get_std_buffer : cflow_state -> Loggers.t option
val get_err_buffer : cflow_state -> Loggers.t option
val get_profiling_buffer : cflow_state -> Loggers.t option
val get_branch_and_cut_status : cflow_state -> Loggers.t option
val get_progress_bar : cflow_state -> (bool * int * int * int) option
val get_current_phase_title : cflow_state -> string option
val get_causal_flow_table : cflow_state -> Utilities.story_table option
val get_trivial_compression_table : cflow_state -> Utilities.story_table option
val get_weak_compression_table : cflow_state -> Utilities.story_table option
val get_strong_compression_table : cflow_state -> Utilities.story_table option
val get_error_list : cflow_state -> Utilities.error_log option
val save_current_phase_title : cflow_state ref option -> string -> unit
val reset_current_phase_title : cflow_state ref option -> unit
val save_progress_bar : cflow_state ref option -> bool * int * int * int -> unit
val reset_progress_bar : cflow_state ref option -> unit

val save_causal_flow_table :
  cflow_state ref option -> Utilities.story_table -> unit

val save_trivial_compression_table :
  cflow_state ref option -> Utilities.story_table -> unit

val save_weak_compression_table :
  cflow_state ref option -> Utilities.story_table -> unit

val save_strong_compression_table :
  cflow_state ref option -> Utilities.story_table -> unit

val save_error_list : cflow_state ref option -> Utilities.error_log -> unit
val redirect_std_buffer : cflow_state ref option -> Loggers.t option -> unit
val redirect_err_buffer : cflow_state ref option -> Loggers.t option -> unit

val redirect_profiling_buffer :
  cflow_state ref option -> Loggers.t option -> unit

val redirect_branch_and_cut_buffer :
  cflow_state ref option -> Loggers.t option -> unit
