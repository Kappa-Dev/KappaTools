(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(*Expert mode values*)
let defaultExtArraySize = ref 5
let defaultGraphSize = ref 5
let defaultLiftSetSize = ref 5
let defaultHeapSize = ref 5
let debug_modeOn = ref false

(* expert option for stories *)

(* Number of potential states that are put in the cache per binding site, so as to handler with side effects in stories. None -> Unlimited cache *)

(** Memory **)
let cache_size = ref (None : int option)

(* Cut concurrent events (for all observables) before generating the blackboard *)

(** Precomputation **)
let do_global_cut = true

(* Cut pseudo-inverse events *)
let cut_pseudo_inverse_event = true

(* Cut concurrent events (for the current observale) before generating the blackboard *)
let do_local_cut = true

(* Cut separable components *)
let do_detect_separable_components = true

(* Whenever we do not know whether an event has to be selected or, not, check whether this is not the last one that can parform a requested action *)

(** Propagation heuristics **)
let look_up_for_better_cut = true

(* Whenever an event is removed, checked whether there is not only one left to perform a required action *)
let look_down_for_better_cut = true
let log_number_of_causal_flows = true

(*User definable values*)
let time_independent = ref false
let blacklist_events = ref false

(*XLS output for the grids during compression*)
let dump_grid_before_weak_compression = false
let dump_grid_before_strong_compression = false
let dump_grid_after_branching_during_weak_compression = false
let dump_grid_after_branching_during_strong_compression = false
let xlsweakFileName = "grid_weak_compression"
let xlsstrongFileName = "grid_strong_compression"
let get_cache_size () = !cache_size
