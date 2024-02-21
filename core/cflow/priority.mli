(*
  * priority.mli
  *
  * Creation: 03/02/2016
  * Last modification: Time-stamp: <2016-02-19 14:21:39 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  *
  *
  * Some parameters references can be tuned thanks to
  * command-line options other variables has to be set
  * before compilation
  *
  * Copyright 2011 Institut National de Recherche en
  * Informatique et en Automatique.  All rights reserved.
  * This file is distributed
  * under the terms of the GNU Library General Public
  * License *)

(** parameters to tune which event is discarded next *)

type selection_strategy =
  | All_remaining_events
  | Wire_with_the_least_number_of_events
  | Wire_with_the_most_number_of_events

type try_to_remove_first = Late_events | Early_events
type level

val string_of_level : level -> string

(*val strictly_higher_level: level -> level -> bool*)
val min_level : level -> level -> level
val highest : level
val high : level
val above_average : level
val average : level
val bellow_average : level
val low : level
val lowest : level
val lower : level -> level option
val higher : level -> level option

module LevelMap : SetMap.Map with type elt = level

type priorities = {
  creation: level;
  unbinding: level;
  removal: level;
  other_events: level;
  substitution: level;
  side_effects: level;
  candidate_set_of_events: selection_strategy;
  try_to_remove_first: try_to_remove_first;
}

(** each event is associated with a level corresponding of its actions (if multiple action, then, the least corresponding level is selected *)

(** Events with the least level of priority are removed first *)

(** Among them, the choice is driven by the fields 'candidate_set_of_events' and 'try_to_remove_first' *)

val causal : priorities
val weak : priorities
val strong : priorities
val n_story : int ref
val n_branch : int ref
