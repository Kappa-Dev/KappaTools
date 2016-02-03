(**
  * priority.ml
  *
  *
  * Creation:                      <2013-07-30 feret>
  * Last modification: Time-stamp: <2016-02-03 20:18:59 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type selection_strategy = All_remaining_events | Wire_with_the_least_number_of_events | Wire_with_the_most_number_of_events
type try_to_remove_first = Late_events | Early_events

type level = Highest | High | Above_average | Average | Bellow_average | Low | Lowest

let weight x =
  match
    x
  with
  | Highest -> 0
  | High -> 1
  | Above_average -> 2
  | Average -> 3
  | Bellow_average -> 4
  | Low -> 5
  | Lowest -> 6

let compare_level lvl1 lvl2 = compare (weight lvl1) (weight lvl2)
let min_level a b = if compare a b <= 0 then a else b
let max_level a b = if compare a b <= 0 then b else a

let highest = Highest
let high = High
let above_average = Above_average
let average = Average
let bellow_average = Bellow_average
let low = Low
let lowest = Lowest

let string_of_level level =
  match
    level
  with
  | Highest -> "highest"
  | High  -> "high"
  | Above_average -> "above average"
  | Average -> "average"
  | Bellow_average -> "bellow average"
  | Low -> "low"
  | Lowest -> "lowest"

let lower level =
  match
    level
  with
  | Highest -> Some High
  | High  -> Some Above_average
  | Above_average -> Some Average
  | Average -> Some Bellow_average
  | Bellow_average -> Some Low
  | Low -> Some Lowest
  | Lowest -> None

let higher level =
  match
    level
  with
  | Highest -> None
  | High  -> Some Highest
  | Above_average -> Some High
  | Average -> Some Above_average
  | Bellow_average -> Some Average
  | Low -> Some Bellow_average
  | Lowest -> Some Low


module LevelSetMap = SetMap.Make (struct type t = level let compare = compare end)
module LevelMap = LevelSetMap.Map

type priorities =
  {
    creation: level ;
    unbinding: level ;
    removal: level ;
    other_events: level ;
    substitution: level ;
    side_effects: level ;
    candidate_set_of_events: selection_strategy;
    try_to_remove_first: try_to_remove_first;
  }
(** each event is associated with a level corresponding of its actions (if multiple action, then, the least corresponding level is selected *)
(** Events with the least level of priority are removed first *)
(** Among them, the choice is driven by the fields 'candidate_set_of_events' and 'try_to_remove_first' *)

let causal =
  {
    creation = Highest ;
    unbinding = Highest ;
    removal = Highest ;
    other_events = Highest ;
    substitution = Highest ;
    side_effects = Highest ;
    candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  }

let weak =
  {
    creation = Highest ;
    unbinding = Highest ;
    removal = Highest ;
    other_events = Highest ;
    substitution = High ;
    side_effects = High ;
    candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  }

let strong =
  {
    creation = Highest ;
    unbinding = High ;
    removal = Above_average ;
    other_events = Average ;
    substitution = Bellow_average ;
    side_effects = Low ;
    candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  }

let n_story = ref 1
let n_branch = ref 1
