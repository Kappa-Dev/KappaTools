(**
  * priority.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 30/07/2013
  * Last modification: 30/07/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type selection_strategy = All_remaining_events | Wire_with_the_least_number_of_events | Wire_with_the_most_number_of_events
type try_to_remove_first = Late_events | Early_events											  
  
type level = int 
let compare_level = compare 
let min_level a b = if compare a b <= 0 then a else b 
let max_level a b = if compare a b <= 0 then b else a

let gen_level_not_zero (keya,dataa) (keyb,datab) =
  if keya = 0
  then
    keyb,datab
  else if keyb = 0
  then keya,dataa
  else if compare keya keyb < 0
  then keya,dataa
  else keyb,datab
	   
let min_level_not_zero a b = gen_level_not_zero a b
let max_level_not_zero a b = gen_level_not_zero b a
						
let default = 0

module LevelMap = Mods.IntMap
		    
type priorities = 
  { 
    max_level: level ;
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
    max_level = 0;
    creation = 0;
    unbinding = 0;
    removal = 0;
    other_events = 0;
    substitution = 0;
    side_effects = 0;
    candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  }

let weak = 
  { 
    max_level = 1 ;
    creation = 0 ;
    unbinding = 0 ;
    removal = 0 ; 
    other_events = 0;
    substitution = 1;
    side_effects = 1;
     candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  }

let strong = 
  { 
    max_level = 5 ;
    creation = 0 ;
    unbinding = 1 ;
    removal = 2 ;
    other_events = 3 ;
    substitution = 4 ;
    side_effects = 5 ;
    candidate_set_of_events = Wire_with_the_least_number_of_events;
    try_to_remove_first = Late_events;
  } 

let n_story = ref 1 
let n_branch = ref 1 

let zero = (0:level) 
 
