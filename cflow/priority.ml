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


type level = int 
let compare_level = compare 
let min_level a b = if compare a b <=0 then a else b 
let default = 0 

module LevelMap = Map.Make (struct type t = level let compare = compare_level end)

type priorities = 
  { 
    max_level: level ;
    creation: level ;
    unbinding: level ;
    removal: level ;
    other_events: level ;
    substitution: level ; 
    side_effects: level
  }


let strong1 = 
  { 
    max_level = 1 ;
    creation = 0 ;
    unbinding = 0 ;
    removal = 0 ; 
    other_events = 0;
    substitution = 1;
    side_effects = 1;
  }

let strong2 = 
  { 
    max_level = 5 ;
    creation = 0 ;
    unbinding = 1 ;
    removal = 2 ;
    other_events = 3 ;
    substitution = 4 ;
    side_effects = 5 ; 
  } 


let weak = strong1 
let causal = 
  {
    max_level = 0;
    creation = 0;
    unbinding = 0;
    removal = 0;
    other_events = 0;
    substitution = 0;
    side_effects = 0;
  }

let n_story = ref 1 
let n_branch = ref 1 

let zero = (0:level) 
