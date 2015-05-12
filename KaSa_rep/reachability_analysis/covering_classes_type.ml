 (**
  * covering_classes_type.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Type definitions for the covering classes relations between the left hand site of a rule and its sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering_classes_type") message exn (fun () -> default)

let local_trace = false

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif

type sites_covering_classes  = int list list AgentMap.t
                                  
type covering_classes =
  {
    store_covering_classes : sites_covering_classes
  }

(************************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

module Inf_array = Int_storage.Nearly_inf_Imperatif
                     
module Set_list_id = Set_and_map.Make
  (struct
    type t = int
    let compare = compare
   end)
    
module Covering_classes =
  struct
    type t = int list
    let compare = compare
  end

module Dictionary_of_Covering_classes = Dictionary.Dictionary_of_Ord (Covering_classes)
                                                                    
type pair_dic = (unit, unit) Dictionary_of_Covering_classes.dictionary
    
type remanent =
  {
    store_dic              : pair_dic;
    store_pointer_backward : Set_list_id.set Inf_array.t
  }
