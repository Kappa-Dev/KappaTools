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

type agent_dic  = Ckappa_sig.agent_dic
type set        = Cckappa_sig.Site_map_and_set.set
type value_site = (Ckappa_sig.site_name, Ckappa_sig.site_name) Ckappa_sig.site_type
type port       = Cckappa_sig.state_index Cckappa_sig.interval Cckappa_sig.port

type sites_covering_classes  = int list list
                             
type covering_classes =
  {
    store_sites_modified_set : (set AgentMap.t * value_site list AgentMap.t);
    store_covering_classes   : (sites_covering_classes *  string) AgentMap.t
  }

(************************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

module Inf_array = Int_storage.Nearly_inf_Imperatif
                     
module Covering_class =
  struct
    type t = int list
    let compare = compare
  end

module Dictionary_of_Covering_class = Dictionary.Dictionary_of_Ord (Covering_class)
                                                                    
type pair_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
type index_dic = (unit, unit) Dictionary_of_Covering_class.dictionary
type test_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
    
type remanent =
  {
    store_pointer_backward   : set Inf_array.t;
    store_dic                : (set Inf_array.t * pair_dic);
    store_new_index_dic      : index_dic;
    store_test_new_index_dic : test_dic;
  }
