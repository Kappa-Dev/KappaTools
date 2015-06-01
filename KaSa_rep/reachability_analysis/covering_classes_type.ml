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

type agent_dic = Ckappa_sig.agent_dic
type site = int
type set = Cckappa_sig.Site_map_and_set.set
type 'a map = 'a Cckappa_sig.Site_map_and_set.map
type agent_name = Cckappa_sig.agent_name
type agent_index = int

(*state information*)
type port_min = set AgentMap.t
type port_max = set AgentMap.t

(*site effect*)
type know_unbinding = (agent_index * agent_name * site * 
                         agent_index * agent_name * site) list

(*deletion*)
type document_del = (agent_index * agent_name * site) map
type undocument_del= (agent_index * agent_name * site) map
type deletion = document_del AgentMap.t * undocument_del AgentMap.t

type covering_classes =
  {
    store_modified_map     : (int map) AgentMap.t;
    store_half_break       : set AgentMap.t;
    store_unbinding        : know_unbinding;
    store_remove_map       : deletion;
    store_covering_classes : ((site * set * set) list list) AgentMap.t 
                             * port_min * port_max
  }

(************************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

module Inf_array = Int_storage.Nearly_inf_Imperatif

module Covering_class =
  struct
    type t = (site * set * set) list
    let compare = compare
  end

module Modified_class =
  struct
    type t = site list
    let compare = compare
  end

(*site effects*)
type orignial_site = site
type new_index     = site
type unknow_unbinding = (orignial_site * new_index) list

module Halfbreak_class =
  struct
    type t = unknow_unbinding
    let compare = compare
  end

(*Dictionary*)

module Dictionary_of_Covering_class = Dictionary.Dictionary_of_Ord (Covering_class)
module Dictionary_of_Modified_class = Dictionary.Dictionary_of_Ord (Modified_class)
module Dictionary_of_Halfbreak_class = Dictionary.Dictionary_of_Ord (Halfbreak_class)

type pair_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type index_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
type test_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type modif_dic  = (unit, unit) Dictionary_of_Modified_class.dictionary
type halfbreak_dic = (unit, unit) Dictionary_of_Halfbreak_class.dictionary

type remanent =
    {
      store_pointer_backward    : set Inf_array.t;
      store_dic                 : pair_dic;
      store_new_index_dic       : index_dic;
      store_test_new_index_dic  : test_dic;
      store_modif_new_index_dic : modif_dic;
      store_halfbreak_dic       : halfbreak_dic;
    }
