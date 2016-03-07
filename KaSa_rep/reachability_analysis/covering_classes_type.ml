 (**
  * covering_classes_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Type definitions for the covering classes relations between the left hand site of a rule and its sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Int_storage
open Cckappa_sig
open Dictionary

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering_classes_type") message exn (fun () -> default)

let local_trace = false

type 'a map = 'a Site_map_and_set.Map.t
type set    = Site_map_and_set.Set.t

type covering_classes =
  {
    store_modified_map     : int map Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.t;
    store_covering_classes : int list list Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.t;
  }

(************************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

module Inf_array = Nearly_inf_Imperatif

module Covering_class =
  struct
    type t = int list
    let compare = compare
  end

module Modified_class =
  struct
    type t = int list
    let compare = compare
  end
  
(*Dictionary*)

module Dictionary_of_Covering_class = Dictionary_of_Ord (Covering_class)
module Dictionary_of_Modified_class = Dictionary_of_Ord (Modified_class)

type pair_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type index_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
type test_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type modif_dic  = (unit, unit) Dictionary_of_Modified_class.dictionary

type remanent =
    {
      store_pointer_backward    : set Inf_array.t;
      store_dic                 : pair_dic;
      store_new_index_dic       : index_dic;
      store_test_new_index_dic  : test_dic;
      store_modif_new_index_dic : modif_dic;
    }

(************************************************************************************)
(* cckappa_sig is the signature for an intermediary representation of
   Kappa, there is no covering class, thus this type should not be defined
   here *)
(* Please put any type/module definition related to covering class in a
   file reachability/covergin_class_sig.ml *)

type cv_id = int
type agent_id = Cckappa_sig.agent_id
type agent_name = Cckappa_sig.agent_name
type rule_id = Cckappa_sig.rule_id
type site_name = Cckappa_sig.site_name

module AgentCV_map_and_set =
  Map_wrapper.Make (
    SetMap.Make (
      struct
        type t = agent_name * cv_id
        let compare = compare
      end))

module AgentIDCV_map_and_set =
  Map_wrapper.Make (
    SetMap.Make (
      struct
        type t = agent_id * cv_id
        let compare = compare
      end))

module AgentsRuleCV_map_and_set =
  Map_wrapper.Make
    (SetMap.Make (
      struct
        type t = agent_id * agent_name * rule_id * cv_id
        let compare = compare
      end))

module AgentCV_setmap =
  SetMap.Make (
    struct
      type t = agent_name * cv_id
      let compare = compare
    end)

module AgentsCV_setmap =
  SetMap.Make
    (struct 
      type t = agent_id * agent_name * cv_id
      let compare = compare
     end)

module AgentSiteCV_setmap =
  SetMap.Make (
    struct
      type t = agent_name * site_name * cv_id
      let compare = compare
    end)

module AgentRuleCV_setmap =
  SetMap.Make (
    struct
      type t = agent_name * rule_id * cv_id
      let compare = compare
    end)

module AgentsRuleCV_setmap =
  (SetMap.Make (
    struct
      type t = agent_id * agent_name * rule_id * cv_id
      let compare = compare
    end))

module AgentSiteRuleCV_setmap =
  SetMap.Make (
    struct
      type t = agent_name * site_name * rule_id * cv_id
      let compare = compare
    end)

module Project2bdu_creation =
  SetMap.Proj2 (AgentRuleCV_setmap)(Rule_setmap)(AgentCV_setmap)

module Project2bdu_potential =
  SetMap.Proj2 (AgentSiteRuleCV_setmap)(Rule_setmap)(AgentSiteCV_setmap)
    
module Project2_bdu_views =
  SetMap.Proj2 (AgentsRuleCV_setmap)(Rule_setmap)(AgentsCV_setmap)

module Project2_modif =
  Map_wrapper.Proj (AgentsSite_map_and_set) (AgentSite_map_and_set)

module Project_agent =
  Map_wrapper.Proj (AgentCV_map_and_set) (AgentIDCV_map_and_set)
