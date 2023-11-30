(**
 * covering_classes_type.ml
 * openkappa
 * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: 2016, the 2th of March
 * Last modification: Time-stamp: <Feb 13 2018>
 *
 * Type definitions for the covering classes relations between the left hand site of a rule and its sites.
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)
(****************************************************************************************)

type cv_id

module Cv_id_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = cv_id and type dimension = int

val dummy_cv_id : cv_id
val int_of_cv_id : cv_id -> int
val cv_id_of_int : int -> cv_id

(****************************************************************************************)

type covering_classes = {
  store_modified_map:
    Ckappa_sig.c_site_name Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  store_covering_classes:
    Ckappa_sig.c_site_name list list
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
}

module Dictionary_of_List_sites :
  Dictionary.Dictionary
    with type key = cv_id
     and type value = Ckappa_sig.c_site_name list

type pair_dic = (unit, unit) Dictionary_of_List_sites.dictionary

module CV_map_and_set : Map_wrapper.S_with_logs with type elt = cv_id

type remanent = {
  store_pointer_backward:
    CV_map_and_set.Set.t Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t;
  store_dic: pair_dic;
}

(****************************************************************************************)

module AgentCV_map_and_set :
  Map_wrapper.S_with_logs with type elt = Ckappa_sig.c_agent_name * cv_id

module AgentIDCV_map_and_set :
  Map_wrapper.S_with_logs with type elt = Ckappa_sig.c_agent_id * cv_id

module AgentsRuleCV_map_and_set :
  Map_wrapper.S_with_logs
    with type elt =
      Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_rule_id
      * cv_id

(****************************************************************************************)

module AgentCV_setmap : SetMap.S with type elt = Ckappa_sig.c_agent_name * cv_id

module AgentsCV_setmap :
  SetMap.S
    with type elt = Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * cv_id

module AgentSiteCV_setmap :
  SetMap.S
    with type elt = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * cv_id

module AgentRuleCV_setmap :
  SetMap.S
    with type elt = Ckappa_sig.c_agent_name * Ckappa_sig.c_rule_id * cv_id

module AgentsRuleCV_setmap :
  SetMap.S
    with type elt =
      Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_rule_id
      * cv_id

module AgentSiteRuleCV_setmap :
  SetMap.S
    with type elt =
      Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_rule_id
      * cv_id

module Project2bdu_creation :
  SetMap.Projection2
    with type elt_a = Ckappa_sig.c_agent_name * Ckappa_sig.c_rule_id * cv_id
     and type elt_b = Ckappa_sig.c_rule_id
     and type elt_c = Ckappa_sig.c_agent_name * cv_id
     and type 'a map_a = 'a AgentRuleCV_setmap.Map.t
     and type 'a map_b = 'a Ckappa_sig.Rule_setmap.Map.t
     and type 'a map_c = 'a AgentCV_setmap.Map.t

module Project2bdu_potential :
  SetMap.Projection2
    with type elt_a =
      Ckappa_sig.c_agent_name
      * Ckappa_sig.c_site_name
      * Ckappa_sig.c_rule_id
      * cv_id
     and type elt_b = Ckappa_sig.c_rule_id
     and type 'a map_a = 'a AgentSiteRuleCV_setmap.Map.t
     and type 'a map_b = 'a Ckappa_sig.Rule_setmap.Map.t
     and type elt_c = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * cv_id
     and type 'a map_c = 'a AgentSiteCV_setmap.Map.t

module Project2_bdu_views :
  SetMap.Projection2
    with type elt_a =
      Ckappa_sig.c_agent_id
      * Ckappa_sig.c_agent_name
      * Ckappa_sig.c_rule_id
      * cv_id
     and type elt_b = Ckappa_sig.c_rule_id
     and type 'a map_a = 'a AgentsRuleCV_setmap.Map.t
     and type 'a map_b = 'a Ckappa_sig.Rule_setmap.Map.t
     and type elt_c = Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * cv_id
    (* find the appropriate type names *)
     and type 'a map_c = 'a AgentsCV_setmap.Map.t

module Project2_modif :
  Map_wrapper.Projection
    with type elt_a =
      Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name
     and type elt_b = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name
     and type 'a map_a = 'a Ckappa_sig.AgentsSite_map_and_set.Map.t
     and type 'a map_b = 'a Ckappa_sig.AgentSite_map_and_set.Map.t

type predicate_covering_classes = {
  store_covering_classes_predicate:
    remanent Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  store_list_of_site_type_in_covering_classes:
    Ckappa_sig.c_site_name list AgentCV_map_and_set.Map.t;
  store_covering_classes_id: cv_id list Ckappa_sig.AgentSite_map_and_set.Map.t;
  store_remanent_triple:
    (Dictionary_of_List_sites.key
    * Dictionary_of_List_sites.value
    * Ckappa_sig.Site_map_and_set.Set.t)
    list
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  site_correspondence:
    (Ckappa_sig.c_site_name
     Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t
    * Ckappa_sig.c_site_name
      Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t)
    Cv_id_nearly_Inf_Int_storage_Imperatif.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
}
