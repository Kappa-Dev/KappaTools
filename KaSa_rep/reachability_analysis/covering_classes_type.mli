 (**
  * covering_classes_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 2th of March
  * Last modification: 
  * 
  * Type definitions for the covering classes relations between the left hand site of a rule and its sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type 'a map = 'a Cckappa_sig.Site_map_and_set.Map.t
type set    = Cckappa_sig.Site_map_and_set.Set.t

type covering_classes =
  {
    store_modified_map     : Cckappa_sig.site_name map Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.t;
    store_covering_classes : Cckappa_sig.site_name list list Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.t;
  }

module Dictionary_of_Covering_class : Dictionary.Dictionary with type value = Cckappa_sig.site_name list
module Dictionary_of_Modified_class : Dictionary.Dictionary with type value = Cckappa_sig.site_name list

type pair_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type index_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
type test_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type modif_dic  = (unit, unit) Dictionary_of_Modified_class.dictionary

type remanent =
    {
      store_pointer_backward    : set  Int_storage.Nearly_inf_Imperatif.t;
      store_dic                 : pair_dic;
      store_new_index_dic       : index_dic;
      store_test_new_index_dic  : test_dic;
      store_modif_new_index_dic : modif_dic;
    }

type cv_id = int (* this type declaration should be hidden *)
	       
(*type agent_id = Cckappa_sig.agent_id
type agent_name = Cckappa_sig.agent_name
type rule_id = Cckappa_sig.rule_id
type site_name = Cckappa_sig.site_name*)

module AgentCV_map_and_set: Map_wrapper.S_with_logs with type elt = Cckappa_sig.agent_name * cv_id

module AgentIDCV_map_and_set: Map_wrapper.S_with_logs with type elt = Cckappa_sig.agent_id * cv_id

module AgentsRuleCV_map_and_set: Map_wrapper.S_with_logs 
  with type elt = Cckappa_sig.agent_id * Cckappa_sig.agent_name * Cckappa_sig.rule_id * cv_id

module AgentCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_name * cv_id

module AgentsCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_id * Cckappa_sig.agent_name * cv_id

module AgentSiteCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_name * Cckappa_sig.site_name * cv_id

module AgentRuleCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_name * Cckappa_sig.rule_id * cv_id

module AgentsRuleCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_id * Cckappa_sig.agent_name * Cckappa_sig.rule_id * cv_id

module AgentSiteRuleCV_setmap: SetMap.S with type elt = Cckappa_sig.agent_name * Cckappa_sig.site_name * Cckappa_sig.rule_id * cv_id

module Project2bdu_creation: SetMap.Projection2
       with type elt_a = Cckappa_sig.agent_name * Cckappa_sig.rule_id * cv_id (* find the appropriate type names *)
  and type elt_b = Cckappa_sig.rule_id
  and type elt_c = Cckappa_sig.agent_name * cv_id (* find the appropriate type names *)
  and type 'a map_a = 'a AgentRuleCV_setmap.Map.t
  and type 'a map_b = 'a Cckappa_sig.Rule_setmap.Map.t
  and type 'a map_c = 'a AgentCV_setmap.Map.t

module Project2bdu_potential: SetMap.Projection2
  with type elt_a = Cckappa_sig.agent_name * Cckappa_sig.site_name * Cckappa_sig.rule_id * cv_id (* find the appropriate type names *)
  and type elt_b = Cckappa_sig.rule_id
  and type 'a map_a = 'a AgentSiteRuleCV_setmap.Map.t
  and type 'a map_b = 'a Cckappa_sig.Rule_setmap.Map.t
  and type elt_c = Cckappa_sig.agent_name * Cckappa_sig.site_name * cv_id (* find the appropriate type names *)
  and type 'a map_c = 'a AgentSiteCV_setmap.Map.t

module Project2_bdu_views: SetMap.Projection2
  with type elt_a = Cckappa_sig.agent_id * Cckappa_sig.agent_name * Cckappa_sig.rule_id * cv_id (* find the appropriate type names *)
  and type elt_b = Cckappa_sig.rule_id
  and type 'a map_a = 'a AgentsRuleCV_setmap.Map.t
  and type 'a map_b = 'a Cckappa_sig.Rule_setmap.Map.t
  and type elt_c = Cckappa_sig.agent_id * Cckappa_sig.agent_name * cv_id (* find the appropriate type names *)
  and type 'a map_c = 'a AgentsCV_setmap.Map.t

module Project2_modif: Map_wrapper.Projection
  with type elt_a = Cckappa_sig.agent_id * Cckappa_sig.agent_name * Cckappa_sig.site_name (* find the appropriate type names *)
  and type elt_b = Cckappa_sig.agent_name * Cckappa_sig.site_name
  and type 'a map_a = 'a Cckappa_sig.AgentsSite_map_and_set.Map.t
  and type 'a map_b = 'a Cckappa_sig.AgentSite_map_and_set.Map.t
