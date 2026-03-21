(**
  * quark.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2011, the 7th of March
 * Last modification: Time-stamp: <Feb 27 2018>
  *
  * Type definitions for the influence relations between rules and sites.
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let local_trace = false

module Label = Influence_labels.Int_labels
module Labels = Influence_labels.Extensive (Label)
module StringMap = Mods.StringMap

type agent_quark = Ckappa_sig.c_agent_name

type site_quark =
  Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state

module BoundSite = SetMap.Make (struct
  type t =
    Ckappa_sig.c_rule_id
    * Labels.label
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end)

module SiteMap =
  Int_storage.Extend
    (Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif)
    (Int_storage.Extend
       (Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif)
       (Ckappa_sig.State_index_quick_nearly_Inf_Int_storage_Imperatif))

module CounterMap =
  Int_storage.Extend
    (Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif)
    (Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif)

module DeadSiteMap =
  Int_storage.Extend
    (Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif)
    (Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif)

type agents_quarks =
  Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

type sites_quarks =
  Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
  SiteMap.t

type counters_quarks =
  Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
  CounterMap.t

type quarks = {
  dead_agent:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    StringMap.t;
  dead_sites:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    Cckappa_sig.KaSim_Site_map_and_set.Map.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  dead_states:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    DeadSiteMap.t;
  dead_agent_plus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    StringMap.t;
  dead_sites_plus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    Cckappa_sig.KaSim_Site_map_and_set.Map.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  dead_states_plus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    DeadSiteMap.t;
  dead_agent_minus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    StringMap.t;
  dead_sites_minus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    Cckappa_sig.KaSim_Site_map_and_set.Map.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  dead_states_minus:
    Labels.label_set Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.t
    DeadSiteMap.t;
  agent_modif_plus: agents_quarks;
  agent_modif_minus: agents_quarks;
  agent_test: agents_quarks;
  agent_var_minus: agents_quarks;
  site_test_bound: sites_quarks;
  site_modif_bound_minus: sites_quarks;
  site_modif_bound_plus: sites_quarks;
  site_modif_minus: sites_quarks;
  site_test: sites_quarks;
  site_var_minus: sites_quarks;
  site_bound_var_minus: sites_quarks;
  site_bound_var_plus: sites_quarks;
  site_modif_plus: sites_quarks;
  agent_var_plus: agents_quarks;
  site_var_plus: sites_quarks;
  counter_test_geq: counters_quarks;
  counter_test_leq: counters_quarks;
  counter_delta_plus: counters_quarks;
  counter_delta_minus: counters_quarks;
}

type influence_map = Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type influence_maps = {
  wake_up_map: influence_map;
  influence_map: influence_map;
}
