(**
 * covering_classes_type.ml
 * openkappa
 * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: 2015, the 23th of Feburary
 * Last modification: Time-stamp: <Feb 13 2018>
 *
 * Type definitions for the covering classes relations between the left hand site of a rule and its sites.
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let local_trace = false
let _ = local_trace

type covering_classes = {
  store_modified_map:
    Ckappa_sig.c_site_name Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  store_covering_classes:
    Ckappa_sig.c_site_name list list
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
}

(***************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

type cv_id = int

module Cv_id_nearly_Inf_Int_storage_Imperatif :
  Int_storage.Storage with type key = cv_id and type dimension = int =
  Int_storage.Nearly_inf_Imperatif

let dummy_cv_id = 0
let int_of_cv_id (a : cv_id) : int = a
let cv_id_of_int (a : int) : cv_id = a

(***************************************************************************)

module List_sites = struct
  type t = Ckappa_sig.c_site_name list (*value type*)

  let compare = compare
  let print = Pp.list Pp.space (fun _ _ -> ())
end

module CV_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = cv_id

  let compare = compare
  let print = Format.pp_print_int
end))

module Dictionary_of_List_sites :
  Dictionary.Dictionary
    with type key = cv_id
     and type value = Ckappa_sig.c_site_name list =
  Dictionary.Dictionary_of_Ord (List_sites)

type pair_dic = (unit, unit) Dictionary_of_List_sites.dictionary

type remanent = {
  store_pointer_backward:
    CV_map_and_set.Set.t Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.t;
  store_dic: pair_dic;
}

module AgentCV_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * cv_id

  let compare = compare
  let print _ _ = ()
end))

module AgentIDCV_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_agent_id * cv_id

  let compare = compare
  let print _ _ = ()
end))

module AgentsRuleCV_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_rule_id
    * cv_id

  let compare = compare
  let print _ _ = ()
end))

(******************************************************************************)

module AgentCV_setmap = SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * cv_id

  let compare = compare
  let print _ _ = ()
end)

module AgentsCV_setmap = SetMap.Make (struct
  type t = Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * cv_id

  let compare = compare
  let print _ _ = ()
end)

module AgentSiteCV_setmap = SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * cv_id

  let compare = compare
  let print _ _ = ()
end)

module AgentRuleCV_setmap = SetMap.Make (struct
  type t = Ckappa_sig.c_agent_name * Ckappa_sig.c_rule_id * cv_id

  let compare = compare
  let print _ _ = ()
end)

module AgentsRuleCV_setmap = SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_id
    * Ckappa_sig.c_agent_name
    * Ckappa_sig.c_rule_id
    * cv_id

  let compare = compare
  let print _ _ = ()
end)

module AgentSiteRuleCV_setmap = SetMap.Make (struct
  type t =
    Ckappa_sig.c_agent_name
    * Ckappa_sig.c_site_name
    * Ckappa_sig.c_rule_id
    * cv_id

  let compare = compare
  let print _ _ = ()
end)

(****************************************************************************)

module Project2bdu_creation =
  SetMap.Proj2 (AgentRuleCV_setmap) (Ckappa_sig.Rule_setmap) (AgentCV_setmap)

module Project2bdu_potential =
  SetMap.Proj2 (AgentSiteRuleCV_setmap) (Ckappa_sig.Rule_setmap)
    (AgentSiteCV_setmap)

module Project2_bdu_views =
  SetMap.Proj2 (AgentsRuleCV_setmap) (Ckappa_sig.Rule_setmap) (AgentsCV_setmap)

module Project2_modif =
  Map_wrapper.Proj
    (Ckappa_sig.AgentsSite_map_and_set)
    (Ckappa_sig.AgentSite_map_and_set)

type predicate_covering_classes = {
  store_covering_classes_predicate:
    remanent Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
  store_list_of_site_type_in_covering_classes:
    Ckappa_sig.c_site_name list AgentCV_map_and_set.Map.t;
  store_covering_classes_id: cv_id list Ckappa_sig.AgentSite_map_and_set.Map.t;
  (*rewrite/ change type of this function ?*)
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
