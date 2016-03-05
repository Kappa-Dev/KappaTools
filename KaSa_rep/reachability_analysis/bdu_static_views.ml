(**
   * bdu_static_views.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   * 
   * Creation: 2016, the 18th of Feburary
   * Last modification: 
   * 
   * Compute the relations between sites in the BDU data structures
   * 
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche 
   * en Informatique et en Automatique.  
   * All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)

open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_static_views") message exn
    (fun () -> default)

let local_trace = false

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif

module AgentSite_map_and_set = Cckappa_sig.AgentSite_map_and_set

module AgentRuleCV_setmap = Covering_classes_type.AgentRuleCV_setmap

module Rule_setmap = Cckappa_sig.Rule_setmap

module AgentCV_setmap = Covering_classes_type.AgentCV_setmap

module Project2bdu_creation = Covering_classes_type.Project2bdu_creation

module AgentsRuleCV_map_and_set = Covering_classes_type.AgentsRuleCV_map_and_set

module AgentsRuleCV_setmap = Covering_classes_type.AgentsRuleCV_setmap

module AgentSiteRuleCV_setmap = Covering_classes_type.AgentSiteRuleCV_setmap

module AgentSiteCV_setmap = Covering_classes_type.AgentSiteCV_setmap

module Project2bdu_potential = Covering_classes_type.Project2bdu_potential
    
module AgentsCV_setmap = Covering_classes_type.AgentsCV_setmap
    
module Project2_bdu_views = Covering_classes_type.Project2_bdu_views

module AgentsSite_map_and_set = Cckappa_sig.AgentsSite_map_and_set
 
module Project2_modif = Covering_classes_type.Project2_modif

type pre_static =
  {
    store_modification_sites  :
    (int list * Site_map_and_set.Set.t) AgentsSite_map_and_set.Map.t;
    store_test_sites :
      (int list * Site_map_and_set.Set.t) AgentsSite_map_and_set.Map.t;
    store_test_modification_sites :
      (int list * Site_map_and_set.Set.t) AgentsSite_map_and_set.Map.t;
    (*views that are tested and modificated without agent_id, will be used in
      update function*)
    store_modif_map      : (int list * Site_map_and_set.Set.t) AgentSite_map_and_set.Map.t;
    store_test_map       : (int list * Site_map_and_set.Set.t) AgentSite_map_and_set.Map.t;
    store_test_modif_map : (int list * Site_map_and_set.Set.t) AgentSite_map_and_set.Map.t;
  }

type bdu_analysis_static =
  {
    store_pre_static : pre_static;
    store_covering_classes: Covering_classes_type.remanent Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.t;
    store_covering_classes_id : (int list * int list) AgentSite_map_and_set.Map.t;
    (*rewrite/ change type of this function ?*)
    store_remanent_triple: ((int * int list * Site_map_and_set.Set.t) list) AgentMap.t;
    store_proj_bdu_creation_restriction_map: 
      Mvbdu_wrapper.Mvbdu.mvbdu AgentCV_setmap.Map.t Rule_setmap.Map.t;
    store_modif_list_restriction_map:
      Mvbdu_wrapper.Mvbdu.hconsed_association_list AgentsRuleCV_map_and_set.Map.t;
    store_proj_bdu_potential_restriction_map :
      (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
      AgentSiteCV_setmap.Map.t Rule_setmap.Map.t;
    store_proj_bdu_test_restriction :
      Mvbdu_wrapper.Mvbdu.mvbdu AgentsCV_setmap.Map.t Rule_setmap.Map.t;
  }

(************************************************************************************)
(*implementation of pre_static*)

let collect_modification_sites parameter error rule_id diff_direct store_result =
  (*from a pair of Map (agent_id, agent_type, site) -> rule_id :: old_result)*)
  let add_link error (agent_id, agent_type, site_type) rule_id store_result =
    let error, (l, old) =
      match AgentsSite_map_and_set.Map.find_option_without_logs parameter error
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', current_set = Site_map_and_set.Set.add parameter error rule_id 
      Site_map_and_set.Set.empty 
    in
    let error = Exception.check warn parameter error error' (Some "line 65") Exit in
    let error, result =
      AgentsSite_map_and_set.Map.add_or_overwrite parameter error
        (agent_id, agent_type, site_type) 
        (l, current_set) store_result
    in
    error, result
  in
  let error, store_result =
    Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.fold parameter error
      (fun parameter error agent_id agent_modif store_result ->
        if Site_map_and_set.Map.is_empty agent_modif.agent_interface
        then error, store_result
        else
          let agent_type = agent_modif.agent_name in
          (*return*)
          let error, store_result =
            Site_map_and_set.Map.fold
              (fun site_type _ (error, store_result) ->
                let error, store_result =
                  add_link error (agent_id, agent_type, site_type) rule_id store_result
                in
                error, store_result
              ) agent_modif.agent_interface (error, store_result)
          in
          error, store_result
      ) diff_direct store_result
  in
  let store_result =
    AgentsSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(*update of the views due to modification without agent_id*)

let collect_modif_map parameter error store_modification_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type))
    parameter
    error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    ) store_modification_sites

(************************************************************************************)
(*collect a set of rule_id of test rule and modification *)

let collect_test_sites parameter error rule_id viewslhs 
    store_result =
  let add_link (agent_id, agent_type, site_type) rule_id store_result =
    let error, (l, old) =
      match AgentsSite_map_and_set.Map.find_option_without_logs parameter error
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', current_set =
      Site_map_and_set.Set.add parameter error rule_id Site_map_and_set.Set.empty
    in
    let error = Exception.check warn parameter error error' (Some "line 137") Exit in
    let error, result =
      AgentsSite_map_and_set.Map.add_or_overwrite
        parameter error
        (agent_id, agent_type, site_type)
        (l, current_set) store_result
    in
    error, result
  in
  let error, store_result =
    Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.fold parameter error
      (fun parameter error agent_id agent store_result ->
       match agent with
       | Unknown_agent _ | Ghost -> error, store_result
       | Dead_agent (agent,_,_,_)
       | Agent agent ->
         let agent_type = agent.agent_name in
         let error, store_result_test =
           Site_map_and_set.Map.fold
             (fun site_type _ (error, store_result) ->
               let error, store_result_test =
                 add_link (agent_id, agent_type, site_type) rule_id store_result
               in
               error, store_result_test
             ) agent.agent_interface (error, store_result)
         in
         error, store_result_test
      ) viewslhs store_result
  in
  let store_result =
    AgentsSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(*valuations of the views that are created without agent_id*)

let collect_test_map parameter error store_test_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type))
    parameter
    error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    ) store_test_sites

(************************************************************************************)
(*TODO: modification and test rule that has rule_id union together.
For example:
modification: agent_type:0:site_type:0:[5;6]
test: agent_type:0:site_type:0:[4;5;6;7]
=> result: agent_type:0:site_type:0:[4;5;6;7]
*)

let collect_test_modification_sites
    parameter error store_modification_map store_test_map store_result =
  let add_link error (agent_id, agent_type, site_type) rule_id_set store_result =
    let error, (l, old) =
      match AgentsSite_map_and_set.Map.find_option_without_logs parameter error 
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error, union = Site_map_and_set.Set.union parameter error old rule_id_set in
    let error, result =
      AgentsSite_map_and_set.Map.add_or_overwrite
        parameter error 
        (agent_id, agent_type, site_type)
        (l, union) store_result
    in
    error, result
  in
  AgentsSite_map_and_set.Map.fold2
    parameter error
    (*exists in 'a t*)
    (fun parameter error (agent_id, agent_type, site_type) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_id, agent_type, site_type) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_id, agent_type, site_type) (l1, s1) (l2, s2) store_result ->
      let error',union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 212") Exit in   
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) union store_result
      in
      error, store_result
    ) store_modification_map store_test_map store_result

(*valuations of the views that are created without agent_id*)

let collect_test_modif_map parameter error store_test_modification_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type)
    )
    parameter error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    )
    store_test_modification_sites

(************************************************************************************)

let scan_rule_pre_static parameter error rule_id rule handler_bdu store_result =
  (*-------------------------------------------------------------------------------*)
  (*update of the views due to modification with agent_id*)
  let error, store_modification_sites =
    collect_modification_sites
      parameter
      error
      rule_id
      rule.diff_direct
      store_result.store_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*update of the views due to modification without agent_id*)
  let error, store_modif_map =
    collect_modif_map
      parameter
      error
      store_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations of the views that are tested with agent_id*)
  let error, store_test_sites =
    collect_test_sites
      parameter
      error
      rule_id
      rule.rule_lhs.views
      store_result.store_test_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations of the views that are tested without agent_id*)
  let error, store_test_map =
    collect_test_map
      parameter
      error
      store_test_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification with agent_id*)
  let error, store_test_modification_sites =
    collect_test_modification_sites
      parameter
      error
      store_modification_sites
      store_test_sites
      store_result.store_test_modification_sites
  in
  (*-------------------------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification
    without agent_id*)
  let error, store_test_modif_map =
    collect_test_modif_map
      parameter
      error
      store_test_modification_sites
  in
  error, handler_bdu,
  {
    store_modification_sites      = store_modification_sites;
    store_test_sites              = store_test_sites;
    store_test_modification_sites = store_test_modification_sites;
    store_modif_map               = store_modif_map;
    store_test_map                = store_test_map;
    store_test_modif_map          = store_test_modif_map;
  }

(************************************************************************************)
(*implementation of bdu_analysis_static*)

let site_covering_classes parameter error covering_classes =
  let add_link (agent_type, site_type) cv_id store_result =
    let error, (l, old) =
      match AgentSite_map_and_set.Map.find_option_without_logs parameter error
        (agent_type, site_type) store_result 
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      AgentSite_map_and_set.Map.add_or_overwrite parameter error
        (agent_type, site_type) (l, cv_id :: old) store_result
    in
    error, result
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.fold parameter error
      (fun parameter error agent_type_cv remanent store_result ->
        (*get a list of covering_class_id from remanent*)
        let cv_dic = remanent.Covering_classes_type.store_dic in
        (*fold a dictionary*)
        let error, store_result =
          Covering_classes_type.Dictionary_of_Covering_class.fold
            (fun list_of_site_type ((),()) cv_id (error, store_result) ->
              (*get site_cv in value*)
              List.fold_left (fun (error, store_result) site_type_cv ->
                let error, result =
                  add_link (agent_type_cv, site_type_cv) cv_id store_result
                in 
                error, result
              ) (error, store_result) list_of_site_type
            ) cv_dic (error, store_result)
        in
        error, store_result
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) covering_classes AgentSite_map_and_set.Map.empty
  in
  let store_result =
    AgentSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)

let new_index_pair_map parameter error l = (*JF:  it should be computed only once *)
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 = Cckappa_sig.Site_map_and_set.Map.add parameter error h k map1 in
      let error, map2 = Cckappa_sig.Site_map_and_set.Map.add parameter error k h map2 in   
      aux tl (k+1) map1 map2 error
  in
  let error', (map1, map2) = aux l 1 Cckappa_sig.Site_map_and_set.Map.empty
    Cckappa_sig.Site_map_and_set.Map.empty error in
  let error = Exception.check warn parameter error error' (Some "line 49") Exit in
  error,(map1,map2)

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  let error', set =
    List.fold_left (fun (error,current_set) elt ->
      Cckappa_sig.Site_map_and_set.Set.add parameter error elt current_set
    ) (error, Cckappa_sig.Site_map_and_set.Set.empty) list
  in
  let error = Exception.check warn parameter error error' (Some "line 509") Exit in
  error, set

(************************************************************************************)
(* From each covering class, with their new index for sites, build 
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map restriction
*)

let collect_remanent_triple parameter error store_remanent store_result =
  Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.fold parameter error 
    (fun parameter error agent_type remanent store_result ->
      let store_dic = remanent.Covering_classes_type.store_dic in
      (*-----------------------------------------------------------------*)
      let error, triple_list =
        Covering_classes_type.Dictionary_of_Covering_class.fold
          (fun list _ cv_id (error, current_list) ->
            let error, set = list2set parameter error list in
            let triple_list = (cv_id, list, set) :: current_list in
            error, triple_list
          ) store_dic (error, [])
      in
      (*-----------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error 
          agent_type
          (List.rev triple_list)
          store_result
      in
      error, store_result
    ) store_remanent store_result

(************************************************************************************)
(*creation rules*)

let build_bdu parameter handler error pair_list =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let error, handler, list_a =
    Mvbdu_wrapper.Mvbdu.build_association_list
      parameter
      handler
      error
      pair_list
  in
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter handler error bdu_true list_a
  in
  error, handler, bdu_result
  
let collect_bdu_creation_restriction_map parameter handler error rule_id rule 
    store_remanent_triple store_result =
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link handler (agent_type, rule_id, cv_id) bdu store_result =
    let error, old_bdu =
      match
        AgentRuleCV_setmap.Map.find_option
          (agent_type, rule_id, cv_id) store_result
      with
      | None -> error, bdu_false
      (*default value when there is no creation in this rule*)
      | Some bdu -> error, bdu
    in
    (* In the case when the agent is created twice, we take the union *)
    let error, handler, bdu_new =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error old_bdu bdu 
    in
    let result_map =
      AgentRuleCV_setmap.Map.add (agent_type, rule_id, cv_id) bdu_new store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type' triple_list (handler,store_result) ->
      List.fold_left (fun (error, (handler,store_result)) (agent_id, agent_type) ->
        let error, agent = Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.get parameter error agent_id rule.rule_rhs.views in
        match agent with
	| Some Unknown_agent _ | Some Dead_agent _ 
        | None -> warn parameter error (Some "168") Exit (handler,store_result)
	| Some Ghost -> error, (handler,store_result)
        | Some Agent agent ->
          if agent_type' = agent_type 
          then
            (*-----------------------------------------------------------------*)
            (*get map restriction from covering classes*)
            let error, get_pair_list =
              List.fold_left (fun (error, current_list) (cv_id, list, set) ->
		let error, (map_new_index_forward, _) =
                  new_index_pair_map parameter error list
                in
                (*-----------------------------------------------------------------*)
		let add site state (error, store_result) = 
                  let error, site' =
                    match Site_map_and_set.Map.find_option parameter error
                      site map_new_index_forward 
                    with
                    | error, None -> warn parameter error (Some "282") Exit 0
                    | error, Some s -> error, s
                  in
                  Site_map_and_set.Map.add parameter error site' state store_result 
		in
                (*-----------------------------------------------------------------*)
		let error', map_res =
		  Site_map_and_set.Map.fold_restriction_with_missing_associations
                    parameter error
                    (fun site port -> add site port.site_state.min)
		    (fun site -> add site 0)
		    set
		    agent.agent_interface
		    Site_map_and_set.Map.empty
                in
		let error = 
                  Exception.check warn parameter error error' (Some "line 212") Exit
                in
                error, (cv_id, map_res) :: current_list)
		(error, []) triple_list
            in
            (*-----------------------------------------------------------------*)
            (*fold a list and get a pair of site and state and rule_id*)
            let error, handler, store_result  =
              List.fold_left 
                (fun (error, handler, store_result) (cv_id,map_res) ->
                  let error, pair_list =
                    Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
                        let pair_list = (site', state) :: current_list in
                        error, pair_list
                      ) map_res (error, [])
                  in
		  let error, handler, bdu_creation =
		    build_bdu parameter handler error pair_list
		  in
		   let error, handler, store_result =
		     add_link handler (agent_type, rule_id, cv_id) bdu_creation store_result
		   in
		   error, handler, store_result 
                ) (error, handler, store_result) get_pair_list
            in
            error, (handler, store_result)
          else error, (handler, store_result)
      ) (error, (handler, store_result)) rule.actions.creation
    ) store_remanent_triple (handler, store_result)

(*projection with rule_id*)
(*FIXME: return handler*)

let collect_proj_bdu_creation_restriction_map parameter handler_bdu error
    rule_id rule store_remanent_triple 
    store_result =
  let store_init_bdu_creation_restriction_map =
    AgentRuleCV_setmap.Map.empty
  in
  let error, (handler_bdu, store_bdu_creation_restriction_map) =
    collect_bdu_creation_restriction_map
      (* collect should work directly on the partitioned map (store_result) *)
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_init_bdu_creation_restriction_map
  in
  let error, handler_bdu, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler_bdu error 
  in
  let (error, handler_bdu), store_result' =
    Project2bdu_creation.proj2_monadic
      parameter
      (error, handler_bdu)
      (fun (agent_type, rule_id, cv_id) -> rule_id)
      (fun (agent_type, rule_id, cv_id) -> agent_type, cv_id)
      bdu_true
      (fun parameter (error, handler_bdu) bdu bdu' ->
        let error, handler_bdu, bdu_union = Mvbdu_wrapper.Mvbdu.mvbdu_and
          parameter handler_bdu error bdu bdu'
        in
        (error, handler_bdu), bdu_union
      )
      store_bdu_creation_restriction_map
  in
  let store_result =
     Rule_setmap.Map.fold
      Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler_bdu), store_result

(************************************************************************************)
(*modification rule with creation rules*)

let collect_modif_list_restriction_map
    parameter handler error rule_id rule store_remanent_triple store_result =
  let add_link (agent_id, agent_type, rule_id, cv_id) list_a store_result =
    (*the association must be unique *)
    let error, result_map =
      AgentsRuleCV_map_and_set.Map.add_or_overwrite parameter error
        (agent_id, agent_type, rule_id, cv_id) list_a store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.fold parameter error 
    (fun parameter error agent_id agent_modif (handler, store_result) ->
      if Site_map_and_set.Map.is_empty agent_modif.agent_interface
      then error, (handler, store_result)
      else
        let agent_type = agent_modif.agent_name in
	let error, triple_list =
	  match
	    AgentMap.get parameter error agent_type store_remanent_triple
	  with
	  | error, None -> warn parameter error (Some "Line 476") Exit []
	  | error, Some x -> error, x
	in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          List.fold_left (fun (error, current_list) (cv_id, list, set) ->
            (*-----------------------------------------------------------------*)
            (*new index for site type in covering class*)
            let error, (map_new_index_forward, _) =
              new_index_pair_map parameter error list
            in
            (*-----------------------------------------------------------------*)
            let error', map_res =
              Site_map_and_set.Map.fold_restriction parameter error
                (fun site port (error, store_result) ->
                  let state = port.site_state.min in
                  let error, site' = 
                    Site_map_and_set.Map.find_default_without_logs
                      parameter error 0 site map_new_index_forward 
                  in
                  let error, map_res =
                    Site_map_and_set.Map.add parameter error 
                      site'
                      state
                      store_result
                  in
                  error, map_res
                ) set agent_modif.agent_interface Site_map_and_set.Map.empty
            in
	    let error = Exception.check warn parameter error error' (Some "line 293") Exit 
            in
            error, (cv_id, map_res) :: current_list
	  ) (error, []) triple_list
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, handler, store_result =
          List.fold_left 
            (fun (error, handler, store_result) (cv_id,map_res) ->
	      if Site_map_and_set.Map.is_empty map_res
	      then error, handler, store_result
	      else
		begin
                  (*get a list of pair (site, state) in a map of new indexes of site.*)
		  let error, pair_list =
                    Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
			let pair_list = (site', state) :: current_list in
			error, pair_list
                      ) map_res (error, [])
		  in
		  (*-----------------------------------------------------------------*)
		  (*build list_a*)
		  let error, handler, list_a =
		    Mvbdu_wrapper.Mvbdu.build_association_list
		      parameter
		      handler
		      error
		      pair_list
		  in
		  let error, store_result =
		    add_link (agent_id, agent_type, rule_id, cv_id) list_a store_result
		  in
		  error, handler, store_result
		end
	    )
	    (error, handler, store_result)
	    get_pair_list
	in error, (handler, store_result))
    rule.diff_direct (handler, store_result)


(************************************************************************************)
(*build bdu for potential side effects*)

let store_bdu_potential_restriction_map_aux parameter handler error store_remanent_triple 
    store_potential_side_effects store_result =
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link handler (agent_type, new_site_type, rule_id, cv_id) bdu store_result =
    (*build a list_a*)
    let error, handler, list =
      Mvbdu_wrapper.Mvbdu.build_reverse_sorted_association_list 
        parameter handler error [new_site_type, 0] (*state is 0*)
    in
    let result_map =
      AgentSiteRuleCV_setmap.Map.add 
        (agent_type, new_site_type, rule_id, cv_id) (bdu, list) store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type' triple_list (handler, store_result) ->
      (*map of potential partner side_effect with site is bond*)
      Common_static.AgentRule_map_and_set.Map.fold
        (fun (agent_type, rule_id) pair_list (error, (handler, store_result)) ->
	 if agent_type' = agent_type
         then
           let error, get_pair_list =
             List.fold_left (fun (error, current_list) (cv_id, list, set) ->
	       (*-----------------------------------------------------------------*)
               (*get new indexes for sites*)
	       let error, (map_new_index_forward, _) =
                 new_index_pair_map parameter error list
               in
	       (*-----------------------------------------------------------------*)
               let error', map_res =
                 List.fold_left
		   (fun (error, map_res) (site, state) ->
		     if Site_map_and_set.Set.mem site set
		     then
		       let error, site' =
			 Site_map_and_set.Map.find_default_without_logs 
                           parameter error 0 site map_new_index_forward
		       in
		       let error, old =
			 Site_map_and_set.Map.find_default_without_logs
			   parameter error
			   []
			   site'
			   map_res
		       in
		       let error, map_res =
			 Site_map_and_set.Map.add_or_overwrite parameter error
			   site'
			   (state :: old)
			   map_res
                       in
                       error, map_res
		     else error, map_res
		   ) (error, Site_map_and_set.Map.empty) pair_list
               in
	       (*-----------------------------------------------------------------*)
               let error =
                 Exception.check warn parameter error error' (Some "line 630") Exit
	       in
	       error,
	       Site_map_and_set.Map.fold (fun site' list_state list ->
                 (cv_id, site', list_state) :: list) map_res current_list)
               (error, []) triple_list
           in
	   (*-----------------------------------------------------------------*)
           let error, handler, store_result =
             List.fold_left
	       (fun (error, handler, store_result) (cv_id, site', map_res) ->
		 let error, handler, bdu =
		   List.fold_left (fun (error, handler, bdu) state ->
		     (*------------------------------------------------------------*)     
                     (*build bdu_potential side effects*)
		     let error, handler, bdu_potential_effect =
		       build_bdu parameter handler error [site', state]
		     in
                     (*union of bdu and bdu effect*)
		     let error, handler, bdu =
		       Mvbdu_wrapper.Mvbdu.mvbdu_or 
                         parameter handler error bdu bdu_potential_effect
		     in
		     error, handler, bdu)
		     (error, handler, bdu_false)
		     map_res
		 in 
		 let error, handler, store_result =
		   add_link handler (agent_type, site', rule_id, cv_id) bdu store_result
		 in
		 error, handler, store_result
	       )
	       (error, handler, store_result)
	       get_pair_list
           in
	   error, (handler, store_result)
         else
	   error, (handler, store_result)
	) store_potential_side_effects (error, (handler, store_result))
    ) store_remanent_triple (handler, store_result)

(************************************************************************************)
(*build bdu_potential in the case of binding*)

let store_bdu_potential_effect_restriction_map parameter handler error 
    store_remanent_triple 
    store_potential_side_effects store_result =
  let _, store_potential_bind = store_potential_side_effects in
  let error', (handler, store_result) =
    store_bdu_potential_restriction_map_aux
      parameter
      handler
      error
      store_remanent_triple
      store_potential_bind
      store_result
  in
  let error =
    Exception.check warn parameter error error' (Some "line 675") Exit 
  in
  error, (handler, store_result)

(************************************************************************************)
(*projection with rule_id*)

let collect_proj_bdu_potential_restriction_map parameter handler error
    store_remanent_triple
    store_potential_side_effects 
    store_result =
  let store_init_bdu_potential_restriction_map =
    AgentSiteRuleCV_setmap.Map.empty
  in
  let error, (handler, store_bdu_potential_restriction_map) =
    (* this function should work directly on the partitioned map (store_result) *)
    store_bdu_potential_effect_restriction_map
      parameter
      handler
      error
      store_remanent_triple
      store_potential_side_effects
      store_init_bdu_potential_restriction_map
  in
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  (*an empty hconsed list*)
  let error, handler, empty = 
    Mvbdu_wrapper.Mvbdu.build_reverse_sorted_association_list parameter handler error [] in
  let (error, handler), store_result' =
    Project2bdu_potential.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_type, new_site_name, rule_id, cv_id) -> rule_id)
      (fun (agent_type, new_site_name, rule_id, cv_id) -> agent_type, new_site_name, cv_id)
      (bdu_true, empty)
      (fun _ (error, handler) _ pair' -> (error, handler), pair')
      store_bdu_potential_restriction_map
  in
  let store_result =
    Rule_setmap.Map.fold
      Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler), store_result

(************************************************************************************)

let collect_bdu_test_restriction_map parameter handler error rule_id rule 
    store_remanent_triple store_result =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.fold parameter error
    (fun parameter error agent_id agent (handler,store_result) ->
      match agent with
      | Unknown_agent _
      (* Unfortunatly, we can do nothing with undefined agents in the views domain *)
      (* They will be handled with properly in the agents domain *)
      | Ghost -> error, (handler, store_result)
      | Dead_agent (agent, _, _, _) ->
	  let agent_type = agent.agent_name in
	  let error, triple_list =
	    match
	      AgentMap.unsafe_get parameter error agent_type store_remanent_triple
	    with
	    | error, None -> error, []
	    | error, Some x -> error, x
	  in
	  let error, store_result =
	    List.fold_left
	      (fun (error, store_result) (cv_id, _, _) ->
	       error,
	       AgentsRuleCV_setmap.Map.add 
                 (agent_id, agent_type, rule_id, cv_id) bdu_false store_result
	      )
	      (error, store_result) triple_list
	  in
	  error, (handler, store_result)
      | Agent agent ->
        let agent_type = agent.agent_name in
	let error, triple_list =
	  match
	    AgentMap.unsafe_get parameter error agent_type store_remanent_triple
	  with
	  | error, None ->  error, []
	  | error, Some x -> error, x
	in
	(*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          List.fold_left
	    (fun (error, current_list) (cv_id, list, set) ->
	     (*-----------------------------------------------------------------*)
	     (*new index for site type in covering class*)
	     let error, (map_new_index_forward, _) =
	       new_index_pair_map parameter error list
	     in
             (*-----------------------------------------------------------------*)
             let error', map_res =
	       Site_map_and_set.Map.fold_restriction
		 parameter error
		 (fun site port (error, store_result) ->
                  let state = port.site_state.min in
                  let error, site' = 
                    Site_map_and_set.Map.find_default parameter error 
		      0 site map_new_index_forward 
                  in
                  let error, map_res =
                    Site_map_and_set.Map.add parameter error 
		      site'
		      state
		      store_result
                  in
                  error, map_res
		 ) set agent.agent_interface Site_map_and_set.Map.empty
             in
	     let error = Exception.check warn parameter error error' 
					 (Some "line 178") Exit 
             in
             error, (cv_id, map_res) :: current_list)
	    (error, []) triple_list
        in
        (*-----------------------------------------------------------------*)
        let error, handler, store_result =
          List.fold_left 
            (fun (error, handler, store_result) (cv_id,map_res) ->
	     if Site_map_and_set.Map.is_empty map_res 
		then
		  error, handler, store_result
	     else
	       begin 
		 let error, pair_list =
		   Site_map_and_set.Map.fold
		     (fun site' state (error, current_list) ->
		      let pair_list = (site', state) :: current_list in
		      error, pair_list
		     ) map_res (error, [])
		 in
		 (*build bdu_test*)
		 let error, handler, bdu_test =
		   build_bdu parameter handler error pair_list
		 in
		 let error, store_result =
		   error, AgentsRuleCV_setmap.Map.add
                     (agent_id, agent_type, rule_id, cv_id) 
                     bdu_test 
                     store_result
		 in
		 error, handler, store_result
	       end)
            
	    (error, handler, store_result) get_pair_list
        in
        error, (handler, store_result)
    ) rule.rule_lhs.views  (handler,store_result)
    
(************************************************************************************)

let collect_proj_bdu_test_restriction parameter handler error
    rule_id rule store_remanent_triple 
    store_result =
  let store_init_bdu_test_restriction_map =
    AgentsRuleCV_setmap.Map.empty
  in
  let error, (handler, store_bdu_test_restriction_map) =
    (* collect should work directly on the partitioned map (store_result) *)
    collect_bdu_test_restriction_map 
      parameter
      handler
      error
      rule_id
      rule
      store_remanent_triple
      store_init_bdu_test_restriction_map
  in
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let (error, handler), store_result' =
    Project2_bdu_views.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_id, agent_type, rule_id, cv_id) -> rule_id)
      (fun (agent_id, agent_type, rule_id, cv_id) -> (agent_id, agent_type, cv_id))
      bdu_true
      (fun parameter (error, handler) bdu bdu' ->
        let error, handler, bdu_union = Mvbdu_wrapper.Mvbdu.mvbdu_and
          parameter handler error bdu bdu'
        in
        (error, handler), bdu_union
      )
      store_bdu_test_restriction_map
  in
  let store_result =
     Rule_setmap.Map.fold
      Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler), store_result

(************************************************************************************)

let scan_rule_static parameter error handler_kappa handler_bdu rule_id rule
    store_potential_side_effects compil store_result =
  (*------------------------------------------------------------------------------*)
  (*pre_static*)
  let error, handler_bdu, store_pre_static =
    scan_rule_pre_static
      parameter
      error
      rule_id
      rule
      handler_bdu
      store_result.store_pre_static
  in
  (*------------------------------------------------------------------------------*)
  let error, store_covering_classes =
    Covering_classes_main.covering_classes
      parameter
      error
      handler_kappa
      compil
  in
  (*------------------------------------------------------------------------------*)
  (*static information of covering classes: from sites -> covering_class id list*)
  let error, store_covering_classes_id =
    site_covering_classes
      parameter
      error
      store_covering_classes
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_remanent_triple =
    (* JF: it should be computed only once, not for each rule *)
    collect_remanent_triple
      parameter
      error
      store_covering_classes
      store_result.store_remanent_triple
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_creation_restriction_map =
    collect_proj_bdu_creation_restriction_map
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_proj_bdu_creation_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let error, (handler_bdu, store_modif_list_restriction_map) =
    collect_modif_list_restriction_map
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_modif_list_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_potential_restriction_map =
    collect_proj_bdu_potential_restriction_map
      parameter
      handler_bdu
      error
      store_remanent_triple
      store_potential_side_effects
      store_result.store_proj_bdu_potential_restriction_map
  in
  (*-------------------------------------------------------------------------------*)
  let (error, handler_bdu), store_proj_bdu_test_restriction =
    collect_proj_bdu_test_restriction
      parameter
      handler_bdu
      error
      rule_id
      rule
      store_remanent_triple
      store_result.store_proj_bdu_test_restriction
  in
  (*-------------------------------------------------------------------------------*)
  error, handler_bdu,
  {
    store_pre_static = store_pre_static;
    store_covering_classes = store_covering_classes;
    store_covering_classes_id = store_covering_classes_id;
    store_remanent_triple     = store_remanent_triple;
    store_proj_bdu_creation_restriction_map = store_proj_bdu_creation_restriction_map;
    store_modif_list_restriction_map        = store_modif_list_restriction_map;
    store_proj_bdu_potential_restriction_map = store_proj_bdu_potential_restriction_map;
    store_proj_bdu_test_restriction          = store_proj_bdu_test_restriction;

  }

(************************************************************************************)
(*rule*)

let init_pre_static =
  let init_modification        = AgentsSite_map_and_set.Map.empty in
  let init_test                = AgentsSite_map_and_set.Map.empty in
  let init_test_modification   = AgentsSite_map_and_set.Map.empty in
  let init_modif_map           = AgentSite_map_and_set.Map.empty in
  let init_test_map            = AgentSite_map_and_set.Map.empty in
  let init_test_modif_map      = AgentSite_map_and_set.Map.empty in
  let init_pre_static =
    {
      store_modification_sites      = init_modification;
      store_test_sites              = init_test;
      store_test_modification_sites = init_test_modification;
      store_modif_map               = init_modif_map;
      store_test_map                = init_test_map;
      store_test_modif_map          = init_test_modif_map;
    }
  in init_pre_static

let init_bdu_analysis_static parameter error =
  let error, init_covering_classes =
    Cckappa_sig.Agent_type_storage_quick_nearly_inf_Imperatif.create parameter error 0 
  in
  let init_covering_classes_id = AgentSite_map_and_set.Map.empty in
  let error, init_remanent_triple            = AgentMap.create parameter error 0 in
  let init_proj_bdu_creation_restriction_map = Rule_setmap.Map.empty in
  let init_modif_list_restriction_map        = AgentsRuleCV_map_and_set.Map.empty in
  let init_proj_bdu_potential_restriction_map  = Rule_setmap.Map.empty in
  let init_proj_bdu_test_restriction           = Rule_setmap.Map.empty in
  let init_bdu_analysis_static =
    {
      store_pre_static = init_pre_static;
      store_covering_classes    = init_covering_classes;
      store_covering_classes_id = init_covering_classes_id;
      store_remanent_triple     = init_remanent_triple;
      store_proj_bdu_creation_restriction_map = init_proj_bdu_creation_restriction_map;
      store_modif_list_restriction_map        = init_modif_list_restriction_map;
      store_proj_bdu_potential_restriction_map = init_proj_bdu_potential_restriction_map;
      store_proj_bdu_test_restriction = init_proj_bdu_test_restriction;
    }
  in
  error, init_bdu_analysis_static

(************************************************************************************)

let scan_rule_set parameter handler_bdu error handler_kappa compiled 
   store_potential_side_effects =
  let error, init = init_bdu_analysis_static parameter error in
  let error, (handler_bdu, store_results) =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule (handler_bdu, store_result) ->
        let error, handler_bdu, store_result =
          scan_rule_static
            parameter
            error
	    handler_kappa
	    handler_bdu
            rule_id
            rule.Cckappa_sig.e_rule_c_rule
            store_potential_side_effects
            compiled
            store_result
        in
        error, (handler_bdu, store_result)
      ) compiled.Cckappa_sig.rules (handler_bdu, init)
  in
  error, (handler_bdu, store_results)
