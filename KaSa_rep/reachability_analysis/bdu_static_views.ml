(**
   * bdu_static_views.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 18th of Feburary
   * Last modification: Time-stamp: <Aug 03 2016>
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_static_views") message exn
    (fun () -> default)

let local_trace = true

type pre_static =
  {
    store_modification_sites  : Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
    store_test_sites : Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
    store_test_modification_sites : Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentsSite_map_and_set.Map.t;
    (*views that are tested and modificated without agent_id, will be used in
      update function*)
    store_modif_map: Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentSite_map_and_set.Map.t;
    store_test_map : Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentSite_map_and_set.Map.t;
    store_test_modif_map: Ckappa_sig.Rule_map_and_set.Set.t Ckappa_sig.AgentSite_map_and_set.Map.t;
  }

type bdu_analysis_static =
  {
    store_pre_static : pre_static;
    store_covering_classes: Covering_classes_type.remanent
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
    store_covering_classes_id : Covering_classes_type.cv_id list Ckappa_sig.AgentSite_map_and_set.Map.t;
    (*rewrite/ change type of this function ?*)
    store_remanent_triple:
      ((Covering_classes_type.Dictionary_of_List_sites.key *
        Covering_classes_type.Dictionary_of_List_sites.value *
        Ckappa_sig.Site_map_and_set.Set.t) list)
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t;
    store_proj_bdu_creation_restriction_map:
      Ckappa_sig.Views_bdu.mvbdu
        Covering_classes_type.AgentCV_setmap.Map.t
        Ckappa_sig.Rule_setmap.Map.t;
    store_modif_list_restriction_map:
      Ckappa_sig.Views_bdu.hconsed_association_list
        Covering_classes_type.AgentsRuleCV_map_and_set.Map.t;
    store_proj_bdu_potential_restriction_map :
      (Ckappa_sig.Views_bdu.mvbdu * Ckappa_sig.Views_bdu.hconsed_association_list)
        Covering_classes_type.AgentSiteCV_setmap.Map.t
        Ckappa_sig.Rule_setmap.Map.t;
    store_proj_bdu_test_restriction :
      Ckappa_sig.Views_bdu.mvbdu
        Covering_classes_type.AgentsCV_setmap.Map.t
        Ckappa_sig.Rule_setmap.Map.t;
  }

(************************************************************************************)
(*implementation of pre_static*)

let collect_modification_sites parameter error rule_id diff_direct store_result =
  let add_link error (agent_id, agent_type, site_type) rule_id store_result =
    let error', current_set =
      Ckappa_sig.Rule_map_and_set.Set.add
        parameter
        error
        rule_id
        Ckappa_sig.Rule_map_and_set.Set.empty
    in
    let error = Exception.check warn parameter error error' (Some "line 65") Exit in
    let error, result =
      Ckappa_sig.AgentsSite_map_and_set.Map.add_or_overwrite parameter error
        (agent_id, agent_type, site_type)
        current_set store_result
    in
    error, result
  in
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun _parameter error agent_id agent_modif store_result ->
         if Ckappa_sig.Site_map_and_set.Map.is_empty agent_modif.Cckappa_sig.agent_interface
         then error, store_result
         else
           let agent_type = agent_modif.Cckappa_sig.agent_name in
           (*return*)
           let error, store_result =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type _ (error, store_result) ->
                  let error, store_result =
                    add_link error (agent_id, agent_type, site_type) rule_id store_result
                  in
                  error, store_result
               ) agent_modif.Cckappa_sig.agent_interface (error, store_result)
           in
           error, store_result
      ) diff_direct store_result
  in
  let store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

(*update of the views due to modification without agent_id*)

let collect_modif_map parameter error store_modification_sites =
  Covering_classes_type.Project2_modif.monadic_proj_map
    (fun _parameter error (_agent_id, agent_type, site_type) ->
       error, (agent_type, site_type))
    parameter
    error
    Ckappa_sig.Rule_map_and_set.Set.empty
    (fun parameter error s1 s2 ->
       let error', new_set = Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2 in
       let error = Exception.check warn parameter error error' (Some "line 118") Exit in
       error, new_set
    ) store_modification_sites

(************************************************************************************)
(*collect a set of rule_id of test rule and modification *)

let collect_test_sites parameter error rule_id viewslhs
    store_result =
  let add_link (agent_id, agent_type, site_type) rule_id store_result =
    let error', current_set =
      Ckappa_sig.Rule_map_and_set.Set.add
        parameter
        error
        rule_id
        Ckappa_sig.Rule_map_and_set.Set.empty
    in
    let error = Exception.check warn parameter error error' (Some "line 137") Exit in
    let error, result =
      Ckappa_sig.AgentsSite_map_and_set.Map.add_or_overwrite
        parameter error
        (agent_id, agent_type, site_type)
        current_set store_result
    in
    error, result
  in
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun _parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Unknown_agent _
         | Cckappa_sig.Ghost -> error, store_result
         | Cckappa_sig.Dead_agent (agent,_,_,_)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, store_result_test =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type _ (_error, store_result) ->
                  let error, store_result_test =
                    add_link (agent_id, agent_type, site_type) rule_id store_result
                  in
                  error, store_result_test
               ) agent.Cckappa_sig.agent_interface (error, store_result)
           in
           error, store_result_test
      ) viewslhs store_result
  in
  let store_result =
    Ckappa_sig.AgentsSite_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

(*valuations of the views that are created without agent_id*)

let collect_test_map parameter error store_test_sites =
  Covering_classes_type.Project2_modif.monadic_proj_map
    (fun _parameter error (_agent_id, agent_type, site_type) ->
       error, (agent_type, site_type))
    parameter
    error
    (Ckappa_sig.Rule_map_and_set.Set.empty)
    (fun parameter error s1 s2 ->
       let error', new_set = Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2 in
       let error = Exception.check warn parameter error error' (Some "line 118") Exit in
       error, ( new_set)
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
    let error, old =
      match Ckappa_sig.AgentsSite_map_and_set.Map.find_option_without_logs parameter error
              (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, union =
      Ckappa_sig.Rule_map_and_set.Set.union parameter error old rule_id_set in
    let error, result =
      Ckappa_sig.AgentsSite_map_and_set.Map.add_or_overwrite
        parameter error
        (agent_id, agent_type, site_type)
        union store_result
    in
    error, result
  in
  Ckappa_sig.AgentsSite_map_and_set.Map.fold2
    parameter error
    (*exists in 'a t*)
    (fun _parameter error (agent_id, agent_type, site_type) s1 store_result ->
       let error, store_result =
         add_link error (agent_id, agent_type, site_type) s1 store_result
       in
       error, store_result
    )
    (*exists in 'b t*)
    (fun _parameter error (agent_id, agent_type, site_type) s2 store_result ->
       let error, store_result =
         add_link error (agent_id, agent_type, site_type) s2 store_result
       in
       error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_id, agent_type, site_type) s1 s2 store_result ->
       let error',union = Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2 in
       let error = Exception.check warn parameter error error' (Some "line 212") Exit in
       let error, store_result =
         add_link error (agent_id, agent_type, site_type) union store_result
       in
       error, store_result
    ) store_modification_map store_test_map store_result

(*valuations of the views that are created without agent_id*)

let collect_test_modif_map parameter error store_test_modification_sites =
  Covering_classes_type.Project2_modif.monadic_proj_map
    (fun _parameter error (_agent_id, agent_type, site_type) ->
       error, (agent_type, site_type)
    )
    parameter error
    (Ckappa_sig.Rule_map_and_set.Set.empty)
    (fun parameter error s1 s2 ->
       let error', new_set = Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2 in
       let error = Exception.check warn parameter error error' (Some "line 118") Exit in
       error, (new_set)
    )
    store_test_modification_sites

(************************************************************************************)

let scan_rule_pre_static parameter error (rule_id:Ckappa_sig.c_rule_id) rule handler_bdu store_result =
  (*-------------------------------------------------------------------------------*)
  (*update of the views due to modification with agent_id*)
  let error, store_modification_sites =
    collect_modification_sites
      parameter
      error
      rule_id
      rule.Cckappa_sig.diff_direct
      store_result.store_modification_sites
  in
  (*--------------------------------------------------------------*)
  (*update of the views due to modification without agent_id*)
  let error, store_modif_map =
    collect_modif_map
      parameter
      error
      store_modification_sites
  in
  (*-------------------------------------------------------------*)
  (*valuations of the views that are tested with agent_id*)
  let error, store_test_sites =
    collect_test_sites
      parameter
      error
      rule_id
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      store_result.store_test_sites
  in
  (*---------------------------------------------------------------*)
  (*valuations of the views that are tested without agent_id*)
  let error, store_test_map =
    collect_test_map
      parameter
      error
      store_test_sites
  in
  (*---------------------------------------------------------------*)
  (*valuations and update of the views that are tested and modification with agent_id*)
  let error, store_test_modification_sites =
    collect_test_modification_sites
      parameter
      error
      store_modification_sites
      store_test_sites
      store_result.store_test_modification_sites
  in
  (*--------------------------------------------------------------*)
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

(******************************************************************)
(*implementation of bdu_analysis_static*)

let site_covering_classes parameter error covering_classes =
  let add_link (agent_type, site_type) cv_id store_result =
    let error, old =
      match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameter error
              (agent_type, site_type) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, result =
      Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite parameter error
        (agent_type, site_type) (cv_id :: old) store_result
    in
    error, result
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
      (fun _parameter error agent_type_cv remanent store_result ->
         (*get a list of covering_class_id from remanent*)
         let cv_dic = remanent.Covering_classes_type.store_dic in
         (*fold a dictionary*)
         let error, store_result =
           Covering_classes_type.Dictionary_of_List_sites.fold
             (fun list_of_site_type ((),()) cv_id (error, store_result) ->
                (*get site_cv in value*)
                List.fold_left (fun (_error, store_result) site_type_cv ->
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
      ) covering_classes Ckappa_sig.AgentSite_map_and_set.Map.empty
  in
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map (fun x -> x) store_result
  in
  error, store_result

(******************************************************************)

let new_index_pair_map parameter error l = (*JF:  it should be computed only once *)
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 = Ckappa_sig.Site_map_and_set.Map.add parameter error h k map1 in
      let error, map2 = Ckappa_sig.Site_map_and_set.Map.add parameter error k h map2 in
      aux
        tl
        (Ckappa_sig.site_name_of_int ((Ckappa_sig.int_of_site_name k)+1))
        map1
        map2
        error
  in
  let error', (map1, map2) =
    aux
      l
      (Ckappa_sig.site_name_of_int 1)
      Ckappa_sig.Site_map_and_set.Map.empty
      Ckappa_sig.Site_map_and_set.Map.empty error
  in
  let error = Exception.check warn parameter error error' (Some "line 49") Exit in
  error,(map1,map2)

(*****************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  let error', set =
    List.fold_left (fun (error,current_set) elt ->
        Ckappa_sig.Site_map_and_set.Set.add parameter error elt current_set
      ) (error, Ckappa_sig.Site_map_and_set.Set.empty) list
  in
  let error = Exception.check warn parameter error error' (Some "line 509") Exit in
  error, set

(*******************************************************************)
(* From each covering class, with their new index for sites, build
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map restriction
*)

let collect_remanent_triple parameter error store_remanent store_result =
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error agent_type remanent store_result ->
       let store_dic = remanent.Covering_classes_type.store_dic in
       (*-----------------------------------------------------------------*)
       let error, triple_list =
         Covering_classes_type.Dictionary_of_List_sites.fold
           (fun list _ cv_id (error, current_list) ->
              let error, set = list2set parameter error list in
              let triple_list = (cv_id, list, set) :: current_list in
              error, triple_list
           ) store_dic (error, [])
       in
       (*--------------------------------------------------------*)
       let error, store_result =
         Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
           parameter
           error
           agent_type
           (List.rev triple_list)
           store_result
       in
       error, store_result
    ) store_remanent store_result

(******************************************************************)
(*creation rules*)

let build_bdu parameter handler error (pair_list: (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list) =
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true
      parameter handler error
  in
  let error, handler, list_a =
    Ckappa_sig.Views_bdu.build_association_list
      parameter
      handler
      error
      pair_list
  in
  let error, handler, bdu_result =
    Ckappa_sig.Views_bdu.mvbdu_redefine
      parameter handler error bdu_true list_a
  in
  error, handler, bdu_result

(************************************************************************************)

let collect_bdu_creation_restriction_map parameter handler error
    rule_id rule
    store_remanent_triple store_result =
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false
      parameter handler error
  in
  (*-----------------------------------------------------------------*)
  let add_link handler error (agent_type, rule_id, cv_id) bdu store_result =
    let error, old_bdu =
      match
        Covering_classes_type.AgentRuleCV_setmap.Map.find_option
          (agent_type, rule_id, cv_id) store_result
      with
      | None -> error, bdu_false
      (*default value when there is no creation in this rule*)
      | Some bdu -> error, bdu
    in
    (* In the case when the agent is created twice, we take the union *)
    let error, handler, bdu_new =
      Ckappa_sig.Views_bdu.mvbdu_or
        parameter handler error old_bdu bdu
    in
    let result_map =
      Covering_classes_type.AgentRuleCV_setmap.Map.add
        (agent_type, rule_id, cv_id) bdu_new store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error agent_type' triple_list (handler,store_result) ->
       List.fold_left (fun (error, (handler,store_result)) (agent_id, agent_type) ->
           let error, agent =
             Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
               parameter error agent_id rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
           in
           match agent with
           | Some Cckappa_sig.Unknown_agent _
           | Some Cckappa_sig.Dead_agent _
           | None -> warn parameter error (Some "168") Exit (handler,store_result)
           | Some Cckappa_sig.Ghost -> error, (handler,store_result)
           | Some Cckappa_sig.Agent agent ->
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
                         match Ckappa_sig.Site_map_and_set.Map.find_option
                                 parameter
                                 error
                                 site
                                 map_new_index_forward
                         with
                         | error, None -> warn parameter error (Some "282") Exit
                                            Ckappa_sig.dummy_site_name
                         | error, Some s -> error, s
                       in
                       Ckappa_sig.Site_map_and_set.Map.add parameter error
                         site'
                         state
                         store_result
                     in
                     (*-----------------------------------------------------------------*)
                     let error', map_res =
                       Ckappa_sig.Site_map_and_set.Map.fold_restriction_with_missing_associations
                         parameter error
                         (fun site port -> add site port.Cckappa_sig.site_state.Cckappa_sig.min)
                         (fun site -> add site Ckappa_sig.dummy_state_index)
                         set
                         agent.Cckappa_sig.agent_interface
                         Ckappa_sig.Site_map_and_set.Map.empty
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
                        Ckappa_sig.Site_map_and_set.Map.fold
                          (fun site' state (error, current_list) ->
                             let pair_list = (site', state) :: current_list in
                             error, pair_list
                          ) map_res (error, [])
                      in
                      let error, handler, bdu_creation =
                        build_bdu parameter handler error pair_list
                      in
                      let error, handler, store_result =
                        add_link handler error
                          (agent_type, rule_id, cv_id)
                          bdu_creation store_result
                      in
                      error, handler, store_result
                   ) (error, handler, store_result) get_pair_list
               in
               error, (handler, store_result)
             else error, (handler, store_result)
         ) (error, (handler, store_result)) rule.Cckappa_sig.actions.Cckappa_sig.creation
    ) store_remanent_triple (handler, store_result)

(*projection with rule_id*)

let collect_proj_bdu_creation_restriction_map parameter handler_bdu error
    rule_id rule store_remanent_triple
    store_result =
  let store_init_bdu_creation_restriction_map =
    Covering_classes_type.AgentRuleCV_setmap.Map.empty
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
    Ckappa_sig.Views_bdu.mvbdu_true
      parameter handler_bdu error
  in
  let (error, handler_bdu), store_result' =
    Covering_classes_type.Project2bdu_creation.proj2_monadic
      parameter
      (error, handler_bdu)
      (fun (_agent_type, rule_id, _cv_id) -> rule_id)
      (fun (agent_type, _rule_id, cv_id) -> agent_type, cv_id)
      bdu_true
      (fun parameter (error, handler_bdu) bdu bdu' ->
         let error, handler_bdu, bdu_union =
           Ckappa_sig.Views_bdu.mvbdu_and
             parameter handler_bdu error bdu bdu'
         in
         (error, handler_bdu), bdu_union
      )
      store_bdu_creation_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold
      Ckappa_sig.Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler_bdu), store_result

(************************************************************************************)
(*modification rule with creation rules*)

let collect_modif_list_restriction_map
    parameter handler error rule_id rule store_remanent_triple store_result =
  let add_link error (agent_id, agent_type, rule_id, cv_id) list_a store_result =
    (*the association must be unique *)
    let error, result_map =
      Covering_classes_type.AgentsRuleCV_map_and_set.Map.add_or_overwrite parameter error
        (agent_id, agent_type, rule_id, cv_id) list_a store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error agent_id agent_modif (handler, store_result) ->
       if Ckappa_sig.Site_map_and_set.Map.is_empty agent_modif.Cckappa_sig.agent_interface
       then error, (handler, store_result)
       else
         let agent_type = agent_modif.Cckappa_sig.agent_name in
         let error, triple_list =
           match
             Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
               parameter
               error
               agent_type
               store_remanent_triple
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
                 Ckappa_sig.Site_map_and_set.Map.fold_restriction parameter error
                   (fun site port (error, store_result) ->
                      let state = port.Cckappa_sig.site_state.Cckappa_sig.min in
                      let error, site' =
                        Ckappa_sig.Site_map_and_set.Map.find_default_without_logs
                          parameter
                          error
                          Ckappa_sig.dummy_site_name
                          site
                          map_new_index_forward
                      in
                      let error, map_res =
                        Ckappa_sig.Site_map_and_set.Map.add parameter error
                          site'
                          state
                          store_result
                      in
                      error, map_res
                   ) set agent_modif.Cckappa_sig.agent_interface Ckappa_sig.Site_map_and_set.Map.empty
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
                if Ckappa_sig.Site_map_and_set.Map.is_empty map_res
                then error, handler, store_result
                else
                  begin
                    (*get a list of pair (site, state) in a map of new indexes of site.*)
                    let error, pair_list =
                      Ckappa_sig.Site_map_and_set.Map.fold
                        (fun site' state (error, current_list) ->
                           let pair_list = (site', state) :: current_list in
                           error, pair_list
                        ) map_res (error, [])
                    in
                    (*-----------------------------------------------------------------*)
                    (*build list_a*)
                    let error, handler, list_a =
                      Ckappa_sig.Views_bdu.build_association_list
                        parameter
                        handler
                        error
                        pair_list
                    in
                    let error, store_result =
                      add_link error (agent_id, agent_type, rule_id, cv_id) list_a store_result
                    in
                    error, handler, store_result
                  end
             )
             (error, handler, store_result)
             get_pair_list
         in error, (handler, store_result))
    rule.Cckappa_sig.diff_direct (handler, store_result)

(************************************************************************************)
(*build bdu for potential side effects*)

let store_bdu_potential_restriction_map_aux parameter handler error store_remanent_triple
    store_potential_side_effects store_result =
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false
      parameter handler error
  in
  (*-----------------------------------------------------------------*)
  let add_link handler error (agent_type, new_site_type, rule_id, cv_id) bdu store_result =
    (*build a list_a*)
    let error, handler, list =
      Ckappa_sig.Views_bdu.build_reverse_sorted_association_list
        parameter handler error [new_site_type, Ckappa_sig.dummy_state_index] (*state is 0*)
    in
    let result_map =
      Covering_classes_type.AgentSiteRuleCV_setmap.Map.add
        (agent_type, new_site_type, rule_id, cv_id) (bdu, list) store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error agent_type' triple_list (handler, store_result) ->
       (*map of potential partner side_effect with site is bond*)
       Ckappa_sig.AgentRule_map_and_set.Map.fold
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
                           if Ckappa_sig.Site_map_and_set.Set.mem site set
                           then
                             let error, site' =
                               Ckappa_sig.Site_map_and_set.Map.find_default_without_logs
                                 parameter
                                 error
                                 Ckappa_sig.dummy_site_name
                                 site
                                 map_new_index_forward
                             in
                             let error, old =
                               Ckappa_sig.Site_map_and_set.Map.find_default_without_logs
                                 parameter error
                                 []
                                 site'
                                 map_res
                             in
                             let error, map_res =
                               Ckappa_sig.Site_map_and_set.Map.add_or_overwrite parameter error
                                 site'
                                 (state :: old)
                                 map_res
                             in
                             error, map_res
                           else error, map_res
                        ) (error, Ckappa_sig.Site_map_and_set.Map.empty) pair_list
                    in
                    (*-----------------------------------------------------------------*)
                    let error =
                      Exception.check warn parameter error error' (Some "line 630") Exit
                    in
                    error,
                    Ckappa_sig.Site_map_and_set.Map.fold (fun site' list_state list ->
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
                             Ckappa_sig.Views_bdu.mvbdu_or
                               parameter handler error bdu bdu_potential_effect
                           in
                           error, handler, bdu)
                         (error, handler, bdu_false)
                         map_res
                     in
                     let error, handler, store_result =
                       add_link handler error (agent_type, site', rule_id, cv_id) bdu store_result
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
    Covering_classes_type.AgentSiteRuleCV_setmap.Map.empty
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
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true
      parameter handler error
  in
  (*an empty hconsed list*)
  let error, handler, empty =
    Ckappa_sig.Views_bdu.build_reverse_sorted_association_list
      parameter handler error []
  in
  let (error, handler), store_result' =
    Covering_classes_type.Project2bdu_potential.proj2_monadic
      parameter
      (error, handler)
      (fun (_agent_type, _new_site_name, rule_id, _cv_id) -> rule_id)
      (fun (agent_type, new_site_name, _rule_id, cv_id) -> agent_type, new_site_name, cv_id)
      (bdu_true, empty)
      (fun _ (error, handler) _ pair' -> (error, handler), pair')
      store_bdu_potential_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold
      Ckappa_sig.Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler), store_result

(************************************************************************************)

let collect_bdu_test_restriction_map parameter handler error rule_id rule
    store_remanent_triple store_result =
  let error, handler, bdu_false =
    Ckappa_sig.Views_bdu.mvbdu_false
      parameter handler error
  in
  (*-----------------------------------------------------------------*)
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
    (fun parameter error agent_id agent (handler,store_result) ->
       match agent with
       | Cckappa_sig.Unknown_agent _
       (* Unfortunatly, we can do nothing with undefined agents in the views domain *)
       (* They will be handled with properly in the agents domain *)
       | Cckappa_sig.Ghost -> error, (handler, store_result)
       | Cckappa_sig.Dead_agent (agent, _, _, _) ->
         let agent_type = agent.Cckappa_sig.agent_name in
         let error, triple_list =
           match
             Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
               parameter error agent_type store_remanent_triple
           with
           | error, None -> error, []
           | error, Some x -> error, x
         in
         let error, store_result =
           List.fold_left
             (fun (error, store_result) (cv_id, _, _) ->
                error,
                Covering_classes_type.AgentsRuleCV_setmap.Map.add
                  (agent_id, agent_type, rule_id, cv_id) bdu_false store_result
             )
             (error, store_result) triple_list
         in
         error, (handler, store_result)
       | Cckappa_sig.Agent agent ->
         let agent_type = agent.Cckappa_sig.agent_name in
         let error, triple_list =
           match
             Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
               parameter error agent_type store_remanent_triple
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
                  Ckappa_sig.Site_map_and_set.Map.fold_restriction
                    parameter error
                    (fun site port (error, store_result) ->
                       let state = port.Cckappa_sig.site_state.Cckappa_sig.min in
                       let error, site' =
                         Ckappa_sig.Site_map_and_set.Map.find_default parameter error
                           Ckappa_sig.dummy_site_name
                           site map_new_index_forward
                       in
                       let error, map_res =
                         Ckappa_sig.Site_map_and_set.Map.add parameter error
                           site'
                           state
                           store_result
                       in
                       error, map_res
                    ) set agent.Cckappa_sig.agent_interface
                    Ckappa_sig.Site_map_and_set.Map.empty
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
                if Ckappa_sig.Site_map_and_set.Map.is_empty map_res
                then
                  error, handler, store_result
                else
                  begin
                    let error, pair_list =
                      Ckappa_sig.Site_map_and_set.Map.fold
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
                      error, Covering_classes_type.AgentsRuleCV_setmap.Map.add
                        (agent_id, agent_type, rule_id, cv_id)
                        bdu_test
                        store_result
                    in
                    error, handler, store_result
                  end)

             (error, handler, store_result) get_pair_list
         in
         error, (handler, store_result)
    ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views  (handler,store_result)

(************************************************************************************)

let collect_proj_bdu_test_restriction parameter handler error
    rule_id rule store_remanent_triple
    store_result =
  let store_init_bdu_test_restriction_map =
    Covering_classes_type.AgentsRuleCV_setmap.Map.empty
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
  let error, handler, bdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true
      parameter handler error
  in
  let (error, handler), store_result' =
    Covering_classes_type.Project2_bdu_views.proj2_monadic
      parameter
      (error, handler)
      (fun (_agent_id, _agent_type, rule_id, _cv_id) -> rule_id)
      (fun (agent_id, agent_type, _rule_id, cv_id) -> (agent_id, agent_type, cv_id))
      bdu_true
      (fun parameter (error, handler) bdu bdu' ->
         let error, handler, bdu_union =
           Ckappa_sig.Views_bdu.mvbdu_and
             parameter handler error bdu bdu'
         in
         (error, handler), bdu_union
      )
      store_bdu_test_restriction_map
  in
  let store_result =
    Ckappa_sig.Rule_setmap.Map.fold
      Ckappa_sig.Rule_setmap.Map.add
      store_result'
      store_result
  in
  (error, handler), store_result

(************************************************************************************)

let scan_rule_static parameter log_info error handler_kappa handler_bdu
    (rule_id:Ckappa_sig.c_rule_id) rule
    store_potential_side_effects compil store_result =
  (*------------------------------------------------------------------------------*)
  (*pre_static*)
  let error, log_info = StoryProfiling.StoryStats.add_event parameter error
      (StoryProfiling.Scan_rule_static (Ckappa_sig.int_of_rule_id rule_id))
      None log_info
  in
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
  let error, log_info = StoryProfiling.StoryStats.close_event parameter error
      (StoryProfiling.Scan_rule_static (Ckappa_sig.int_of_rule_id rule_id))
      None log_info
  in
  error, log_info, handler_bdu,
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
  let init_modification        = Ckappa_sig.AgentsSite_map_and_set.Map.empty in
  let init_test                = Ckappa_sig.AgentsSite_map_and_set.Map.empty in
  let init_test_modification   = Ckappa_sig.AgentsSite_map_and_set.Map.empty in
  let init_modif_map           = Ckappa_sig.AgentSite_map_and_set.Map.empty in
  let init_test_map            = Ckappa_sig.AgentSite_map_and_set.Map.empty in
  let init_test_modif_map      = Ckappa_sig.AgentSite_map_and_set.Map.empty in
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
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create parameter error 0
  in
  let init_covering_classes_id = Ckappa_sig.AgentSite_map_and_set.Map.empty in
  let error, init_remanent_triple =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create parameter error 0
  in
  let init_proj_bdu_creation_restriction_map = Ckappa_sig.Rule_setmap.Map.empty in
  let init_modif_list_restriction_map        = Covering_classes_type.AgentsRuleCV_map_and_set.Map.empty in
  let init_proj_bdu_potential_restriction_map  = Ckappa_sig.Rule_setmap.Map.empty in
  let init_proj_bdu_test_restriction           = Ckappa_sig.Rule_setmap.Map.empty in
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

let scan_rule_set parameter log_info handler_bdu error handler_kappa compiled
    store_potential_side_effects =
  let error, init = init_bdu_analysis_static parameter error in
  let error, (handler_bdu, log_info, store_results) =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule (handler_bdu, log_info, store_result) ->
         let error, log_info, handler_bdu, store_result =
           scan_rule_static
             parameter
             log_info
             error
             handler_kappa
             handler_bdu
             rule_id
             rule.Cckappa_sig.e_rule_c_rule
             store_potential_side_effects
             compiled
             store_result
         in
         error, (handler_bdu, log_info, store_result)
      ) compiled.Cckappa_sig.rules (handler_bdu, log_info, init)
  in
  error, (handler_bdu, log_info, store_results)
