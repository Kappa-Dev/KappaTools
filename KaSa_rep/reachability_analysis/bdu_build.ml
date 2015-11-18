(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 9th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build_common
    
(************************************************************************************)
(*function mapping a list of covering class, return triple
  (list of new_index, map1, map2)
  For example:
  - intput : covering_class_list [0;1]
  - output : 
  {new_index_of_covering_class: [1;2],
  map1 (from 0->1, 1->2),
  map2 (from 1->0, 2->1)
  }
*)

let new_index_pair_map parameter error l =
  let rec aux acc k map1 map2 =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let map1 = Map.add h k map1 in
      let map2 = Map.add k h map2 in
      aux tl (k+1) map1 map2
  in aux l 1 Map.empty Map.empty

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  error,List.fold_left (fun current_set elt ->
      Set.add elt current_set
  ) Set.empty list

(************************************************************************************)
(* From each covering class, with their new index for sites, build 
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map restriction
*)

let collect_remanent_triple parameter error store_remanent store_result =
  AgentMap.fold parameter error 
    (fun parameter error agent_type remanent store_result ->
      let store_dic = remanent.store_dic in
      (*-----------------------------------------------------------------*)
      let error, triple_list =
        Dictionary_of_Covering_class.fold
          (fun list _ id (error, current_list) ->
            let error, set = list2set parameter error list in
            let triple_list = (id, list, set) :: current_list in
            error, triple_list
          ) store_dic (error, [])
      in
      (*-----------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error agent_type
          (List.rev triple_list)
          store_result
      in
      error, store_result
    ) store_remanent store_result

(************************************************************************************)
(*test rule*)

let collect_test_restriction parameter error rule_id rule store_remanent_triple
    store_result =
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent triple_list store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
        List.fold_left (fun (error, current_list) (id, list, set) ->
          (*-----------------------------------------------------------------*)
          (*new index for site type in covering class*)
          let error, (map_new_index_forward, _) =
            new_index_pair_map parameter error list
          in
          (*-----------------------------------------------------------------*)
          let error, map_res =
            Site_map_and_set.Map.monadic_fold_restriction parameter error
              (fun parameter error site port store_result ->
                let state = port.site_state.min in
                let site' = Site_map_and_set.Map.find_default
                  0 site map_new_index_forward in
                let map_res =
                  Site_map_and_set.Map.add
                    site'
                    state
                    store_result
                in
                error, map_res
              ) set agent.agent_interface Site_map_and_set.Map.empty
          in
          error, ((id, map_res) :: current_list)
        ) (error, []) triple_list
        in
        (*-----------------------------------------------------------------*)
        let error, pair_list =
          List.fold_left 
            (fun (error, current_list) (id, map_res) ->
              let error, triple_list =
                Site_map_and_set.Map.fold
                  (fun site' state (error, current_list) ->
                    let triple_list = (id, site', state) :: current_list in
                    error, triple_list
                  ) map_res (error, [])
              in
              error, (rule_id, triple_list) :: current_list
            ) (error, []) get_pair_list
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        let new_list = List.concat [pair_list; old_list] in
        (*-----------------------------------------------------------------*)
        let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list)
            store_result
        in
        error, store_result
    ) rule.rule_lhs.views store_remanent_triple store_result

(************************************************************************************)
(*creation rules*)

let collect_creation_restriction parameter error rule_id rule store_remanent_triple
  store_result   =
  let error, init = AgentMap.create parameter error 0 in
  AgentMap.fold parameter error
    (fun parameter error agent_type' triple_list store_result ->
      List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
        let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
        match agent with
        | None -> warn parameter error (Some "168") Exit store_result
        | Some Ghost -> error, store_result
        | Some Agent agent ->
          if agent_type' = agent_type 
          then
            (*-----------------------------------------------------------------*)
            (*get map restriction from covering classes*)
            let error, get_pair_list =
              List.fold_left (fun (error, current_list) (id, list, set) ->
                (*-----------------------------------------------------------------*)
                (*new index for site type in covering class*)
                let error, (map_new_index_forward, _) =
                  new_index_pair_map parameter error list
                in
                (*-----------------------------------------------------------------*)
                let error, map_res =
                  Site_map_and_set.Map.monadic_fold_restriction parameter error
                    (fun parameter error site port store_result ->
                      let state = port.site_state.min in
                      let site' = Site_map_and_set.Map.find_default
                        0 site map_new_index_forward in
                      let map_res =
                        Site_map_and_set.Map.add
                          site'
                          state
                          store_result
                      in
                      error, map_res
                    ) set agent.agent_interface Site_map_and_set.Map.empty
                in
                error, ((id, map_res) :: current_list)
              ) (error, []) triple_list
            in
            (*-----------------------------------------------------------------*)
            (*fold a list and get a pair of site and state and rule_id*)
            let error, pair_list =
              List.fold_left 
                (fun (error, current_list) (id, map_res) ->
                  let error, pair_list =
                    Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
                        let pair_list = (id, site', state) :: current_list in
                        error, pair_list
                      ) map_res (error, [])
                  in
                  error, (rule_id, pair_list) :: current_list
                ) (error, []) get_pair_list
            in       
            (*-----------------------------------------------------------------*)
            let error, old_list =
              match AgentMap.unsafe_get parameter error agent_type store_result with
              | error, None -> error, []
              | error, Some l -> error, l
            in
            let new_list = List.concat [pair_list; old_list] in
            (*-----------------------------------------------------------------*)
            let error, store_result =
              AgentMap.set
                parameter
                error
                agent_type
                (List.rev new_list)
                store_result
            in
            error, store_result
          else error, store_result
      ) (error, store_result) rule.actions.creation
    ) store_remanent_triple store_result

(************************************************************************************)
(*modification rule with creation rules*)

let collect_modif_restriction parameter error rule_id rule store_remanent_triple
    store_result =
  AgentMap.fold2_common parameter error 
    (fun parameter error agent_id agent_modif triple_list store_result ->
      if Site_map_and_set.Map.is_empty agent_modif.agent_interface
      then error, store_result
      else
        let agent_type = agent_modif.agent_name in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          List.fold_left (fun (error, current_list) (id, list, set) ->
            (*-----------------------------------------------------------------*)
            (*new index for site type in covering class*)
            let error, (map_new_index_forward, _) =
              new_index_pair_map parameter error list
            in
            (*-----------------------------------------------------------------*)
            let error, map_res =
              Site_map_and_set.Map.monadic_fold_restriction parameter error
                (fun parameter error site port store_result ->
                  let state = port.site_state.min in
                  let site' = Site_map_and_set.Map.find_default
                    0 site map_new_index_forward in
                  let map_res =
                    Site_map_and_set.Map.add
                      site'
                      state
                      store_result
                  in
                  error, map_res
                ) set agent_modif.agent_interface Site_map_and_set.Map.empty
            in
            error, ((id, map_res) :: current_list)
          ) (error, []) triple_list
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, pair_list =
          List.fold_left 
            (fun (error, current_list) (id, map_res) ->
              let error, pair_list =
                Site_map_and_set.Map.fold
                  (fun site' state (error, current_list) ->
                    let pair_list = (id, site', state) :: current_list in
                    error, pair_list
                  ) map_res (error, [])
              in
              error, (rule_id, pair_list) :: current_list
            ) (error, []) get_pair_list
        in       
        (*-----------------------------------------------------------------*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        let new_list = List.concat [pair_list; old_list] in
        (*-----------------------------------------------------------------*)
        let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list)
            store_result
        in
        error, store_result
    ) rule.diff_direct store_remanent_triple store_result
