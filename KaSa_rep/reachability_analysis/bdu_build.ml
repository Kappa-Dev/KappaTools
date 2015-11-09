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
(*fixpoint iteration function*)
(*from an array of rule, build bdu:{of creation rule; test; list of modification}.
  the idea of iterate function: 
  - if working list is empty then return the bdu_remanent_array;
  - pop the first element inside the working list;
  - get 'rule' type from the rule_array;
  - build bdu_creation; bdu_test, and a list of action;
  - build a list of type List_sig.list for modif_list (list of action);
  - check if it  is an enable rule or not:
    it is an enable rule if the result of intersection of test and init is not empty;
    + if it is an enable rule return the result.
    + if it is not an enable rule: call iterate update function:
      ++ bdu_creation intersection (mvbdu_and) with bdu_test, 
         then use the result above: redefine (redefine function in mvbdu) it
         with the list of modif;
      ++ do the union (mvbdu_or) of bdu_assignment (the intersection above) and bdu_creation;
      ++ store this bdu_update in bdu_array, with the index is rule_id.
*)

(************************************************************************************)
(*is enable rule*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then true
  else false
    
let comp_is_enable parameter error handler bdu_init bdu_test bdu_creation =
  let error, handler, bdu_X =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  if not (is_belong bdu_X bdu_init)
  then
    error, true
  else
    error, false

(************************************************************************************)
(*update bdu*)

let compute_update parameter error rule_id handler bdu_test modif_list bdu_creation
    bdu_remanent_array =
  let error, handler, bdu_inter_test_creation =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  let error, handler, bdu_assigment =
    f parameter error bdu_inter_test_creation
      (redefine parameter error parameter handler bdu_inter_test_creation)
      modif_list
  in
  let error, handler, bdu_update =
    f parameter error bdu_assigment
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment)
      bdu_creation
  in
  let bdu_remanent_array =
    bdu_remanent_array.(rule_id) <- bdu_update;
    bdu_remanent_array
  in
  error, bdu_remanent_array
    
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
(*map restriction in covering classes, with new index*)

let collect_remanent_restriction parameter error agent store_remanent_triple =
  let error, init = AgentMap.create parameter error 0 in
  AgentMap.fold parameter error
    (fun parameter error agent_type triple_list store_result ->
      let error, pair_list =
        List.fold_left (fun (error, current_list) (id, list, set) ->
          (*-----------------------------------------------------------------*)
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
          (*-----------------------------------------------------------------*)
          error, ((id, map_res) :: current_list)
        ) (error, []) triple_list
      in
      (*-----------------------------------------------------------------*)
      let new_agent_type = agent.agent_name in
      let error, old_list =
        match AgentMap.unsafe_get parameter error new_agent_type store_result with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      let new_list = List.concat [pair_list; old_list] in
      (*-----------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          new_agent_type
          new_list
          store_result
      in
      error, store_result
    ) store_remanent_triple init
    
(************************************************************************************)
(*test rule*)

let collect_test_restriction parameter error rule_id rule store_remanent_triple
    store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, store_remanent_restriction =
          collect_remanent_restriction
            parameter
            error
            agent
            store_remanent_triple
        in
        (*get*)
        let error, pair_list =
          match AgentMap.unsafe_get parameter error agent_type 
            store_remanent_restriction with
            | error, None -> error, []
            | error, Some l -> error, l
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, fourth_list =
          List.fold_left 
            (fun (error, current_list) (id, map_res) ->
              let error, l =
                Site_map_and_set.Map.fold
                  (fun site' state (error, current_list) ->
                    let fourth_list = (rule_id, id, site', state) :: current_list in
                    error, fourth_list
                  ) map_res (error, [])
              in
              let new_fourth_list =
                List.concat [l; current_list]
              in
              error, new_fourth_list
            ) (error, []) pair_list
        in
        (*-----------------------------------------------------------------*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        let new_list = List.concat [fourth_list; old_list] in
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
    ) rule.rule_lhs.views store_result

(************************************************************************************)
(*creation rules*)

let collect_creation_restriction parameter error rule_id rule store_remanent_triple
    store_result =
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
    match agent with
    | None -> warn parameter error (Some "") Exit store_result
    | Some Ghost -> error, store_result
    | Some Agent agent ->
      (*-----------------------------------------------------------------*)
      (*get map restriction from covering classes*)
      let error, store_remanent_restriction =
        collect_remanent_restriction
          parameter
          error
          agent
          store_remanent_triple
      in
      (*get*)
      let error, pair_list =
        match AgentMap.unsafe_get parameter error agent_type 
          store_remanent_restriction with
          | error, None -> error, []
          | error, Some l -> error, l
      in
      (*-----------------------------------------------------------------*)
      (*fold a list and get a pair of site and state and rule_id*)
      let error, fourth_list =
        List.fold_left 
          (fun (error, current_list) (id, map_res) ->
            let error, l =
              Site_map_and_set.Map.fold
                (fun site' state (error, current_list) ->
                  let fourth_list = (rule_id, id, site', state) :: current_list in
                  error, fourth_list
                ) map_res (error, [])
            in
            let new_fourth_list =
              List.concat [l; current_list]
            in
            error, new_fourth_list
          ) (error, []) pair_list
      in
      (*-----------------------------------------------------------------*)
      let error, old_list =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      let new_list = List.concat [fourth_list; old_list] in
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
  ) (error, store_result) rule.actions.creation

(************************************************************************************)
(*modification rule-REMOVE*)

let collect_modif_restriction parameter error rule_id rule store_remanent_triple
    store_result =
  AgentMap.fold parameter error 
    (fun parameter error agent_id agent_modif store_result ->
      if Site_map_and_set.Map.is_empty agent_modif.agent_interface
      then error, store_result
      else
        let agent_type = agent_modif.agent_name in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, store_remanent_restriction =
          collect_remanent_restriction
            parameter
            error
            agent_modif
            store_remanent_triple
        in
        (*get*)
        let error, pair_list =
          match AgentMap.unsafe_get parameter error agent_type 
            store_remanent_restriction with
            | error, None -> error, []
            | error, Some l -> error, l
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, fourth_list =
          List.fold_left 
            (fun (error, current_list) (id, map_res) ->
              let error, l =
                Site_map_and_set.Map.fold
                  (fun site' state (error, current_list) ->
                    let fourth_list = (rule_id, id, site', state) :: current_list in
                    error, fourth_list
                  ) map_res (error, [])
              in
              let new_fourth_list =
                List.concat [l; current_list]
              in
              error, new_fourth_list
            ) (error, []) pair_list
        in
        (*-----------------------------------------------------------------*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        let new_list = List.concat [fourth_list; old_list] in
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
    ) rule.diff_direct store_result
