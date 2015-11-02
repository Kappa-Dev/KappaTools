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
open Set_and_map
open Bdu_build
open Bdu_build
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig

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
(* From each covering class, with their new index for sites, build 
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map
*)

let a_map_restriction parameter error agent_type id_list set agent map =
  let add_link (agent_type, site) state store_result =
    let error, (l, old) =
      try Int2Map_Modif.find_map parameter error (agent_type, site) store_result
      with Not_found -> error, ([], Site_map_and_set.empty_set)
    in
    let error, current_set =
      Site_map_and_set.add_set parameter error state old
    in
    let error, new_set =
      Site_map_and_set.union parameter error current_set old
    in
    let error, result =
      Int2Map_Modif.add_map parameter error (agent_type, site)
        (l, new_set) store_result
    in
    error, result
  in
  let rec aux acc =
    match acc with
    | [] -> []
    | id :: tl ->
      let error, map_restriction =
        Site_map_and_set.fold_map_restriction parameter error
          (fun site port (error, store_result) ->
            let state = port.site_state.min in
            (*find site' with new_index inside map*)
            let error, site' =
              try Site_map_and_set.find_map parameter error site map
              with Not_found -> error, 0
            in
            (*add into a restriction map with state of a new site'*)
            let error, new_pair_map = (*TEST*)
              add_link (agent_type, site) state store_result
            in
            error, new_pair_map
          ) set agent.agent_interface Int2Map_Modif.empty_map
      in
      map_restriction :: aux tl
  in
  aux id_list

let a_list_of_map_restriction parameter error agent_type id_list list_set agent list_map =
  (*check inside each covering class id of a list of map*)
  let rec aux acc acc' =
    match acc, acc' with
    | [], [] | _, [] | [], _ -> []
    | set :: tl, (map, _) :: tl' ->
      let  map_restriction =
        a_map_restriction
          parameter
          error
          agent_type
          id_list
          set
          agent
          map
      in
      List.concat [map_restriction; aux tl tl']
  in
  aux list_set list_map

let compute_map_restriction parameter error agent_type id_list list_set agent list_map =
  (*let rec aux acc =
    match acc with
    | [] -> []
    | id :: tl ->
      let list =*)
        a_list_of_map_restriction
          parameter
          error
          agent_type
          id_list
          list_set
          agent
          list_map
(*in
      List.concat[list; aux tl]
  in
aux id_list*)

(*map restriction of test rules*)

let build_map_restriction parameter error agent_type agent covering_class_set store_result =
  let error, init = AgentMap.create parameter error 0 in
  let error,store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_type_cv set store_result ->
        let (id_set_list, list_set), (id_list, list_map) = set in
        let map_restriction_list =
          compute_map_restriction
            parameter
            error
            agent_type_cv
            id_list
            list_set
            agent
            list_map
        in
        if agent_type = agent_type_cv
        then
          (*get old*)
          let error, old_list =
            match AgentMap.unsafe_get parameter error agent_type_cv store_result with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let new_list = List.concat [map_restriction_list; old_list] in
          (*store*)
          let error, store_result =
            AgentMap.set
              parameter
              error
              agent_type_cv
              (List.rev new_list)
              store_result
          in
          error, store_result
        else 
        error, store_result
      ) covering_class_set store_result
  in
  error, store_result

(*TODO*)
let collect_restriction_bdu_test parameter error rule covering_class_set store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        let error, store_result =
          build_map_restriction
            parameter
            error
            agent_type
            agent
            covering_class_set
            store_result
        in
        error, store_result
    ) rule.rule_lhs.views store_result

(*test build bdu test from remanent of covering class*)
open Covering_classes_type
open Covering_classes_list2set

let collect_bdu_test parameter error rule store_remanent store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type remanent store_result ->
      let error, init = AgentMap.create parameter error 0 in
      let store_dic = remanent.store_dic in
      let error, (id_list, site_set_list) =
        Dictionary_of_Covering_class.fold
          (fun list _ index (error, (index_list, current_list)) ->
            let error, list2set =
              list2set parameter error list
            in
            let list_set = list2set :: current_list in
            let list_index = index :: index_list in
            error, (List.rev list_index, List.rev list_set)
          ) store_dic (error, ([], []))
      in
      let result =
        let rec aux acc =
          match acc with
          | [] -> []
          | set :: tl ->
            let error, result =
              AgentMap.fold parameter error
                (fun parameter error agent_id agent store_result ->
                  match agent with
                  | Ghost -> error, store_result
                  | Agent agent ->
                    let agent_type_test = agent.agent_name in
                    Dictionary_of_Covering_class.fold
                      (fun list _ index (error, (index_list, store_result1)) ->
                        let error, (store_map, _) =
                          new_index_pair_map parameter error list
                        in
                        (*map restriction*)
                        let map_restriction =
                          Site_map_and_set.fold_map
                            (fun site port current_list ->
                              (site, port.site_state.min) :: current_list
                            ) agent.agent_interface []
                        in
                        (*let error, map_restriction =
                          Site_map_and_set.fold_map_restriction parameter error
                            (fun site port (error, store_result) ->
                              let state = port.site_state.min in
                              let error, site' =
                                try Site_map_and_set.find_map parameter error site store_map
                                with Not_found -> error, 0
                              in
                              let error, map_res =
                                Site_map_and_set.add_map 
                                  parameter error site state store_result
                              in
                              error, map_res
                            ) set agent.agent_interface Site_map_and_set.empty_map
                        in*)
                        let new_list = List.concat [map_restriction; store_result1] in
                        let index_list = index :: index_list in
                        error, (List.rev index_list, new_list)
                      ) store_dic (error, ([], []))
                ) rule.rule_lhs.views ([], [])
            in
           result :: aux tl
        in
        aux site_set_list
      in
      (*let error, old =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      let new_list = List.concat [result; old] in*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          result
          store_result
      in
      error, store_result
    ) store_remanent store_result
