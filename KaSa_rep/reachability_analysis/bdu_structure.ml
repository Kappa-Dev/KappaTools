(**
  * bdu_structure.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 4th of November
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Bdu_analysis_type
open Covering_classes_type
open Cckappa_sig
open Bdu_build

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU structure") message exn (fun () -> default)  

let trace = false

(*************************************************************************************)
(*collect remanent test as a map function*)

let collect_remanent_test_map parameter error rule_id rule store_remanent_test store_result =
  let add_link (agent_type, cv_id, site, state) rule_id store_result =
    let (l, old) =
      Map_test.Map.find_default ([], Site_map_and_set.Set.empty)
        (agent_type, cv_id, site, state) store_result
    in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set = Site_map_and_set.Set.union current_set old in
    let result_map =
      Map_test.Map.add (agent_type, cv_id, site, state)
        (l, new_set) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type fourth_list store_result ->
      let error, map =
        List.fold_left
          (fun (error, store_result) (rule_id, id, site', state) ->
            let error, result =
              add_link (agent_type, id, site', state) rule_id store_result
            in
            error, result
          ) (error, store_result) fourth_list
      in
      error, map          
    ) store_remanent_test store_result

(*************************************************************************************)
(*collect remanent creation as a map function*)

let collect_remanent_creation_map parameter error rule_id rule store_remanent_creation
    store_result =
  let add_link (agent_type, cv_id, site, state) rule_id store_result =
    let (l, old) =
      Map_creation.Map.find_default ([], Site_map_and_set.Set.empty)
        (agent_type, cv_id, site, state) store_result
    in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set = Site_map_and_set.Set.union current_set old in
    let result_map =
      Map_creation.Map.add (agent_type, cv_id, site, state)
        (l, new_set) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type fourth_list store_result ->
      let error, map =
        List.fold_left
          (fun (error, store_result) (rule_id, id, site', state) ->
            let error, result =
              add_link (agent_type, id, site', state) rule_id store_result
            in
            error, result
          ) (error, store_result) fourth_list
      in
      error, map          
    ) store_remanent_creation store_result

(*************************************************************************************)
(*collect remanent modification as a map function (this function content creation rules)*)

let collect_remanent_modif_map parameter error rule_id rule store_remanent_modif
    store_result =
  let add_link (agent_type, cv_id, site, state) rule_id store_result =
    let (l, old) =
      Map_modif.Map.find_default ([], Site_map_and_set.Set.empty)
        (agent_type, cv_id, site, state) store_result
    in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set = Site_map_and_set.Set.union current_set old in
    let result_map =
      Map_modif.Map.add (agent_type, cv_id, site, state)
        (l, new_set) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type fourth_list store_result ->
      let error, map =
        List.fold_left
          (fun (error, store_result) (rule_id, id, site', state) ->
            let error, result =
              add_link (agent_type, id, site', state) rule_id store_result
            in
            error, result
          ) (error, store_result) fourth_list
      in
      error, map          
    ) store_remanent_modif store_result

(*************************************************************************************)
(*collect remanent modification as a map function (this function without creation rules)*)

let collect_remanent_modif_op_map parameter error rule_id rule store_remanent_creation_map
    store_remanent_triple store_result =
  let add_link (agent_type, cv_id, site, state) rule_id store_result =
    let (l, old) =
      Map_modif_creation.Map.find_default ([], Site_map_and_set.Set.empty)
        (agent_type, cv_id, site, state) store_result
    in
    let current_set =
      Site_map_and_set.Set.add rule_id old
    in
    let new_set = Site_map_and_set.Set.union current_set old in
    let result_map =
      Map_modif_creation.Map.add (agent_type, cv_id, site, state)
        (l, new_set) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
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
        let error, store_result =
          List.fold_left (fun (error, store_result) (id, map_res) ->
            let error, store_result =
              Site_map_and_set.Map.fold
                (fun site state (error, store_result) ->
                  let error, store_result =
                    if Map_creation.Map.is_empty store_remanent_creation_map
                    then
                      let error, store_result =
                        add_link (agent_type, id, site, state) rule_id store_result
                      in
                      error, store_result
                    else
                      (*-----------------------------------------------------------------*)
                      Map_creation.Map.fold (fun (agent_type', id', site', state')
                        (l1, s2) (error, store_result) ->
                          if Site_map_and_set.Set.mem rule_id s2
                          then
                            let store_result =
                              Map_modif_creation.Map.remove
                                (agent_type', id', site', state') store_result
                            in
                            error, store_result
                          else
                            let error, store_result =
                              add_link (agent_type, id, site, state) rule_id store_result
                            in
                            error, store_result                        
                      ) store_remanent_creation_map (error, store_result)
                  in
                  error, store_result
                ) map_res (error, store_result)
            in
            (*-----------------------------------------------------------------*)
            error, store_result
          ) (error, store_result) pair_list
        in
        error, store_result
    ) rule.diff_direct store_result

(*************************************************************************************)
(* Build BDU test, creation and a list of modification rules*)
(*************************************************************************************)


