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

let collect_remanent_test_map parameter error store_remanent_test =
  let add_link (agent_type, rule_id) triple_list store_result =
    let (l, old) =
      Map_test.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_test.Map.add (agent_type, rule_id)
        (l, List.concat [triple_list; old]) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type l store_result ->
      List.fold_left (fun (error, store_result) (rule_id, triple_list) ->
        let error, map =
          add_link (agent_type, rule_id) triple_list store_result
        in
        error, map
      ) (error, store_result) l
    ) store_remanent_test Map_test.Map.empty

(*************************************************************************************)
(*collect remanent creation as a map function*)

let collect_remanent_creation_map parameter error store_remanent_creation =
  let add_link (agent_type, rule_id) triple_list store_result =
    let (l, old) =
      Map_creation.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_creation.Map.add (agent_type, rule_id)
        (l, List.concat [triple_list; old]) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type l store_result ->
      let error, map =
        List.fold_left
          (fun (error, store_result) (rule_id, triple_list) ->
            let error, result =
              add_link (agent_type, rule_id) triple_list store_result
            in
            error, result
          ) (error, store_result) l
      in
      error, map          
    ) store_remanent_creation Map_creation.Map.empty

(*************************************************************************************)
(*collect remanent modification as a map function (this function content creation rules)*)

let collect_remanent_creation_set_map parameter error store_remanent_creation  =
  let add_link (agent_type, triple_list) rule_id store_result =
    let (l, old) =
      Map_creation_set.Map.find_default ([], Site_map_and_set.Set.empty)
        (agent_type, triple_list) store_result
    in
    let current_set = Site_map_and_set.Set.add rule_id old in
    let new_set = Site_map_and_set.Set.union current_set old in
    let result_map =
      Map_creation_set.Map.add (agent_type, triple_list) (l, new_set) store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type l store_result ->
      let error, map =
        List.fold_left
          (fun (error, store_result) (rule_id, triple_list) ->
            let error, result =
              add_link (agent_type, triple_list) rule_id store_result
            in
            error, result
          ) (error, store_result) l
      in
      error, map          
    ) store_remanent_creation Map_creation_set.Map.empty

(*************************************************************************************)
(*TODO*)

let collect_remanent_modif_map parameter error store_remanent_modif store_creation_map 
    =
  let add_link (agent_type, rule_id) triple_list store_result =
    let (l, old) =
      Map_modif_creation.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_modif_creation.Map.add (agent_type, rule_id)
        (l, List.concat [triple_list; old]) store_result
    in
    error, result_map
  in
  AgentMap.fold parameter error
    (fun parameter error agent_type l store_result ->
      List.fold_left (fun (error, store_result) (rule_id, triple_list) ->
        Map_creation_set.Map.fold (fun (agent_type', triple_list')
          (l1, s2) (error, store_result) ->
            if agent_type = agent_type'
            then
              if Site_map_and_set.Set.mem rule_id s2
              then
                let store_result =
                  Site_map_and_set.Set.fold (fun rule_id' store_result ->
                    Map_modif_creation.Map.remove (agent_type', rule_id') store_result
                  ) s2 store_result
                in
                error, store_result
              else
                add_link (agent_type, rule_id) triple_list store_result
            else
              error, store_result
        ) store_creation_map (error, store_result)
      ) (error, store_result) l
    ) store_remanent_modif Map_modif_creation.Map.empty

(*************************************************************************************)
(* Build BDU test, creation and a list of modification rules*)
(*************************************************************************************)


