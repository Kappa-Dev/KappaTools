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
(*collect remanent modification as a map function without creation rules*)

let collect_remanent_modif_opt_map parameter error store_remanent_modif_opt =
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
    ) store_remanent_modif_opt Map_modif_creation.Map.empty

(*************************************************************************************)
(* Build BDU test, creation and a list of modification rules*)
(*************************************************************************************)


