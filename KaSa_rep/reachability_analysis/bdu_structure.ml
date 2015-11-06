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
(*collect remanent test as a map function:
  (rule_id, agent_type, covering_class_id) (list, set)
*)

let collect_remanent_test_map parameter error rule_id rule store_remanent store_result =
  let add_link (rule_id, cv_id, agent_type) (list, set) store_result =
    let (l, old) =
      Map_test.Map.find_default ([], [])
        (rule_id, cv_id, agent_type) store_result
    in
    let result_map =
      Map_test.Map.add (rule_id, cv_id, agent_type)
        (l, (list, set) :: old) store_result
    in
    error, result_map
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type remanent agent store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type_test = agent.agent_name in
        let store_dic = remanent.store_dic in
        let error, test_map =
          Dictionary_of_Covering_class.fold
            (fun list _ id (error, store_result) ->
              let error, set = list2set parameter error list in
              (*add_link*)
              let error, result_map =
                add_link (rule_id, id, agent_type_test) (list, set) store_result
              in
              error, result_map
            ) store_dic (error, Map_test.Map.empty)
        in
        error, test_map
        (*store*)
        (*let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type_test
            test_map
            store_result
        in
        error, store_result*)
    ) store_remanent rule.rule_lhs.views store_result
