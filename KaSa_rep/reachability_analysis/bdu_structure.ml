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
open Cckappa_sig
open Bdu_build
open Bdu_build_common

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
(*collect remanent modification as a map function*)

let collect_remanent_modif_map parameter error store_remanent_modif =
  let add_link (agent_type, rule_id) triple_list store_result =
    let (l, old) =
      Map_modif.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_modif.Map.add (agent_type, rule_id)
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
    ) store_remanent_modif Map_modif.Map.empty


(*************************************************************************************)
(* Build BDU test, creation and a list of modification rules*)
(*************************************************************************************)

(*************************************************************************************)
(*build bdu for test rules*)

(*let collect_test_bdu parameter error store_test_map = (*REMOVE*)
  let error, init = AgentMap.create parameter error 0 in
  Map_test.Map.fold (fun (agent_type, rule_id) (l1, l2) (error, store_result) ->
    (*return a list of a pair (site, state) *)
    let error, pair_list =
      List.fold_left (fun (error, current_list) (cv_id, site, state) ->
        let pair_list = (site, state) :: current_list in
        error, pair_list
      ) (error, []) l2
    in
    (*-----------------------------------------------------------------------*)
    (*build bdu test from this pair_list*)
    let error, (handler, bdu_test) =
      build_bdu parameter error pair_list
    in
    (*-----------------------------------------------------------------------*)
    (*store handler, bdu_test in a map with (agent_type, rule_id) *)
    let error, old_list = 
      match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = (rule_id, bdu_test) :: old_list in
    (*-----------------------------------------------------------------------*)
    let error, store_result =
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_list)
        store_result
    in
    error, store_result
  ) store_test_map (error, init)*)

(*TEST*)
let collect_test_bdu_map parameter error store_test_map =
  let add_link (agent_type, rule_id) bdu store_result =
    let (l, old) =
      Map_test_bdu.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_test_bdu.Map.add (agent_type, rule_id)
        (l, bdu :: old) store_result
    in
    error, result_map
  in
  Map_test.Map.fold (fun (agent_type, rule_id) (l1, l2) (error, store_result) ->
    (*return a list of a pair (site, state) *)
    let error, pair_list =
      List.fold_left (fun (error, current_list) (cv_id, site, state) ->
        let pair_list = (site, state) :: current_list in
        error, pair_list
      ) (error, []) l2
    in
    (*build bdu test from this pair_list*)
    let error, (handler, bdu_test) =
      build_bdu parameter error pair_list
    in
    (*store handler, bdu_test in a map with (agent_type, rule_id) *)
    let error, store_result =
      add_link (agent_type, rule_id) bdu_test store_result
    in
    error, store_result
  ) store_test_map (error, Map_test_bdu.Map.empty)


(*************************************************************************************)
(*build bdu for creation rules*)

(*let collect_creation_bdu parameter error store_creation_map =
  let error, init = AgentMap.create parameter error 0 in
  Map_creation.Map.fold (fun (agent_type, rule_id) (l1, l2) (error, store_result) ->
    (*return a list of a pair (site, state) *)
    let error, pair_list =
      List.fold_left (fun (error, current_list) (cv_id, site, state) ->
        let pair_list = (site, state) :: current_list in
        error, pair_list
      ) (error, []) l2
    in
    (*-----------------------------------------------------------------------*)
    (*build bdu test from this pair_list*)
    let error, (handler, bdu_creation) =
      build_bdu parameter error pair_list
    in
    (*-----------------------------------------------------------------------*)
    (*store handler, bdu_test in a map with (agent_type, rule_id) *)
    let error, old_list = 
      match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = (rule_id, bdu_creation) :: old_list in
    (*-----------------------------------------------------------------------*)
    let error, store_result =
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_list)
        store_result
    in
    error, store_result
  ) store_creation_map (error, init)*)

(*FIXME, use this map instead*)
let collect_creation_bdu_map parameter error store_creation_map =
  let add_link (agent_type, rule_id) bdu store_result =
    let (l, old) =
      Map_creation_bdu.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_creation_bdu.Map.add (agent_type, rule_id)
        (l, bdu :: old) store_result
    in
    error, result_map
  in
  Map_creation.Map.fold (fun (agent_type, rule_id) (l1, l2) (error, store_result) ->
    (*return a list of a pair (site, state) *)
    let error, pair_list =
      List.fold_left (fun (error, current_list) (cv_id, site, state) ->
        let pair_list = (site, state) :: current_list in
        error, pair_list
      ) (error, []) l2
    in
    (*build bdu test from this pair_list*)
    let error, (handler, bdu_creation) =
      build_bdu parameter error pair_list
    in
    (*store handler, bdu_test in a map with (agent_type, rule_id) *)
    let error, store_result =
      add_link (agent_type, rule_id) bdu_creation store_result
    in
    error, store_result
  ) store_creation_map (error, Map_creation_bdu.Map.empty)

(*************************************************************************************)
(*list of modification rules*)

let collect_modif_list_map parameter error store_modif_map =
  let add_link (agent_type, rule_id) list store_result =
    let (l, old) =
      Map_modif_list.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_modif_list.Map.add (agent_type, rule_id)
        (l, (List.concat [list; old])) store_result
    in
    error, result_map
  in
  Map_modif.Map.fold (fun (agent_type, rule_id) (l1, l2) (error, store_result) ->
    (*return a list of a pair (site, state) *)
    let error, list =
      List.fold_left (fun (error, current_list) (cv_id, site, state) ->
        let list = site :: current_list in
        error, list
      ) (error, []) l2
    in
    (*build hanlder and list_a from modification list*)
    (*let error, (handler, list_a) =
      List_algebra.build_list
        (Boolean_mvbdu.list_allocate parameter)
        error
        parameter
        handler
        list
    in*)
    (*store handler, bdu_test in a map with (agent_type, rule_id) *)
    let error, store_result =
      add_link (agent_type, rule_id) list store_result
    in
    error, store_result
  ) store_modif_map (error, Map_modif_list.Map.empty)

