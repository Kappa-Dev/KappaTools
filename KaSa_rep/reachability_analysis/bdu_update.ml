(**
  * bdu_update.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 12th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Int_storage
open Bdu_analysis_type
open Bdu_contact_map
open Covering_classes_type
open Covering_classes
open SetMap

open Print_bdu_analysis

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU update function") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*a bond is discovered for the first time:
  For example:
  'r0' A() ->
  'r1' A(x) ->
  'r2' A(x!B.x) -> A(x)
  'r3' A(x), B(x) -> A(x!1), B(x!1)
  'r4' A(x,y), C(y) -> A(x,y!1), C(y!1)
  
  'r3' has a bond on the rhs, for any (rule_id, state) belong to side
  effects of A(x); state is compatible with B(x!1), add rule_id into update
  function.

  - map (agent_type_cv, covering_class_id) -> rule_id list of modified sites
*)

let store_covering_classes_modification_update_aux parameter error agent_type_cv
    site_type_cv cv_id store_test_modification_map store_result =
  let add_link (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 75") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite parameter error (agent_type, cv_id) (l, new_set)
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let error, (l, rule_id_set) =
    match Int2Map_Test_Modif.Map.find_option_without_logs parameter error
      (agent_type_cv, site_type_cv) store_test_modification_map
    with
    | error, None -> error, ([], Site_map_and_set.Set.empty)
    | error, Some (l, s) -> error, (l, s)
  in
  let error, result =
    add_link (agent_type_cv, cv_id) rule_id_set store_result
  in
    (*-------------------------------------------------------------------------------*)
    (*map this map*)
  let store_result =
    Int2Map_CV_Modif.Map.map (fun (l, x) -> List.rev l, x) result
  in
  error, store_result
    
(************************************************************************************)

let store_covering_classes_modification_update parameter error
    store_test_modification_map
    store_covering_classes_id =
  let error, store_result =
    Int2Map_CV.Map.fold
      (fun (agent_type_cv, site_type_cv) (l1, l2) store_result ->
        List.fold_left (fun (error, store_current_result) cv_id ->
          let error, result =
            store_covering_classes_modification_update_aux
              parameter
              error
              agent_type_cv
              site_type_cv
              cv_id
              store_test_modification_map
              store_current_result
          in
          error, result
        ) store_result l2
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) store_covering_classes_id (error, Int2Map_CV_Modif.Map.empty)
  in
  let store_result =
    Int2Map_CV_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*update function added information of rule_id in side effects*)

let store_covering_classes_modification_side_effects parameter error 
    store_test_modification_map
    store_potential_side_effects
    covering_classes
    store_result =
  let add_link (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 169") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite parameter error (agent_type, cv_id) (l, new_set) 
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let _, store_potential_side_effects_bind = store_potential_side_effects in
  let error, store_result =
    Int2Map_potential_effect.Map.fold 
      (fun (agent_type_partner, rule_id_effect) pair_list (error, store_result) ->
        List.fold_left (fun (error, store_result) (site_type_partner, state) ->
          let error, store_result =
            AgentMap.fold parameter error
              (fun parameter error agent_type_cv remanent store_result ->
                let cv_dic = remanent.store_dic in
                let error, store_result =
                  Dictionary_of_Covering_class.fold
                    (fun list_of_site_type ((), ()) cv_id (error, store_result) ->
                    (*get a set of rule_id in update(c)*)
                      let error, (l, rule_id_set) =
                        match Int2Map_Test_Modif.Map.find_option_without_logs parameter error
                          (agent_type_partner, site_type_partner)
                          store_test_modification_map
                        with
                        | error, None -> error, ([], Site_map_and_set.Set.empty)
                        | error, Some (l, s) -> error, (l, s)
                      in
                      (*add rule_id_effect into rule_id_set*)
                      let error, new_rule_id_set =
                        Site_map_and_set.Set.add parameter error rule_id_effect rule_id_set
                      in
                      let error, store_result =
                        add_link (agent_type_partner, cv_id) new_rule_id_set store_result
                      in
                      error, store_result
                    ) cv_dic (error, store_result)
                in
                error, store_result
              ) covering_classes store_result
          in
          error, store_result
        ) (error, store_result) pair_list
      ) store_potential_side_effects_bind (error, store_result)
  in
  error, store_result

(************************************************************************************)
(*combine update(c) and update(c') of side effects together*)

let store_covering_classes_modification_update_full parameter error
    store_test_modification_map
    store_potential_side_effects
    store_covering_classes_id
    covering_classes
    store_result
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 251") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite
        parameter error (agent_type, cv_id) (l, new_set) store_result
    in
    error, result
  in
  let error, store_update_modification =
    store_covering_classes_modification_update
      parameter
      error
      store_test_modification_map
      store_covering_classes_id
  in
  let init_cv_modification_side_effects  = Int2Map_CV_Modif.Map.empty in
  let error, store_update_with_side_effects =
    store_covering_classes_modification_side_effects
      parameter
      error
      store_test_modification_map
      store_potential_side_effects
      covering_classes
      init_cv_modification_side_effects
  in
  (*---------------------------------------------------------------------------*)
  (*fold 2 map*)
  Int2Map_CV_Modif.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (_, rule_id_set) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (_, rule_id_set) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*both*)
    (fun parameter error (agent_type, cv_id) (_, s1) (_, s2) store_result ->
      let error, union_set =
        Site_map_and_set.Set.union parameter error s1 s2
      in
      let error, store_result =
        add_link error (agent_type, cv_id) union_set store_result
      in
      error, store_result
    )
    store_update_modification
    store_update_with_side_effects
    store_result
