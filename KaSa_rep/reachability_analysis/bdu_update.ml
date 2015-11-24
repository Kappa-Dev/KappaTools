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
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default ([], Site_map_and_set.Set.empty)
	(agent_type, cv_id) store_result in
    let error',new_set = Site_map_and_set.Set.union parameter error rule_id_set old in
    let error = Exception.check warn parameter error error' (Some "line 51") Exit in    
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, new_set) store_result
    in
    error, result
  in
  let (l1, s2) = Int2Map_Test_Modif.Map.find_default ([], Site_map_and_set.Set.empty)
    (agent_type_cv, site_type_cv) store_test_modification_map
  in
  let error, result =
    add_link (agent_type_cv, cv_id) s2 store_result
  in
  error, result
    
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
