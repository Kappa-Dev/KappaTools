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
open Set_and_map

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
*)

(*map (agent_type_cv, covering_class_id) -> rule_id list of modified sites*)

let store_covering_classes_modification_update_aux parameter error agent_type_cv
    site_type_cv covering_class_id store_modification_sites store_result =
  let add_link (agent_type_cv, site_type_cv, cv_id) rule_id store_result =
    (*searching in the result whether or not those signatures are already inside*)
    let l, old =
      try Int2Map_CV_Modif.find (agent_type_cv, site_type_cv, cv_id) store_result
      with Not_found -> [], []
    in
    (*add the fresh signature into the old result and store them*)
    Int2Map_CV_Modif.add (agent_type_cv, site_type_cv, cv_id)
      (l, rule_id :: old) store_result
  in
  (*Example:
    - modification_sites: 
    [agent_type:0@site_type:0:rule_id:2;
     agent_type:0@site_type:0:rule_id:1]
    - covering class:
    [agent_type:0@site_type:0:covering_class_id:0]
    - map each covering class to the list of rule that may modify the view of
    covering class.
    - result:
    [agent_type:0@site_type:0:covering_class_id:0:rule_id:2;
     agent_type:0@site_type:0:covering_class_id:0:rule_id:1]
  *)
  Int2Map_Modif.fold
    (fun (agent_type_modif, site_modif) (m1, m2) store_result ->
      (*if m1 <> []
        then ()
      else
        ();*)
      List.fold_left (fun store_current_result rule_id_modif ->
        if agent_type_cv = agent_type_modif &&
          site_type_cv  = site_modif
        then
          add_link (agent_type_cv, site_type_cv, covering_class_id) rule_id_modif 
            store_current_result
        else
          store_current_result
      ) store_result m2
    ) store_modification_sites store_result
    
(************************************************************************************)

let store_covering_classes_modification_update parameter error store_modification_sites
    store_covering_classes_id =
  let store_result =
    Int2Map_CV.fold
      (fun (agent_type_cv, site_type_cv) (l1, l2) store_result ->
        if l1 <> []
        then
          ()
        else
          ();
        List.fold_left (fun store_current_result covering_class_id ->
          store_covering_classes_modification_update_aux
            parameter
            error
            agent_type_cv
            site_type_cv
            covering_class_id
            store_modification_sites
            store_current_result
        ) store_result l2
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) store_covering_classes_id Int2Map_CV_Modif.empty
  in
  let store_result =
    Int2Map_CV_Modif.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*update function when discover a binding site. Added rule_id of side
  effect into the above update function.*)
(*TODO*)

let binding_hb_effect_aux parameter error agent_type_1 site_type_1
    state_1 agent_type_2 site_type_2 store_half_break
    store_covering_classes_modification_update store_result =
  let add_link (agent_type, site_type, cv_id) rule_id_effect store_result =
    let l, old =
      try Int2Map_CV_Modif.find (agent_type, site_type, cv_id) store_result
      with Not_found -> [], []
    in
    Int2Map_CV_Modif.add (agent_type, site_type, cv_id)
      (l, rule_id_effect :: old) store_result
  in
  Int2Map_HalfBreak_effect.fold
    (fun (agent_type_effect, site_type_effect) (l1, l2) store_result ->
      if l1 <> []
        then ()
      else ();
      List.fold_left (fun store_current_result (rule_id_effect, state_effect) ->
        Int2Map_CV_Modif.fold (fun (agent_type_cv, site_type_cv, covering_class_id)
          (m1, m2) store_result ->
            if m1 <> []
            then ()
            else ();
            List.fold_left (fun current_result rule_id_update ->
              (*the first binding agent belong to side effect;
                the second binding agent belong to update_function*)
              if agent_type_1 = agent_type_effect &&
                 site_type_1  = agent_type_effect &&
                 state_1      = state_effect      &&
                 agent_type_2 = agent_type_cv     &&
                 site_type_2  = site_type_cv
              then
                add_link (agent_type_cv, site_type_cv, covering_class_id) 
                  rule_id_effect current_result
              else
                current_result
            ) store_result m2
        ) store_covering_classes_modification_update store_current_result
      ) store_result l2
    ) store_half_break store_result

(************************************************************************************)
(*TODO*)

let binding_remove_effect_aux parameter error agent_type_1 site_type_1 agent_type_2
    site_type_2 store_remove_effect store_covering_classes_modification_update
    store_result =
  let add_link (agent_type, site_type, cv_id) rule_id_effect store_result =
    let l, old =
      try Int2Map_CV_Modif.find (agent_type, site_type, cv_id) store_result
      with Not_found -> [], []
    in
    Int2Map_CV_Modif.add (agent_type, site_type, cv_id)
      (l, rule_id_effect :: old) store_result
  in
  Int2Map_Remove_effect.fold
    (fun (agent_type_effect, site_type_effect) (l1, l2) store_result ->
      if l1 <> []
      then ()
      else ();
      List.fold_left (fun store_current_result rule_id_effect ->
        Int2Map_CV_Modif.fold 
          (fun (agent_type_cv, site_type_cv, covering_class_id) (m1, m2)
            store_result ->
              if m1 <> []
              then ()
              else ();
              List.fold_left (fun current_result rule_id_update ->
                if agent_type_1 = agent_type_effect &&
                   site_type_1  = site_type_effect  &&
                   agent_type_2 = agent_type_cv     &&
                   site_type_2  = site_type_cv
                then
                  add_link (agent_type_cv, site_type_cv, covering_class_id)
                    rule_id_effect current_result
                else
                  current_result
              ) store_result m2
          ) store_covering_classes_modification_update store_current_result
      ) store_result l2
    ) store_remove_effect store_result

(************************************************************************************)

let store_binding_update parameter error 
    store_covering_classes_modification_update
    store_side_effects
    store_binding_dual =
  (*get binding information*)
  (*TODO: the forward direction first A-B; reverse B-A*)
  let store_binding_forward, store_binding_reverse = store_binding_dual in
  (*get side effect information*)
  let store_half_break, store_remove_effect = store_side_effects in
  let store_result_hb,
    store_result_remove,
    store_result_hb_remove,
    store_result_update_aux
    =
    Int2Map_CM_state.fold
      (fun (agent_type_1, site_type_1, state_1)
        (l1, l2) store_result ->
          if l1 <> []
          then ()
          else
            ();
          List.fold_left (fun 
            (store_current_result_hb, store_current_result_remove, _, _)
            (agent_type_2, site_type_2, state_2) ->
              (*binding with half break side effect*)
              let store_binding_hb =
                binding_hb_effect_aux
                  parameter
                  error
                  agent_type_1
                  site_type_1
                  state_1
                  agent_type_2
                  site_type_2
                  store_half_break
                  store_covering_classes_modification_update
                  store_current_result_hb
              in
              (*binding with remove side effect*)
              let store_binding_remove =
                binding_remove_effect_aux
                  parameter
                  error
                  agent_type_1
                  site_type_1
                  agent_type_2
                  site_type_2
                  store_remove_effect
                  store_covering_classes_modification_update
                  store_current_result_remove
              in
              (*combine half break and remove side effect*)
              let store_binding_hb_remove =
                Int2Map_CV_Modif.merge 
                  store_binding_hb store_binding_remove
              in
              (*combine half_break and remove with previous binding function*)
              let store_update_aux =
                Int2Map_CV_Modif.merge
                  store_covering_classes_modification_update
                  store_binding_hb_remove
              in
              (store_binding_hb,
               store_binding_remove,
               store_binding_hb_remove,
               store_update_aux
              )
          ) store_result l2
      ) store_binding_forward 
      (Int2Map_CV_Modif.empty,
       Int2Map_CV_Modif.empty,
       Int2Map_CV_Modif.empty,
       Int2Map_CV_Modif.empty
      )
  in
  let store_result_hb =
    Int2Map_CV_Modif.map (fun (l, x) -> List.rev l, x) store_result_hb
  in
  let store_result_remove =
    Int2Map_CV_Modif.map (fun (l, x) -> List.rev l, x) store_result_remove
  in
  let store_result_hb_remove =
    Int2Map_CV_Modif.map (fun (l, x) -> List.rev l, x) store_result_hb_remove
  in
  let store_result_update_aux =
    Int2Map_CV_Modif.map (fun (l, x) -> List.rev l, x) store_result_update_aux
  in
  error,
  (store_result_hb,
   store_result_remove,
   store_result_hb_remove,
   store_result_update_aux
  )
