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
open SetMap
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build
open Fifo
open Printf
open Mvbdu_wrapper

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn (fun () -> default) 

let local_trace = false

let dump_channel parameter  f =
  if local_trace ||  Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_log parameter)
let dump_formatter parameter  f =
  if local_trace ||  Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_formatter parameter) 

(************************************************************************************)
(*update bdu:
  - (bdu_X U bdu_creation) U [\rho[update_views] | \rho \in bdu_X (inter) bdu_test views]
*)

(*Xn intersection with bdu_test and modif and then union with X_n*)

let compute_bdu_update_aux parameter handler error bdu_test list_a bdu_X =
  (*do the intersection X_n and bdu_test*)
  let error, handler, bdu_inter =
    Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_X bdu_test
  in
  (*redefine with modification list*)
  let error, handler, bdu_redefine = 
    Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter handler error bdu_inter list_a
  in
  (*do the union of bdu_redefine and bdu_X*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_redefine bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function for views*)

let compute_bdu_update_views parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function deal with agent creation*)

let compute_bdu_update_creation parameter handler error bdu_creation bdu_X =
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_creation bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function deal with side effects*)

let compute_bdu_update_side_effects parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result
    
(************************************************************************************)
(*side effects in the case of half break*)
(*TODO: check the reverse binding: B.x - A.x *)

let store_new_result_hb_map
    parameter
    error 
    half_break_map
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
 let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
 in
 Int2Map_HalfBreak_effect.Map.fold
   (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
     List.fold_left (fun (error, store_result) (rule_id_eff, state) ->
       Int2Map_syn.Map.fold
         (fun rule_id set (error, store_result) ->
           Set_pair.Set.fold 
             (fun ((agent_type1, site_type1, state1),(agent_type2,site_type2, state2))
               (error, store_result) ->
                 if state = state2
                 then
                   if agent_type1 = agent_type_eff &&
                     site_type1 = site_type_eff
                   then
                     Int2Map_CV_Modif.Map.fold
                       (fun (agent_type, cv_id) 
                         (l', rule_id_set) (error, store_result) ->
                           Site_map_and_set.Set.fold
                             (fun rule_id_update (error, current_result) ->
                               if agent_type = agent_type2
                               then
                                 let error, result =
                                   add_link (agent_type, cv_id) rule_id_eff
                                     current_result
                                 in
                                 error, result
                               else
                                 error, current_result
                             ) rule_id_set (error, store_result)
                       ) store_covering_classes_modification_update
                       (error, store_result)
                   else
                     error, store_result
                 else
                   error, store_result
             ) set (error, store_result)
         ) store_contact_map (error, store_result)
     ) (error, store_result) l2
   ) half_break_map (error, store_result_map)
   
(************************************************************************************)
(*side effects in the case of remove*)

let store_new_result_remove_map
  parameter
  error
  remove_map
  store_covering_classes_modification_update
  store_contact_map
  store_result_map
    =
  let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
  in
  Int2Map_Remove_effect.Map.fold
    (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
      List.fold_left (fun (error, store_result) rule_id_eff ->
        Int2Map_syn.Map.fold
          (fun rule_id set (error, store_result) ->
            Set_pair.Set.fold
              (fun ((agent_type1, site_type1, state1),(agent_type2, site_type2, state2))
                (error, store_result) ->
                  if agent_type1 = agent_type_eff && 
                    site_type1 = site_type_eff
                  then
                    let error, store_result =
                      Int2Map_CV_Modif.Map.fold
                        (fun (agent_type, cv_id) (l', rule_id_set)
                          (error, store_result) ->
                            let error, store_result =
                              Site_map_and_set.Set.fold
                                (fun rule_id_update (error, current_result) ->
                                  if agent_type = agent_type2
                                  then
                                    add_link (agent_type, cv_id) rule_id_eff current_result
                                  else
                                    error, current_result
                                ) rule_id_set (error, store_result)
                            in
                            error, store_result
                        ) store_covering_classes_modification_update
                        (error, store_result)
                    in
                    error, store_result
                  else
                    error, store_result
              ) set (error, store_result)
          ) store_contact_map (error, store_result)
      ) (error, store_result) l2
    ) remove_map (error, store_result_map)

(************************************************************************************)
(*combine the result of half break and remove*)

let collect_hb_remove_map parameter error 
    store_side_effects
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let (half_break_map, remove_map) = store_side_effects in
  let error, store_hb_map =
    store_new_result_hb_map
      parameter
      error
      half_break_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  let error, store_remove_map =
    store_new_result_remove_map
      parameter
      error
      remove_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_map
    store_remove_map
    store_result_map
    
(************************************************************************************)
(*combine the result of update(c) and half break and remove side
  effects. This is final update function for side effect*)

let collect_update_hb_remove_map parameter error 
    store_side_effects
    store_contact_map
    store_covering_classes_modification_update
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let error, store_hb_remove_map =
    collect_hb_remove_map
      parameter
      error
      store_side_effects
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_remove_map
    store_covering_classes_modification_update
    store_result_map

(************************************************************************************)
(*write a function add update(c) into working list*)

let add_update_to_wl parameter error store_covering_classes_modification_update wl =
  (* JF: this function should not fold over the full map; 
     this function should receive the list/set of couple to be updated 
     I do not have the spec, but I think that they should contain only all the couple (agent_type,cv_id) that has associated with a new view) *)
  Int2Map_CV_Modif.Map.fold
    (fun (agent_type, cv_id) (l1, s1) (error, wl) ->
      let _ = dump_channel parameter
	(fun stderr ->
	  let () = Printf.fprintf stderr 
            "in add_update_to_wl:agent_type:%i:cv_id:%i\n" agent_type cv_id 
          in
	  let _ = Site_map_and_set.Set.iter 
	    (Printf.fprintf stderr "in add_update_to_wl: set of rule_id:%i\n") s1
	  in
	  ())
      in 
      let error, wl =
        Site_map_and_set.Set.fold (fun rule_id (error, wl) ->
	  let _ = dump_channel parameter (fun stderr ->
            Printf.fprintf stderr "in add_update_to_wl:rule_id:%i\n" rule_id) 
          in
	  let error, wl = IntWL.push parameter error rule_id wl in
          error, wl
	) s1 (error, wl)
      in
      error, wl
    ) store_covering_classes_modification_update (error, wl)
    
(************************************************************************************)
(*fixpoint*)

(*from rule_id get the bdu_creation_map, bdu_test_map, and modif_list_map*)

let collect_bdu_creation_and_modif_list
    parameter
    error
    rule_id
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    =
  let error, bdu_creation_map = 
    match Map_final_creation_bdu.Map.find_option rule_id
      store_proj_bdu_creation_restriction_map
    with
    | None -> error, Map_agent_type_creation_bdu.Map.empty
    | Some map -> error, map
  in
  let error, modif_list_map =
    match Map_final_modif_list.Map.find_option rule_id
      store_proj_modif_list_restriction_map
    with
    | None -> error, Map_agent_id_modif_list.Map.empty
    | Some map -> error, map
  in
   let error, bdu_test_map = 
     match 
       Map_final_test_bdu.Map.find_option rule_id 
         store_proj_bdu_test_restriction_map
     with
     | None -> error, Map_agent_id_test_bdu.Map.empty
     | Some map -> error, map
   in
   error, (bdu_creation_map, modif_list_map, bdu_test_map)

(*form rule_id get bdu_potential_map, and modif_list_map (when state of the site is free)*)

let collect_bdu_potential_and_list parameter error rule_id
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map =
  let error, bdu_potential_map =
    match Map_final_potential_bdu.Map.find_option rule_id 
      store_proj_bdu_potential_restriction_map 
    with
    | None -> error, Map_agent_type_potential_bdu.Map.empty
    | Some map -> error, map
  in
  let error, potential_list_map =
    match Map_final_potential_list.Map.find_option rule_id
      store_proj_potential_list_restriction_map
    with
    | None -> error, Map_agent_type_potential_list.Map.empty
    | Some map -> error, map
  in
  error, (bdu_potential_map, potential_list_map)

(************************************************************************************)
(*check is_enable of all using the projection function*)

let is_enable parameter handler error bdu_false 
    rule_id bdu_proj_views store_bdu_update_map =
  let is_enable =
    Map_triple_views.Map.for_all
      (fun (agent_id, agent_type, cv_id)  bdu_test ->
        let error, bdu_X =
          match Map_bdu_update.Map.find_option (agent_type, cv_id) store_bdu_update_map
          with
          | None -> error, bdu_false
          | Some bdu -> error, bdu
        in
        (*do the intersection of bdu_test and bdu_X*)
        let error, handler, bdu_inter =
          Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
        in
        (*check is enable*)
        if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
        then false
        else true
      ) bdu_proj_views
  in
  error, is_enable

(************************************************************************************)
(*compute view in the case of it is a new view and check if it has new bond*)

let compute_view_new_and_bond parameter handler error
    is_new_view 
    is_new_bond
    store_new_result_map
    store_covering_classes_modification_update
    wl_tl
    store_result =
  let error, (handler, new_wl, store_result) =
    if is_new_view
    then
      begin
        if is_new_bond
        then
          let error, new_wl =
            add_update_to_wl
              parameter
              error
              store_new_result_map
              wl_tl
          in
          error, (handler, new_wl, store_result)
        else
          let error, new_wl =
            add_update_to_wl
              parameter
              error
              store_covering_classes_modification_update
              wl_tl
          in
          error, (handler, new_wl, store_result)
      end
    else
      error, (handler, wl_tl, store_result)
  in
  error, (handler, new_wl, store_result)

(************************************************************************************)
(*compute view that is enabled*)

let compute_views_enabled parameter handler error bdu_true bdu_false
    rule_id
    bdu_test_map
    bdu_creation_map 
    modif_list_map 
    bdu_potential_map
    potential_list_map
    store_new_result_map 
    is_new_bond
    wl_tl
    store_covering_classes_modification_update 
    bdu_proj_views
    store_bdu_update_map =
  (*-----------------------------------------------------------------------*)
  (* add_link should collect the list/set of (agent_type,cv_id) for which
     something has changed, so that add_update_to_wl can focus on these
     pairs *)
  let add_link handler (agent_type, cv_id) bdu_update store_result =
    let error, bdu_old =
      match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with
      | None -> error, bdu_false
      | Some old -> error, old
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_update bdu_old
    in  
    (*-----------------------------------------------------------------------*)
    (*checking if it is a new view*)
    let _ = dump_channel parameter (fun stderr -> Printf.fprintf stderr 
      "Ag:%i\n" agent_type)
    in 
    (*-----------------------------------------------------------------------*)
    if Mvbdu_wrapper.Mvbdu.equal bdu_union bdu_old
    then
      let _ = dump_channel parameter (fun stderr -> Printf.fprintf stderr
        "Nothing has changed\n")
      in
      error, handler, false, store_result
    else
      let error, handler, bdu_diff =
	Mvbdu_wrapper.Mvbdu.mvbdu_xor parameter handler error bdu_union bdu_old
      in
      let () = dump_channel parameter (fun stderr ->
	Printf.fprintf stderr "new views:\b" ;
	Mvbdu_wrapper.Mvbdu.print stderr "" bdu_diff) in 
      let store_result =
        Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
      in
      error, handler, true, store_result
  in
  (*-----------------------------------------------------------------------*)
  (*deal with views*)
  let error, (handler, wl_tl, store_result) =
    Map_triple_views.Map.fold
      (fun (agent_id, agent_type, cv_id) _ (error, (handler, wl_tl, store_result)) ->
       (*-----------------------------------------------------------------------*)
        let _ = dump_channel parameter 
          (fun stderr -> Printf.fprintf stderr
            "Ag:%i Type: %i Cvid: %i\n" agent_id agent_type cv_id)
        in 
        let error, bdu_X =
          match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result with
          | None -> error, bdu_false
          | Some bdu -> error, bdu
        in
        let error, bdu_test =
          match Map_triple_views.Map.find_option (agent_id, agent_type, cv_id) bdu_test_map
          with
          | None -> error, bdu_true
          | Some bdu -> error, bdu
        in
        (*TODO: Q:do I need to get the triple like in bdu_test*)
        let error, list =
          match Map_agent_id_modif_list.Map.find_option agent_id modif_list_map with
          | None -> error, (*Mvbdu_wrapper.Mvbdu.empty_list*) []
          | Some l -> error, l
        in
        (*JF: the list should not be built each time, it should be stored
          with the rest of static information*)
        let error, handler, list_a =
          Mvbdu_wrapper.Mvbdu.build_list
            parameter
            handler
            error
            list
        in
        let error, handler, bdu_update =
          compute_bdu_update_views
            parameter
            handler
            error
            bdu_test
            list_a
            bdu_X
        in 
        (*TEST*)
        (*let _ =
          dump_channel parameter (fun stderr -> 
          Printf.fprintf stderr
          "in views_can_apply:(agent_type:%i, cv_id:%i)\n bdu_update:\n"
	  agent_type cv_id;
          Mvbdu_wrapper.Mvbdu.print stderr "" bdu_update)
          in*)
        (*-----------------------------------------------------------------------*)
        let error, handler, is_new_view, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, new_wl, store_result) =
          compute_view_new_and_bond
            parameter
            handler
            error
            is_new_view
            is_new_bond
            store_new_result_map
            store_covering_classes_modification_update
            wl_tl
            store_result
        in
        error, (handler, new_wl, store_result)          
      )
      bdu_proj_views
      (error, (handler, wl_tl, store_bdu_update_map))
  in
  (*-----------------------------------------------------------------------*)
  (*JF:start to deal with agent creation*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_creation_bdu.Map.fold
      (fun (agent_type, cv_id) bdu_creation (error, (handler, wl_tl, store_result)) ->
        (*let cv_id = 0 in *)
        (*JF: the following should be applied for each covering class of
          the agent agent_type, but I do not know where this information is
          stored *)
       let _ = dump_channel parameter 
          (fun stderr -> Printf.fprintf stderr
            "CREATION: Type: %i Cvid: %i\n" agent_type cv_id)
        in 
       let error, bdu_X =
	  match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
	  with
	  | None -> error, bdu_false
	  | Some bdu -> error, bdu
	in
	let error, handler, bdu_update =
          compute_bdu_update_creation
            parameter
            handler
            error
            bdu_creation
            bdu_X
	in
	let error, handler, is_new_view, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
          compute_view_new_and_bond
            parameter
            handler
            error
            is_new_view
            is_new_bond
            store_new_result_map
            store_covering_classes_modification_update
            wl_tl
            store_result
        in
        error, (handler, wl_tl, store_result)          
      )
      bdu_creation_map
      (error, (handler, wl_tl, store_result))
  in 
  (*-----------------------------------------------------------------------*)
  (*JF: start fo deal with side effects*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_potential_bdu.Map.fold 
      (* JF: to do use a fold2 *)
      (fun (agent_type, cv_id) bdu_test (error, (handler, wl_tl, store_result)) ->
        let error, list = 
          match
            Map_agent_type_potential_list.Map.find_option agent_type potential_list_map
          with
          | None -> error, []
          | Some l -> error, l
        in 
        (*TODO*)
        (* JF: the list should not be built each time, it should be stored
           with the rest of static information *)
        let error,handler,list =
          Mvbdu_wrapper.Mvbdu.build_list
            parameter
	    handler
	    error
	    list
        in
        (*JF: the following should be applied for each covering class of
          the agent agent_type, but I do not know where this information is
          stored *)
        let error, bdu_X =
	  match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
	  with
	  | None -> error, bdu_false
	  | Some bdu -> error, bdu
	in
	let error, handler, bdu_update =
          compute_bdu_update_side_effects
            parameter
            handler
            error
            bdu_test
            list
            bdu_X
	in
	let error, handler, is_new_view, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
          compute_view_new_and_bond
            parameter
            handler
            error
            is_new_view
            is_new_bond
            store_new_result_map
            store_covering_classes_modification_update
            wl_tl
            store_result
        in
        error, (handler, wl_tl, store_result)          
      )
      bdu_potential_map
      (*store_proj_bdu_views*)
      (error, (handler, wl_tl, store_result))
  in 
  error, (handler, wl_tl, store_result)

(************************************************************************************)
(*fixpoint iteration with/without initial state*)

let collect_bdu_fixpoint_with_init parameter handler error 
    bdu_true
    bdu_false
    (wl_creation:Fifo.IntWL.WSet.elt list * Fifo.IntWL.WSet.elt list *
       Fifo.IntWL.WSet.t)
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map (*CHECK ME*)
    store_proj_bdu_views
    is_new_bond
    store_new_result_map
    store_covering_classes_modification_update
    store_bdu_init_restriction_map
    store_result_map
    =
  (*-----------------------------------------------------------------------*)
  let add_link handler (agent_type, cv_id) bdu store_result =
    let bdu_old =
      Map_bdu_update.Map.find_default bdu_false (agent_type, cv_id) store_result
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
    in
    let result_map =
      Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------------*)
  (*in case having initial state the bdu_iter will be the union of bdu_init
    and bdu_iter*)
  let error, store_bdu_fixpoint_init_map =
    Map_bdu_update.Map.fold2_with_logs
      (fun parameter error str str_opt exn ->
        let error, _ = warn parameter error str_opt exn Not_found in
        error)
      parameter
      error
      (*exists 'a t*)
      (fun parameter error (agent_type, cv_id) bdu store_result ->
        let error, store_result =
          add_link handler (agent_type, cv_id) bdu store_result
        in
        error, store_result
      )
      (*exists 'b t*)
      (fun parameter error (agent_type, cv_id) bdu store_result ->
        let error, store_result =
          add_link handler (agent_type, cv_id) bdu store_result
        in
        error, store_result
      )
      (*both cases*)
      (fun parameter error (agent_type, cv_id) bdu bdu' store_result ->
        let error, handler, union_bdu =
          Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu bdu'
        in
        let error, store_result =
          add_link handler (agent_type, cv_id) union_bdu store_result
        in
        error, store_result
      )
      store_result_map
      store_bdu_init_restriction_map
      Map_bdu_update.Map.empty
  in
  (*-----------------------------------------------------------------------*)
  (*add update(c) into working list*)
  let error, wl_init_creation =
    add_update_to_wl parameter error store_covering_classes_modification_update
      wl_creation
  in
  (*-----------------------------------------------------------------------*)
  let _ =
    dump_channel parameter (fun stderr -> 
      fprintf stderr "wl_init_creation:\n";
      Fifo.IntWL.print_wl parameter wl_init_creation)
  in
  (*-----------------------------------------------------------------------*)
  (*iterate function*)
  let rec aux acc_wl (error, handler, store_bdu_fixpoint_init_map) =
    if IntWL.is_empty acc_wl
    then
      error, (handler, store_bdu_fixpoint_init_map)
    else
      (*-----------------------------------------------------------------------*)
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) = IntWL.pop parameter error acc_wl in
      match rule_id_op with
      | None -> error, (handler, store_bdu_fixpoint_init_map) (* Put a warning in error *)
      | Some rule_id ->
        (*----------------------------------------------------------------------*)
        (*compute bdu_creation, bdu_test and modif_list for this rule_id*)
	let _ = dump_channel parameter 
          (fun stderr -> Printf.fprintf stderr "Test for rule:%i\n" rule_id)
        in
        (*--------------------------------------------------------------------*)
        let error, bdu_proj_views =
	  match Map_rule_id_views.Map.find_option rule_id store_proj_bdu_views with
	  | None -> error, Map_triple_views.Map.empty
	  | Some m -> error, m
	in
        (*--------------------------------------------------------------------*)
        (*Q:return list_a instead of a pair of list*)
        let error, (bdu_creation_map, modif_list_map, bdu_test_map) =
          collect_bdu_creation_and_modif_list
            parameter
            error
            rule_id
            store_proj_bdu_creation_restriction_map
            store_proj_modif_list_restriction_map
            store_proj_bdu_test_restriction_map
        in
        (*--------------------------------------------------------------------*)
        let error, (bdu_potential_map, potential_list_map) =
          collect_bdu_potential_and_list
            parameter
            error
            rule_id
            store_proj_bdu_potential_restriction_map
            store_proj_potential_list_restriction_map
        in
        (*--------------------------------------------------------------------*)
        (*is for all bdu_test sastify a covering_class?*)
        let error, is_enable = 
          is_enable
            parameter
            handler
            error
            bdu_false
            rule_id
            bdu_proj_views
            store_bdu_fixpoint_init_map
        in
        (*-----------------------------------------------------------------------*)
        begin
          if is_enable
          then
            let _ = dump_channel parameter 
              (fun stderr -> Printf.fprintf stderr "enabled\n")
            in
            let error, (handler, new_wl, store_new_result) =
              compute_views_enabled
                parameter
                handler
                error
                bdu_true
                bdu_false
		rule_id 
		bdu_proj_views
                bdu_creation_map
                modif_list_map
                bdu_potential_map
                potential_list_map
                store_new_result_map
                is_new_bond
                wl_tl
                store_covering_classes_modification_update
                bdu_proj_views
                store_bdu_fixpoint_init_map
            in
            aux new_wl (error, handler, store_new_result)
          else
            let _ = dump_channel parameter 
              (fun stderr -> Printf.fprintf stderr "disabled\n") 
            in
            aux wl_tl (error, handler, store_bdu_fixpoint_init_map)
        end
  in
  (*start with init_map and union with initial state*)
  aux wl_init_creation (error, handler, store_bdu_fixpoint_init_map)

(************************************************************************************)
(*final fixpoint iteration*)

let collect_bdu_fixpoint_map parameter handler error 
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map
    store_proj_bdu_views
    is_new_bond
    store_covering_classes_modification_update
    store_new_result_map
    store_bdu_init_restriction_map
    store_result_map
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error
  in 
  let error, handler, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error 
  in
  (*-----------------------------------------------------------------------*)
  (*fixpoint*)
  let error, (handler, store_bdu_fixpoint_map) =
     collect_bdu_fixpoint_with_init
       parameter
       handler
       error
       bdu_true
       bdu_false
       wl_creation
       store_proj_bdu_creation_restriction_map
       store_proj_modif_list_restriction_map
       store_proj_bdu_test_restriction_map
       store_proj_bdu_potential_restriction_map
       store_proj_potential_list_restriction_map
       store_bdu_test_restriction_map
       store_proj_bdu_views
       is_new_bond
       store_new_result_map
       store_covering_classes_modification_update
       store_bdu_init_restriction_map
       store_result_map
  in
  error, (handler, store_bdu_fixpoint_map)

(************************************************************************************)
(*fixpoint iteration without initial state*)
(*REMOVE*)

(*let compute_bdu_update_test parameter handler error bdu_test list_a bdu_creation bdu_X =
  (*to the first one with bdu_test*)
  let error, handler, bdu_Xn = 
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  (*do the union per creation*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_Xn bdu_creation
  in
  error, handler, bdu_result*)

(*the final update*)

(*let compute_bdu_update parameter handler error bdu_test list_a bdu_creation
    bdu_potential list_b bdu_X =
  let error, handler, bdu_X_update_test =
    compute_bdu_update_test
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_creation
      bdu_X
  in
  let error, handler, bdu_update_test_side_effects =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_potential
      list_b
      bdu_X_update_test
  in
  error, handler, bdu_update_test_side_effects*)

(************************************************************************************)
(*compute a view that can apply*)

(*let views_can_apply parameter handler error agent_id agent_type cv_id bdu_true
    bdu_false
    bdu_test_map
    modif_list_map
    store_update_map 
    =
  let error, bdu_test =
    match Map_triple_views.Map.find_option (agent_id,agent_type,cv_id) bdu_test_map
    with
    | None -> error, bdu_true
    | Some bdu -> error, bdu
  in
  (*-----------------------------------------------------------------------*)
  let error, bdu_X =
    match Map_bdu_update.Map.find_option (agent_type, cv_id) store_update_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu
  in
 (* (*-----------------------------------------------------------------------*)
  let error, bdu_creation = (* JF: not in the good place, it is not related to a given agent_id and/or a cv_id, you should do it after in a separate routine, by folding over the creation for this rule *)
    match Map_agent_type_creation_bdu.Map.find_option agent_type bdu_creation_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu
  in*)
  (*-----------------------------------------------------------------------*)
  let error, modif_list =
    match Map_agent_id_modif_list.Map.find_option agent_id modif_list_map
    with
    | None -> error, []
    | Some l -> error, l
  in
  let error, handler, list_a =
    Mvbdu_wrapper.Mvbdu.build_list (* JF: the list should not be built each time, it should be stored with the rest of static information *)
      parameter
      handler
      error
      modif_list
  in
  (*-----------------------------------------------------------------------*)
  (*side effects*)
  let error, bdu_potential = error,bdu_false (*
    match Map_agent_type_potential_bdu.Map.find_option agent_type bdu_potential_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu*)
  in
 (* let error, potential_list =
    match Map_agent_type_potential_list.Map.find_option agent_type potential_list_map
    with
    | None -> error, []
    | Some l -> error, l
  in*)
  let error, handler, list_b =
    Mvbdu_wrapper.Mvbdu.build_list  (* JF: the list should not be built each time, it should be stored with the rest of static information *)
      parameter
      handler
      error
      [] (* I put this to separate the computation of side effects (*potential_list*)*)
  in
  (*-----------------------------------------------------------------------*)
  let error, handler, bdu_update =
    compute_bdu_update
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_false (* JF: I put bdu_false to separate the computation of modification and creation (* bdu_creation*)*)
      bdu_false (* JF: I put bdu_false to separate the computation of side_effects (* bdu_potential *)*)
      list_b 
      bdu_X
  in
  (*TEST*)
  let _ =
    dump_channel parameter (fun stderr -> 
      Printf.fprintf stderr "in views_can_apply:(agent_type:%i, cv_id:%i)\n bdu_update:\n"
	agent_type cv_id;
      Mvbdu_wrapper.Mvbdu.print stderr "" bdu_update)
  in
  error, handler, bdu_update*)

(*let views_can_apply parameter handler error agent_id agent_type cv_id bdu_true
    bdu_false
    bdu_test_map
    modif_list_map
    store_update_map 
    =
  let error, bdu_test =
    match Map_triple_views.Map.find_option (agent_id,agent_type,cv_id) bdu_test_map
    with
    | None -> error, bdu_true
    | Some bdu -> error, bdu
  in
  (*-----------------------------------------------------------------------*)
  let error, bdu_X =
    match Map_bdu_update.Map.find_option (agent_type, cv_id) store_update_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu
  in
 (* (*-----------------------------------------------------------------------*)
  let error, bdu_creation = (* JF: not in the good place, it is not related to a given agent_id and/or a cv_id, you should do it after in a separate routine, by folding over the creation for this rule *)
    match Map_agent_type_creation_bdu.Map.find_option agent_type bdu_creation_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu
  in*)
  (*-----------------------------------------------------------------------*)
  let error, modif_list =
    match Map_agent_id_modif_list.Map.find_option agent_id modif_list_map
    with
    | None -> error, []
    | Some l -> error, l
  in
  error, (bdu_test, bdu_X, modif_list)

  (*let error, handler, list_a =
    Mvbdu_wrapper.Mvbdu.build_list (* JF: the list should not be built each time, it should be stored with the rest of static information *)
      parameter
      handler
      error
      modif_list
  in*)
  (*-----------------------------------------------------------------------*)
  (*side effects*)
  let error, bdu_potential = error,bdu_false (*
    match Map_agent_type_potential_bdu.Map.find_option agent_type bdu_potential_map
    with
    | None -> error, bdu_false
    | Some bdu -> error, bdu*)
  in
 (* let error, potential_list =
    match Map_agent_type_potential_list.Map.find_option agent_type potential_list_map
    with
    | None -> error, []
    | Some l -> error, l
  in*)
  let error, handler, list_b =
    Mvbdu_wrapper.Mvbdu.build_list  (* JF: the list should not be built each time, it should be stored with the rest of static information *)
      parameter
      handler
      error
      [] (* I put this to separate the computation of side effects (*potential_list*)*)
  in
  (*-----------------------------------------------------------------------*)
  let error, handler, bdu_update =
    compute_bdu_update
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_false (* JF: I put bdu_false to separate the computation of modification and creation (* bdu_creation*)*)
      bdu_false (* JF: I put bdu_false to separate the computation of side_effects (* bdu_potential *)*)
      list_b 
      bdu_X
  in
  (*TEST*)
  (*let _ =
    dump_channel parameter (fun stderr -> 
      Printf.fprintf stderr "in views_can_apply:(agent_type:%i, cv_id:%i)\n bdu_update:\n"
	agent_type cv_id;
      Mvbdu_wrapper.Mvbdu.print stderr "" bdu_update)
  in*)
  error, handler, bdu_update*)

(*let is_enable' parameter handler error bdu_true bdu_false
    bdu_test_map
    store_bdu_test_restriction_map 
    store_bdu_update_map = 
  let is_enable =
    Map_test_bdu.Map.for_all
      (fun (agent_id, agent_type, rule_id', cv_id) _ ->
        let error, is_bdu_test_empty, bdu_test =
          match 
            Map_agent_id_test_bdu.Map.find_option agent_id bdu_test_map
          with
          | None -> error, true, bdu_true (*if bdu_test is empty*)
          | Some bdu_test -> error, false, bdu_test
        in
        (*TEST*)
        (*let _ = fprintf stdout "in is_enable:bdu_test\n";
          Mvbdu_wrapper.Mvbdu.print stdout "" bdu_test
        in*)
        (*-----------------------------------------------------------------------*)
        (*search bdu_X inside a store_result_map*)
        let error, bdu_X =
          match Map_bdu_update.Map.find_option (agent_type, cv_id)
            store_bdu_update_map
          with
          | None -> error, bdu_false (*the initial state of bdu_iterate is bdu_false*)
          | Some bdu -> error, bdu
        in
        (*-----------------------------------------------------------------------*)
        (*do the intersection of the bdu_test and bdu_iterate*)
        let error, handler, bdu_inter =
          Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error 
            bdu_test bdu_X
        in
        (*-----------------------------------------------------------------------*)
        (*it is an enable rule when the intersection of bdu_test and
          bdu_iter is different than empty; or it is an enable when
          bdu_test is empty*)
        if is_bdu_test_empty
        then
          true
        else
          if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
          then false
          else true
      ) store_bdu_test_restriction_map
  in
  error, is_enable*)

(*let collect_bdu_fixpoint_without_init parameter handler error 
    (* rule*)
    bdu_true
    bdu_false
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map (*CHECK ME*)
    store_proj_bdu_views
    is_new_bond
    store_new_result_map
    store_covering_classes_modification_update
    store_result_map
    =
  (*-----------------------------------------------------------------------*)
  let error, (handler, store_bdu_fixpoint_map) =
    let rec aux acc_wl (error, handler, store_bdu_update_map) =
      if IntWL.is_empty acc_wl
      then
        error, (handler, store_bdu_update_map)
      else
        (*-----------------------------------------------------------------------*)
        (*pop the first element (rule_id) in this working list*)
        let error, (rule_id_op, wl_tl) = IntWL.pop parameter error acc_wl in
        match rule_id_op with
        | None -> error, (handler, store_bdu_update_map) (* Put a warning in error *)
        | Some rule_id ->
	   let _ = dump_channel parameter 
             (fun stderr -> Printf.fprintf stderr "Test for rule:%i\n" rule_id) 
           in
           let error, bdu_proj_views =
	     match Map_rule_id_views.Map.find_option rule_id store_proj_bdu_views with
	     | None -> error, Map_triple_views.Map.empty
	     | Some m -> error, m
	   in
	   (*----------------------------------------------------------------------*)
          (*compute bdu_creation, bdu_test and modif_list for this rule_id*)
          let error, (bdu_creation_map, modif_list_map, bdu_test_map) =
            collect_bdu_creation_and_modif_list
              parameter
              error
              rule_id
              store_proj_bdu_creation_restriction_map
              store_proj_modif_list_restriction_map
              store_proj_bdu_test_restriction_map
          in
          (*--------------------------------------------------------------------*)
          (*get a map of bdu_potential and list of potential partner in side effects*)
          let error, (bdu_potential_map, potential_list_map) =
            collect_bdu_potential_and_list
              parameter
              error
              rule_id
              store_proj_bdu_potential_restriction_map
              store_proj_potential_list_restriction_map
          in
          (*--------------------------------------------------------------------*)
          (*is for all bdu_test sastify a covering_class?*)
          let error, is_enable = 
            is_enable
              parameter
              handler
              error
              bdu_false
              rule_id
              store_proj_bdu_views
              store_bdu_update_map
          in
          (*-----------------------------------------------------------------------*)
          begin
            if is_enable
            then
              let _ = dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr "enabled\n") 
              in
	      let error, (handler, new_wl, store_new_result) =
                compute_views_enabled
                  parameter
                  handler
                  error
                  bdu_true
                  bdu_false
		  rule_id 
                  bdu_proj_views
		  bdu_creation_map
                  modif_list_map
                  bdu_potential_map
                  potential_list_map
                  store_new_result_map
                  is_new_bond
                  wl_tl
                  store_covering_classes_modification_update
                  store_proj_bdu_views
                  store_bdu_update_map
              in
              aux new_wl (error, handler, store_new_result)
            else
              let _ = dump_channel parameter
                (fun stderr -> Printf.fprintf stderr "disabled\n")
              in
              aux wl_tl (error, handler, store_bdu_update_map)
          end
    in
    (*start with init map without union with initial state*)
    aux wl_creation (error, handler, store_result_map)
  in
  error, (handler, store_bdu_fixpoint_map)*)

(*let collect_bdu_fixpoint_map parameter handler error 
    (*rule/meaningless*)
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map
    store_proj_bdu_views
    is_new_bond
    store_new_result_map
    store_covering_classes_modification_update
    store_bdu_init_restriction_map
    store_result_map
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error
  in 
  let error, handler, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error 
  in
  (*-----------------------------------------------------------------------*)
  (*union the result of bdu_fixpoint_map with initial state*)
   if not (Map_bdu_update.Map.is_empty store_bdu_init_restriction_map)
   then
    (*FIXME*)
    (*there is initial state*)
     let error, (handler, store_bdu_fixpoint_map) = (* there should be only one function, in the case there is no init, something will be fold over store_bdu_init_restriction_map  before doing the fixpoint, if it is empty the fold will do nothing, but the code should be shared *)
      collect_bdu_fixpoint_with_init
        parameter
        handler
        error
         (*rule*)
        bdu_true
        bdu_false
        wl_creation
        store_proj_bdu_creation_restriction_map
        store_proj_modif_list_restriction_map
        store_proj_bdu_test_restriction_map
        store_proj_bdu_potential_restriction_map
        store_proj_potential_list_restriction_map
        store_bdu_test_restriction_map
        store_proj_bdu_views
        is_new_bond
        store_new_result_map
        store_covering_classes_modification_update
        store_bdu_init_restriction_map
        store_result_map
    in
    error, (handler, store_bdu_fixpoint_map)
  else
    (*there is no initial state*)
    let error, (handler, store_bdu_fixpoint_map) =
      collect_bdu_fixpoint_without_init
        parameter
        handler
        error
       (*rule*)
        bdu_true
        bdu_false
        wl_creation
        store_proj_bdu_creation_restriction_map
        store_proj_modif_list_restriction_map
        store_proj_bdu_test_restriction_map
        store_proj_bdu_potential_restriction_map
        store_proj_potential_list_restriction_map
        store_bdu_test_restriction_map
        store_proj_bdu_views
        is_new_bond
        store_new_result_map
        store_covering_classes_modification_update
        store_result_map
    in
    error, (handler, store_bdu_fixpoint_map)
      *)
