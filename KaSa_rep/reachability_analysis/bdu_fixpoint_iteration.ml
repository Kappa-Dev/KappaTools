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

(************************************************************************************)
(*update bdu:
  - (bdu_X U bdu_creation) U [\rho[update_views] | \rho \in bdu_X (inter) bdu_test views]
*)

(*Xn intersection with bdu_test and modif and then union with X_n*)

let compute_bdu_update_test parameter handler error bdu_test list_a bdu_X =
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

(*REMOVE:per creation*)
let compute_bdu_update_creation parameter handler error bdu_creation bdu_X' =
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_X' bdu_creation
  in
  error, handler, bdu_result

(*the final update*)
let compute_bdu_update parameter handler error bdu_test list_a bdu_creation bdu_X =
  (*to the first one with bdu_test*)
  let error, handler, bdu_Xn = 
    compute_bdu_update_test
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
  error, handler, bdu_result

(************************************************************************************)
(*a bond is discovered for the first time*)

(*rule that has the views that are tested and bond: if one can apply this
  rule, then add rule of side effects into the update(c), and then add
  update(c') to the working list.*)
 
let collect_test_has_bond_rhs parameter error rule_id rule store_result =
  let add_bond rule_id set store_result =
    let error, old =
      match Map_test_bond.Map.find_option rule_id store_result with
      | None -> error, Map_site_address.Set.empty
      | Some s -> error, s
    in
    let union_set = Map_site_address.Set.union set old in
    (*check if a bond is discovered for the first time*)
    if Map_site_address.Set.equal union_set old
    then error, false, store_result
    else 
      (*it is discovered for the first time*)
      let result_map =
        Map_test_bond.Map.add rule_id union_set store_result
      in
      error, true, result_map
  in
  let error, bool, store_result =
    List.fold_left (fun (error, b, store_result) (site_add1, site_add2) ->
      let error, set =
        (*if Map_site_address.Set.mem (site_add1, site_add2) set
          then error, set
          else*)
        error, Map_site_address.Set.add (site_add1, site_add2) Map_site_address.Set.empty
      in
      let error, is_new_bond, store_result =
        add_bond rule_id set store_result
      in
      error, is_new_bond, store_result
    ) (error, false, store_result) rule.actions.bind
  in
  error, bool, store_result
    
(*write a function if discover a new bond, add rule_id into working list*)
    
let add_rule_id_to_update_aux parameter error
    rule_id_eff
    bond_rhs_map
    store_covering_classes_modification_update
    store_result
    =
  let error, store_new_update_map =
    Int2Map_CV_Modif.Map.fold
      (fun (agent_type, cv_id) (l', rule_id_set) (error, store_result) ->
	let error, store_result =
	  Map_test_bond.Map.fold (fun rule_id set (error, store_result) ->
	    let error, store_result =
		(*This is the case: A(x!1), B(x!)*)
		Map_site_address.Set.fold (fun (_, site_add2) (error, store_result) ->
		  let agent_type2_bond_rhs = site_add2.agent_type in
		  (*if agent_type in update is equal to side_address2 in bond_rhs*)
		  if agent_type2_bond_rhs = agent_type
		  then
		    (*add rule_id_effect inside rule_id_set with
		      agent_type2*)
		    let error, new_rule_id_set =
		      Site_map_and_set.Set.add parameter error rule_id_eff rule_id_set
		    in
		    (*add this new rule_id_set into map*)
		    let result =
		      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
			(l', new_rule_id_set) store_result (*store in update function*)
		    in
		    error, result
		  else
		    error, store_result (*return the orginal update function*)
		) set (error, store_result)
	      in
	      error, store_result
	  ) bond_rhs_map (error, store_result)
	in
	error, store_result
      ) store_covering_classes_modification_update
      (error, store_covering_classes_modification_update)
  in
  error, store_new_update_map

(*checking the state of binding in the case of half break side effects. Remove does not has information about state of binding.*)
let state_compatible
    parameter
    error
    state
    bond_rhs_map
    rule_id_eff
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let error, store_result =
    Int2Map_CM_state.Map.fold
      (fun (agent_type1, site_type1, state1) (l, triple_list) (error, store_result) ->
	let error, store_result_map =
	  List.fold_left (fun (error, store_result) (agent_type2, site_type2, state2) ->
	    (*check the state of side effect is compatible with 
	      the state of it binding state in contact map?*)
	    if state = state2
	    then
	      let error, store_new_update_map =
		add_rule_id_to_update_aux
		  parameter
		  error
		  rule_id_eff
		  bond_rhs_map
		  store_covering_classes_modification_update
		  store_result
	      in
	      error, store_new_update_map
	    else
	      error, store_result
	  ) (error, store_result) triple_list
	in
	error, store_result_map
      ) store_contact_map (error, store_result_map)
  in
  error, store_result

(*side effects in the case of half break, add into update function*)
let store_new_result_hb_map
    parameter
    error
    half_break_map
    bond_rhs_map
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let error, store_new_result_hb_map =
    Int2Map_HalfBreak_effect.Map.fold
      (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
        let error, store_result_map =
          List.fold_left (fun (error, store_result) (rule_id_eff, state) ->
	    let error, store_result =
	      state_compatible
		parameter
		error
		state
		bond_rhs_map
		rule_id_eff
		store_covering_classes_modification_update
		store_contact_map
		store_result
	    in
	    error, store_result
          ) (error, store_result) l2
        in
	error, store_result_map
      ) half_break_map (error, store_result_map)
  in
  error, store_new_result_hb_map

(*side effects in the case of remove, add into update function*)
let store_new_result_remove_map
  parameter
  error
  remove_map
  bond_rhs_map
  store_covering_classes_modification_update
  store_result_map
  =
  let error, store_result =
    Int2Map_Remove_effect.Map.fold
      (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
	let error, store_result_map =
	  List.fold_left (fun (error, store_result) rule_id_eff ->
	    let error, store_result =
	      add_rule_id_to_update_aux
		parameter
		error
		rule_id_eff
		bond_rhs_map
		store_covering_classes_modification_update
		store_result
	    in
	    error, store_result
	  ) (error, store_result) l2
	in
	error, store_result_map
      ) remove_map (error, store_result_map)
  in
  error, store_result

(* add side effects into update function*)
let store_new_result_map
    parameter
    error
    remove_map
    half_break_map
    bond_rhs_map
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, rule_id_set) store_result
    in
    error, result
  in
  let error, store_result_hb_map =
    store_new_result_hb_map
      parameter
      error
      half_break_map
      bond_rhs_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  let error, store_result_remove_map =
    store_new_result_remove_map
      parameter
      error
      remove_map
      bond_rhs_map
      store_covering_classes_modification_update
      Int2Map_CV_Modif.Map.empty
  in
  let error, store_result_map =
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
      store_result_hb_map
      store_result_remove_map
      store_result_map
  in
  error, store_result_map

(*add side effects into update function*)  
let add_rule_id_to_update
    parameter
    error 
    store_contact_map
    bond_rhs_map
    store_side_effects
    store_covering_classes_modification_update
    store_result_map
    =
  let (half_break_map, remove_map) = store_side_effects in
  let error, store_result =
    store_new_result_map
      parameter
      error
      remove_map
      half_break_map
      bond_rhs_map
      store_covering_classes_modification_update
      store_contact_map
      store_result_map
  in
  error, store_result
  
(*write a function add update(c) into working list*)

let add_update_to_wl parameter error store_covering_classes_modification_update wl =
  Int2Map_CV_Modif.Map.fold
    (fun (agent_type, cv_id) (l1, s1) (error, store_wl) ->
      let error, result =
        Site_map_and_set.Set.fold (fun rule_id (error, wl) ->
          let error, wl = IntWL.push parameter error rule_id wl
          in
          error, wl
        ) s1 (error, store_wl)
      in
      error, result
    ) store_covering_classes_modification_update (error, wl)

(*write a function add update(c) with side effects into working list, 
  if we discovered a new bond*)
    
(*let add_update_bond_to_wl parameter error
    is_new_bond
    store_new_result_map
    store_covering_classes_modification_update
    wl
    =
  if is_new_bond
  then
  (*add side_effect with new update(c) into wl*)
    let error, new_wl =
      add_update_to_wl
	parameter
	error
	store_new_result_map
	wl
    in
    error, new_wl
  else
    (*if it is not a new bond then return add update (c) into wl*)
    let error, new_wl =
      add_update_to_wl
	parameter
	error
	store_covering_classes_modification_udpate
	wl
    in
    error, new_wl*)

(************************************************************************************)
(*fixpoint*)

(*from rule_id get the bdu_creation_map, bdu_test_map, and modif_list_map*)

let collect_bdu_creation_and_modif_list
    parameter
    bdu_false 
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

(*check is_enable of all *)
let is_enable 
    parameter
    handler
    error
    bdu_test_map
    bdu_true
    bdu_false
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
          Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
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
  error, is_enable

(*fixpoint iteration*)

let collect_bdu_update_map parameter handler error 
    rule 
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_bdu_test_restriction_map
    is_new_bond
      store_test_has_bond_rhs
      store_new_result_map
    store_covering_classes_modification_update
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error 
  in 
  let error, handler, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error 
  in
  (*-----------------------------------------------------------------------*)
  let add_link handler (agent_type, cv_id) bdu_update store_result =
    let error, bdu_old =
      match
	Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with
        Some old -> error, old
      | None -> error, bdu_false (*initial state of bdu_X is bdu_false*)
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_update bdu_old
    in
    (*-----------------------------------------------------------------------*)
    (*Check if a view is seen for the first time by testing if bdu_union = bdu_old*)
    if Mvbdu_wrapper.Mvbdu.equal bdu_union bdu_old
    then
      (*it is not seen for the first time*)
      error, handler, false, store_result
    else
      (*-----------------------------------------------------------------------*)
      (*it is seend for the first them then add bdu_update*)
      let result_map =
        Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
      in
      error, handler, true, result_map
  in
  (*-----------------------------------------------------------------------*)
  (*iterate function over a working list*)
  let rec aux acc_wl (error, handler, store_bdu_update_map) =
    if IntWL.is_empty acc_wl
    then
      error, (handler, store_bdu_update_map)
    else
      (*-----------------------------------------------------------------------*)
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) =
        IntWL.pop parameter error acc_wl
      in
      match rule_id_op with
      | None -> error, (handler, store_bdu_update_map) (* Put a warning in error *)
      | Some rule_id ->
        (*-----------------------------------------------------------------------*)
        (*compute bdu_creation, bdu_test and modif_list for this rule_id*)
        let error, (bdu_creation_map, modif_list_map, bdu_test_map) =
          collect_bdu_creation_and_modif_list
            parameter
            bdu_false
            error
            rule_id
            store_proj_bdu_creation_restriction_map
            store_proj_modif_list_restriction_map
            store_proj_bdu_test_restriction_map
        in
        (*-----------------------------------------------------------------------*)
        (*is for all bdu_test sastify a covering_class?*)
        let error, is_enable = 
          is_enable
            parameter
            handler
            error
            bdu_test_map
            bdu_true
            bdu_false
            store_bdu_test_restriction_map
            store_bdu_update_map
        in
        (*-----------------------------------------------------------------------*)
        if is_enable
        then
          (*do a fold inside bdu_test of the covering_class and then do the update*)
          let error, (handler, new_wl, store_new_result) =
            Map_test_bdu.Map.fold
              (fun (agent_id, agent_type, rule_id', cv_id) _ 
              (error, (handler, store_wl, store_update_map)) ->
                let error, bdu_test =
                  match
                    Map_agent_id_test_bdu.Map.find_option agent_id bdu_test_map
                  with
                  | None -> error, bdu_true (*bdu_test is empty*)
                  | Some bdu -> error, bdu
                in
                (*-----------------------------------------------------------------------*)
                (*search bdu_X inside a store_result_map*)
                let error, bdu_X =
                  match Map_bdu_update.Map.find_option (agent_type, cv_id)
                    store_update_map
                  with
                  | None -> error, bdu_false (*the initial state is bdu_false*)
                  | Some bdu -> error, bdu
                in
                (*-----------------------------------------------------------------------*)
                (*TODO:do the fold later over agent_type not agent_id*)
                (*get bdu_creation from bdu_creation_map*)
                let error, bdu_creation =
                  match
                    Map_agent_type_creation_bdu.Map.find_option agent_type
                      bdu_creation_map
                  with
                  | None -> error, bdu_false
                  | Some bdu -> error, bdu
                in
                (*-----------------------------------------------------------------------*)
                (*get modif_list of agent_id from modif_list_map*)
                let error, modif_list =
                  match
                    Map_agent_id_modif_list.Map.find_option agent_id
                      modif_list_map
                  with
                  | None -> error, []
                  | Some l -> error, l
                in
                (*build_list from modif_list*)
                let error, handler, list_a =
                  Mvbdu_wrapper.Mvbdu.build_list
                    parameter
                    handler
                    error
                    modif_list
                in
                (*-----------------------------------------------------------------------*)
                let error, handler, bdu_update =
                  compute_bdu_update
                    parameter
                    handler
                    error
                    bdu_test
                    list_a
                    bdu_creation (*TODO:do the fold later over agent_type not agent_id*)
                    bdu_X
                in
                (*-----------------------------------------------------------------------*)
                let error, handler, is_new_view, store_result =
                  add_link handler (agent_type, cv_id) bdu_update store_update_map
                in
                (*-----------------------------------------------------------------------*)
                begin
                  if is_new_view
                  then
		   (*is a new bond discovered?*)
                    begin
                      if is_new_bond
                      then
                        (*add update(c') into wl_tl; side_effect*)
			let error, new_wl =
			  add_update_to_wl
			    parameter
			    error
			    store_new_result_map
			    wl_tl
			in
			error, (handler, new_wl, store_result)
                      else
			(*add update(c) into wl_tl*)
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
                    error, (handler, store_wl, store_result)
                end
              ) store_bdu_test_restriction_map 
              (error, (handler, wl_tl, store_bdu_update_map))
          in
          (*-----------------------------------------------------------------------*)
          (*iterate with new function*)
          aux new_wl (error, handler, store_new_result)
        else
          aux wl_tl (error, handler, store_bdu_update_map)
  in
  aux wl_creation (error, handler, Map_bdu_update.Map.empty)
