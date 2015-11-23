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
open Bdu_build_common
open Bdu_structure
open Fifo
open Printf

(************************************************************************************)
(*is enable rule*)

let is_belong bdu bdu_init = true (* to fill *) 
 (* let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then true
  else false*)
    
let comp_is_enable parameter error handler bdu_test bdu_X =
  let error,handler, bdu_false =
    f parameter
      (boolean_mvbdu_false parameter handler error) parameter
  in 
  let error, handler, bdu_result =
    f parameter 
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_X
  in
  if not (is_belong bdu_X bdu_false)
  then
    error, true
  else
    error, false

(************************************************************************************)
(*update bdu:
  - (bdu_X U bdu_creation) U [\rho[update_views] | \rho \in bdu_X (inter) bdu_test views]
*)

let compute_update parameter error handler bdu_test list_a bdu_creation bdu_X =
  (*union bdu_X with bdu_creation*)
  let error, handler, bdu_X1 = 
    f parameter 
      (boolean_mvbdu_or parameter handler error parameter bdu_X) bdu_creation
  in
  (*intersection of X and bdu_test*)
  let error, handler, bdu_inter_test_X =
    f parameter 
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_X
  in
  (*redefine with a list of modification*)
  let error, handler, bdu_assigment =
    f parameter 
      (redefine parameter error parameter handler bdu_inter_test_X) list_a
  in
  (*union with bdu_X*)
  let error, handler, bdu_update =
    f parameter 
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment) bdu_X1
  in
  error, bdu_update

(************************************************************************************)
(*a bond is discovered for the first time*)

(*rule that has the views that are tested and bond: if one can apply this
  rule, then add rule of side effects into the update(c), and then add
  update(c') to the working list.*)
 
(*let collect_rhs_bond parameter error rule_id rule store_result =
  List.fold_left (fun (error, set) (site_add1, site_add2) ->
    if Map_site_address.Set.mem (site_add1, site_add2) set
    then error, set
    else
      error, Map_site_address.Set.add (site_add1, site_add2) set
  ) (error, store_result) rule.actions.bind*)

let store_test_has_bond_rhs parameter error rule_id rule store_result =
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_add_map store_result ->
      match agent with
      | Ghost -> error, store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        let error, set =
          Site_map_and_set.Map.fold
            (fun site site_add2 (error, set) ->
              let site_add1 = Cckappa_sig.build_address agent_id agent_type site in
              if Map_site_address.Set.mem (site_add1, site_add2) set
              then error, set
              else
                error, Map_site_address.Set.add (site_add1, site_add2) set
            ) site_add_map (error, Map_site_address.Set.empty)
        in
        (*get old*)
        let error, old_set =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, Map_site_address.Set.empty
          | error, Some s -> error, s
        in
        let new_set = Map_site_address.Set.union set old_set in
        (*store*)
        let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type
            new_set
            store_result
        in
        error, store_result
    ) rule.rule_lhs.views rule.rule_rhs.bonds store_result


(*rule that has the views that are tested: if one can apply this rule, then
  add update(c), to the working list*)


(************************************************************************************)
(*fixpoint*)

let collect_bdu_update_map parameter handler error rule wl_creation 
    store_remanent_test
    store_test_bdu_map
    store_remanent_creation
    store_creation_bdu_map
    store_remanent_modif
    store_modif_list_map
    store_result
    =
    let error,handler, bdu_false =
      f parameter
	(boolean_mvbdu_false parameter handler error) parameter
    in
    (*  let error, (handler, bdu_init) = bdu_init parameter error in*)
    let add_link (agent_type, cv_id) bdu_update store_result =
    let (l, old) =
      match
	Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with Some (l,old) -> l,old
	 | None -> [],[]
    in
    let result_map = (*FIXME: list or bdu_init ? *)
      Map_bdu_update.Map.add (agent_type, cv_id) (l, bdu_update :: []) 
        store_result
    in
    error, result_map
  in  
  (*iterate function over working list*)
  let rec aux acc_wl bdu_X (error, store_bdu_update_map) =
    if IntWL.is_empty acc_wl
    then
      error, store_bdu_update_map
    else
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) =
        IntWL.pop parameter error acc_wl
      in
      match rule_id_op with
      | None -> error, store_bdu_update_map
      | Some rule_id ->
        (*--------------------------------------------------------------------*)
        (*get the local view that is tested for this rule_id with new indexes*)
        let error, (agent_type_test, cv_id_test, bdu_test) =
          (*take a global view *)
          AgentMap.fold parameter error
            (fun parameter error agent_id agent (ag, id, store_bdu_result) ->
              match agent with
              | Ghost -> error, (ag, id, store_bdu_result)
              | Agent agent ->
                let agent_type = agent.agent_name in
                (*TEST*)
                
                (*get the local view: (agent_id, rule_id, cv_id, pair_list)list *)
                let error, get_test_list =
                  match
                    AgentMap.unsafe_get parameter error agent_type store_remanent_test 
                  with
                  | error, None -> error, []
                  | error, Some l -> error, l
                in
                let error, (cv_id, bdu_test) =
                  List.fold_left (fun (error, (cv_id, bdu_result))
                    (agent_id, rule_id', cv_id, _) ->
                      begin
                        if rule_id = rule_id'
                        then
                          let (l, bdu_test) =
                            Map_test_bdu.Map.find_default ([],bdu_false)
			      (agent_id, agent_type, rule_id, cv_id) store_test_bdu_map
			  in
                          error, (cv_id,bdu_test)
                        else
                          error, (cv_id, bdu_result)
                      end
		  )
		    (error, (id, store_bdu_result))
		    get_test_list
                in		
                error, (agent_type, cv_id, bdu_test)
            ) rule.rule_lhs.views (0, 0, bdu_false)
        in
        (*--------------------------------------------------------------------*)
        (*get the local view that is created for this rule_id with new_indexes*)
        let error, bdu_creation =
          (*take a global view*)
          List.fold_left (fun (error, store_bdu_result) (agent_id, agent_type) ->
            let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
            match agent with
            | None -> warn parameter error (Some "line 163") Exit store_bdu_result
            | Some Ghost -> error, store_bdu_result
            | Some Agent agent ->
              (*get the local view (rule_id, cv_id, pair_list) list*)
              let error, get_creation_list =
                match
                  AgentMap.unsafe_get parameter error agent_type store_remanent_creation
                with
                | error, None -> error, []
                | error, Some l -> error, l
                  
              in
              let error, bdu_creation =
                List.fold_left (fun (error, store_result) (rule_id', cv_id, _) ->
                  begin
                    if rule_id = rule_id'
                    then 
                      let (l, bdu_creation) =
			match
			  Map_creation_bdu.Map.find_option 
                            (agent_type, rule_id, cv_id) store_creation_bdu_map
			with
			  None -> failwith "3 bdu fixpoint iteration"
			| Some (a,b) -> a,b
                      in
                      error, bdu_creation
                    else
                      error, store_result
                  end
                ) (error, store_bdu_result) get_creation_list
              in
              error, bdu_creation
	  )
	    (error, bdu_false)
	    rule.actions.creation
        in
        (*--------------------------------------------------------------------*)
        (*get the local update view due to modification for this rule_id
          with new indexes*)
        let error, modif_list =
          (*take a global view*)
          AgentMap.fold parameter error
	    (fun parameter error agent_id agent_modif store_result ->
	      begin
		if Site_map_and_set.Map.is_empty agent_modif.agent_interface
		then error, store_result
		else               
		  let agent_type = agent_modif.agent_name in
		  (*get the local update view (agent_id, rule_id, cv_id, pair_list) list*)
		  let error, get_modif_list =
		    match
		      AgentMap.unsafe_get parameter error agent_type store_remanent_modif
		    with
		    | error, None -> error, []
		    | error, Some l -> error, l
		  in
		  let error, modif_list =
		    List.fold_left (fun (error, store_result)
		      (agent_id, rule_id', cv_id, _) ->
			begin
			  if rule_id = rule_id'
			  then
			    let (l, modif_list) =
			      Map_modif_list.Map.find_default ([], [])
				(agent_id, agent_type, rule_id, cv_id) store_modif_list_map
			    in
			    error, modif_list
			  else
			    error, store_result
			end
		    ) (error, store_result) get_modif_list
		  in
		  error, modif_list
	      end
	    ) rule.diff_direct []
        in
        let error, (handler, list_a) =
          List_algebra.build_list
            (Boolean_mvbdu.list_allocate parameter)
            error
            parameter
            handler
            modif_list
        in
        (*--------------------------------------------------------------------*)
        (*test the enable rule of the view that is tested with bdu_X*)	
	let error, is_enable =
          comp_is_enable
            parameter
            error
            handler
	    bdu_test 	   
            bdu_X
        in
        begin
          if is_enable
          then
            (*--------------------------------------------------------------------*)
            (*discover a new view, update bdu for this view*)
            let error, bdu_update =
              compute_update
                parameter
                error
                handler
		bdu_test
		list_a
                bdu_creation
                bdu_X
            in
            (*do step 3: add update(c) in the working list*)
            

            
            (*-----------------------------------------------------------------------*)
            (*store bdu_update inside map of the views that is tested (per agent, cv)*)
            let error, store_new_bdu_update_map =
              add_link (agent_type_test, cv_id_test) bdu_update store_bdu_update_map
            in
            (*--------------------------------------------------------------------*)
            (*TODO: continue in the new working list when added update_function and/or
              discover binding side.
              continue with the tail of working list with this update*)
            aux wl_tl bdu_update (error, store_new_bdu_update_map)
          (*--------------------------------------------------------------------*)
          else
            (*continue with the result*)
            aux wl_tl bdu_X (error, store_bdu_update_map)
        end
  in
  aux wl_creation bdu_false (error, store_result) (*start from bdu_init*)
