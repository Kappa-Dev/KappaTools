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
open Mvbdu_wrapper

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn (fun () -> default) 

let local_trace = false

(************************************************************************************)
(*it is an enable rule when the intersection between bdu_test and
  bdu_remanent is different than empty set*)

let is_bdu_test_enable parameter handler error bdu_false bdu_test bdu_X =
  (*if bdu_test is empty*)
  if Mvbdu_wrapper.Mvbdu.equal bdu_test bdu_false
  then
    (*then it is an enable rule*)
    error, true
  else
    (*if bdu_test is not empty, then do the intersection with bdu_X*)
    begin
      let error, handler, bdu_inter =
        Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
      in
      (*then test the enable of this result*)
      if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
      then 
        (*if it is empty then it is not enable*)
        error, false
      else
        error, true        
    end  

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

(*the final update *)
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
  (*do the union with bdu_creation*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_Xn bdu_creation
  in
  error, handler, bdu_result

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
  let add_link rule_id set store_result =
    let (l, old) =
      match
	Map_test_bond.Map.find_option rule_id store_result
      with Some (l,old) -> l, old
      | None -> [], Map_site_address.Set.empty
    in
    let result_map = 
      Map_test_bond.Map.add rule_id (l, set) store_result
    in
    error, result_map
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_add_map store_result ->
      match agent with
      | Ghost -> error, store_result
      | Dead_agent _ ->
	warn parameter error (Some "line 156, rhs should not have dead agents") Exit store_result
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
        let error, store_result =
          add_link rule_id set store_result
        in
        error, store_result
    ) rule.rule_lhs.views rule.rule_rhs.bonds store_result

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


(************************************************************************************)
(*fixpoint*)

(*Testing*)
open Print_bdu_build_map
open Remanent_parameters_sig

let print_list rule_id l = 
  let rec aux acc =
    match acc with
    | [] -> []
    | (site, state) :: tl -> 
      fprintf stdout "rule_id:%i:site:%i:state:%i\n" rule_id site state; aux tl
  in
  aux l

let print_set parameter set =
  if Map_site_address.Set.is_empty set
  then fprintf stdout "empty\n"
  else
  Map_site_address.Set.iter (fun (site_add1, site_add2) ->
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} -- "
      site_add1.Cckappa_sig.agent_index site_add1.Cckappa_sig.agent_type 
      site_add1.Cckappa_sig.site;
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} \n"
      site_add2.Cckappa_sig.agent_index site_add2.Cckappa_sig.agent_type 
      site_add2.Cckappa_sig.site
  ) set

let collect_bdu_update_map parameter handler error 
    rule 
    wl_creation
    store_remanent_triple
    store_test_bdu_map
    store_creation_bdu_map
    store_modif_list_map
    store_test_has_bond_rhs
    store_covering_classes_modification_update
    store_result
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error 
  in 
  let add_link (agent_type, cv_id) bdu_update store_result =
    let (l, old) =
      match
	Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with Some (l,old) -> l,old
      | None -> [],[]
    in
    let result_map = (*FIXME: list or bdu_init ? *)
      Map_bdu_update.Map.add (agent_type, cv_id) (l, bdu_update :: old) 
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
        (*get bdu of local view that is created for this rule_id with new_indexes*)
        let error, bdu_creation =
          error, bdu_false
        in
        (*let error, (cv_id_creation, bdu_creation) =
          (*take a global view*)
          List.fold_left (fun (error, (cv_id, store_bdu_result))
            (agent_id, agent_type) ->
            let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
            match agent with
          | Some Dead_agent _ 
            | None -> warn parameter error (Some "line 163") Exit
              (cv_id, store_bdu_result)
            | Some Ghost -> error, (cv_id, store_bdu_result)
            | Some Agent agent ->
              (*covering classes*)
              let error, triple_list =
                match 
                  AgentMap.unsafe_get parameter error agent_type store_remanent_triple
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let error, (cv_id, bdu_creation) =
                List.fold_left (fun _ (cv_id, _, _) ->
                  let (l, bdu_creation) =
		    match
		      Map_creation_bdu.Map.find_option
                        (agent_type, rule_id, cv_id) store_creation_bdu_map
		    with
		      None -> ([], bdu_false) (*this is a default value*)
		    | Some (a, b) -> a, b
                  in
                  error, (cv_id, bdu_creation)
                ) (error, (cv_id, store_bdu_result)) triple_list
              in
              error, (cv_id, bdu_creation)
          ) (error, (0, bdu_false)) rule.actions.creation
        in*)
        (*--------------------------------------------------------------------*)
        (*get the local update view due to modification for this rule_id
          with new indexes*)
        let error, modif_list =
          (*take a global view*)
          AgentMap.fold2_common parameter error
	    (fun parameter error agent_id agent_modif triple_list store_result ->
	      begin
		if Site_map_and_set.Map.is_empty agent_modif.agent_interface
		then error, store_result
		else               
		  let agent_type = agent_modif.agent_name in
                  let error, modif_list =
                    List.fold_left (fun _ (cv_id, _, _) ->
                      let (l, modif_list) =
			Map_modif_list.Map.find_default ([], [])
			  (agent_id, agent_type, rule_id, cv_id) store_modif_list_map
		      in
		      error, modif_list
                    ) (error, store_result) triple_list
                  in
                  error, modif_list
              end
	    ) rule.diff_direct store_remanent_triple []
        in
        let error, handler, list_a =
          Mvbdu_wrapper.Mvbdu.build_list
            parameter
            handler
            error
            modif_list
        in
        (*--------------------------------------------------------------------*)
        (*get the local view that is tested for this rule_id with new indexes*)
        let error, store_result_bdu_update_map =
          (*take a global view *)
          AgentMap.fold parameter error
            (fun parameter error agent_id agent store_bdu_result ->
              match agent with
              | Ghost -> error, store_bdu_result
              | Dead_agent (agent,_,_) ->
                begin
                   (*continue with the tail of working list and the result*)
                  aux wl_tl bdu_X (error, store_bdu_result)
                end
              | Agent agent ->
                let agent_type = agent.agent_name in
                (*--------------------------------------------------------------------*)
                (*covering classes*)
                let error, triple_list =
                  match 
                    AgentMap.unsafe_get parameter error agent_type store_remanent_triple
                  with
                  | error, None -> error, []
                  | error, Some l -> error, l
                in
                let error, (cv_id, bdu_test) =
                  List.fold_left (fun _ (cv_id, _, _) ->
                    let (l, bdu_test) =
                      Map_test_bdu.Map.find_default ([], bdu_false)
		        (agent_id, agent_type, rule_id, cv_id) store_test_bdu_map
		    in
                    error, (cv_id, bdu_test)
                  ) (error, (0, bdu_false)) triple_list
                in
                (*--------------------------------------------------------------------*)
                (*TODO:get a set of binding site_address of agent_type*)
                let error, result_bdu_update_map =
                  (*TODO:search (agent_type, rule_id) in the set of bond_rhs*)
                  let (l, bond_rhs_set) =
                    Map_test_bond.Map.find_default ([], Map_site_address.Set.empty)
                      rule_id store_test_has_bond_rhs
                  in
                  (*first check the enable condition of this rule*)
                  let error, is_enable =
                    is_bdu_test_enable
                      parameter
                      handler
                      error
                      bdu_false
                      bdu_test
                      bdu_X
                  in
                  aux wl_tl bdu_X (error, store_bdu_result)
                  (*begin
                    if is_enable
                    then
                      (*let _ = fprintf stdout "YES\n" in*)
                      (*check if the set of bond on the rhs is empty?
                        if it is empty, there is no bond on rhs else
                        discover a bond on the rhs.*)
                      begin
                        if Map_site_address.Set.is_empty bond_rhs_set
                        then
                          (*compute bdu_update*)
                          let error, handler, bdu_update =
                            compute_bdu_update
                              parameter
                              handler
                              error
                              bdu_test
                              list_a
                              bdu_creation
                              bdu_X
                          in
                          (*store bdu_udpate into result*)
                          let error, store_new_bdu_update_map =
                            add_link (agent_type, cv_id) bdu_update store_bdu_result
                          in
                          (*add update(c) into the tail of working list*)
                          let error, new_wl_tl_update =
                            add_update_to_wl
                              parameter
                              error
                              store_covering_classes_modification_update
                              wl_tl
                          in
                          aux new_wl_tl_update bdu_update (error, store_new_bdu_update_map)
                        else
                          (*discover a bond on the rhs*)
                          (*compute bdu_update*)
                          let error, handler, bdu_update =
                            compute_bdu_update
                              parameter
                              handler
                              error
                              bdu_test
                              list_a
                              bdu_creation
                              bdu_X
                          in
                          (*store bdu_update into result*)
                          let error, store_new_bdu_update_map =
                            add_link (agent_type, cv_id) bdu_update store_bdu_result
                          in
                          (*TODO:add rule that has side effect into update(c)*)
                          (*let new_wl_tl_update_side_effect =
                            in
                          aux new_wl_tl_update_side_effect bdu_update
                            (error, store_bdu_result)*)
                          aux wl_tl bdu_update (error, store_new_bdu_update_map)
                      end
                    else
                      (*continue with the tail of working list and the result*)
                      aux wl_tl bdu_X (error, store_bdu_result)
                  end*)
                in
               	error, result_bdu_update_map                
            ) rule.rule_lhs.views store_bdu_update_map
        in
        error, store_result_bdu_update_map
  in
  aux wl_creation bdu_false (error, store_result)
