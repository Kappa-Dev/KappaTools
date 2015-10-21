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
open Fifo
open Bdu_side_effects
open Set_and_map
open Bdu_build
open Bdu_creation

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(************************************************************************************)
(*build bdu for test (rule on the lhs) and modified sites list*)

let build_bdu_test_list_direct parameter error rule_lhs_views rule_diff_direct
    store_result =
  let error, (bdu_handler, bdu_init) = bdu_init parameter error in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
      match agent with
	| Ghost -> error, store_result
	| Agent agent ->
	  (*if there is no modified site then return the result*)
	  if Site_map_and_set.is_empty_map site_modif.agent_interface
	  then
	    error, store_result
	  else
	    let agent_type = agent.agent_name in
	    (*build a list of pair (site, state); build bdu from this pair*)
	    let site_list, (bdu_handler, bdu_test) =
	      Site_map_and_set.fold_map
		(fun site port (current_list, _) ->
		  let state = int_of_port port in
		  let l = (site, state) :: current_list in
		  let error, (bdu_handler, bdu) =
		    build_bdu parameter error l
		  in
		  l, (bdu_handler, bdu)
		) agent.agent_interface ([], (bdu_handler, bdu_init))
	    in
	    (*build modif list TODO: agent_type of site_modif*)
	    let modif_list =
	      Site_map_and_set.fold_map
		(fun site port current_list ->
		  let state = int_of_port port in
		  (site, state) :: current_list
		) site_modif.agent_interface []
	    in
	    (*REMARK: get these information at each rule and not a set of rule, because 
	      will fold inside an array of rule*)
	    let error, store_result =
	      AgentMap.set
		parameter
		error
		agent_type
		(site_list, bdu_test, modif_list)
		store_result
	    in
	    error, store_result
	    (*get old*)
	    (*let error, (bdu_test, modif_list) =
	      match AgentMap.unsafe_get parameter error agent_type store_result with
		| error, None -> error, (bdu_init, [])
		| error, Some (bdu_test, modif_list) -> error, (bdu_test, modif_list)
	    in
	    (*get new*)
	    let
	    in
	    (*store*)
	    let
	    in*)
    ) rule_lhs_views rule_diff_direct store_result

(************************************************************************************)    
(*added a list of rule_id in [creation action ++ update function] into working list.*)

(*this function used for testing rule_id inside update function when store inside 
  working list*)
    
let collect_wl_update parameter error store_update =
  let error, init = AgentMap.create parameter error 0 in
  let (_, _, _, store_final_update) = store_update in
  Int2Map_CV_Modif.fold_map 
    (fun (agent_type, site_type, cv_id) (l1, s2) (error, store_result) ->
      (*-------------------------------------------------------------------------*)
      (*put rule_id into a working list*)
      let wl = IntWL.empty in
      let error, wl =
        Site_map_and_set.fold_set (fun rule_id (error, wl) ->
          let error, wl =
            IntWL.push parameter error rule_id wl
          in
          error, wl      
        ) s2 (error, wl)
      in
      (*old*)
      let error, old_wl =
	match AgentMap.unsafe_get parameter error agent_type store_result with
	  | error, None -> error, IntWL.empty
	  | error, Some wl -> error, wl
      in
      (*new wl*)
      let in_list, out_list, set = wl in
      let old_in_list, old_out_list, old_set = old_wl in
      let error, new_set =
	IntWL.WSet.union parameter error set old_set
      in
      let new_wl =
	List.concat [in_list; old_in_list],
	List.concat [out_list; old_out_list], new_set
      in
      (*store*)
      let error, store_result =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  new_wl
	  store_result
      in
      error, store_result
    ) store_final_update (error, init)

(*test creation rule working list only.*)
let collect_wl_creation parameter error rule_id viewsrhs creation
    store_result =
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
      | None -> warn parameter error (Some "line 162") Exit store_result
      | Some Ghost -> error, store_result
      | Some Agent agent ->
	(*creation a working list*)
	let wl = IntWL.empty in
	(*push rule_id of creation into this working list first*)
	let error, wl_creation = IntWL.push parameter error rule_id wl in
	(*store this working list associate with its agent_type*)
	let error, old_wl =
	  match AgentMap.unsafe_get parameter error agent_type store_result with
	    | error, None -> error, IntWL.empty
	    | error, Some wl -> error, wl
	in
	let in_list, out_list, set_wl = wl_creation in
	let old_in_list, old_out_list, old_set_wl = old_wl in
	let error, new_set_wl =
	  IntWL.WSet.union parameter error set_wl old_set_wl
	in
	let new_wl =
	  List.concat [in_list; old_in_list],
	  List.concat [out_list; old_out_list], new_set_wl
	in
	(*store*)
	let error, store_result =
	  AgentMap.set
	    parameter
	    error
	    agent_type
	    new_wl
	    store_result
	in
	error, store_result
  ) (error, store_result) creation

(*test update function with only working list combine with wl in creation*)
let collect_wl_creation_update parameter error store_wl_creation store_update =
  let error, init = AgentMap.create parameter error 0 in
  let (_, _, _, store_final_update) = store_update in
  Int2Map_CV_Modif.fold_map
    (fun (agent_type, site_type, cv_id) (l1, s2) (error, store_result) ->
    (*get wl in creation*)
      let error, wl_creation =
	match AgentMap.unsafe_get parameter error agent_type store_wl_creation with
	  | error, None -> error, IntWL.empty
	  | error, Some wl -> error, wl
      in
      (*push rule_id in update into wl creation*)
      let error, wl_update_creation =
	Site_map_and_set.fold_set (fun rule_id (error, wl) ->
	  let error, wl =
	    IntWL.push parameter error rule_id wl
	  in
	  (error, wl)
	) s2 (error, wl_creation)
      in
      (*get old*)
      let error, old_wl =
	match AgentMap.unsafe_get parameter error agent_type store_result with
	  | error, None -> error, IntWL.empty
	  | error, Some wl -> error, wl
      in
      (*new*)
      let in_list, out_list, set_wl = wl_update_creation in
      let old_in_list, old_out_list, old_set_wl = old_wl in
      let error, new_set_wl =
	IntWL.WSet.union parameter error set_wl old_set_wl
      in
      let new_wl =
	List.concat [in_list; old_in_list],
	List.concat [out_list; old_out_list], new_set_wl
      in
      (*store*)
      let error, store_result =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  new_wl
	  store_result
      in
      error, store_result
    ) store_final_update (error, init)
