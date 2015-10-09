(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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
  let error, (bdu_handler, bdu_init) = bdu_init parameter in
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
(*added a list of rule_id in an update function into working list.*)

let collect_wl_rule_id_update parameter error handler rule store_update store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type set _ ->
      (*-------------------------------------------------------------------------*)
      (*put rule_id into a working list*)
      let wl = IntWL.empty in
      let error, wl =
	Site_map_and_set.fold_set (fun rule_id (error, wl) ->
	  let error, wl =
	    IntWL.push parameter error rule_id wl
	  in
	  error, wl
	) set (error, wl)
      in
      (*-------------------------------------------------------------------------*)
      (*from wl contain rule_id, create an array, then from rule_id get rule_type*)
      let nrules = Handler.nrules parameter error handler in
      let error, empty_rule = Preprocess.empty_rule parameter error in
      let rule_array = Array.make nrules empty_rule in
      let rule_array =
	IntWL.fold_left (fun rule_array rule_id' ->
	  let rule_array =
	    rule_array.(rule_id') <- rule;
	    rule_array
	  in
	  rule_array
	) rule_array wl
      in
      (*-------------------------------------------------------------------------*)
      (*FIXME: REMOVE? get old*)
      let error, (old_wl, old_array) =
	match AgentMap.unsafe_get parameter error agent_type store_result with
	  | error, None -> error, (IntWL.empty, [||])
	  | error, Some (wl, array) -> error, (wl, array)
      in
      (*new wl*)
      let in_list, out_list, set = wl in
      let old_in_list, old_out_list, old_set = old_wl in
      let error, new_set =
	IntWL.WSet.union parameter error set old_set
      in
      (*-------------------------------------------------------------------------*)
      (*FIXME: REMOVE? get new*)
      let new_wl =
	List.concat [in_list; old_in_list], List.concat [out_list; old_out_list], new_set
      in
      (*FIXME: REMOVE? new array*)
      let new_array = Array.append rule_array old_array in
      (*-------------------------------------------------------------------------*)
      (*store*)
      let error, store_result =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (wl, new_array) (*FIXME: array or new_array?*)
	  store_result
      in
      error, store_result
    ) store_update store_result
