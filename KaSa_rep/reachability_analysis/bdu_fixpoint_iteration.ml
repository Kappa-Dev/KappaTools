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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

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
