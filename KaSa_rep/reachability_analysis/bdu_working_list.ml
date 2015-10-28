(**
  * bdu_working_list.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Fifo
open Bdu_analysis_type
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(************************************************************************************)    
(*added a list of rule_id in [creation action ++ update function] into working list.*)

(*adding rule_id of [update function] inside working list*)
    
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

(*-------------------------------------------------------------------------*)
(*adding rule_id of [creation rule] inside working list.*)
    
let collect_wl_creation parameter error rule_id viewsrhs creation store_result =
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
    
(*-------------------------------------------------------------------------*)
(*adding rule_id of [update function and creation] inside a working list*)

let collect_wl_creation_update parameter error store_wl_creation store_wl_update =
  let error, init = AgentMap.create parameter error 0 in
  AgentMap.fold parameter error
    (fun parameter error agent_type wl_update store_result ->
      (*get wl of creation rule.*)
      let error, wl_creation =
	match AgentMap.unsafe_get parameter error agent_type store_wl_creation with
	  | error, None -> error, IntWL.empty
	  | error, Some wl -> error, wl
      in
      (*combine wl of creation and wl of update together*)
      let (in_list_update, out_list_update, set_update) = wl_update in
      let (in_list_creation, out_list_creation, set_creation) = wl_creation in
      let error, new_set =
	IntWL.WSet.union parameter error set_update set_creation
      in
      let new_wl =
	List.concat [in_list_creation; in_list_update],
	List.concat [out_list_creation; out_list_update],
	new_set
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
    ) store_wl_update init
  
(************************************************************************************)
(*combine working list into one function, store as an 
  (rule_id, rule_id_creation,...) wl *)

(*let collect_rule_id_working_list parameter error store_update rule_id viewsrhs creation
    store_result =
  let _, store_result_creation, _ = store_result in
  let error, store_wl_update =
    collect_wl_update
      parameter
      error
      store_update
  in
  let error, store_wl_creation =
    collect_wl_creation
      parameter
      error
      rule_id
      viewsrhs
      creation
      store_result_creation
  in
  let error, store_wl_creation_update =
    collect_wl_creation_update
      parameter
      error
      store_wl_creation
      store_wl_update
  in
  error, (store_wl_update, store_wl_creation, store_wl_creation_update)*)
