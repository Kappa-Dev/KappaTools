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
open Bdu_build
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)  

let trace = false

(************************************************************************************)
(*build bdu for test (rule on the lhs) and modified sites list*)

(*let build_bdu_test_list_direct parameter error rule_lhs_views rule_diff_direct
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
    ) rule_lhs_views rule_diff_direct store_result*)

(************************************************************************************)    
(*added a list of rule_id in [creation action ++ update function] into working list.*)

(*adding rule_id of update function inside working list*)
    
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
(*adding rule_id of creation rule inside working list.*)
    
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
(*adding rule_id of update function and creation inside a working list*)

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
(*from a list of rule_id inside the final working list, return a 'rule' type for its 
  and store them inside an array *)

let collect_rule_in_wl parameter error handler rule store_wl_creation_update
    store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type wl store_result ->
      (*get number of rules*)
      let nrules = Handler.nrules parameter error handler in
      let error, empty_rule = Preprocess.empty_rule parameter error in
      (*create an empty array content all empty_rule*)
      let rule_array = Array.make nrules empty_rule in
      let rule_array =
	IntWL.fold_left (fun rule_array rule_id ->
	  let rule_array =
	    rule_array.(rule_id) <- rule;
	    rule_array
	  in
	  rule_array
	) rule_array wl
      in
      (*get old?*)
      let error, (old_wl, old_array) =
	match AgentMap.unsafe_get parameter error agent_type store_result with
	  | error, None -> error, (IntWL.empty, [||])
	  | error, Some (wl, a) -> error, (wl, a)
      in
      let new_array = Array.append rule_array old_array in
      (*store*)
      let error, store_result =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (wl, rev_array new_array) (*TEST*)
	  store_result
      in
       error, store_result
    ) store_wl_creation_update store_result

(************************************************************************************)    
(*TODO: fold inside creation action, and used wl 
  in update function to generate bdu_creation array *)

let collect_rule_creation_in_wl parameter error handler rule store_wl_creation_update
    viewsrhs creation store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type' wl_update _ ->
      List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
	let error, agent = AgentMap.get parameter error agent_id viewsrhs in
	match agent with
	  | None -> warn parameter error (Some "line 94") Exit store_result
	  | Some Ghost -> error, store_result
	  | Some Agent agent ->
	    let nrules = Handler.nrules parameter error handler in
	    let error, empty_rule = Preprocess.empty_rule parameter error in
	    let rule_array = Array.make nrules empty_rule in
	    let rule_array =
	      IntWL.fold_left (fun rule_array rule_id ->
		let rule_array =
		  rule_array.(rule_id) <- rule;
		  rule_array
		in
		rule_array
	      ) rule_array wl_update
	    in
	    (*get old*)
	    let error, old_array =
	      match AgentMap.unsafe_get parameter error agent_type store_result with
		| error, None -> error, [||]
		| error, Some a -> error, a
	    in
	    let new_array = Array.append rule_array old_array in
	    let error, store_result =
	      AgentMap.set
		parameter
		error
		agent_type
		new_array
		store_result
	    in
	    error, store_result
      ) (error, store_result) creation
    ) store_wl_creation_update store_result
    
(************************************************************************************)
(*fixpoint iteration function*)
(*from an array of rule, build bdu:{of creation rule; test; list of modification}.
  the idea of iterate function: 
  - if working list is empty then return the bdu_remanent_array;
  - pop the first element inside the working list;
  - get 'rule' type from the rule_array;
  - build bdu_creation; bdu_test, and a list of action;
  - build a list of type List_sig.list for modif_list (list of action);
  - check if it  is an enable rule or not:
    it is an enable rule if the result of intersection of test and init is not empty;
    + if it is an enable rule return the result.
    + if it is not an enable rule: call iterate update function:
      ++ bdu_creation intersection (mvbdu_and) with bdu_test, 
         then use the result above: redefine (redefine function in mvbdu) it
         with the list of modif;
      ++ do the union (mvbdu_or) of bdu_assignment (the intersection above) and bdu_creation;
      ++ store this bdu_update in bdu_array, with the index is rule_id.
*)

(*1. from 'rule' type inside rule_array, build an array of bdu_creation;
  then later fold this array each element to test with bdu_test and modif_list*)

(*build bdu_creation for a rule*)
    
let build_bdu_creation parameter error rule (*store_result*) =
  let error, store_result = AgentMap.create parameter error 0 in
  let error, (handler, bdu_init) = bdu_init parameter error in
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
    match agent with
      | None -> warn parameter error (Some "line 29") Exit store_result
      | Some Ghost -> error, store_result
      | Some Agent agent ->
	let error, (l, (handler, bdu_creation)) =
	  Site_map_and_set.fold_map
	    (fun site port (error, (current_list, _))->
	      let state = int_of_port port in
	      let l = (site, state) :: current_list in
	      let error, (handler, bdu_creation) =
		build_bdu parameter error l
	      in
	      error, (l, (handler, bdu_creation))
	    ) agent.agent_interface (error, ([], (handler, bdu_init)))
	in
	(*store a creation rule*)
	let error, store_result =
	  AgentMap.set
	    parameter
	    error
	    agent_type
	    (handler, bdu_creation)
	    store_result
	in
	error, store_result
  ) (error, store_result) rule.actions.creation

(*build bdu_test and list of modif action of a rule*)
let build_bdu_test_modif_list parameter error rule =
  let error, (handler, bdu_init) = bdu_init parameter error in
  let error, store_result = AgentMap.create parameter error 0 in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
      match agent with
	| Ghost -> error, store_result
	| Agent agent ->
	  let agent_type = agent.agent_name in
	  (*build bdu_test*)
	  let error, (l, (handler, bdu_test)) =
	    Site_map_and_set.fold_map
	      (fun site port (error, (current_list, _)) ->
		let state = int_of_port port in
		let l = (site, state) :: current_list in
		let error, (handler, bdu_test) =
		  build_bdu parameter error l
		in
		error, (l, (handler, bdu_test))
	      ) agent.agent_interface (error, ([], (handler, bdu_init)))
	  in
	  (*build list of modif*)
	  let error, modif_list =
	    Site_map_and_set.fold_map
	      (fun site port (error, current_list) ->
		let state = int_of_port port in
		let l =
		  (site, state) :: current_list in
		error, l
	      ) site_modif.agent_interface (error, [])
	  in
          (*store this pair*)
	  let error, store_result =
	    AgentMap.set
	      parameter
	      error
	      agent_type
	      ((handler, bdu_test), modif_list)
	      store_result
	  in
	  error, store_result
    ) rule.rule_lhs.views rule.diff_direct store_result

(*is enable rule*)
let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then true
  else false
    
let comp_is_enable parameter error handler bdu_init bdu_test bdu_creation =
  let error, handler, bdu_X =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  if not (is_belong bdu_X bdu_init)
  then
    error, true
  else
    error, false

(*update bdu*)
let compute_update parameter error rule_id handler bdu_test modif_list bdu_creation
    bdu_remanent_array =
  let error, handler, bdu_inter_test_creation =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  let error, handler, bdu_assigment =
    f parameter error bdu_inter_test_creation
      (redefine parameter error parameter handler bdu_inter_test_creation)
      modif_list
  in
  let error, handler, bdu_update =
    f parameter error bdu_assigment
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment)
      bdu_creation
  in
  let bdu_remanent_array =
    bdu_remanent_array.(rule_id) <- bdu_update;
    bdu_remanent_array
  in
  error, bdu_remanent_array

(*from this working list get the bdu_array of bdu_creation*)
let collect_bdu_creation_array parameter error handler_sig store_rule_in_wl store_result =
  let error, (handler, bdu_init) = bdu_init parameter error in
  AgentMap.fold parameter error
    (fun parameter error agent_type (wl, rule_array) store_result ->
      let nrules = Handler.nrules parameter error handler_sig in
      let bdu_array = Array.make nrules bdu_init in
      let error, bdu_array =
	let rec aux acc_wl (error, store_result) =
	  if IntWL.is_empty acc_wl
	  then error, store_result
	  else
	  (*pop the first element*)
	    let error, (rule_id_op, wl_tl) =
	      IntWL.pop parameter error acc_wl
	    in
	    match rule_id_op with
	      | None -> error, store_result
	      | Some rule_id ->
		let rule = Array.get rule_array rule_id in
		let error, store_bdu_creation =
		  build_bdu_test_modif_list parameter error rule
		in
		let error, ((handler, bdu_creation), modif_list) =
		  match AgentMap.unsafe_get parameter error agent_type
		    store_bdu_creation with
		      | error, None -> error, ((handler, bdu_init), [])
		      | error, Some ((handler, bdu), l) -> error, ((handler, bdu), l)
		in
		(*create empty bdu_creation array*)
		let bdu_array =
		  bdu_array.(rule_id) <- bdu_creation;
		  bdu_array;
		in
		(*let _ = Printf.fprintf stdout "rule_id:%i:agent_type:%i:nrules:%i\n"
		  rule_id agent_type nrules
		in *)
		let error, wl_tl_array = aux wl_tl (error, store_result) in
		error, (Array.append bdu_array wl_tl_array)
	in aux wl (error, bdu_array) (*empty*)
      in
      let error, store_result =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  bdu_array
	  store_result
      in
      error, store_result
    ) store_rule_in_wl store_result

(*test first*)
let collect_bdu_iterate_array parameter error handler_sig store_rule_in_wl store_result =
  let error, (handler, bdu_init) = bdu_init parameter error in
  AgentMap.fold parameter error
    (fun parameter error agent_type (wl, rule_array) store_result_array ->
      (*test*)
      (* let _ =
	Printf.fprintf stdout "Creation BDU:\n";
	let error, store = AgentMap.create parameter error 0 in
	Array.iteri (fun index rule ->
	  let _ =
	    let error, store =
	      Bdu_creation.collect_creation
		parameter
		error
		rule.rule_rhs.views
		rule.actions.creation
		store
	    in
	    Print_bdu_analysis.print_bdu_array_creation_aux parameter error store
	  in
	  ()
	) rule_array
      in*)
      (*auxiliary function *)
      let error, bdu_array =
	let rec aux acc_wl (error, bdu_remanent_array) =
	  (*if wl is empty then return the result*)
	  if IntWL.is_empty acc_wl
	  then error, bdu_remanent_array
	  else
	    (*pop the first element inside this work list*)
	    let error, (rule_id_op, wl_tl) =
	      IntWL.pop parameter error acc_wl
	    in
	    match rule_id_op with
	      | None -> error, bdu_remanent_array
	      | Some rule_id ->
	      (*get 'rule' type inside rule_array by rule_id index*)
		let rule = Array.get rule_array rule_id in
		(*build bdu_creation*)
		let error, store_bdu_creation =
		  build_bdu_creation parameter error rule
		in
		let error, (handler, bdu_creation) =
		  match AgentMap.unsafe_get parameter error agent_type
		    store_bdu_creation with
		    | error, None -> error, (handler, bdu_init)
		    | error, Some (handler, bdu) -> error, (handler, bdu)
		in
		(*build bdu_test and modif list*)
		let error, store_bdu_test_modif_list =
		  build_bdu_test_modif_list parameter error rule
		in
		let error, ((handler, bdu_test), modif_list) =
		  match AgentMap.unsafe_get parameter error agent_type
		    store_bdu_test_modif_list with
		      | error, None -> error, ((handler, bdu_init), [])
		      | error, Some ((handler, bdu_test), l) ->
			error, ((handler, bdu_test), l)
		in
		(*build List_sig.list for modif_list*)
		let error, (handler, list_a) =
		  List_algebra.build_list
		    (list_allocate parameter)
		    error
		    parameter
		    handler
		    modif_list
		in
		(*iterate function*)
		(*check if it is not an enable rule*)
		let error, is_enable =
		  comp_is_enable
		    parameter
		    error
		    handler
		    bdu_init
		    bdu_test
		    bdu_creation
		in
		if is_enable
		then
		  let error, bdu_update_array =
		    compute_update
		      parameter
		      error
		      rule_id
		      handler
		      bdu_test
		      list_a
		      bdu_creation
		      bdu_remanent_array
		  in
		  aux wl_tl (error, bdu_update_array)
	      (*if yes then continue wl with bdu_update_array*)
		else
		(*continue the wl_tl*)
		  aux wl_tl (error, bdu_remanent_array)
	in aux wl (error, [||]) (*empty*)
      in
      (*get old*)
      let error, old_bdu_array =
	match AgentMap.unsafe_get parameter error agent_type store_result_array with
	  | error, None -> error, [||]
	  | error, Some array -> error, array
      in
      (*new*)
      let new_array = Array.append bdu_array old_bdu_array in
      (*store*)
      let error, store_result_array =
	AgentMap.set
	  parameter
	  error
	  agent_type
	  new_array
	  store_result_array
      in
      error, store_result_array
    ) store_rule_in_wl store_result
