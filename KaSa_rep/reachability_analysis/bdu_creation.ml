(**
  * bdu_analysis.ml
  * openkappa
  * Jérôme Feret & Kim Ly Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 29th of September 
  * Last modification: Time-stamp: <2015-10-19 17:37:14 feret>
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open Bdu_build_common
open Fifo

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(*reverse an array*)
let rev_in_place xs =
  let n = Array.length xs in
  let j = ref (n - 1) in
  for i = 0 to n/2 - 1 do
    let c = xs.(i) in
    xs.(i) <- xs.(!j);
    xs.(!j) <- c;
    decr j
  done

let rev_array xs =
  let ys = Array.copy xs in
  rev_in_place ys;
  ys

(************************************************************************************)    
(*compute bdu for initial state or creation action*)

(*REMOVE*)

let int_of_port port = port.site_state.min

let collect_creation parameter error viewsrhs creation store_result =
  let error, (handler, bdu_init) = bdu_init parameter error in
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
      | None -> warn parameter error (Some "line 29") Exit store_result
      | Some Ghost -> error, store_result
      | Some Agent agent ->
        (*build a list of site and an array store bdu from a pair (site,state)*)
        let list, (handler, bdu_array) = 
          Site_map_and_set.fold_map
            (fun site port (current_list, _) ->
              let state = int_of_port port in
              let l = (site, state) :: current_list in
              let error, (handler, bdu) =
                build_bdu parameter error l in
              l, (handler, [|bdu|])
            ) agent.agent_interface ([], (handler, [||]))
        in
        (*get old*)
        let error, (old_list, (handler, old_array)) =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, ([], (handler, [||]))
          | error, Some (l, (handler, array)) -> error, (l, (handler, array))
        in
        (*new*)
        let new_list = List.concat [list; old_list] in
        let new_array = Array.append bdu_array old_array in
        (*store the result in an array*)
        let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list, (handler, rev_array new_array))
            store_result
        in
        error, store_result
  ) (error, store_result) creation

(************************************************************************************)
(*collect creation return the information if there is a binding site*)

(*let collect_creation_pair parameter error viewsrhs creation store_result =
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
    | None -> warn parameter error (Some "line 92") Exit store_result
    | Some Ghost -> error, store_result
    | Some Agent agent ->
      let pair_list =
        Site_map_and_set.fold_map
          (fun site port current_list ->
            let state = int_of_port port in
            (site, state) :: current_list
          ) agent.agent_interface []
      in
      (*get old*)
      let error, old_list =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      (*new*)
      let new_pair_list = List.concat [pair_list; old_list] in
      (*store*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          new_pair_list
          store_result
      in
      error, store_result
  ) (error, store_result) creation*)

(************************************************************************************)
(*return a list of creation rule; add rules with empty lhs into a working list.*)

(*REMOVE*)

let collect_rule_creation parameter error handler rule rule_id viewsrhs creation
    store_result =
  (*let error, store = AgentMap.create parameter error 0 in*)
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
    | None -> warn parameter error (Some "line 94") Exit store_result 
    | Some Ghost -> error, store_result
    | Some Agent agent ->
      (*-------------------------------------------------------------------------*)
      (*Put rule_id into a working list*)
      let wl = IntWL.empty in
      let error, wl = IntWL.push parameter error rule_id wl in
      (*-------------------------------------------------------------------------*)
      (*from wl contain rule_id, create an array to take rule_type *)
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
      (*get old*)
      let error, (old_list, old_wl, old_array) =
        match AgentMap.unsafe_get parameter error agent_type store_result with
        | error, None -> error, ([], IntWL.empty, [||])
        | error, Some (l, wl, a) -> error, (l, wl, a)
       in
      (*-------------------------------------------------------------------------*)
      (*get new*)
      let new_list = List.concat [[rule_id]; old_list] in
       (*new working list*)
      let in_list, out_list, set = wl in
      let old_in_list, old_out_list, old_set = old_wl in
      let error, new_set =
        IntWL.WSet.union parameter error set old_set
      in
      let new_wl =
        List.concat [in_list; old_in_list], List.concat [out_list; old_out_list], new_set
      in
      (*new array*)
      let new_array = Array.append rule_array old_array in
      (*-------------------------------------------------------------------------*)
      (*store*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          (List.rev new_list, new_wl, rev_array new_array)
          store_result
      in
      error, store_result)
    (error, store_result) creation
    
(************************************************************************************)
(*fixpoint iteration events:
  - first: add rules with empty lhs into a working list.
  - second: add the bond of initial state into a contact map
  (a bound is discovered for the first time).
  - third: add views of the initial state (a view is seen for the first time).
*)

(*let fixpoint parameter error rule_id store_rule_array store_bdu_result = (*FIXME*)
  (*-----------------------------------------------------------------------------*)
  (*first step: add rules with empty lhs into a working list*)
  AgentMap.fold parameter error
    (fun parameter error agent_type (_, rule_array, _) store_bdu_result ->
      (*working list with creation rules*)
      let error, wl =
        index_rule_wl
          parameter
          error        
          rule_array
      in
      error, wl
    ) store_rule_array store_bdu_result*)


(*build [bdu_creation] for a rule*)
    
let build_bdu_creation parameter error rule (*store_result*) =  (*REMOVE*)
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

(************************************************************************************)
(*build [bdu_test and list of modif] action of a rule*)

let build_bdu_test_modif_list parameter error rule = (*REMOVE*)
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
