(**
  * bdu_analysi.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 26th of June
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Cckappa_sig
open Int_storage
open Mvbdu_sig
open Bdu_analysis_type
open Print_bdu_analysis

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(*---------------------------------------------------------------------------------*)
(*common function for building bdu from a list of pair (site, state)*)

let f parameter error a' x y =
  match x y with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a =
        Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> a')
      in error, handler, a
  
let build_bdu parameter error pair_list =
  (*build bdu for this list*)
  let remanent_bdu = Sanity_test.remanent parameter in
  let error        = remanent_bdu.Sanity_test_sig.error in
  let allocate     = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  (*'b: memo_tables; 'a: mvbdu_dic; 'c: list_dic*)
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent_bdu.Sanity_test_sig.mvbdu_handler
  in
  let a_val = Mvbdu_sig.Leaf true in
  let b_val = Mvbdu_sig.Leaf false in
  (*build bdu from a_val: 
    a',a'_id: output of build_already_compressed_cell;
    a'', a''_id: output of compress_node
  *)
  let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val
  in
  (*build bdu_b from b_val*)
  let error, handler, b', b'_id, b'', b''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in
  (*---------------------------------------------------------------------------*)    
  (*build bdu_list from a list of pair [site, state] computed above in cv*)
  let error, (handler, list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      pair_list
  in
  (*compute redefine in a list_a, a': mvbdu_input*)
  let error, handler, mvbdu =
    f parameter error a' 
      (Boolean_mvbdu.redefine parameter error parameter handler a') list_a
  in
  (*---------------------------------------------------------------------------*)
  (*return redefine*)
  error, (handler, mvbdu)

(************************************************************************************)    
(*build initial bdu: false branch*)

let bdu_init parameter =
  let remanent_bdu = Sanity_test.remanent parameter in
  let error        = remanent_bdu.Sanity_test_sig.error in
  let allocate     = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent_bdu.Sanity_test_sig.mvbdu_handler
  in
  let a_val = Mvbdu_sig.Leaf false in
  let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val
  in
  error, (handler, a')

(************************************************************************************)    
(*compute bdu for initial state or creation actions*)

let int_of_port port = port.site_state.min

let collect_creation parameter error viewsrhs creation store_creation =
  let error, (handler, bdu_init) = bdu_init parameter in
  List.fold_left (fun (error, store_creation) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
      | None -> warn parameter error (Some "line 120") Exit store_creation
      | Some Ghost -> error, store_creation
      | Some Agent agent ->
        (*get the site and state on the rhs*)
        let (pair_list, (handler, bdu)) =
          Site_map_and_set.fold_map
            (fun site port (current_list, _)  ->
              let state = int_of_port port in
              let l = (site, state) :: current_list in
	      let error, (handler, bdu) =
		build_bdu parameter error l
	      in
	      l, (handler, bdu)
            ) agent.agent_interface ([], (handler, bdu_init))
        in
	(*---------------------------------------------------------------------------*)
        (*get old*)
        let error, (old_list, (handler, old_bdu)) =
          match AgentMap.unsafe_get parameter error agent_type store_creation with
            | error, None -> error, ([], (handler, bdu_init))
            | error, Some (l, (handler, b)) -> error, (l, (handler, b))
        in
	let new_list = List.concat [pair_list; old_list] in
	let error, handler, new_bdu =
	  f parameter error old_bdu
            (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter old_bdu) bdu
	in
	(*--------------------------------------------------------------------------*)
        (*store*)
        let error, store_creation =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list, (handler, new_bdu))
            store_creation
        in
        error, store_creation
  ) (error, store_creation) creation

(************************************************************************************)    
(*iteration fixpoint: round-robin algorithm.
  Ex: X_0 U r_0(X_0) U r_1(X_0) U ...
*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init  in
  if is_eq
  then
    true
  else
    false

let iteration_aux parameter error viewslhs diff_direct bdu_creation store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_iteration ->
      (*there is no modification*)
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then
        error, store_iteration
      else
        match agent with
          (*-------------------------------------------------------------------------*)
          | Ghost ->
            (*if it is a Ghost agent then it is false*)
            let agent_type = site_modif.agent_name in
            let error, (old_list, (handler, old_bdu)) =
              match AgentMap.unsafe_get parameter error agent_type store_iteration with
                | error, None -> error, ([], (handler, bdu_init))
                | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
            in
            (*PRINT*)
            let _ =
              handler.Memo_sig.print_mvbdu stdout "" old_bdu
            in
            error, store_iteration
          (*-------------------------------------------------------------------------*)
          | Agent agent ->
            let agent_type = agent.agent_name in
            (*compute bdu of test rule*)
            let (site_test_list, (handler, bdu_test)) =
              Site_map_and_set.fold_map
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, (handler, bdu))
                agent.agent_interface ([], (handler, bdu_init))
            in
            (*-----------------------------------------------------------------------*)
            (*compute bdu of direct rule*)
            let (site_direct_list, (handler2, bdu_direct)) =
              Site_map_and_set.fold_map
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, (handler, bdu)
                ) site_modif.agent_interface ([], (handler, bdu_init))
            in
            (*--------------------------------------------------------------------------*)
            (*check the enabled rule; it is enabled if the intercession of X
              and bdu_test is different than empty*)
            (*X is an old_bdu union with bdu_creation, started at bdu_init*)
            let error, (old_list, (handler, old_bdu)) =
              match AgentMap.unsafe_get parameter error agent_type store_iteration with
                | error, None -> error, ([], (handler, bdu_init))
                | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
            in
            let error, handler, bdu_X =
              f parameter error old_bdu
                (Boolean_mvbdu.boolean_mvbdu_or
                   parameter handler error parameter old_bdu) bdu_creation
            in
            (*check enabled rule: intercession of bdu_X and bdu_test*)
            let error, handler, bdu_X_test =
              f parameter error bdu_X
                (Boolean_mvbdu.boolean_mvbdu_and
                   parameter handler error parameter bdu_X) bdu_test
            in
            if not (is_belong bdu_X_test bdu_init)
            then
              (*------------------------------------------------------------------------*)
              begin
                (*check if bdu_test belong to bdu_X*)
                let error, handler, bdu_is_test_in_X =
                  f parameter error bdu_X_test
                    (Boolean_mvbdu.boolean_mvbdu_and
                       parameter handler error parameter bdu_X_test) bdu_test
                in
                if not (is_belong bdu_is_test_in_X bdu_init)
                then
                  (*update bdu_X and bdu_direct*)
                  let error, handler, bdu_X_direct =
                    f parameter error bdu_X
                      (Boolean_mvbdu.boolean_mvbdu_or
                         parameter handler error parameter bdu_X) bdu_direct
                  in
                  let error, store_iteration =
                    AgentMap.set
                      parameter
                      error
                      agent_type
                      ([], (handler, bdu_X_direct))
                      store_iteration
                  in
                  (*PRINT*)
                  let _ =
                    handler.Memo_sig.print_mvbdu stdout "" bdu_X_direct
                  in
                  error, store_iteration
                else
                  (*bdu test is not belong to X*)
                  error, store_iteration
              end
            else
              error, store_iteration
    ) viewslhs diff_direct store_result

let iteration_fixpoint parameter error viewslhs diff_direct bdu_creation store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type (l, (handler, bdu_creation)) store_result ->
      iteration_aux parameter error viewslhs diff_direct bdu_creation store_result   
    ) bdu_creation store_result

(************************************************************************************)
(*iteration fixpoint with influence map.
  Ex: X = X_0 U r0(X_0)
      U r_1(X) 
*)

let iteration_fixpoint_influence_aux parameter error viewslhs diff_direct bdu_creation
    store_result = (*TODO*)
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then
        error, store_result
      else
        match agent with
          | Ghost -> error, store_result (*TODO*)
          | Agent agent ->
            let (store_test, store_direct, store_iteration) = store_result in
            let agent_type = agent.agent_name in
            (*------------------------------------------------------------------------*)
            (*build bdu_test*)
            let (site_list_test, (handler, bdu_test)) =
              Site_map_and_set.fold_map
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, (handler, bdu)
                ) agent.agent_interface ([], (handler, bdu_init))
            in
            (*get old bdu_test*)
            let error, (old_test_list, (handler, old_bdu_test)) =
              match AgentMap.unsafe_get parameter error agent_type store_test with
                | error, None -> error, ([], (handler, bdu_init))
                | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
            in
            let new_test_list = List.concat [site_list_test; old_test_list] in
            let error, handler, new_bdu_test =
              f parameter error old_bdu_test
                (Boolean_mvbdu.boolean_mvbdu_or
                   parameter handler error parameter old_bdu_test) bdu_test
            in
            let error, store_test =
              AgentMap.set
                parameter
                error
                agent_type 
                (new_test_list, (handler, new_bdu_test))
                store_test
            in
            (*------------------------------------------------------------------------*)
            (*build bdu_direct*)
            let (site_list_direct, (handler, bdu_direct)) =
              Site_map_and_set.fold_map
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, (handler, bdu)
                ) site_modif.agent_interface ([], (handler, bdu_init))
            in
            (*get old bdu_direct*)
            let error, (old_direct_list, (handler, old_bdu_direct)) =
              match AgentMap.unsafe_get parameter error agent_type store_direct with
                | error, None -> error, ([], (handler, bdu_init))
                | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
            in
            let error, handler, new_bdu_direct =
              f parameter error old_bdu_direct 
                (Boolean_mvbdu.boolean_mvbdu_or
                   parameter handler error parameter old_bdu_direct) bdu_direct
            in
            let error, store_direct =
              AgentMap.set parameter error agent_type
                ([], (handler, new_bdu_direct))
                store_direct
            in
            (*------------------------------------------------------------------------*)
            (*get old iteration*)
            let error, (old_list, (handler, old_bdu)) =
              match AgentMap.unsafe_get parameter error agent_type store_iteration with
                | error, None -> error, ([], (handler, bdu_init))
                | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
            in
            let error, handler, bdu_X =
              f parameter error old_bdu
                (Boolean_mvbdu.boolean_mvbdu_or
                   parameter handler error parameter old_bdu) bdu_creation
            in
            (*check the enabled rule of bdu_X and bdu_test x0*)
            let error, handler, bdu_X_test_0 =
              f parameter error bdu_X
                (Boolean_mvbdu.boolean_mvbdu_and
                   parameter handler error parameter bdu_X) bdu_test
            in
            (*------------------------------------------------------------------------*)
            if not (is_belong bdu_X_test_0 bdu_init)
            then
              (*r0(X0)*)
              begin               
                let error, handler, bdu_is_test_in_X_0 =
                  f parameter error bdu_X_test_0
                    (Boolean_mvbdu.boolean_mvbdu_and
                       parameter handler error parameter bdu_X_test_0) bdu_test
                in
                (*---------------------------------------------------------------------*)
                if not (is_belong bdu_is_test_in_X_0 bdu_init)
                then
                  (*update X0 U r0(X0)*)
                  let error, handler, bdu_X_direct_0 =
                    f parameter error bdu_X
                      (Boolean_mvbdu.boolean_mvbdu_or
                         parameter handler error parameter bdu_X) bdu_direct
                  in
                  (*let _ =
                    fprintf stdout "bdu_old_test\n";
                    handler.Memo_sig.print_mvbdu stdout "" old_bdu_test;
                    fprintf stdout "bdu_X_direct_0\n";
                    handler.Memo_sig.print_mvbdu stdout "" bdu_X_direct_0
                  in*)
                  (*do r1 is in bdu_X_direct_0*)
                  begin
                    let error, handler, bdu_is_r1_in_X_direct_X0 =
                      f parameter error old_bdu_test
                        (Boolean_mvbdu.boolean_mvbdu_and
                           parameter handler error parameter old_bdu_test) 
                        bdu_X_direct_0
                    in
                    (*-----------------------------------------------------------------*)
                    if not (is_belong bdu_is_r1_in_X_direct_X0 bdu_init)
                    then
                      (*update direct_X1 in bdu_X_direct_0*)
                      let error, handler, bdu_X0_direct_1 =
                        f parameter error old_bdu_direct
                          (Boolean_mvbdu.boolean_mvbdu_or
                             parameter handler error parameter old_bdu_direct)
                          bdu_X_direct_0
                      in
                      (*let _ =
                        fprintf stdout "bdu_X0_direct_1\n";
                        handler.Memo_sig.print_mvbdu stdout "" bdu_X0_direct_1
                      in*)
                      (*---------------------------------------------------------------*)
                      (*store to result1*)
                      let error, store_iteration =
                        AgentMap.set
                          parameter
                          error
                          agent_type
                          ([], (handler, bdu_X0_direct_1))
                          store_iteration
                      in
                      error, (store_test, store_direct, store_iteration)
                    else
                      error, store_result
                  end
                else
                  error, store_result
              end
            else
              error, store_result
    ) viewslhs diff_direct store_result

let iteration_fixpoint_influence parameter error viewslhs diff_direct bdu_creation
    store_result =
  AgentMap.fold parameter error
    (fun paramater error agent_type (l, (handler, bdu_creation)) store_result ->
      iteration_fixpoint_influence_aux
        parameter
        error
        viewslhs
        diff_direct
        bdu_creation
        store_result
    ) bdu_creation store_result


(************************************************************************************)
(*side effects: unknow binding - know binding - deletion*)

(*unknow binding*)

let collect_half_break parameter error kappa_handler store_half_break half_break =
  let error, (handler, bdu_init) = bdu_init parameter in
  List.fold_left (fun (error, store_half_break) (site_add, state) ->
    let site = site_add.site in
    let agent_type = site_add.agent_type in
    (*---------------------------------------------------------------------------------*)
    (*get state*)
    let error, (min, max) =
      match state with
	| None ->
	  begin
	    let error, value_state =
	      Misc_sa.unsome
		(Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
		   parameter
		   error
		   (agent_type, site)
		   kappa_handler.states_dic)
		(fun error -> warn parameter error (Some "line 184")
		  Exit (Dictionary_of_States.init ()))
	    in
	    let error, last_entry =
	      Dictionary_of_States.last_entry parameter error value_state
	    in
	    error, (1, last_entry)
	  end
	| Some interval -> error, (interval.min, interval.max)
    in
    let pair_list = [site, min] in
    (*---------------------------------------------------------------------------------*)
    (*build bdu for half_break*)
    let error, (handler, bdu_half_break) = build_bdu parameter error pair_list in
    (*---------------------------------------------------------------------------------*)
    (*get old*)
    let error, (old_list, (handler, old_bdu)) =
      match AgentMap.unsafe_get parameter error agent_type store_half_break with
	| error, None -> error, ([], (handler, bdu_init))
	| error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
    in
    let new_list = List.concat [pair_list; old_list] in
    let error, handler, new_bdu =
      f parameter error old_bdu
        (Boolean_mvbdu.boolean_mvbdu_or
	   parameter handler error parameter old_bdu) bdu_half_break
    in
    (*---------------------------------------------------------------------------------*)
    (*store*)
    AgentMap.set
      parameter
      error
      agent_type
      (List.rev new_list, (handler, new_bdu))
      store_half_break
  ) (error, store_half_break) half_break

(*------------------------------------------------------------------------------*)
(*remove actions (deletion): document site*)

let collect_remove_know_site parameter error kappa_handler 
    index agent agent_type store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  let (pair_list, (handler, bdu)) =
    Site_map_and_set.fold_map (fun site _ (current_list,_) ->
      (*get state from state_dic*)
      let error, state_dic =
        Misc_sa.unsome
          (Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
             parameter
             error
             (agent_type, site)
             kappa_handler.states_dic
          )
          (fun error -> warn parameter error (Some "line 238") Exit
            (Cckappa_sig.Dictionary_of_States.init()))
      in
      let error, last_entry = Cckappa_sig.Dictionary_of_States.last_entry parameter
        error state_dic
      in
      let l = (site, last_entry) :: current_list in (*TEST*)
      let error, (handler, bdu_remove) = build_bdu parameter error l in
      l, (handler, bdu_remove)
    ) agent.agent_interface ([], (handler, bdu_init))
  in
  (*get old*)
  let error, (old_list, (handler, old_bdu)) =
    match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, ([], (handler, bdu_init))
      | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
  in
  let new_list = List.concat [pair_list; old_list] in
  let error, handler, new_bdu =
    f parameter error old_bdu
      (Boolean_mvbdu.boolean_mvbdu_or
         parameter handler error parameter old_bdu) bdu
  in
  (*store*)
  AgentMap.set
    parameter
    error
    agent_type
    (List.rev new_list, (handler, new_bdu))
    store_result

(*------------------------------------------------------------------------------*)
(*remove action (deletion): undocument, the state is the total state *)

let collect_remove_undocument_site parameter error kappa_handler
    index agent_type list_undoc store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  let (list, (handler, bdu)) =
    List.fold_left (fun (current_list, _) site ->
      (*get state from state_dic*)
      let error, state_dic =
        Misc_sa.unsome
          (Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
             parameter
             error
             (agent_type, site)
             kappa_handler.states_dic
          )
          (fun error -> warn parameter error (Some "line 286") Exit
            (Cckappa_sig.Dictionary_of_States.init()))
      in
      let error, last_entry = Cckappa_sig.Dictionary_of_States.last_entry parameter
        error state_dic
      in
      let l = (site, last_entry) :: current_list in (*TEST*)
      let error, (handler, bdu) = build_bdu parameter error l in
      l, (handler, bdu)
    ) ([], (handler, bdu_init)) list_undoc
  in
  (*get old*)
  let error, (old_list, (handler, old_bdu)) =
    match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, ([], (handler, bdu_init))
      | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
  in
  let new_list = List.concat [list; old_list] in
  let error, handler, new_bdu =
    f parameter error old_bdu
      (Boolean_mvbdu.boolean_mvbdu_or
         parameter handler error parameter old_bdu) bdu
  in
  (*store*)
  AgentMap.set
    parameter
    error
    agent_type
    (List.rev new_list, (handler, new_bdu))
    store_result

(*------------------------------------------------------------------------------*)
(*bdu of remove action is an union of bdu know_site and bdu undoc site*)

let collect_remove parameter error kappa_handler store_result remove =
  let error, (handler, bdu_init) = bdu_init parameter in
  List.fold_left (fun (error, store_result) (index, agent, list_undoc) ->
    let agent_type = agent.agent_name in
    let (store_know, store_undoc, store_union) = store_result in
    let error, know_site =
      collect_remove_know_site
        parameter
        error
        kappa_handler
        index
        agent
        agent_type
        store_know
    in
    let error, undoc_site =
      collect_remove_undocument_site
        parameter
        error
        kappa_handler
        index
        agent_type
        list_undoc
        store_undoc
    in
    (*get bdu*)
    let error, (l, (handler, bdu_know)) =
      match AgentMap.unsafe_get parameter error agent_type know_site with
        | error, None -> error, ([], (handler, bdu_init))
        | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
    in
    let error, (l, (handler, bdu_undoc)) =
      match AgentMap.unsafe_get parameter error agent_type undoc_site with
        | error, None -> error, ([], (handler, bdu_init))
        | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
    in
    (*remove actions is a union of know_site and undoc site*)
    let error, handler, union_remove =
      f parameter error bdu_know
        (Boolean_mvbdu.boolean_mvbdu_or
           parameter handler error parameter bdu_know) bdu_undoc
    in
    let error, result_remove =
      AgentMap.set
        parameter
        error
        agent_type
        (handler, union_remove)
        store_union
    in
    error, (know_site, undoc_site, result_remove)
  ) (error, store_result) remove
    
(************************************************************************************)
(*Covering class*)

let collect_test_modif parameter error viewslhs diff_reverse store_result = (*REMOVE*)
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
    (*If there is no modified site then return the enabled rules*)
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then error, store_result
      else
        match agent with
          | Ghost -> error, store_result
          | Agent agent ->
            let agent_type = agent.agent_name in
            (*------------------------------------------------------------------------*)
	    let (pair_list, (handler, bdu)) =
	      Site_map_and_set.fold_map
		(fun site port (current_list, _) ->
		  let state = int_of_port port in
		  let l = (site, state) :: current_list in
		  (*build bdu*)
		  let error, (handler, bdu) = build_bdu parameter error l in
		  l, (handler, bdu)
		) agent.agent_interface ([], (handler, bdu_init))
	    in
            (*------------------------------------------------------------------------*)
	    (*get old*)
	    let error, (old_list, (handler, old_bdu)) =
	      match AgentMap.unsafe_get parameter error agent_type store_result with
		| error, None -> error, ([], (handler, bdu_init))
		| error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
	    in
	    (*new*)
	    let new_list = List.concat [pair_list; old_list] in
	    let error, handler, new_bdu =
	      f parameter error old_bdu
		(Boolean_mvbdu.boolean_mvbdu_or
		   parameter handler error parameter old_bdu) bdu
	    in
            (*------------------------------------------------------------------------*)
	    AgentMap.set
	      parameter
	      error
	      agent_type
	      (List.rev new_list, (handler, new_bdu))
	      store_result
    ) viewslhs diff_reverse store_result

(************************************************************************************)
(*iteration function of bdu creation union with bdu of test and
  modification (covering class)*)

let iteration_created_cv parameter error store_creation store_cv store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (_, (handler, bdu_created))
      (_, (handler, bdu_cv)) store_result ->
	let error, handler, bdu_iterate =
	  f parameter error bdu_created
            (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter bdu_created) bdu_cv
	in
	(*store*)
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (handler, bdu_iterate)
	  store_result
    ) store_creation store_cv store_result

(************************************************************************************)
(*iteration function of side effect (half_break) and covering class*)

let iteration_half_cv parameter error store_cv store_half_break store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (_, (handler, bdu_cv))
      (_, (handler, bdu_hf)) store_result ->
      let error, handler, bdu_iterate =
	f parameter error bdu_cv 
          (Boolean_mvbdu.boolean_mvbdu_or
	     parameter handler error parameter bdu_cv) bdu_hf
      in
      AgentMap.set
	parameter
	error
	agent_type
	(handler, bdu_iterate)
	store_result
    ) store_cv store_half_break store_result

(************************************************************************************)
(*iteration function of remove and covering class*)

let iteration_remove_cv parameter error store_cv store_remove store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (_, (handler, bdu_cv))
      (handler, bdu_remove)
      store_result ->
      let error, handler, bdu_iterate =
	f parameter error bdu_cv 
          (Boolean_mvbdu.boolean_mvbdu_or
	     parameter handler error parameter bdu_cv) bdu_remove
      in
      AgentMap.set
	parameter
	error
	agent_type
	(handler, bdu_iterate)
	store_result
    ) store_cv store_remove store_result
  
(************************************************************************************)
(*iteration function of half_break, remove and covering class*)

let iteration_half_remove_cv parameter error store_remove_cv store_half store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (handler, bdu_remove_cv)
      (_, (handler, bdu_half))
      store_result ->
      let error, handler, bdu_iterate =
	f parameter error bdu_remove_cv 
          (Boolean_mvbdu.boolean_mvbdu_or
	     parameter handler error parameter bdu_remove_cv) bdu_half
      in
      AgentMap.set
	parameter
	error
	agent_type
	(handler, bdu_iterate)
	store_result
    ) store_remove_cv store_half store_result
    
(************************************************************************************)
(*iteration function of side effect (half_break), creation and covering class*)
    
let iteration_half_created_cv parameter error store_iterate_created_cv store_half_break
    store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (handler, bdu_iterate)
      (_, (handler, bdu_half_break)) store_result ->
	let error, handler, bdu_iterate =
	  f parameter error bdu_iterate
            (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter bdu_iterate) bdu_half_break
	in
	(*store*)
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (handler, bdu_iterate)
	  store_result
    ) store_iterate_created_cv store_half_break store_result

(************************************************************************************)
(*iteration function of half_break, remove, creation and covering class*)

let iteration_half_remove_created_cv parameter error store_half_created_cv store_remove
    store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type
      (handler, bdu_iterate_half_created_cv)
      (handler, bdu_remove)
      store_result ->
        let error, handler, bdu =
          f parameter error bdu_iterate_half_created_cv
            (Boolean_mvbdu.boolean_mvbdu_or
               parameter handler error parameter bdu_iterate_half_created_cv) bdu_remove
        in
        (*store*)
        AgentMap.set
          parameter
          error
          agent_type
          (handler, bdu)
          store_result
    ) store_half_created_cv store_remove store_result

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule rules store_result =
  (*------------------------------------------------------------------------------*)
  (*creation rules*)
  let error, store_creation =
    collect_creation
      parameter
      error
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_creation
  in
  (*------------------------------------------------------------------------------*)
  (*iteration fixpoint*)
  let error, store_iteration_fixpoint =
    iteration_fixpoint
      parameter
      error
      rule.rule_lhs.views
      rule.diff_direct
      store_creation
      store_result.store_iteration_fixpoint
  in
  (*------------------------------------------------------------------------------*)
  (*iteration fixpoint influence*)
  (*let error, store_iteration_fixpoint_influence =
    iteration_fixpoint_influence
      parameter
      error
      rule.rule_lhs.views
      rule.diff_direct
      store_creation
      store_result.store_iteration_fixpoint_influence
  in*)
  (*------------------------------------------------------------------------------*)
  (*side effect - half_break*)
  let error, store_half_break =
    collect_half_break
      parameter
      error
      handler
      store_result.store_half_break
      rule.actions.half_break
  in
  (*------------------------------------------------------------------------------*)
  (*side effect - deletion*)
  let error, store_remove =
    collect_remove
      parameter
      error
      handler
      store_result.store_remove
      rule.actions.remove
  in
  (*------------------------------------------------------------------------------*)
  (*test and modification: covering class*)
  let error, store_test_modif =
    collect_test_modif
      parameter
      error
      rule.rule_lhs.views
      rule.diff_reverse
      store_result.store_test_modif
  in
  (*------------------------------------------------------------------------------*)
  (*iteration: creation and covering class*)
  let error, store_iterate_created_cv =
    iteration_created_cv
      parameter
      error
      store_creation
      store_test_modif
      store_result.store_iterate_created_cv
  in
  (*------------------------------------------------------------------------------*)
  (*iteration: half_break and covering class*)
  let error, store_iterate_half_cv =
    iteration_half_cv
      parameter
      error
      store_test_modif
      store_half_break
      store_result.store_iterate_half_cv
  in
  (*------------------------------------------------------------------------------*)
  (*iteration: remove and covering class*)
  let (_, _, bdu_remove) = store_remove in
  let error, store_iterate_remove_cv =
    iteration_remove_cv
      parameter
      error
      store_test_modif
      bdu_remove
      store_result.store_iterate_remove_cv
  in
  (*------------------------------------------------------------------------------*)
  (*iteration: half_break, remove and covering class*)
  let error, store_iterate_half_remove_cv =
    iteration_half_remove_cv
      parameter
      error
      store_iterate_remove_cv
      store_half_break
      store_result.store_iterate_remove_cv
  in
  (*------------------------------------------------------------------------------*)
  (*iteration: half_break, creation and cv*)
  let error, store_iterate_half_created_cv =
    iteration_half_created_cv
      parameter
      error
      store_iterate_created_cv
      store_half_break
      store_result.store_iterate_half_created_cv
  in
  (*------------------------------------------------------------------------------*)
  let error, store_iterate_half_remove_created_cv =
    iteration_half_remove_created_cv
      parameter
      error
      store_iterate_half_created_cv
      bdu_remove
      store_result.store_iterate_half_remove_created_cv
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation            = store_creation;
    store_iteration_fixpoint  = store_iteration_fixpoint;
    (*store_iteration_fixpoint_influence = store_iteration_fixpoint_influence;*)
    store_half_break = store_half_break;
    store_remove     = store_remove;
    store_test_modif = store_test_modif;
    store_iterate_created_cv      = store_iterate_created_cv;
    store_iterate_half_cv         = store_iterate_half_cv;
    store_iterate_remove_cv       = store_iterate_remove_cv;
    store_iterate_half_remove_cv  = store_iterate_half_remove_cv;
    store_iterate_half_created_cv = store_iterate_half_created_cv;
    store_iterate_half_remove_created_cv = store_iterate_half_remove_created_cv
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init_creation           = AgentMap.create parameter error 0 in
  let error, init_iteration_fixpoint = AgentMap.create parameter error 0 in
  let error, init_test = AgentMap.create parameter error 0 in
  let error, init_direct = AgentMap.create parameter error 0 in
  let error, init_iteration = AgentMap.create parameter error 0 in
  let error, init_half_break   = AgentMap.create parameter error 0 in
  let error, init_remove_know  = AgentMap.create parameter error 0 in
  let error, init_remove_undoc = AgentMap.create parameter error 0 in
  let error, init_remove       = AgentMap.create parameter error 0 in
  let error, init_test_modif   = AgentMap.create parameter error 0 in
  let error, init_iterate_created_cv      = AgentMap.create parameter error 0 in
  let error, init_iterate_half_cv         = AgentMap.create parameter error 0 in
  let error, init_iterate_remove_cv       = AgentMap.create parameter error 0 in
  let error, init_iterate_half_remove_cv  = AgentMap.create parameter error 0 in
  let error, init_iterate_half_created_cv = AgentMap.create parameter error 0 in
  let error, init_iterate_half_remove_created_cv = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation           = init_creation;
      store_iteration_fixpoint = init_iteration_fixpoint;
      (*store_iteration_fixpoint_influence = init_test, init_direct, init_iteration;*)
      store_half_break = init_half_break;
      store_remove     = (init_remove_know, init_remove_undoc, init_remove);
      store_test_modif = init_test_modif;
      store_iterate_created_cv      = init_iterate_created_cv;
      store_iterate_half_cv         = init_iterate_half_cv;
      store_iterate_remove_cv       = init_iterate_remove_cv;
      store_iterate_half_remove_cv  = init_iterate_half_remove_cv;
      store_iterate_half_created_cv = init_iterate_half_created_cv;
      store_iterate_half_remove_created_cv = init_iterate_half_remove_created_cv;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_results =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in
        let error, result =
          scan_rule
            parameter
            error
            handler
            rule.e_rule_c_rule
            rules
            store_result
        in
        error, result
      ) rules init_bdu
  in
  error, store_results

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.rules in
  let _ = print_result parameter error result in
  error, result

