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
open Fifo
open Boolean_mvbdu

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

open Memo_sig

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
        (*let _ =
          fprintf stdout "bdu_creation\n";
          handler.print_mvbdu stdout "" bdu
        in*)
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
            (boolean_mvbdu_or
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
(*iteration fixpoint with creation rule: round-robin algorithm.
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
    (fun parameter error agent_id agent site_modif store_result ->
      let (store_test, store_direct, store_iteration) = store_result in
      (*if there is no modification*)
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then error, store_result
      else
        match agent with
          | Ghost -> error, store_result
          | Agent agent ->
            (*compute bdu_test*)     
            let agent_type = agent.agent_name in
            let (site_test_list, bdu_test_list) =
              Site_map_and_set.fold_map 
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, [bdu]
                ) agent.agent_interface ([], [])
            in
            let error, (old_test_list, old_bdu_test_list) =
              match AgentMap.unsafe_get parameter error agent_type store_test with
                | error, None -> error, ([], [])
                | error, Some (l, ls) -> error, (l, ls)
            in
            let new_test_list = List.concat [List.rev site_test_list; old_test_list] in
            let new_bdu_test_list = 
              List.concat [List.rev bdu_test_list; old_bdu_test_list] in
            let error, store_test =
              AgentMap.set
                parameter
                error
                agent_type
                (new_test_list, new_bdu_test_list)
                store_test
            in
            (*let bdu_test_queue =
              List.fold_left (fun a b -> Queue.push b a) Queue.empty new_bdu_test_list
            in
            let _ =
              Queue.iter (fun q ->
                let _ = 
                  fprintf stdout "bdu_test_queue:\n";
                  handler.print_mvbdu stdout "" q
                in
                ()
              ) bdu_test_queue
            in*)
            (*-------------------------------------------------------------------*)
            (*compute bdu_direct*)
             let (site_direct_list, bdu_direct_list) =
              Site_map_and_set.fold_map 
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, [bdu]
                ) site_modif.agent_interface ([], [])
            in
            let error, (old_direct_list, old_bdu_direct_list) =
              match AgentMap.unsafe_get parameter error agent_type store_direct with
                | error, None -> error, ([], [])
                | error, Some (l, ls) -> error, (l, ls)
            in
            let new_direct_list = List.concat [List.rev site_direct_list; old_direct_list] in
            let new_bdu_direct_list = 
              List.concat [List.rev bdu_direct_list; old_bdu_direct_list] in
            let error, store_direct =
              AgentMap.set
                parameter
                error
                agent_type
                (new_direct_list, new_bdu_direct_list)
                store_direct
            in
            (*let bdu_direct_queue =
              List.fold_left (fun a b -> Queue.push b a) Queue.empty new_bdu_direct_list
            in
            let _ =
              Queue.iter (fun q ->
                let _ = 
                  fprintf stdout "bdu_direct_queue:\n";
                  handler.print_mvbdu stdout "" q
                in
                ()
              ) bdu_direct_queue
            in*)

            (*let t =
              Queue.fold (fun a bdu_test ->
                Queue.fold (fun b bdu_direct ->
                  
                ) (handler, bdu_init) bdu_direct_queue
              ) (handler, bdu_init) bdu_test_queue
            in*)

            let (site_list, (handler, bdu_direct)) =
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
            (*-------------------------------------------------------------------*)
            (*iteration*)
            let error, (handler, old_bdu) =
              match AgentMap.unsafe_get parameter error agent_type store_iteration with
                | error, None -> error, (handler, bdu_init)
                | error, Some (handler, bdu) -> error, (handler, bdu)
            in
            let error, handler, bdu_X =
              f parameter error bdu_init
                (boolean_mvbdu_or parameter handler error parameter bdu_init) bdu_creation
            in
            (*Testing first then do the iteration*)
            let _ =
              fprintf stdout "bdu_X\n";
              handler.Memo_sig.print_mvbdu stdout "" bdu_X;
            in
            (*iteration in a bdu_test working list*)
            (*let handler, bdu_iteration =
              Queue.fold (fun a bdu_test ->
                (*test if this bdu is an enabled rule or not*)
                let error, handler, bdu_X_test =
                  f parameter error bdu_X
                    (boolean_mvbdu_and parameter handler error parameter bdu_X) bdu_test
                in
               
                (*-------------------------------------------------------------------*)
                if not (is_belong bdu_X_test bdu_init)
                then
                  let error, handler, bdu_update =
                    f parameter error bdu_X
                      (boolean_mvbdu_or parameter handler error parameter bdu_X) bdu_direct
                  in
                  (*union the result*)
                  (*let error, handler, bdu_iteration =
                    f parameter error bdu_update
                      (boolean_mvbdu_or parameter handler error parameter bdu_update)
                      old_bdu
                  in*)
                  (*let _ =
                    fprintf stdout "update\n";
                    handler.Memo_sig.print_mvbdu stdout "" bdu_update;
                    fprintf stdout "old_bdu\n";
                    handler.Memo_sig.print_mvbdu stdout "" old_bdu
                  in*)
                  (handler, bdu_update)
                else
                  let _ = fprintf stdout "false\n" in
                  (*taking old_bdu union with iteration before?TODO*)
                  handler, old_bdu
              ) (handler, bdu_X) bdu_test_queue
            in*)
            let error, store_iteration =
              AgentMap.set
                parameter
                error
                agent_type
                (handler, bdu_X)
                store_iteration
            in
            error, (store_test, store_direct, store_iteration)
    ) viewslhs diff_direct store_result

let iteration_fixpoint parameter error viewslhs diff_direct bdu_creation store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type (_, (handler, bdu_creation)) store_result ->
      iteration_aux
        parameter
        error
        viewslhs
        diff_direct
        bdu_creation
        store_result
    )  bdu_creation store_result

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
  let error, store_iteration =
    iteration_fixpoint
      parameter
      error
      rule.rule_lhs.views
      rule.diff_direct
      store_creation
      store_result.store_iteration
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation  = store_creation;
    store_iteration = store_iteration;
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init_creation  = AgentMap.create parameter error 0 in
  let error, init_test      = AgentMap.create parameter error 0 in
  let error, init_direct    = AgentMap.create parameter error 0 in
  let error, init_iteration = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation  = init_creation;
      store_iteration = (init_test, init_direct, init_iteration);
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

