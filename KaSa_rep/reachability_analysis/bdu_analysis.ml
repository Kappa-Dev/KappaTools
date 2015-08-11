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
open Bdu_analysis_type
open Print_bdu_analysis
open Boolean_mvbdu
open Memo_sig
open Mvbdu_sig
open Fifo

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

(*let t parameter error =
  let wl = IntWlist.empty in
  let wl = IntWlist.push parameter error 1 wl in
  Format.printf "%s\n" (IntWlist.print_work_list wl);
  let wl = IntWlist.push parameter error 2 wl in
  Format.printf "%s\n" (IntWlist.print_work_list wl);
  let wl = IntWlist.push parameter error 3 wl in
  Format.printf "%s\n" (IntWlist.print_work_list wl);
  let wl = IntWlist.push parameter error 2 wl in
  Format.printf "%s\n" (IntWlist.print_work_list wl);
  let out, wl = IntWlist.pop parameter error wl in
  Format.printf "%s\n" (IntWlist.print_work_list wl);
  match out with
    | None -> Format.printf "Out is none\n"
    | Some o -> Format.printf "Out is %d\n" o*)

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
  let a_val = Leaf true in
  let b_val = Leaf false in
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
      | None -> warn parameter error (Some "line 138") Exit store_creation
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
  - It is a enabled rule when the intersection between bdu_test and
  remanent_bdu is different than empty set (is_belong).

*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then
    true
  else
    false

(* Build a working list for test rule*)

let work_list_lhs parameter error viewslhs store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      match agent with
        | Ghost -> error, store_result
        | Agent agent ->
          let agent_type = agent.agent_name in
          let site_list, bdu_test =
            Site_map_and_set.fold_map 
              (fun site port (current_list, _) ->
                let state = int_of_port port in
                let l = (site, state) :: current_list in
                let error, (handler, bdu) =
                  build_bdu parameter error l
                in
                l, bdu
              ) agent.agent_interface ([], bdu_init)
          in
          (*create an empty work list*)
          let wl = BduWlist.empty in
          (*push bdu_test inside this work list*)
          let in_list, out_list, pool = BduWlist.push parameter error bdu_test wl in
          (*get old site list and work list*)
          let error, (old_list, (old_in_list, old_out_list, old_pool)) =
            match AgentMap.unsafe_get parameter error agent_type store_result with
              | error, None -> error, ([], wl)
              | error, Some (l, wl) -> error, (l, wl)
          in
          (*new*)
          let new_list = List.concat [site_list; old_list] in
          let new_pool = BduWlist.union parameter error pool old_pool in
          let new_wl = 
            (List.concat [in_list; old_in_list], List.concat [out_list; old_out_list], new_pool)
          in
          (*store*)
          let error, store_result =
            AgentMap.set
              parameter
              error
              agent_type
              (new_list, new_wl)
              store_result
          in
          error, store_result
    ) viewslhs store_result

(*one step of checking it is an enabled rule*)
let first_step parameter error agent_type handler bdu_creation bdu_remanent bdu_test bdu_direct store_result =
  (*X is a union of bdu_creation and bdu_remanent started at bdu_init*)
  let error, handler, bdu_X0 =
    f parameter error bdu_creation
      (boolean_mvbdu_or parameter handler error parameter bdu_creation) bdu_remanent
  in
  (*compute intersection of bdu_test and bdu_remanent*)
  let error, handler, bdu_X = 
    f parameter error bdu_X0
      (boolean_mvbdu_and parameter handler error parameter bdu_X0) bdu_test
  in
  (*check if bdu_X is enable rule, if not then update bdu_X and store to
    bdu_remanent else return bdu_remanent*)
  if not (is_belong bdu_X bdu_remanent)
  then
    let error, handler, bdu_update =
      f parameter error bdu_X0
        (boolean_mvbdu_or parameter handler error parameter bdu_X0) bdu_direct
    in
    (*store to bdu_remanent*)
    let error, store_result =
      AgentMap.set
        parameter
        error
        agent_type
        (handler, bdu_update)
        store_result
    in
    error, store_result
  else
    error, store_result

let enable_rules parameter error agent_type in_list bdu_creation bdu_remanent bdu_direct
    store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  List.fold_left (fun acc h ->
    let error, store_result_enable = 
      first_step parameter error agent_type handler bdu_creation bdu_remanent h bdu_direct 
        store_result
    in
    store_result_enable
  ) store_result in_list

(*fixpoint iteration function*)
let iteration_aux parameter error viewslhs result_lhs diff_direct bdu_creation store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
      (*check if there is no modification*)
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then error, store_result
      else
        match agent with
          | Ghost -> error, store_result
          | Agent agent ->
            let agent_type = agent.agent_name in
            (*put test rule inside work list*)
            let error, wl = work_list_lhs parameter error viewslhs result_lhs in
            let error, (_, (in_list, _, _)) =
              match AgentMap.unsafe_get parameter error agent_type wl with
                | error, None -> error, ([], BduWlist.empty)
                | error, Some (l, wl) -> error, (l, wl)
            in
            (*getting bdu_direct*)
            let (_, bdu_direct) =
              Site_map_and_set.fold_map
                (fun site port (current_list, _) ->
                  let state = int_of_port port in
                  let l = (site, state) :: current_list in
                  let error, (handler, bdu) =
                    build_bdu parameter error l
                  in
                  l, bdu
                ) site_modif.agent_interface ([], bdu_init)
            in
            (*bdu_remanent - is the old result*)
            let error, handler, bdu_remanent =
              match AgentMap.unsafe_get parameter error agent_type store_result with
                | error, None -> error, handler, bdu_init
                | error, Some (handler, bdu) -> error, handler, bdu
            in
            (*go through the work list of test rule*)
            let enable = 
              enable_rules parameter error 
                agent_type
                in_list 
                bdu_creation
                bdu_remanent 
                bdu_direct
                store_result
            in
            (*get bdu_enable*)(*TO BE TEST*)
            let error, (handler, bdu_enable) =
              match AgentMap.unsafe_get parameter error agent_type enable with
                | error, None -> error, (handler, bdu_init)
                | error, Some (handler, bdu) -> error, (handler, bdu)
            in
            (*new bdu_remanent update *)
            (*let error, handler, bdu_new_remanent =
              f parameter error bdu_remanent
                (boolean_mvbdu_or parameter handler error parameter bdu_remanent) bdu_enable
            in*)
            (*store*)
            let error, store_result =
              AgentMap.set
                parameter
                error
                agent_type
                (handler, bdu_enable)
                store_result
            in            
            error, store_result
    ) viewslhs diff_direct store_result

let iteration parameter error viewslhs result_lhs diff_direct creation store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type (_, (handler, bdu_creation)) store_result ->
      let error, store_result =
        iteration_aux parameter error viewslhs result_lhs diff_direct bdu_creation store_result
      in
      error, store_result
    ) creation store_result

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
  (*work list of test rule*)
  let error, store_wl_lhs =
    work_list_lhs
      parameter
      error
      rule.rule_lhs.views
      store_result.store_wl_lhs
  in
  (*------------------------------------------------------------------------------*)
  (*fixpoint iteration*)
  let error, store_iteration =
    iteration
      parameter
      error
      rule.rule_lhs.views
      store_wl_lhs
      rule.diff_direct
      store_creation
      store_result.store_iteration
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation = store_creation;
    store_wl_lhs   = store_wl_lhs;
    store_iteration = store_iteration;
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init_creation  = AgentMap.create parameter error 0 in
  let error, init_test_wl   = AgentMap.create parameter error 0 in
  let error, init_iteration = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation = init_creation;
      store_wl_lhs   = init_test_wl;
      store_iteration = init_iteration;
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
