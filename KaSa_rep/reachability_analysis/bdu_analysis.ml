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

let int_of_port port = port.site_state.min

(************************************************************************************)
(*common function of agent interface*)

let common_site_bdu parameter error agent bdu_init =
  Site_map_and_set.fold_map
    (fun site port (current_list, _) ->
      let state = int_of_port port in
      let l = (site, state) :: current_list in
      let error, (handler, bdu) =
        build_bdu parameter error l
      in
      l, bdu
    ) agent.agent_interface ([], bdu_init)

(************************************************************************************)    
(*compute bdu for initial state or creation actions*)

let store_creation parameter error agent_type handler bdu_init list bdu store_result =
  let error, (old_list, (handler, old_bdu)) =
    match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, ([], (handler, bdu_init))
      | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
  in
  (*new*)
  let new_list = List.concat [list; old_list] in
  let error, handler, new_bdu =
    f parameter error old_bdu
      (boolean_mvbdu_or parameter handler error parameter old_bdu) bdu
  in
  (*store*)
  AgentMap.set
    parameter
    error
    agent_type
    (List.rev new_list, (handler, new_bdu))
    store_result

let collect_creation parameter error viewsrhs creation store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
      | None -> warn parameter error (Some "line 138") Exit store_result
      | Some Ghost -> error, store_result
      | Some Agent agent ->
        (*get the site and state on the rhs*)
        let list, bdu = common_site_bdu parameter error agent bdu_init in
        let error, store_result =
          store_creation parameter error agent_type handler bdu_init list bdu store_result
        in
        error, store_result
  ) (error, store_result) creation
    
(************************************************************************************)    
(*fixpoint iteration with creation rule: round-robin algorithm.
  Ex: X_0 U r_0(X_0) U r_1(X_0) U ...
  - It is an enabled rule when the intersection between bdu_test and
  bdu_remanent is different than empty set.

*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then
    true
  else
    false

(************************************************************************************)
(*Function push bdu_test inside work list*)

let push_bdu_wl parameter error agent_type site_list bdu_test handler store_result =
  (*create an empty work list*)
  let wl = BduWlist.empty in
  (*push bdu_test inside this work list*)
  let in_list, out_list, pool = BduWlist.push parameter error bdu_test wl in
  (*get old site list and work list*)
  let error, (old_list, handler, (old_in_list, old_out_list, old_pool)) =
    match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, ([], handler, wl)
      | error, Some (l, handler, wl) -> error, (l, handler, wl)
  in
  (*new*)
  let new_list = List.concat [site_list; old_list] in
  let error, new_pool = BduWlist.WSet.union parameter error pool old_pool in
  let new_wl =
    (List.concat [in_list; old_in_list], List.concat [out_list; old_out_list], new_pool)
  in
  (*store*)
  let error, store_result =
    AgentMap.set
      parameter
      error
      agent_type
      (new_list, handler, new_wl)
      store_result
  in
  error, store_result

(************************************************************************************)
(*store bdu direct *)

let store_bdu_direct parameter error agent_type handler bdu_init bdu_direct store_result =
  let error, (handler, old_bdu_handler) =
    match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, (handler, bdu_init)
      | error, Some (handler, bdu) -> error, (handler, bdu)
  in
  let error, handler, new_bdu =
    f parameter error bdu_direct
      (boolean_mvbdu_or parameter handler error parameter bdu_direct) old_bdu_handler
  in
  let error, store_result =
    AgentMap.set
      parameter
      error
      agent_type
      (handler, new_bdu)
      store_result
  in
  error, store_result
 
(************************************************************************************)
(*checking the enable rule: this function is an apply function*)

let is_enable parameter error bdu_creation bdu_test bdu_direct =
  let error, (handler, bdu_init) = bdu_init parameter in
  (*check bdu_init(bdu_creation) intersection with bdu_test*)
  let error, handler, bdu_X =
    f parameter error bdu_creation
      (boolean_mvbdu_and parameter handler error parameter bdu_creation) bdu_test
  in
  if not (is_belong bdu_X bdu_init)
  then
    true, handler, bdu_direct
  else
    (*if not return bdu_init*)
    false, handler, bdu_creation

(*write a function taking the succ(h): wake_up_map
  r1 -> r2:
  succ(r1) = {r2};
  succ(r2) = empty  
*)

(************************************************************************************)
(*iteration function*)

let fixpoint_iteration parameter error viewslhs diff_direct result_creation store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold2_common parameter error 
    (fun parameter error agent_id agent site_modif store_result ->
      match agent with
        | Ghost -> error, store_result
        | Agent agent ->
          let store_wl_lhs, store_iteration = store_result in
          let agent_type = agent.agent_name in
          let error, (l, (handler, bdu_creation)) =
            match AgentMap.unsafe_get parameter error agent_type result_creation with
              | error, None -> error, ([], (handler, bdu_init))
              | error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
          in
          (*------------------------------------------------------------------------------*)
          (*compute test rule*)
          let site_list, bdu_test = common_site_bdu parameter error agent bdu_init in
          (*------------------------------------------------------------------------------*)
          (*push bdu test rule inside work list*)
          let error, store_wl_lhs =
            push_bdu_wl parameter error agent_type site_list bdu_test handler store_wl_lhs
          in
          (*------------------------------------------------------------------------------*)
          (*compute bdu_direct or site_modif_plus*)
          let site_direct_list, bdu_direct =
            common_site_bdu parameter error site_modif bdu_init in
          let agent_type1 = site_modif.agent_name in
          let error, store_iteration =
            store_bdu_direct parameter error agent_type1 handler bdu_init bdu_direct
              store_iteration
          in
          (*------------------------------------------------------------------------------*)
          (*iteration function*)

          (*check if this work list is not empty?*)
          (*------------------------------------------------------------------------------*)
          (*store*)
          error, (store_wl_lhs, store_iteration)
    ) viewslhs diff_direct store_result

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
  (*fixpoint iteration*)
  let error, store_iteration =
    fixpoint_iteration 
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
    store_creation = store_creation;
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
      store_creation  = init_creation;
      store_iteration = init_test_wl, init_iteration;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_results =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
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
