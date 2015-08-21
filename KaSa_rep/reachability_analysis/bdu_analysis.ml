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
(*compute bdu for initial state or creation actions*)

(*common function of agent interface*)

let common_site_bdu parameter error agent handler bdu_init =
  Site_map_and_set.fold_map
    (fun site port (current_list, _) ->
      let state = int_of_port port in
      let l = (site, state) :: current_list in
      let error, (handler, bdu) =
        build_bdu parameter error l
      in
      l, (handler, bdu)
    ) agent.agent_interface ([], (handler, bdu_init))

let store_common parameter error agent_type handler bdu_init list bdu store_result =
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
      | None -> warn parameter error (Some "line 157") Exit store_result
      | Some Ghost -> error, store_result
      | Some Agent agent ->
        (*get the site and state on the rhs*)
        let list, (handler, bdu) = common_site_bdu parameter error agent handler bdu_init in
        let error, store_result =
          store_common parameter error agent_type handler bdu_init list bdu store_result
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
(*write a function taking the succ(h): wake_up_map
  r1 -> r2:
  succ(r1) = {r2};
  succ(r2) = empty
*)

(*return only the result of succ(h).*)

let get_rule_aux parameter error handler compiled rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let nrules = Handler.nrules parameter error handler in
  if rule_id < nrules
  then
    begin 
      let error, rule = Int_storage.Nearly_inf_Imperatif.get
        parameter error rule_id rules
      in
      match rule with
        | None -> error, None
        | Some rule ->
          error, Some rule.Cckappa_sig.e_rule_c_rule
    end
  else
    error, None

(************************************************************************************)
(*empty mixture*)

let empty_mixture parameter error =
  let error, empty_views = Int_storage.Quick_Nearly_inf_Imperatif.create parameter error 0 in
  let error, empty_bonds = Int_storage.Quick_Nearly_inf_Imperatif.create parameter error 0 in
  {
    c_mixture = Ckappa_sig.EMPTY_MIX;
    views     = empty_views;
    bonds     = empty_bonds;
    plus      = [];
    dot       = []
  }

(*empty rule*)
let empty_rule parameter error =
  let error, empty_diff_views = Int_storage.Quick_Nearly_inf_Imperatif.create parameter error 0 in
  {
    rule_lhs     = empty_mixture parameter error;
    rule_arrow   = Ast.RAR;
    rule_rhs     = empty_mixture parameter error;
    diff_direct  = empty_diff_views;
    diff_reverse = empty_diff_views;
    actions      = empty_actions      
  }

(*collect an array of rule *)

let collect_rule parameter error handler rule_id rule store_result =
  let nrules = Handler.nrules parameter error handler in
  let array = Array.make nrules (empty_rule parameter error) in
  let array =
    array.(rule_id) <- rule;
    array
  in
  let error, old =
    match AgentMap.unsafe_get parameter error rule_id store_result with
      | error, None -> error, [||]
      | error, Some a -> error, a
  in
  let new_array = Array.append array old in
  AgentMap.set
    parameter error rule_id new_array store_result

(*wake_up_map rule_index*)

let collect_wake_up_rule parameter error handler compiled map =
  Quark_type.Int2Set_and_map.fold_map
    (fun (rule_id_a, rule_id_b) _ current_list ->
      let l = (rule_id_a, rule_id_b) :: current_list in
      l
    ) map []

(*push index of an array of rule into working list*)

let index_rule_wl parameter error array_rule =
  let ref_list = ref [] in
  let wl = IntWL.empty in
  Array.iteri 
    begin
      fun index _ ->
        let error, wl = IntWL.push parameter error index wl in
        ref_list := wl :: !ref_list
    end array_rule;
  !ref_list

let collect_rule_wl parameter error rule_id store_rule =
  let error, get_array =
    match AgentMap.unsafe_get parameter error rule_id store_rule with
      | error, None -> error, [||]
      | error, Some a -> error, a
  in
  let wl = index_rule_wl parameter error get_array in
  error, wl

(************************************************************************************)
(*fixpoint interation with wake_up_map*)

(*TODO*)
(*let iteration_aux parameter error handler bdu_init rule_array rule_wl_list result_bdu_array =
  let error, init_store = AgentMap.create parameter error 0 in
  (*check if rule_wl_list is empty*)
  let rec aux acc =
    match acc with
      | [] -> (*FIXME*) result_bdu_array
      | wl :: wl_tl ->
        (*get rule in wl by reference it index in the store_rule array*)
        let in_list, _, _ = wl in
        let t =
          let rec aux' acc' =
            match acc' with
              | [] -> []
              | rule_id :: tl ->
                (*get rule from array at that index*)
                let rule = Array.get rule_array rule_id in
                (*getting rule_test and rule_action from this rule*)
                let error, result =
                  AgentMap.fold2_common parameter error
                    (fun parameter error agent_id agent site_modif store_result ->
                      match agent with
                        | Ghost -> error, store_result
                        | Agent agent ->
                          let agent_type = agent.agent_name in
                          let _list, (handler, bdu_test) = 
                            common_site_bdu parameter error agent handler bdu_init
                          in
                          let _list', (handler, bdu_direct) = 
                            common_site_bdu parameter error site_modif handler bdu_init
                          in
                          (**)
                          let ref_list = ref [] in
                          let ar_list =
                            Array.iteri 
                              begin
                                fun id bdu ->
                                  (*check enable rule*)
                                  let error, handler, bdu_X =
                                    f parameter error bdu_test
                                      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu
                                  in
                                  if not (is_belong bdu_X bdu_init)
                                  then
                                    (*update*)
                                    let error, handler, bdu_update =
                                      f parameter error bdu 
                                        (boolean_mvbdu_or parameter handler error parameter bdu) bdu_direct
                                    in
                                    (*add bdu_update inside an array*)
                                    let new_result = 
                                      result_bdu_array.(id) <- bdu_update;
                                      result_bdu_array
                                    in
                                    (*push succ_wl inside wl_tl*)
                                    (*ref_list := new_result :: !ref_list*)
                                    ()
                                  else
                                    (*continue to the tail*)
                                    (*ref_list := result_bdu_array :: !ref_list*)
                                    ()

                              end result_bdu_array
                            (*!ref_list*)
                          in

                          AgentMap.set
                            parameter error agent_type result_bdu_array store_result


                    ) rule.rule_lhs.views rule.diff_direct init_store
                in

                aux' tl
          in aux' in_list
        in
        (*continue wl_tail*)
        aux wl_tl
  in aux rule_wl_list*)

(*TODO*)
let iter_aux parameter error handler bdu_init rule_id store_rule rule_wl_list bdu_array store_bdu_array =
  let error, rule_array =
    match AgentMap.unsafe_get parameter error rule_id store_rule with
      | error, None -> error, [||]
      | error, Some a -> error, a
  in
  let bdu_creation = bdu_array.(0) in
  let _ = 
    handler.print_mvbdu stdout "" bdu_creation
  in
  (*let _ =
    Array.iteri (fun i bdu ->
      handler.print_mvbdu stdout "" bdu
    ) bdu_array
  in*)
  let rec aux acc =
    match acc with
      | [] -> error, store_bdu_array
      | wl :: tl ->
        (*pop wl*)
        let error, (rule_id_op, wl_tl) = IntWL.pop parameter error wl in
        let _ = 
          match rule_id_op with
          | None -> error, store_bdu_array
          | Some rule_id' ->
            (*using rule_id return rule in store_rule array*)
            let rule = Array.get rule_array rule_id' in
            let error, result = AgentMap.fold2_common parameter error
              (fun parameter error agent_id agent site_modif store_result ->
                match agent with
                  | Ghost -> error, store_result
                  | Agent agent ->
                    let agent_type = agent.agent_name in
                    let _, (handler, bdu_test) =
                      common_site_bdu parameter error agent handler bdu_init
                    in
                    let _, (handler, bdu_direct) =
                      common_site_bdu parameter error site_modif handler bdu_init
                    in
                    let error, handler, bdu_X =
                      f parameter error bdu_creation
                        (boolean_mvbdu_and parameter handler error parameter bdu_creation) bdu_test
                    in
                    if not (is_belong bdu_X bdu_init)
                    then
                      let error, handler, bdu_update =
                        f parameter error bdu_creation
                          (boolean_mvbdu_or parameter handler error parameter bdu_creation) bdu_direct
                      in 
                      (*add*) (*TODO*)
                      let new_array =
                        bdu_array.(rule_id') <- bdu_update;
                        bdu_array
                      in
                      AgentMap.set
                        parameter
                        error
                        rule_id
                        (handler, new_array)
                        store_result
                    else
                      error, store_result
              
              ) rule.rule_lhs.views rule.diff_direct store_bdu_array
            in
            error, result           
        in
        aux tl
  in
  aux rule_wl_list
      
let fixpoint_iteration parameter error rule_id
    handler_sig store_rule rule_wl_list store_creation store_bdu_array =
  let nrules = Handler.nrules parameter error handler_sig in
  let error, (handler, bdu_init) = bdu_init parameter in
  (*add bdu_creation into result_bdu_array*)
  AgentMap.fold parameter error
    (fun parameter error agent_type (_, (handler, bdu_creation)) store_result ->
      (*REMARK:create an empty array for result: added one for bdu_creation *)
      let array = Array.make (nrules + 1) bdu_init in
      let add_result = 
        array.(0) <- bdu_creation;
        array
      in
      let error, store_result =
        AgentMap.set 
          parameter
          error
          agent_type
          (handler, add_result)
          store_result
      in
      (*TODO*)
      let error, (handler, bdu_array) = 
        match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, (handler, [||])
          | error, Some (handler, a) -> error, (handler, a)
      in
      let error, rule =
        iter_aux 
          parameter
          error 
          handler
          bdu_init
          rule_id 
          store_rule
          rule_wl_list 
          bdu_array
          store_result
      in
      error, store_result
    ) store_creation store_bdu_array
    


(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler map rule_id rule compiled store_result =
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
  (*store a list of rule in an array*)
  let error, store_rule =
    collect_rule
      parameter
      error
      handler
      rule_id
      rule
      store_result.store_rule
  in
  (*------------------------------------------------------------------------------*)
  (*working list of rule test; store index of an array rules*)
  let error, rule_wl =
    collect_rule_wl 
      parameter 
      error 
      rule_id 
      store_rule
  in
  (*------------------------------------------------------------------------------*)
  (*store a pair of wake_up_map in a list*)
  let store_succ_list =
    collect_wake_up_rule
      parameter
      error
      handler
      compiled
      map
  in
  (*------------------------------------------------------------------------------*)
  (*let error, rule_array =
    match AgentMap.unsafe_get parameter error rule_id store_rule with
      | error, None -> error, [||]
      | error, Some a -> error, a
  in*)
  let error, store_iteration =
    fixpoint_iteration
      parameter
      error
      rule_id
      handler
      store_rule
      rule_wl
      store_creation
      store_result.store_iteration
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation  = store_creation;
    store_rule      = store_rule;
    store_succ_list = store_succ_list;
    store_rule_wl   = rule_wl;
    store_iteration = store_iteration
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler compiled map rules =
  let error, init_rule      = AgentMap.create parameter error 0 in
  let error, init_creation  = AgentMap.create parameter error 0 in
  let error, init_iteration = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation  = init_creation;
      store_rule      = init_rule;
      store_succ_list = [];
      store_rule_wl   = [];
      store_iteration = init_iteration
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
            map
            rule_id
            rule.e_rule_c_rule
            compiled
            store_result
        in
        error, result
      ) rules init_bdu
  in
  error, store_results

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler map cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil map cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
