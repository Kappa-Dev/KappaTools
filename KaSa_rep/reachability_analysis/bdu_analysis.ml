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
(*compute bdu for initial state or creation action*)

(*common function of agent interface*)

(*TO BE CHECKED*)

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
(*getting side effect information:
  Given a set of rule:
  r1: A(x), C(z) -> A(x!1), C(z!1)
  r2: A(x), B(y) -> A(x!1), B(y!1)
  r3: A(x!_) -> A(x)
  ================================
  r3 is a rule that has side effect. Getting all the potential connection of r3
  A(x!_). (They are: C(z!x.A) -> C(z); B(y!x.A) -> B(y))
  - A(x!1), C(z!1) -> A(x), C(z) and
  - A(x!1), B(y!1) -> A(x), B(y)
  Put those new rules inside if they have (do the intersection) and then do the iteration 
*)
(*TODO*)




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

(*return rule type of wake up map.*)

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
  let error, empty_diff_views =
    Int_storage.Quick_Nearly_inf_Imperatif.create parameter error 0 in
  {
    rule_lhs     = empty_mixture parameter error;
    rule_arrow   = Ast.RAR;
    rule_rhs     = empty_mixture parameter error;
    diff_direct  = empty_diff_views;
    diff_reverse = empty_diff_views;
    actions      = empty_actions      
  }

(*----------------------------------------------------------------------------------*)
(*wake_up_map rule_index*)

let collect_wake_up_rule parameter error handler compiled map =
  Quark_type.Int2Set_and_map.fold_map
    (fun (rule_id_a, rule_id_b) _ current_list ->
      let l = (rule_id_a, rule_id_b) :: current_list in
      l
    ) map []

(*----------------------------------------------------------------------------------*)
(*push index of an array of rule into working list*)

let index_rule_wl parameter error array_rule =
  let wl = IntWL.empty in
  let l  = Array.length array_rule in
  let rec aux k (error, wl) =
    if k >= l
    then
      error, wl
    else
      aux (k+1) (IntWL.push parameter error k wl)
  in
    aux 1 (error, wl)

(************************************************************************************)
(*fixpoint interation with wake_up_map*)

(*build bdu for test rule*)    
let build_test parameter error agent handler bdu_init =
  let _, (handler, bdu_test) =
    common_site_bdu parameter error agent handler bdu_init
  in
  (handler, bdu_test)

(*getting a list of modified rule: var and state*)
let get_modif_list site_modif =
  Site_map_and_set.fold_map
    (fun site port current_list ->
      let state = int_of_port port in
      (site, state) :: current_list
    ) site_modif.agent_interface []

let get_test_aux parameter error handler bdu_init rule =
  let error, init_result = AgentMap.create parameter error 0 in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
      match agent with
        | Ghost -> error, store_result
        | Agent agent ->
          let agent_type = agent.agent_name in
          let handler, bdu_test =
            build_test parameter error agent handler bdu_init
          in
          (*return a list of modif site*)
          let modif_list = get_modif_list site_modif in
          (*return test and site modif of a rule*)
          AgentMap.set
            parameter
            error
            agent_type
            (bdu_test, modif_list)
            store_result
    ) rule.rule_lhs.views rule.diff_direct init_result

let get_test_direct parameter error rule agent_type handler bdu_init =
  (*get test and bdu_direct of this rule*)
  let error, store_test_direct = get_test_aux parameter error handler bdu_init rule in
  let error, (bdu_test, modif_list) =
    match AgentMap.unsafe_get parameter error agent_type store_test_direct with
      | error, None -> error, (bdu_init, [])
      | error, Some (bdu_test, modif_list) -> error, (bdu_test, modif_list)
  in
  error, (bdu_test, modif_list)

(*----------------------------------------------------------------------------------*)
(*it is an enable rule if the result of intersection is not empty*)

let comp_is_enable parameter error handler bdu_init bdu_test bdu_iter =
  let error, handler, bdu_X = 
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_iter
  in
  if not (is_belong bdu_X bdu_init)
  then
    error, true
  else
    error, false

(*----------------------------------------------------------------------------------*)
(*main computation for iteration function*)

let compute_update parameter error rule_id handler bdu_test modif_list bdu_iter bdu_array =
  (*bdu_creation intersection with test, then redefine with the list of modif*)
  let error, handler, bdu_inter_test_creation =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_iter
  in
  let error, handler, bdu_assigment =
    f parameter error bdu_inter_test_creation
      (redefine parameter error parameter handler bdu_inter_test_creation) modif_list
  in
  (*do the union of bdu_assigment and bdu_creation*)
  let error, handler, bdu_update =
    f parameter error bdu_assigment
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment) bdu_iter
  in
  let bdu_array =
    bdu_array.(rule_id) <- bdu_update;
    bdu_array
  in
  error, bdu_array

(*----------------------------------------------------------------------------------*)
(*common function return array for the pair of array *)

let get_old_array parameter error id store_result =
  let error, old =
    match AgentMap.unsafe_get parameter error id store_result with
      | error, None -> error, [||]
      | error, Some a -> error, a
  in
  error, old

let get_old_handler_array parameter error id handler store_result =
  let error, (handler, old) =
    match AgentMap.unsafe_get parameter error id store_result with
      | error, None -> error, (handler, [||])
      | error, Some (handler, a) -> error, (handler, a)
  in
  error, (handler, old)

(*----------------------------------------------------------------------------------*)
(*collect a pair of array: rule array and bdu array(the result of iteration)*)

let collect_pair_array parameter error 
    agent_type handler_sig 
    rule_id rule 
    handler bdu_init 
      bdu_creation 
      store_rule_array 
      store_bdu_array  =
  (*create a pair of array: rule_array and the result of iteration*)
  let nrules = Handler.nrules parameter error handler_sig in
  let rule_array = Array.make (nrules + 1) (empty_rule parameter error) in
  let bdu_array  = Array.make (nrules + 1) bdu_init in
  let rule_array =
    rule_array.(rule_id + 1) <- rule;
    rule_array
  in
  (*put bdu_creation at the first element of the iteration array*)
  let bdu_array =
    bdu_array.(0) <- bdu_creation;
    bdu_array
  in
  (*----------------------------------------------------------------------------------*)
  (*get old rule_array*)
  let error, old_rule_array =
    get_old_array 
      parameter 
      error 
      rule_id
      store_rule_array
  in
  let error, (handler, old_bdu_array) = 
    get_old_handler_array 
      parameter 
      error 
      agent_type
      handler
      store_bdu_array 
  in
  let new_rule_array = 
    Array.append rule_array old_rule_array
  in
  let new_bdu_array = 
    Array.append bdu_array old_bdu_array 
  in
  (*----------------------------------------------------------------------------------*)
  (*store this pair into set*)
  let error, store_rule_array =
    AgentMap.set
      parameter
      error
      rule_id
      new_rule_array
      store_rule_array
  in
  let error, store_bdu_array =
    AgentMap.set
      parameter 
      error 
      agent_type
      (handler, new_bdu_array)
      store_bdu_array
  in
  error, (store_rule_array, store_bdu_array)
  
(*----------------------------------------------------------------------------------*)
(*add succ(h) into the tail of a working list*)

let add_succ_wl parameter error rule_id succ_list wl_tl =
  let rec aux acc (error, wl) =
    match acc with
      | [] -> error, wl
      | (i, i') :: tl ->
        if rule_id != i
        then
          error, wl
        else
          aux tl (IntWL.push parameter error i' wl)
  in
  aux succ_list (error, wl_tl)

(*----------------------------------------------------------------------------------*)
(*auxiliary function for fixpoint iteration function*)

let fixpoint_aux parameter error 
    agent_type 
    rule 
      handler bdu_init 
        rule_array 
        wl
        succ_list
        store_bdu_array =
  let error, (handler, bdu_array) = 
    get_old_handler_array
      parameter
      error 
      agent_type
      handler 
      store_bdu_array 
  in
  let rec aux acc_wl (error, bdu_remanent_array) =
    (*if work list is empty then return bdu_remanent_array*)
    if IntWL.is_empty acc_wl
    then error, bdu_remanent_array
    else
      (*pop the first element in work list*)
      let error, (rule_id_op, wl_tl) = IntWL.pop parameter error acc_wl in
      match rule_id_op with
        | None -> error, bdu_remanent_array
        | Some rule_id ->
          (*get rule type from the array*)
          let rule = Array.get rule_array rule_id in
          (*get bdu_test, and a list of action*)
          let error, (bdu_test, modif_list) =
            get_test_direct
              parameter
              error
              rule
              agent_type 
              handler 
              bdu_init
          in
          (*build a list of type List_sig.list for modif_list*)
          let error, (handler, list_a) =
            List_algebra.build_list
              (list_allocate parameter)
              error
              parameter
              handler
              modif_list
          in
          (*-------------------------------------------------------------------*)
          let error, iterate_array =
            Array.fold_left (fun _ bdu_iter ->
              (*is not enable*)
              let error, is_enable =
                comp_is_enable 
                  parameter 
                  error
                  handler 
                  bdu_init
                  bdu_test
                  bdu_iter 
              in
              (*call iterate function*)
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
                    bdu_iter
                    bdu_array
                in
                (*add succ_h*)
                let error, wl_succ_tl = 
                  add_succ_wl 
                    parameter
                    error 
                    rule_id
                    succ_list
                    wl_tl
                in
                aux wl_succ_tl (error, bdu_update_array)
              else
                aux wl_tl (error, bdu_remanent_array)
            ) (error, [||]) bdu_remanent_array
          in
          error, iterate_array
  in aux wl (error, bdu_array)

(*----------------------------------------------------------------------------------*)

let fixpoint parameter error 
    handler_sig
    rule_id rule
        succ_list
          store_creation 
          store_pair_array =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold parameter error
    (fun parameter error agent_type (_, (handler, bdu_creation)) store_pair_array ->
      let store_rule_array, store_bdu_array = store_pair_array in
      let error, (store_rule_array, store_bdu_array) =
        collect_pair_array
          parameter error
          agent_type handler_sig 
          rule_id rule 
              handler bdu_init 
                bdu_creation
                store_rule_array 
                store_bdu_array
      in
      (*get rule_array to be used in the iteration*)
      let error, old_rule_array = 
        get_old_array 
          parameter 
          error 
          rule_id 
          store_rule_array
      in
      (*build an index of rule and add them into a working list*)
      let error, wl = 
        index_rule_wl 
          parameter
          error
          old_rule_array 
      in
      (*call iteration function*)
      let error, bdu_iterate_array =
        fixpoint_aux
          parameter error
          agent_type
          rule
            handler bdu_init
              old_rule_array
              wl
              succ_list
              store_bdu_array
      in
      (*set*)
      let error, store_bdu_array =
        AgentMap.set
          parameter
          error 
          agent_type 
          (handler, bdu_iterate_array) 
          store_bdu_array
      in
      error, (store_rule_array, store_bdu_array)
    ) store_creation store_pair_array

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
  (*fixpoint iteration with wake_up_map*)
  let error, store_iteration =
    fixpoint parameter error
      handler
      rule_id rule
          store_succ_list
            store_creation
            store_result.store_iteration
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_creation  = store_creation;
    store_succ_list = store_succ_list;
    store_iteration = store_iteration
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler compiled map rules =
  let error, init_creation  = AgentMap.create parameter error 0 in
  let error, init_rule      = AgentMap.create parameter error 0 in
  let error, init_iteration = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_creation  = init_creation;
      store_succ_list = [];
      store_iteration = init_rule, init_iteration
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
  let parameter = Remanent_parameters.update_prefix parameter "" in
  let error, result = scan_rule_set parameter error handler cc_compil map cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
