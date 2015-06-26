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

open Covering_classes
open Covering_classes_type
open Printf
open Cckappa_sig
open Int_storage

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*TYPES*)

type bdu =  bool Mvbdu_sig.mvbdu

type bdu_handler  =
    (Boolean_mvbdu.memo_tables,
     (bool Mvbdu_sig.cell, bool Mvbdu_sig.mvbdu) Boolean_mvbdu.D_mvbdu_skeleton.dictionary,
     (int List_sig.cell, int List_sig.list) Boolean_mvbdu.D_list_skeleton.dictionary,
     bool,
     int) Memo_sig.handler * bool Mvbdu_sig.mvbdu 

type bdu_pair_handler_list  = 
    ((Boolean_mvbdu.memo_tables, 
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.list_dic,
     bool,
     int) Memo_sig.handler * bdu) list 

type bdu_pair_handler  =
    ((Boolean_mvbdu.memo_tables, 
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.list_dic,
     bool, 
     int) Memo_sig.handler * bdu)

type bdu_analysic =
    {
      store_bdu_covering_classes : (bdu_pair_handler * int) list AgentMap.t;
      store_bdu_test             : (state_list * bdu_pair_handler_list) AgentMap.t;
      store_bdu_creation         : (state_list * bdu_pair_handler) AgentMap.t;
      store_bdu_remanent         : bdu_pair_handler;
      store_enabled_rules        : (bool * bdu_handler) list AgentMap.t
    }

(************************************************************************************)    
(*compute BDU for each rule in a covering class*)

let rec print_a_list (l: int List_sig.list) =
  fprintf stdout "list_id:%i:" l.List_sig.id;
  let v = l.List_sig.value in
  match v with
    | List_sig.Empty -> print_string "\n"
    | List_sig.Cons precell ->
      Printf.fprintf stdout "value:[";
      print_precell precell
      
and print_precell p =
  fprintf stdout "site_type:%i:site_state:%i]\n" 
    p.List_sig.variable  p.List_sig.association;
  print_a_list p.List_sig.tail

(*---------------------------------------------------------------------------*)
(*Build bdu from a pair of list (site, state)*)

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
  (*define function f*)
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> a')
        in error, handler, a
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
  (*compute bdu relation*)
  (*check the Leaf is true*)
  let error, handler, bmvbdu_true0 =
    f (Boolean_mvbdu.boolean_mvbdu_true parameter handler error) parameter
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
  let error, handler, mvbdu_redefine =
    f (Boolean_mvbdu.redefine parameter error parameter handler a') list_a
  in
  (*---------------------------------------------------------------------------*)
  (*return redefine*)
  error, (handler, mvbdu_redefine)

(************************************************************************************)    
(*Build init*)

let bdu_remanent parameter =
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
(*Union between two bdus*)

open Mvbdu_sig

let rec union_bdu parameter mvbdu_x mvbdu_y =
  let remanent_bdu = Sanity_test.remanent parameter in
  let error = remanent_bdu.Sanity_test_sig.error in
  let allocate = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  let handler = remanent_bdu.Sanity_test_sig.mvbdu_handler in
  match mvbdu_x.value, mvbdu_y.value with
    | Leaf a, Leaf b ->
      if a = b
      then
        Mvbdu_test.build_without_and_with_compressing
          allocate
          error
          handler
          (Leaf a)
          (Leaf a)
      else
        Mvbdu_test.build_without_and_with_compressing
          allocate
          error
          handler
          (Leaf a)
          (Leaf b)
    | Node x, Leaf a 
    | Leaf a, Node x ->
      (*Build node*)
      Mvbdu_test.build_without_and_with_compressing
        allocate
        error
        handler
        (Leaf a)
        (Node x)
    | Node x, Node y ->
      let b =
        x.variable = y.variable &&
        x.upper_bound = y.upper_bound
      in
      (*if node x = node y*)
      (*let b = Mvbdu_core.mvbdu_equal (Node x) (Node y) in*)
      if b
      then
        (*Build node*)
        Mvbdu_test.build_without_and_with_compressing
          allocate
          error
          handler
          (Node 
             {
               variable = x.variable;
               upper_bound = x.upper_bound;
               branch_true = x.branch_true.id;
               branch_false = x.branch_false.id
             }
          )
          (Node x)
      else (*FIXME*)
        (*Build node*)
        Mvbdu_test.build_without_and_with_compressing
          allocate
          error
          handler
          (Node 
             {
               variable = x.variable;
               upper_bound = x.upper_bound;
               branch_true = x.branch_true.id;
               branch_false = x.branch_false.id
             }
          )
          (Node 
             {
               variable = y.variable;
               upper_bound = y.upper_bound;
               branch_true = y.branch_true;
               branch_false = y.branch_false
             }
        )

(************************************************************************************)
(*intersection between two bdus*)

let inter_bdu parameter mvbdu_x mvbdu_y =
  let remanent_bdu = Sanity_test.remanent parameter in
  let error = remanent_bdu.Sanity_test_sig.error in
  let allocate = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  let handler = remanent_bdu.Sanity_test_sig.mvbdu_handler in
  let a_val = Mvbdu_sig.Leaf false in
  match mvbdu_x.value, mvbdu_y.value with
    | Leaf a, Leaf b ->
      if a = b
      then
        let error, handler, a', _, _, _ =
          Mvbdu_test.build_without_and_with_compressing
            allocate
            error
            handler
            (Leaf a)
            (Leaf a)
        in
        error, (handler, a')
      else
        let error, handler, a', _, _, _ =
          Mvbdu_test.build_without_and_with_compressing
            allocate
            error
            handler
            a_val
            a_val
        in
        error, (handler, a')
    | Leaf _, Node _ | Node _, Leaf _->
      let error, handler, a', _, _, _ =
        Mvbdu_test.build_without_and_with_compressing
          allocate
          error
          handler
          a_val
          a_val
      in
      error, (handler, a')
    | Node x, Node y ->
       let b =
        x.variable = y.variable &&
        x.upper_bound = y.upper_bound
       in
       if b
       then
         let error, handler, a', _, _, _ =
           Mvbdu_test.build_without_and_with_compressing
             allocate
             error
             handler
             (Node 
                {
                  variable = x.variable;
                  upper_bound = x.upper_bound;
                  branch_true = x.branch_true.id;
                  branch_false = x.branch_false.id
                }
             )
             (Node x)
         in
         error, (handler, a')
       else
         let error, handler, a', _, _, _ =
           Mvbdu_test.build_without_and_with_compressing
             allocate
             error
             handler
             a_val
             a_val
         in
         error, (handler, a')

(************************************************************************************)    
(*compute enabled rule: it is an intersection between
  each BDU in a covering class and each BDU test (BDU at each rule). 
  - BDU is OK when the result is not empty, vice versa.
 TODO*)

let enable parameter error bdu_test bdu_false =
  let bool = Mvbdu_sanity.safety_equal_mvbdu bdu_test bdu_false in
  if bool
  then
    true
  else
    false

let enabled_rules parameter error kappa_handler rules bdu_test_type store_result =
  let error, (handler, bdu_false) = bdu_remanent parameter in
  let error, result_enable =
  AgentMap.fold parameter error
    (fun parameter error agent_type pair_bdu store_result ->
      let (_, bdu_list) = pair_bdu in
      let result =
        let rec aux acc =
          match acc with
            | [] -> []
            | (handler, bdu_test) :: tl ->
              let error, result =
                inter_bdu parameter bdu_test bdu_false in
              let enable_rule =
                enable 
                  parameter 
                  error 
                  bdu_test 
                  bdu_false 
              in
              (enable_rule, result) :: aux tl
        in aux bdu_list
      in
      let old_list =
        match AgentMap.unsafe_get
          parameter error agent_type store_result 
        with
          | error, None -> []
          | error, Some s -> s
      in
      let new_list = List.concat [result; old_list] in
      AgentMap.set
        parameter
        error
        agent_type
        new_list
        store_result
    ) 
    bdu_test_type store_result 
  in error, result_enable

(************************************************************************************)
(*compute bdu in covering classes*)

let build_bdu_covering_classes parameter error handler rules store_bdu =
  let error, remanent_type = scan_rule_set_cv parameter error handler rules in
  AgentMap.fold parameter error 
    (fun parameter error agent_type remanent init ->
      let error, bdu =
        let l =
          Dictionary_of_Covering_class.fold
            (fun value _ class_id current_list ->
              let error, bdu = build_bdu parameter error value in
              let l = (bdu, class_id) :: current_list in
              l
            ) remanent.store_dic []
        in
        (*Store bdu of covering class*)
        AgentMap.set
          parameter
          error
          agent_type
          l
          init
      in
      error, bdu
    ) remanent_type store_bdu

(************************************************************************************)    
(*compute bdu for each rule in the lhs*)

let collect_test_bdu parameter error views store_bdu =
  AgentMap.fold
    parameter
    error
    (fun parameter error agent_id agent store_bdu ->
      match agent with
        | Ghost -> error, store_bdu
        | Agent agent ->
          let agent_type = agent.agent_name in
          let pair_list = 
            Site_map_and_set.fold_map
              (fun site port current_list ->
                let state = int_of_port port in
                (site, state) :: current_list
              ) agent.agent_interface []
          in
          (*build bdu*)
          let error, bdu =
            build_bdu
              parameter 
              error
              pair_list
          in          
          let error, old_pair =
            match AgentMap.unsafe_get parameter error
              agent_type store_bdu with
                | error, None -> error, ([], [])
                | error, Some (l,bdu) -> error, (l,bdu)
          in
          let (old_list, old_bdu) = old_pair in
          let new_pair = List.concat [pair_list; old_list] in
          let new_bdu = bdu :: old_bdu in
          AgentMap.set
            parameter
            error
            agent_type
            (new_pair, new_bdu)
            store_bdu
    ) views store_bdu

(************************************************************************************)    
(*compute bdu for initial state or creation actions*)

let collect_creation parameter error viewsrhs creation store_creation =
  List.fold_left (fun (error, store_creation) (agent_id, agent_type) ->
    let error, agent =
      AgentMap.get
        parameter
        error
        agent_id
        viewsrhs
    in
    match agent with
      | None -> warn parameter error (Some "line 772") Exit store_creation
      | Some Ghost -> error, store_creation
      | Some Agent agent ->
        (*get the site and state on the rhs*)
        let site_list =
          Site_map_and_set.fold_map 
            (fun site port current_list ->
              let state = int_of_port port in
              let list_a = (site, state) :: current_list in
              list_a
            )
            agent.agent_interface []
        in
        let error, old_list =
          match AgentMap.unsafe_get
            parameter
            error
            agent_type
            store_creation
          with
              | error, None -> error, []
              | error, Some (s,_) -> error, s
        in
        let new_site = List.concat [site_list; old_list] in
        let error, bdu = build_bdu parameter error new_site in
        let error, store_creation =
          AgentMap.set
            parameter
            error
            agent_type
            (new_site, bdu)
            store_creation
        in
        error, store_creation
  )
    (error, store_creation)
    creation

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule rules store_result =
  (*------------------------------------------------------------------------------*)
  (*compute bdu covering classes*)
  let error, store_bdu_covering_classes =
    build_bdu_covering_classes
      parameter
      error
      handler
      rules
      store_result.store_bdu_covering_classes
  in
  (*------------------------------------------------------------------------------*)
  (*compute bdu test*)
  let error, store_bdu_test =
    collect_test_bdu
      parameter
      error
      rule.rule_lhs.views
      store_result.store_bdu_test
  in
  (*------------------------------------------------------------------------------*)
  (*compute creation*)
  let error, store_bdu_creation =
    collect_creation
      parameter
      error
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_bdu_creation
  in
  (*------------------------------------------------------------------------------*)
  (*init bdu*)
  let error, store_bdu_remanent =
    bdu_remanent parameter
  in
  (*------------------------------------------------------------------------------*)
  (*enabled rules*)
  let error, store_enabled_rules =
    enabled_rules
      parameter
      error
      handler
      rules
      store_bdu_test
      store_result.store_enabled_rules
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_bdu_covering_classes = store_bdu_covering_classes;
    store_bdu_test = store_bdu_test;
    store_bdu_creation = store_bdu_creation;
    store_bdu_remanent = store_bdu_remanent;
    store_enabled_rules = store_enabled_rules
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init_covering_class = AgentMap.create parameter error 0 in
  let error, init_test           = AgentMap.create parameter error 0 in
  let error, init_creation       = AgentMap.create parameter error 0 in
  let error, bdu_remanent        = bdu_remanent parameter in
  let error, init_enabled        = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_bdu_covering_classes = init_covering_class;
      store_bdu_test             = init_test;
      store_bdu_creation         = init_creation;
      store_bdu_remanent         = bdu_remanent;
      store_enabled_rules        = init_enabled
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
(*PRINT*)

let print_bdu_covering_class parameter error bdu =
  AgentMap.print error
    (fun error parameter bdu_list ->
      let _ =
        let rec aux acc =
          match acc with
            | [] -> ()
            | (bdu, class_id) :: tl ->
              fprintf stdout "class_id:%i\n" class_id;
              let (handler, mvbdu_redefine) = bdu in
              let _ = handler.Memo_sig.print_mvbdu stdout "" mvbdu_redefine in
              aux tl
        in
        aux bdu_list
      in
      error
    )
    parameter
    bdu

(*------------------------------------------------------------------------------*)

let print_result_test parameter error bdu_test =
  AgentMap.print error
    (fun error parameter pair ->
      let _ =
        let (l, handler_list) = pair in
        let rec aux acc =
          match acc with
            | [] -> ()
            | bdu :: tl ->
              let (handler, mvbdu_redefine) = bdu in
              let _ = handler.Memo_sig.print_mvbdu stdout "" mvbdu_redefine in
              aux tl
        in
        aux handler_list in
      error
    )
    parameter bdu_test

(*------------------------------------------------------------------------------*)

let print_result_creation parameter error result_creation =
  AgentMap.print
    error
    (fun error parameter p ->
      let _ =
        let (creation_list, bdu) = p in
        let _ = Print_covering_classes.print_pair creation_list in
        let (handler, mvbdu_redefine) = bdu in
        handler.Memo_sig.print_mvbdu stdout "" mvbdu_redefine
      in
      error
    )
    parameter
    result_creation

let print_enabled_rules parameter error result_enable =
  AgentMap.print error
    (fun error parameter pair_enable_list ->
      let _ = 
      let rec aux acc =
          match acc with
            | [] -> ()
            | (b, bdu) :: tl ->
              Mvbdu_sanity.print_flag stdout b; print_newline();
              let handler, mvbdu = bdu in
              let _ = handler.Memo_sig.print_mvbdu stdout "" mvbdu in (*FIXME*)
              aux tl
        in
        aux pair_enable_list
      in
      error
    )
    parameter result_enable

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let error =
    fprintf stdout "BDU of creation rules\n";
    print_result_creation parameter error result.store_bdu_creation
  in
  let error = 
    fprintf stdout "BDU of potential dependencies between sites:\n";
    print_bdu_covering_class parameter error result.store_bdu_covering_classes
  in
  let error =
    fprintf stdout "BDU of test rules\n";
    print_result_test parameter error result.store_bdu_test 
  in
  let error =
    fprintf stdout "BDU of enabled rules\n";
    print_enabled_rules parameter error result.store_enabled_rules
  in
  error

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
