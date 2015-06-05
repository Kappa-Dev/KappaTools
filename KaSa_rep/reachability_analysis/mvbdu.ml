 (**
  * mvbdu.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 3th of June
  * Last modification: 
  * 
  * TO BE FILLED
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)
(*TYPES*)
(*type of agent variable and site variable: plus and minus*)

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif

type site_state = int
type var_id     = int
type pair_var   = (var_id * site_state) 

type store_var =
    {
      store_site_lhs    : int list AgentMap.t;
      store_pair_minus  : pair_var list AgentMap.t;
      store_pair_plus   : pair_var list AgentMap.t
    }

(************************************************************************************)

let sprintf_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
    acc := !acc ^
      if i <> 0
      then Printf.sprintf "; %i" x
      else Printf.sprintf "%i" x
  ) l;
  !acc ^ "}"
    
let print_list l =
  let output = sprintf_list l in
  Printf.fprintf stdout "%s\n" output

let int_of_port port = port.Cckappa_sig.site_state.Cckappa_sig.min

(************************************************************************************)
(*RULE - VAR*)

let scan_rule parameter error kappa_handler rule store_result =
(*get the information of site in the lhs *)
  let error, store_site_lhs =
    AgentMap.fold
      parameter
      error
      (fun parameter error agent_id agent store_result ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_result
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a list of site in the lhs in an interface*)
            let site_list =
              Cckappa_sig.Site_map_and_set.fold_map
                (fun site _ current_list ->
                  let site_list = site :: current_list in
                  site_list
                )
                agent.Cckappa_sig.agent_interface []
            in
            (*get old result list*)
            let error, out_old =
              AgentMap.unsafe_get
                parameter
                error
                agent_type
                store_result
            in
            let old_list =
              match out_old with
                | None -> []
                | Some l -> l
            in
            let new_list = List.concat [site_list; old_list] in
            (*store*)
            AgentMap.set
              parameter
              error
              agent_type
              new_list
              store_result
      )
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      store_result.store_site_lhs
  in
  (*store result*)
  error,
  {
    store_result with
    store_site_lhs = store_site_lhs
  }

(*-----------------------------------------------------------------------------------*)

let scan_mixture_in_var bool parameter error kappa_handler var_id mixture
    result_rules =
  (*check if it is an agant variable or a site variable *)
  let store_pair_result =
    if bool
    then
      result_rules.store_pair_plus
    else
      result_rules.store_pair_minus
  in
  (*compute what is tested in the mixture*)
  let error, store_pair_result =
    AgentMap.fold
      parameter
      error
      (fun parameter errror agent_id agent store_pair_result ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_pair_result
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*---------------------------------------------------------------------------*)
            (*get a list later build bdu, it is a pair (variable, site_state) *)
            let pair_var_state_list =
              Cckappa_sig.Site_map_and_set.fold_map
                (fun site port current_list ->
                  let site_state = int_of_port port in
                  let pair_list = (var_id, site_state) :: current_list in
                  let _ =
                    Printf.fprintf stdout 
                      "var_id:%i:agent_type:%i:site_type:%i:site_state:%i\n"
                      var_id agent_type site site_state
                  in
                  pair_list
                )
                agent.Cckappa_sig.agent_interface []
            in
            (*---------------------------------------------------------------------------*)
            (*get the old information*)
            let error, out_old =
              AgentMap.unsafe_get
                parameter
                error
                agent_type
                store_pair_result
            in
            let old_result_list =
              match out_old with
                | None -> []
                | Some p -> p
            in
            let new_pair = List.concat [pair_var_state_list; old_result_list] in
            (*store*)
            AgentMap.set
              parameter
              error
              agent_type
              new_pair
              store_pair_result
      )
      mixture.Cckappa_sig.views
      result_rules.store_pair_plus (*FIXME*)
  in
  (*store result*)
  if bool
  then
    error, 
    {
      result_rules with
        store_pair_plus  = store_pair_result
    }
  else
    error,
    {
      result_rules with
        store_pair_minus  = store_pair_result
    }

(*-----------------------------------------------------------------------------------*)
let scan_pos_mixture = scan_mixture_in_var true
let scan_neg_mixture = scan_mixture_in_var false

let scan_var parameter error kappa_handler var_id var result_rules =
  (*getting a list variable in a mixture *)
  let rec aux error var list_pos list_neg =
    match var with
      | Ast.KAPPA_INSTANCE (mixture) -> error, mixture :: list_pos, list_neg
      | _ -> error, list_pos, list_neg
  in
  let error, list_pos, list_neg = aux error var [] [] in
  (*get list_pos*)
  let error, result_rules =
    List.fold_left
      (fun (error, result_rules) mixture ->
        scan_pos_mixture
          parameter
          error
          kappa_handler
          var_id
          mixture
          result_rules
      )
      (error, result_rules)
      list_pos
  in
  (*get list_neg*)
  let erro, result_rules =
    List.fold_left (fun (error, result_rules) mixture ->
      scan_neg_mixture
        parameter
        error
        kappa_handler
        var_id
        mixture
        result_rules
    )
      (error, result_rules)
      list_neg
  in
  error, result_rules

(************************************************************************************)
(*RULES - VARS*)

let create_init parameter error n_agents = AgentMap.create parameter error n_agents 

let scan_rule_set parameter error kappa_handler rules =
  let n_agents = kappa_handler.Cckappa_sig.nagents in
  let error, init_lhs = create_init parameter error n_agents in
  let error, init_var_plus = create_init parameter error n_agents in
  let error, init_var_minus = create_init parameter error n_agents in
  (*-----------------------------------------------------------------------------------*)
  let init_store_var =
    {
      store_site_lhs = init_lhs;
      store_pair_plus = init_var_plus;
      store_pair_minus = init_var_minus
    }
  in
  let error, store_result =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error rule_id rule store_result ->
        (*let _ = Printf.fprintf stdout "rule_id:%i\n" rule_id in*)
        scan_rule
          parameter
          error
          kappa_handler
          rule.Cckappa_sig.e_rule_c_rule
          store_result
      )
      rules
      init_store_var     
  in
  error, store_result

(*-----------------------------------------------------------------------------------*)

let scan_var_set parameter error kappa_handler vars result_rules =
  let error, store_var_set =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error var_id var store_result ->
        let (_, (var, _)) = var.Cckappa_sig.e_variable in
        let _ = Printf.fprintf stdout "var_id:%i\n" var_id in
        scan_var
          parameter
          error
          kappa_handler
          var_id
          var
          store_result
      )
      vars
      result_rules
  in
  (*---------------------------------------------------------------------------*)
  (*build bdu here*)
  let error, init = AgentMap.create parameter error 0 in
  let error, store_bdu =
    AgentMap.fold
      parameter
      error
      (fun parameter error agent_type pair_list store_result ->
        (*get a list of pair site (var_id,site_state)*)
        let error, out =
          AgentMap.unsafe_get
            parameter
            error
            agent_type
            store_var_set.store_pair_plus
        in
        let pair_list =
          match out with
            | None -> []
            | Some l -> l
        in
        (*---------------------------------------------------------------------------*)
        (*compute BDU*)
        let remanent_bdu = (Sanity_test.remanent parameter) in        
        let error = remanent_bdu.Sanity_test_sig.error in
        let allocate = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
        let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
          remanent_bdu.Sanity_test_sig.mvbdu_handler
        in
        let a_val = Mvbdu_sig.Leaf true in
        let b_val = Mvbdu_sig.Leaf false in
        (*build bdu_a from a_val*)
        let error, (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler),
          a', a'_id, a'', a''_id =
          Mvbdu_test.build_without_and_with_compressing
            allocate
            error
            handler
            a_val
            a_val
        in
        (*define function f getting handler and its value *)
        let f x y =
          match x y with
            | error, (handler, Some a) -> error, handler, a
            | error, (handler, None) -> 
              let error, a =
                Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> a')
              in
              error, handler, a 
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
        (*compute bdu from bdu_a and bdu_b*)
        let error, handler, bmvbdu_true0 =
          f (Boolean_mvbdu.boolean_mvbdu_true parameter handler error) parameter
        in
        let error, handler, bmvbdu_false0 =
          f (Boolean_mvbdu.boolean_mvbdu_false parameter handler error) parameter
        in
        (*build a list of bdu from a list of pair (var_id, site_state) computed above*)
        let error, (handler, list_a) =
          List_algebra.build_list
            (Boolean_mvbdu.list_allocate parameter)
            error
            parameter
            handler
            pair_list
        in
        (*compute mvbdu of redefine of list_a*)
        let error, handler, mvbdu_redefine =
          f (Boolean_mvbdu.redefine parameter error parameter handler a') list_a
        in
        (*PRINT bdu redefine*)
        let error =
          Boolean_mvbdu.print_boolean_mvbdu
            error
            (Remanent_parameters.update_prefix parameter "mvbdu_redefine:")
            mvbdu_redefine
        in
        (*PRINT memoization tables*)
        let error =
          Boolean_mvbdu.print_memo
            error
            handler
            (Remanent_parameters.update_prefix parameter "Memoization tables:")
        in
        (*store bdu handler*)
        let store_pair_remanent =
          {
            remanent_bdu with
              Sanity_test_sig.error = error;
              Sanity_test_sig.mvbdu_handler = handler
          },
          ("Mvbdu.001", fun remanent ->
            let b = Mvbdu_core.mvbdu_equal a'' b'' in
            remanent, b, None) ::
            (List.map (fun (a, b, c) ->
              a,
              fun remanent -> Mvbdu_sanity.test remanent c b)
               [
                 "Mvbdu.002", a', (true, true, true)
               ])
        in
        let (remanent, bdu_test_list) = store_pair_remanent in
        let store_bdu_list =
          List.fold_left (fun remanent (s, p) ->
            Sanity_test.test remanent p s
          )
            remanent
            bdu_test_list
        in
        (*---------------------------------------------------------------------------*)
        (*store result of list bdu*)
        AgentMap.set
          parameter
          error
          agent_type
          store_bdu_list
          store_result
      )
      store_var_set.store_pair_plus
      init
  in
  error, store_bdu

(************************************************************************************)
(*MAIN*)

let bdu_test parameter error kappa_handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result_rules = scan_rule_set parameter error kappa_handler 
    cc_compil.Cckappa_sig.rules in
  scan_var_set parameter error kappa_handler cc_compil.Cckappa_sig.variables result_rules
