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

(************************************************************************************)
(*compute the list of site in the lhs at each rule *)
let scan_rule parameters error remanent_bdu rule store_result =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameters
    error
    (fun parameters error agent_id agent store_result ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*Getting a list of site in the lhs*)
          let site_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_list ->
                let variable = site in (*FIXME*)
                let site_list = (variable, site) :: current_list in
                site_list
              )
              agent.Cckappa_sig.agent_interface []
          in
          (*compute bdu here?*)(*????*)
          let error = remanent_bdu.Sanity_test_sig.error in
          let allocate = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
          let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
            remanent_bdu.Sanity_test_sig.mvbdu_handler
          in
          let a_val = Mvbdu_sig.Leaf true in
          let b_val = Mvbdu_sig.Leaf false in 
          let error,(handler:('b,'a,'c,bool,int) Memo_sig.handler),a',a'_id,a'',a''_id =
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
                  Exception.warn parameters error (Some " ") (Some "") Exit (fun _ -> a')
                in
                error, handler, a
          in
          let error,handler,b',b'_id,b'',b''_id =
            Mvbdu_test.build_without_and_with_compressing
              allocate
              error
              handler
              b_val
              b_val
          in 
          (*compute handler true and false*)
          let error, handler, bmvbdu_true0 =
            f (Boolean_mvbdu.boolean_mvbdu_true parameters handler error) parameters
          in
          let error, handler, bmvbdu_false0 =
            f (Boolean_mvbdu.boolean_mvbdu_false parameters handler error) parameters
          in
          (*build list*)
          let error, (handler, list_a) =
            List_algebra.build_list
              (Boolean_mvbdu.list_allocate parameters)
              error
              parameters
              handler
              site_list (*FIXME*)
          in
          (*print mvbdu*)
          let error, handler, mvbdu =
            f (Boolean_mvbdu.redefine parameters error parameters handler a') list_a
          in
          let error =
            Boolean_mvbdu.print_boolean_mvbdu 
              error 
              (Remanent_parameters.update_prefix parameters "mvbdu:")
              mvbdu
          in
          (*print memozation tables*)
          let error =
            Boolean_mvbdu.print_memo
              error
              handler
              (Remanent_parameters.update_prefix parameters "Memoization tables:")
          in
          let store_remanent = (*FIXME*)
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
                   "Mvbdu.002", a', (true, true, true) (*???*)
                 ])
          in
          let remanent, bdu_test_list = store_remanent in (*FIXME*)
          let store_test_list =
            List.fold_left (fun remanent (s, p) -> Sanity_test.test remanent p s)
              remanent
              bdu_test_list
          in
          (*store the result of bdu with agent_type here*)(*TODO*)
          (*let error, old =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameters
              error
              agent_type
              store_result
          in
          let old_list =
            match old with
              | None -> Sanity_test_sig.initial_remanent
              | Some l -> l
          in
          let new_list = List.concat [store_test_list; old_list] in*)
          Int_storage.Nearly_inf_Imperatif.set
            parameters
            error
            agent_type
            store_test_list
            store_result
    )
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
    store_result
  
(*in a set of rules*)
let scan_rules parameter error rules =
  let error, init = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, store_result =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error rule_id rule store_result ->
        let _ = Printf.fprintf stdout "rule_id:%i\n" rule_id in
        scan_rule
          parameter
          error
          (Sanity_test.remanent parameter)
          rule.Cckappa_sig.e_rule_c_rule
          store_result
      )
      rules
      init     
  in
  error, store_result

(*compute bdu, taking the list of lhs *)

let bdu_test parameter error cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rules parameter error cc_compil.Cckappa_sig.rules in
  error, result
