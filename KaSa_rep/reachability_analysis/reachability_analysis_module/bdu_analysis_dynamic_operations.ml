 (**
  * bdu_analysis_dynamic_operations.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 20th of January
  * Last modification:
  *
  * Bdu analysis dynamic operations
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Bdu_analysis_dynamic_type
open Bdu_analysis_static_type
open Covering_classes
open Covering_classes_type
open Cckappa_sig
open Int_storage

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu analysis dynamic operations") message exn
    (fun () -> default)

let trace = false

(************************************************************************************)
(*contact map*)

let compute_contact_map_full parameter error handler_kappa =
  let add_link (agent, site, state) set store_result =
    let error, old =
      match Int2Map_CM_state.Map.find_option_without_logs parameter error
        (agent, site, state) store_result
      with
      | error, None -> error, Set_triple.Set.empty
      | error, Some s -> error, s
    in
    let error, union_set = Set_triple.Set.union parameter error old set in
    let error, add_map =
      Int2Map_CM_state.Map.add_or_overwrite parameter error (agent, site, state)
        union_set store_result
    in
    error, add_map
  in
  (*-----------------------------------------------------------------------*)
  (*folding this solution with the information in dual*)
  let error, store_result =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state') store_result ->
        let error, set =
          Set_triple.Set.add_when_not_in parameter error
            (agent', site', state') Set_triple.Set.empty
        in
        let error, store_result =
          add_link (agent, site, state) set store_result
	in
	error, store_result
      ) handler_kappa.dual Int2Map_CM_state.Map.empty
  in
  error, store_result

(*****************************************************************************************)
(*contact map without state information: this computation consider both
  binding in the lhs and rhs.
  For instance:

  r1: A(x), B(x) -> A(x!1), B(x!1)
  r2: A(y!1), C(x!1) -> A(y), C(x)

  The result is:
  - A bond to B; B bond to A
  and
  - A bond to C; C bond to A.
*)

(************************************************************************************)
(*syntactic contact map without initial state*)

let compute_contact_map parameter error rule store_result =
  let add_link set1 set2 store_result =
    let error, old =
      match Int2Map_CM_Syntactic.Map.find_option_without_logs
        parameter error set1 store_result
      with
      | error, None -> error, Set_triple.Set.empty
      | error, Some s -> error, s
    in
    let error, union_set = Set_triple.Set.union parameter error set2 old in
    let error, store_result =
      Int2Map_CM_Syntactic.Map.add_or_overwrite parameter error set1 union_set store_result
    in
    error, store_result
  in
  (*---------------------------------------------------------------------*)
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
    let agent_index1 = site_add1.agent_index in
    let agent_type1 = site_add1.agent_type in
    let site1 = site_add1.site in
    let agent_type2 = site_add2.agent_type in
    let site2 = site_add2.site in
    let agent_index2 = site_add2.agent_index in
    (*---------------------------------------------------------------------*)
    (*find state for each agent*)
    let error, agent1 =
      match AgentMap.get parameter error agent_index1 rule.rule_rhs.views
      with
      | error, None -> warn parameter error (Some "line 141") Exit Ghost
      | error, Some agent -> error, agent
    in
    let error, agent2 =
      match AgentMap.get parameter error agent_index2 rule.rule_rhs.views
      with
      | error, None -> warn parameter error (Some "line 147") Exit Ghost
      | error, Some agent -> error, agent
    in
    (*---------------------------------------------------------------------*)
    let error, set1 =
      match agent1 with
      | Ghost | Unknown_agent _
      | Dead_agent _ -> warn parameter error (Some "line 102") Exit Set_triple.Set.empty
      | Agent agent1 ->
        let error, state1 =
          match Site_map_and_set.Map.find_option_without_logs parameter error
            site1 agent1.agent_interface
          with
          | error, None -> warn parameter error (Some "line 105") Exit 0
          | error, Some port ->
            if port.site_state.max > 0
          then error, port.site_state.max
            else warn parameter error (Some "line 109") Exit 0
        in
        let error, set1 =
          Set_triple.Set.add_when_not_in parameter error
            (agent_type1, site1, state1) Set_triple.Set.empty
        in
        error, set1
    in
    (*---------------------------------------------------------------------*)
    let error, set2 =
      match agent2 with
      | Ghost | Unknown_agent _
      | Dead_agent _ -> warn parameter error (Some "line 140") Exit Set_triple.Set.empty
      | Agent agent2 ->
        let error, state2 =
          match Site_map_and_set.Map.find_option_without_logs parameter error
            site2 agent2.agent_interface
          with
          | error, None -> warn parameter error (Some "line 134") Exit 0
          | error, Some port ->
            if port.site_state.max > 0
            then error, port.site_state.max
            else warn parameter error (Some "line 139") Exit 0
        in
        let error, set2 =
          Set_triple.Set.add_when_not_in parameter error
            (agent_type2, site2, state2) Set_triple.Set.empty
        in
        error, set2
    in
    (*---------------------------------------------------------------------*)
    let error, store_result = add_link set1 set2 store_result in
    error, store_result
  ) (error, store_result) rule.actions.bind

(************************************************************************************)
(*get the binding in initial state*)

let collect_init_map parameter error compiled store_result =
  let add_link set1 set2 store_result =
    let error, old =
      match
        Int2Map_CM_Syntactic.Map.find_option_without_logs parameter error set1 store_result
      with
      | error, None -> error, Set_triple.Set.empty
      | error, Some s -> error, s
    in
    let error, union_set = Set_triple.Set.union parameter error set2 old in
    let error, store_result =
      Int2Map_CM_Syntactic.Map.add_or_overwrite parameter error set1 union_set store_result
    in
    error, store_result
  in
  (*---------------------------------------------------------------------*)
  Nearly_inf_Imperatif.fold parameter error
    (fun parameter error index init store_result ->
      AgentMap.fold parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                (*---------------------------------------------------------------------*)
                let agent_index_target = site_add.agent_index in
                let site_type_target = site_add.site in
                (*---------------------------------------------------------------------*)
                (*get agent_source*)
                let error, agent_source =
                  match AgentMap.get parameter error agent_id
                    init.e_init_c_mixture.views
                  with
                  | error, None -> warn parameter error (Some "line 218") Exit Ghost
                  | error, Some agent -> error, agent
                in
                (*---------------------------------------------------------------------*)
                (*get agent_target*)
                let error, agent_target =
                  match AgentMap.get parameter error agent_index_target
                    init.e_init_c_mixture.views
                  with
                  | error, None -> warn parameter error (Some "line 226") Exit Ghost
                  | error, Some agent -> error, agent
                in
                (*---------------------------------------------------------------------*)
                let error, set1 =
                  match agent_source with
                  | Ghost | Unknown_agent _ | Dead_agent _ ->
                    warn parameter error (Some "line 209") Exit Set_triple.Set.empty
                  | Agent agent1 ->
                    let agent_type1 = agent1.agent_name in
                    let error, state1 =
                      match Site_map_and_set.Map.find_option_without_logs
                        parameter error site_type_source
                        agent1.agent_interface
                      with
                      | error, None -> warn parameter error (Some "line 217") Exit 0
                      | error, Some port ->
                        if port.site_state.max > 0
                        then error, port.site_state.max
                        else warn parameter error (Some "line 222") Exit 0
                    in
                    let error, set1 =
                      Set_triple.Set.add_when_not_in parameter error
                        (agent_type1, site_type_source, state1) Set_triple.Set.empty
                    in
                    error, set1
                in
                (*---------------------------------------------------------------------*)
                let error, set2 =
                  match agent_target with
                  | Ghost | Unknown_agent _ | Dead_agent _ ->
                    warn parameter error (Some "line 232") Exit Set_triple.Set.empty
                  | Agent agent2 ->
                    let agent_type2 = agent2.agent_name in
                    let error, state2 =
                      match Site_map_and_set.Map.find_option_without_logs
                        parameter error site_type_target
                        agent2.agent_interface
                      with
                      | error, None -> warn parameter error (Some "line 241") Exit 0
                      | error, Some port ->
                        if port.site_state.max > 0
                        then error, port.site_state.max
                        else warn parameter error (Some "line 246") Exit 0
                    in
                    let error, set2 =
                      Set_triple.Set.add_when_not_in parameter error
                        (agent_type2, site_type_target, state2) Set_triple.Set.empty
                    in
                    error, set2
                in
                (*-----------------------------------------------------------------------*)
                let error, store_result = add_link set1 set2 store_result in
                error, store_result
              ) bonds_map (error, store_result)
          in
          error, store_result
        )
        init.e_init_c_mixture.bonds
        store_result
    ) compiled.init store_result

(************************************************************************************)
(*union init contact map and syntactic one*)

let compute_syn_contact_map_full parameter error rule compiled store_result =
  let add_link triple_set1 triple_set2 store_result =
    let error, old_set =
      match
        Int2Map_CM_Syntactic.Map.find_option_without_logs parameter error
          triple_set1 store_result
      with
      | error, None -> error, Set_triple.Set.empty
      | error, Some s -> error, s
    in
    let error, union_set = Set_triple.Set.union parameter error triple_set2 old_set in
    let error, result =
      Int2Map_CM_Syntactic.Map.add_or_overwrite
        parameter error triple_set1 union_set store_result
    in
    error, result
  in
  let error, syntactic_contact_map =
    compute_contact_map
      parameter
      error
      rule
      Int2Map_CM_Syntactic.Map.empty
  in
  let error, init_contact_map =
    collect_init_map
      parameter
      error
      compiled
      Int2Map_CM_Syntactic.Map.empty
  in
  Int2Map_CM_Syntactic.Map.fold2
    parameter error
    (*exists in 'a t*)
    (fun parameter error triple_set1 triple_set2 store_result ->
      let error, store_result =
        add_link triple_set1 triple_set2 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error triple_set1' triple_set2' store_result ->
      let error, store_result =
        add_link triple_set1' triple_set2' store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error triple_set triple_set2 triple_set2' store_result ->
      let error, union = Set_triple.Set.union parameter error triple_set2 triple_set2' in
      let error, store_result =
        add_link triple_set union store_result
      in
      error, store_result
    )
    syntactic_contact_map
    init_contact_map
    store_result

(************************************************************************************)
(*a bond is discovered for the first time:
  For example:
  'r0' A() ->
  'r1' A(x) ->
  'r2' A(x!B.x) -> A(x)
  'r3' A(x), B(x) -> A(x!1), B(x!1)
  'r4' A(x,y), C(y) -> A(x,y!1), C(y!1)

  'r3' has a bond on the rhs, for any (rule_id, state) belong to side
  effects of A(x); state is compatible with B(x!1), add rule_id into update
  function.

  - map (agent_type_cv, covering_class_id) -> rule_id list of modified sites
*)

let store_covering_classes_modification_update_aux parameter error agent_type_cv
    site_type_cv cv_id store_test_modification_map store_result =
  let add_link (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 75") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite parameter error (agent_type, cv_id) (l, new_set)
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let error, (l, rule_id_set) =
    match Int2Map_Test_Modif.Map.find_option_without_logs parameter error
      (agent_type_cv, site_type_cv) store_test_modification_map
    with
    | error, None -> error, ([], Site_map_and_set.Set.empty)
    | error, Some (l, s) -> error, (l, s)
  in
  let error, result =
    add_link (agent_type_cv, cv_id) rule_id_set store_result
  in
    (*-------------------------------------------------------------------------------*)
    (*map this map*)
  let store_result =
    Int2Map_CV_Modif.Map.map (fun (l, x) -> List.rev l, x) result
  in
  error, store_result

(************************************************************************************)

let collect_covering_classes_modification_update parameter error
    store_test_modification_map
    store_covering_classes_id =
  let error, store_result =
    Int2Map_CV.Map.fold
      (fun (agent_type_cv, site_type_cv) (l1, l2) store_result ->
        List.fold_left (fun (error, store_current_result) cv_id ->
          let error, result =
            store_covering_classes_modification_update_aux
              parameter
              error
              agent_type_cv
              site_type_cv
              cv_id
              store_test_modification_map
              store_current_result
          in
          error, result
        ) store_result l2
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) store_covering_classes_id (error, Int2Map_CV_Modif.Map.empty)
  in
  let store_result =
    Int2Map_CV_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*update function added information of rule_id in side effects*)

let collect_covering_classes_modification_side_effects parameter error
    store_test_modification_map
    store_potential_side_effects
    covering_classes
    store_result =
  let add_link (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 169") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite parameter error (agent_type, cv_id) (l, new_set)
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let _, store_potential_side_effects_bind = store_potential_side_effects in
  let error, store_result =
    Int2Map_potential_effect.Map.fold
      (fun (agent_type_partner, rule_id_effect) pair_list (error, store_result) ->
        List.fold_left (fun (error, store_result) (site_type_partner, state) ->
          let error, store_result =
            AgentMap.fold parameter error
              (fun parameter error agent_type_cv remanent store_result ->
                let cv_dic = remanent.store_dic in
                let error, store_result =
                  Dictionary_of_Covering_class.fold
                    (fun list_of_site_type ((), ()) cv_id (error, store_result) ->
                    (*get a set of rule_id in update(c)*)
                      let error, (l, rule_id_set) =
                        match Int2Map_Test_Modif.Map.find_option_without_logs parameter error
                          (agent_type_partner, site_type_partner)
                          store_test_modification_map
                        with
                        | error, None -> error, ([], Site_map_and_set.Set.empty)
                        | error, Some (l, s) -> error, (l, s)
                      in
                      (*add rule_id_effect into rule_id_set*)
                      let error, new_rule_id_set =
                        Site_map_and_set.Set.add parameter error rule_id_effect rule_id_set
                      in
                      let error, store_result =
                        add_link (agent_type_partner, cv_id) new_rule_id_set store_result
                      in
                      error, store_result
                    ) cv_dic (error, store_result)
                in
                error, store_result
              ) covering_classes store_result
          in
          error, store_result
        ) (error, store_result) pair_list
      ) store_potential_side_effects_bind (error, store_result)
  in
  error, store_result

(************************************************************************************)
(*combine update(c) and update(c') of side effects together*)

let collect_covering_classes_modification_update_full parameter error
    store_update_modification
    store_update_with_side_effects
    store_result
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_CV_Modif.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', new_set =
      Site_map_and_set.Set.union parameter error rule_id_set old
    in
    let error = Exception.check warn parameter error error' (Some "line 251") Exit in
    let error, result =
      Int2Map_CV_Modif.Map.add_or_overwrite
        parameter error (agent_type, cv_id) (l, new_set) store_result
    in
    error, result
  in
  (*---------------------------------------------------------------------------*)
  (*fold 2 map*)
  Int2Map_CV_Modif.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (_, rule_id_set) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (_, rule_id_set) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) rule_id_set store_result
      in
      error, store_result
    )
    (*both*)
    (fun parameter error (agent_type, cv_id) (_, s1) (_, s2) store_result ->
      let error, union_set =
        Site_map_and_set.Set.union parameter error s1 s2
      in
      let error, store_result =
        add_link error (agent_type, cv_id) union_set store_result
      in
      error, store_result
    )
    store_update_modification
    store_update_with_side_effects
    store_result

(************************************************************************************)
(*PRINT SECTION*)

open Printf

(*dynamic contact map full information*)

let print_contact_map_full_aux parameter error handler_kappa result =
  Int2Map_CM_state.Map.iter (fun (agent1, site1, state1) set ->
    Set_triple.Set.iter (fun (agent2, site2, state2) ->
      let error, agent_string1 =
        try
          Handler.string_of_agent parameter error handler_kappa agent1
        with
          _ -> warn parameter error (Some "line 87") Exit (string_of_int agent1)
      in
      let error, site_string1 =
        try
          Handler.string_of_site_contact_map parameter error handler_kappa agent1 site1
        with
          _ -> warn parameter error (Some "line 92") Exit (string_of_int site1)
      in
      let error, state_string1 =
        try
          Handler.string_of_state parameter error handler_kappa agent1 site1 state1
        with
          _ -> warn parameter error (Some "line 99") Exit (string_of_int state1)
      in
      let error, agent_string2 =
        try
          Handler.string_of_agent parameter error handler_kappa agent2
        with
          _ -> warn parameter error (Some "line 105") Exit (string_of_int agent2)
      in
      let error, site_string2 =
        try
          Handler.string_of_site_contact_map parameter error handler_kappa agent2 site2
        with
          _ -> warn parameter error (Some "line 111") Exit (string_of_int site2)
      in
      let error, state_string2 =
        try
          Handler.string_of_state parameter error handler_kappa agent2 site2 state2
        with
          _ -> warn parameter error (Some "line 117") Exit (string_of_int state2)
      in
      fprintf stdout
        "agent_type:%i:%s@@site_type:%i:%s:state:%i(%s)--agent_type':%i:%s@@site_type':%i:%s:state':%i(%s)\n"
        agent1 agent_string1
        site1 site_string1
        state1 state_string1
        agent2 agent_string2
        site2 site_string2
        state2 state_string2
    ) set
  ) result

let print_contact_map_full parameter error handler_kappa result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "(Full) Contact map and initital state:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites are annotated with the id of binding type:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let () =
    print_contact_map_full_aux
      parameter
      error
      handler_kappa
      result
  in
  ()

(************************************************************************************)
(*syntactic contact map and init map*)

let print_syn_map_aux parameter error handler_kappa result =
  Int2Map_CM_Syntactic.Map.iter
    (fun set1 set2 ->
      Set_triple.Set.iter (fun (agent_type, site_type, state) ->
        let error, agent_string =
          try
            Handler.string_of_agent parameter error handler_kappa agent_type
          with
            _ -> warn parameter error (Some "line 192") Exit (string_of_int agent_type)
        in
        let error, site_string =
          try
            Handler.string_of_site_contact_map
              parameter error handler_kappa agent_type site_type
          with
            _ -> warn parameter error (Some "line 199") Exit (string_of_int site_type)
        in
        let error, state_string =
          try
            Handler.string_of_state parameter error handler_kappa agent_type site_type state
          with
            _ -> warn parameter error (Some "line 205") Exit (string_of_int state)
        in
        Set_triple.Set.iter (fun (agent_type', site_type', state') ->
          let error, agent_string' =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type'
            with
              _ -> warn parameter error (Some "line 212") Exit (string_of_int agent_type')
          in
          let error, site_string' =
            try
              Handler.string_of_site_contact_map
                parameter error handler_kappa agent_type' site_type'
            with
              _ -> warn parameter error (Some "line 218") Exit (string_of_int site_type')
          in
          let error, state_string' =
            try
              Handler.string_of_state parameter error handler_kappa
                agent_type' site_type' state'
            with
              _ -> warn parameter error (Some "line 226") Exit (string_of_int state')
          in
          fprintf stdout
            "agent_type:%i:%s:site_type:%i:%s:state:%i(%s) - > agent_type':%i:%s:site_type':%i:%s:state':%i(%s)\n"
            agent_type agent_string
            site_type site_string
            state state_string
            agent_type' agent_string'
            site_type' site_string'
            state' state_string'
        ) set2
      ) set1
    ) result

let print_syn_contact_map_full parameter error handler_kappa result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "(Syntactic) Contact map and initital state:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites are annotated with the id of binding type:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let () =
    print_syn_map_aux
      parameter
      error
      handler_kappa
      result
  in
  ()

(************************************************************************************)
(*update (c) function*)

let print_covering_classes_modification_aux parameter error handler_kappa compiled result =
  Int2Map_CV_Modif.Map.iter
    ( fun (agent_type, y) (_, s2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 268") Exit (string_of_int agent_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:covering_class_id:%i:@@set of rule_id:"
          agent_type agent_string y
      in
      let _ =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
        (*mapping rule_id of type int to string*)
          let error, rule_id_string =
            try
              Handler.string_of_rule parameter error handler_kappa
                compiled rule_id
            with
              _ -> warn parameter error (Some "line 283") Exit (string_of_int rule_id)
          in
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" rule_id_string in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in ()
        ) s2
    ) result

let print_covering_classes_modification_update parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "List of rules to awake when the state of a site is modified and tested:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let () =
    print_covering_classes_modification_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  ()

(************************************************************************************)
(*update(c'), when discovered a bond for the first time*)

let print_covering_classes_modification_side_effects parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "List of rules to awake when the state of a site is modified and tested and side effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let () =
    print_covering_classes_modification_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  ()

(************************************************************************************)
(*Final update function*)

let print_covering_classes_modification_update_full parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Final list of rules to awake when the state of a site is modified and tested and side effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let () =
    print_covering_classes_modification_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  ()
