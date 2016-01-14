 (**
  * bdu_contact_map.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 11th of September
  * Last modification: 
  * 
  * Compute the contact map 
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Bdu_analysis_type
open Int_storage
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Contact Map") message exn
                 (fun () -> default)                

let trace = false

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
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
    let agent_index1 = site_add1.agent_index in
    let agent_type1 = site_add1.agent_type in
    let site1 = site_add1.site in
    let agent_type2 = site_add2.agent_type in
    let site2 = site_add2.site in
    let agent_index2 = site_add2.agent_index in
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
    match agent1, agent2 with
    | Unknown_agent _, Unknown_agent _ | Dead_agent _, Dead_agent _
    | (Agent _, (Ghost|Dead_agent _|Unknown_agent _))
    | (Ghost, (Agent _|Dead_agent _|Unknown_agent _))
    | (Dead_agent _, (Ghost|Agent _|Unknown_agent _))
    | (Unknown_agent _, (Ghost|Agent _|Dead_agent _))
    | Ghost, Ghost -> warn parameter error (Some "line 156") Exit store_result
    | Agent agent1, Agent agent2 ->
    (*---------------------------------------------------------------------*)
    let error, set1 =
      Site_map_and_set.Map.fold
        (fun _ port1 (error, store_result) ->
          let state1 = port1.site_state.max in
          if state1 > 0
          then
            let error, set = 
              Set_triple.Set.add_when_not_in parameter error
                (agent_type1, site1, state1) store_result
            in
            error, set
          else
            error, store_result
        ) agent1.agent_interface (error, Set_triple.Set.empty)
    in
    (*---------------------------------------------------------------------*)
    let error, set2 =
      Site_map_and_set.Map.fold
        (fun _ port2 (error, store_result) ->
          let state2 = port2.site_state.max in
          if state2 > 0
          then
            let error, set =
              Set_triple.Set.add_when_not_in parameter error
                (agent_type2, site2, state2) store_result 
            in
            error, set
          else
            error, store_result
        ) agent2.agent_interface (error, Set_triple.Set.empty)
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
  Nearly_inf_Imperatif.fold parameter error
    (fun parameter error index init store_result ->
      AgentMap.fold parameter error
        (fun parameter error agent_id bonds_map store_result ->
          let error, store_result =
            Site_map_and_set.Map.fold
              (fun site_type_source site_add (error, store_result) ->
                let agent_index_target = site_add.agent_index in
                (*get agent_source*)
                let error, agent_source =
                  match AgentMap.get parameter error agent_id
                    init.e_init_c_mixture.views 
                  with
                  | error, None -> warn parameter error (Some "line 218") Exit Ghost
                  | error, Some agent -> error, agent
                in
                (*get agent_target*)
                let error, agent_target =
                  match AgentMap.get parameter error agent_index_target
                    init.e_init_c_mixture.views
                  with
                  | error, None -> warn parameter error (Some "line 226") Exit Ghost
                  | error, Some agent -> error, agent
                in
                (*-----------------------------------------------------------------------*)
                match agent_source, agent_target with
                | Unknown_agent _, Unknown_agent _ 
                | Dead_agent _, Dead_agent _ 
                | (Agent _, (Ghost |Dead_agent _|Unknown_agent _))
                | (Ghost, (Agent _|Dead_agent _|Unknown_agent _))
                | (Dead_agent _, (Ghost|Agent _|Unknown_agent _))
                | (Unknown_agent _, (Ghost|Agent _|Dead_agent _))
                | Ghost, Ghost ->
                  warn parameter error (Some "line 238") Exit store_result
                | Agent agent1, Agent agent2 ->
                  let agent_type1 = agent1.agent_name in
                  let error, set1 =
                    Site_map_and_set.Map.fold
                      (fun site1 port1 (error, store_result) ->
                        let state1 = port1.site_state.max in
                        if state1 > 0
                        then
                          let error, set =
                            Set_triple.Set.add_when_not_in parameter error
                              (agent_type1, site1, state1) store_result 
                          in
                          error, set
                        else
                          error, store_result
                      ) agent1.agent_interface (error, Set_triple.Set.empty)
                  in
                  (*---------------------------------------------------------------------*)
                  let agent_type2 = agent2.agent_name in
                  let error, set2 =
                    Site_map_and_set.Map.fold
                      (fun site_type2 port2 (error, store_result) ->
                        let state2 = port2.site_state.max in
                        if state2 > 0
                        then
                          let error, set = 
                            Set_triple.Set.add_when_not_in parameter error
                              (agent_type2, site_type2, state2) store_result 
                          in
                          error, set
                        else
                          error, store_result
                      ) agent2.agent_interface (error, Set_triple.Set.empty)
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
(*contact map*)

let compute_contact_map_full parameter error handler rule =
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
      ) handler.dual Int2Map_CM_state.Map.empty
  in
  error, store_result
