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

(*let compute_contact_map parameter error handler rule =
  let store_result = Int2Map_CM_state.Map.empty in
  let add_link (a, b, s) (c, d, s') store_result =
    let (l, old) =
      Int2Map_CM_state.Map.find_default ([],[]) (a, b, s) store_result in
    let add_map =
      Int2Map_CM_state.Map.add (a, b, s) (l, ((c, d, s') :: []))
	store_result
    in
    error, add_map
  in  
  (*folding this solution with the information in dual*)
  let error, store_result =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent1, (site, state)) (agent', site', state') store_result ->
        (*check on the lhs binding *)
        AgentMap.fold parameter error 
          (fun parameter error agent_id agent store_result ->
            match agent with
	    | Unknown_agent _ | Dead_agent _ ->
	       warn parameter error (Some "line 60, dead agents/sites should not occur in rhs") Exit store_result 
	    | Ghost-> error, store_result
            | Agent agent ->
              let agent_type = agent.agent_name in
              let error, store_result =
                Site_map_and_set.Map.fold
                  (fun site_lhs port (error, store_result) ->
                    let state_lhs = port.site_state.min in
                    (*if Int2Map_CM_state.Map.mem (agent_type, site_lhs, state_lhs)
                      store_result
                    then 
                      (*let result =
                        Int2Map_CM_state.Map.remove
                          (agent_type, site_lhs, state_lhs)
                          store_result
                      in*)
                      error, store_result
                    else*)
	            let error, store_result =
                    add_link (agent1, site, state) (agent', site', state') store_result
	            in
	            error, store_result
                  ) agent.agent_interface (error, store_result)
              in
              error, store_result
          ) rule.rule_lhs.views store_result
      ) handler.dual store_result
  in
  error, store_result*)

let compute_contact_map parameter error rule handler store_result =
  let add_link (agent, site) (agent1, site1) store_result =
    let error, old =
      match Int2Map_test_state.Map.find_option (agent, site) store_result with
      | None -> error, Map_second_agent_bind.Set.empty
      | Some s -> error, s
    in
    let set = Map_second_agent_bind.Set.add (agent1, site1) old in
    let union_set = Map_second_agent_bind.Set.union set old in
    if Map_second_agent_bind.Set.equal union_set old
    then error, store_result
    else
      let add_map =
        Int2Map_test_state.Map.add (agent, site) union_set store_result
      in
      error, add_map
  in  
  (*folding this solution with the information in dual*)
  let error, store_result =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent1, (site, state)) (agent', site', state') store_result ->
        AgentMap.fold parameter error 
          (fun parameter error agent_id agent store_result ->
            let error, store_result =
              (*Site_map_and_set.Map.fold
                (fun _ port _ ->
                  let state = port.site_state.min in*)
                  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
                    let agent1 = site_add1.agent_type in
                    let site1 = site_add1.site in
                    let agent2 = site_add2.agent_type in
                    let site2 = site_add2.site in
                    add_link (agent1, site1) (agent2, site2) store_result
                  ) (error, store_result) rule.actions.bind
                (* ) agent.agent_interface (error, store_result)*)
            in
            error, store_result
          ) rule.diff_direct store_result
      ) handler.dual store_result
  in
  error, store_result

(*****************************************************************************************)
(*contact map*)

let compute_contact_map_full parameter error handler rule =
  let store_result = Int2Map_CM_state.Map.empty in
  let add_link (a, b, s) (c, d, s') store_result =
    let old =
      Int2Map_CM_state.Map.find_default Set_triple.Set.empty (a, b, s) store_result
    in
    let set = Set_triple.Set.add (c, d, s') old in
    let union_set = Set_triple.Set.union old set in
    if Set_triple.Set.equal union_set old
    then error, store_result
    else
      let add_map =
        Int2Map_CM_state.Map.add (a, b, s) union_set store_result
      in
      error, add_map
  in  
  (*folding this solution with the information in dual*)
  let error, store_result =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent1, (site, state)) (agent', site', state') store_result ->
        let error, store_result =
          add_link (agent1, site, state) (agent', site', state') store_result
	in
	error, store_result
      ) handler.dual store_result
  in
  error, store_result

(*let compute_contact_map_aux parameter error contact_map_full contact_map store_result =
  let add_link (a, b, s) (c,d,s') store_result =
    let old =
      Int2Map_CM_state.Map.find_default Set_triple.Set.empty (a, b, s) 
        store_result
    in
    let set = Set_triple.Set.add (c,d,s') old in
    let union_set = Set_triple.Set.union old set in
    if Set_triple.Set.equal union_set old
    then store_result
    else
      let add_map =
        Int2Map_CM_state.Map.add (a, b, s) union_set store_result
      in
      add_map
  in 
  Int2Map_CM_state.Map.fold
    (fun (agent_type1, site_type1, state1) set1 store_result ->
      Int2Map_test_state.Map.fold (fun (agent1, site1) set2 store_result ->
        Set_triple.Set.fold (fun (_, _, state2) store_result ->
          Map_second_agent_bind.Set.fold (fun (agent2, site2) store_result ->
            add_link (agent1, site1, state1) (agent2, site2, state2) store_result
          ) set2 store_result
        ) set1 store_result
      ) contact_map store_result
    ) contact_map_full store_result

let collect_contact contact_map_full contact_map_aux =
  let map_a, map_b =
    Int2Map_CM_state.Map.diff
      contact_map_full contact_map_aux
  in
  map_a, map_b*)
