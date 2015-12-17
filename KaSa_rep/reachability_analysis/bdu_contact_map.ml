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

(*TODO: it is not including initial state: include it in this contact map*)
let compute_contact_map parameter error rule_id rule handler store_result =
  let add_link rule_id (agent, site, state) store_result =
    let error, (l, old) =
      match Int2Map_syn.Map.find_option rule_id store_result with
      | None -> error, ([], Set_triple.Set.empty)
      | Some (l, s) -> error, (l, s)
    in
    let set = Set_triple.Set.add (agent, site, state) old in
    let union_set = Set_triple.Set.union set old in
    if Set_triple.Set.equal union_set old
    then error, store_result
    else
      let add_map =
        Int2Map_syn.Map.add rule_id (l, union_set) store_result
      in
      error, add_map
  in  
  (*folding this solution with the information in dual*)
  let error, (store_result1, store_result2) =
    AgentMap.fold parameter error 
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Unknown_agent _ | Dead_agent _ ->
	  warn parameter error (Some "line 113, dead agents/sites should not occur in rhs") 
            Exit store_result 
	| Ghost-> error, store_result
        | Agent agent ->
          List.fold_left (fun (error, (store_result1, store_result2)) 
            (site_add1, site_add2) ->
            let agent_index1 = site_add1.agent_index in
            let agent1 = site_add1.agent_type in
            let site1 = site_add1.site in
            let agent2 = site_add2.agent_type in
            let site2 = site_add2.site in
            let agent_index2 = site_add2.agent_index in
            let error, map1 =
              Site_map_and_set.Map.fold
                (fun site port (error, store_result) ->
                  let state = port.site_state.min in
                  if agent_id = agent_index1 
                  then
                    add_link rule_id (agent1, site1, state) store_result
                  else
                    error, store_result
                ) agent.agent_interface (error, store_result1)
            in
            let error, map2 = 
              Site_map_and_set.Map.fold
                (fun site port (error, store_result) ->
                  let state = port.site_state.min in
                  if agent_id = agent_index2
                  then
                    add_link rule_id (agent2, site2, state) store_result
                  else
                    error, store_result
                ) agent.agent_interface (error, store_result2)
            in
            error, (map1, map2)
          ) (error, store_result) rule.actions.bind
      ) rule.rule_rhs.views store_result
  in
  let store_result1 = 
    Int2Map_syn.Map.map (fun (l, x) -> List.rev l, x) store_result1
  in
  let store_result2 = 
    Int2Map_syn.Map.map (fun (l, x) -> List.rev l, x) store_result2
  in
  error, (store_result1, store_result2)

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
