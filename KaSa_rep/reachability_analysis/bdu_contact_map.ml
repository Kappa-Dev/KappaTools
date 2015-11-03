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
open Bdu_creation

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

let compute_contact_map parameter error handler rule =
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
            | Ghost -> error, store_result
            | Agent agent ->
              let agent_type = agent.agent_name in
              let error, store_result =
                Site_map_and_set.Map.fold
                  (fun site_lhs port (error, store_result) ->
                    let state_lhs = int_of_port port in
                    if Int2Map_CM_state.Map.mem (agent_type, site_lhs, state_lhs)
                      store_result
                    then 
                      let result =
                        Int2Map_CM_state.Map.remove
                          (agent_type, site_lhs, state_lhs)
                          store_result
                      in
                      error, result
                    else
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
  error, store_result
