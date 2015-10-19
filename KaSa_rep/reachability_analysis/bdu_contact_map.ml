 (**
  * bdu_contact_map.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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

let compute_contact_map parameter error handler =
  let store_result = Int2Map_CM_state.empty in
  (*add_link*)
  let add_link (a, b, s) (c, d, s') store_result =
    let l, old =
      try Int2Map_CM_state.find (a, b, s) store_result
      with Not_found -> [],[]
    in
    Int2Map_CM_state.add (a, b, s) (l, ((c, d, s') :: old)) store_result
  in  
  (*folding this solution with the information in dual*)
  let error, store_result =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state') store_result ->
	let store_result =
          add_link (agent, site, state) (agent', site', state') store_result
	in
	error, store_result
      ) handler.dual store_result
  in
  (*return the result of this contact map*)
  let store_result = Int2Map_CM_state.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(*****************************************************************************************)
(* - Only take the binding information of the rhs; 
   Ex: A(x), B(x) -> A(x!1), B(x!1)
   and not the binding in the lhs; 
   Ex: A(x!1), B(x!1) -> A(x), B(x)
   - Getting information of binding state, then search if these binding belong to dual.
   The result is:
   - A bond to B. 
   and
   - B bond to A.
*)

(*TODO: test with %init*)

let collect_binding_set_rhs parameter error rule store_result =
  let add_link (agent_type, site_set, agent_type') site_set' store_result =
    let l, old_set =
      try Int2Map_CM_Set.find (agent_type, site_set, agent_type') store_result
      with Not_found -> [], BSet.empty_set
    in
    let error, union_site =
      BSet.union parameter error site_set' old_set
    in
    Int2Map_CM_Set.add (agent_type, site_set, agent_type') (l, union_site) store_result
  in
  let error, (store_binding1, store_binding2) =
    List.fold_left (fun (error, store_result) (site_address1, site_address2) ->
      let agent_type1 = site_address1.agent_type in
      let site1       = site_address1.site in
      (*second agent*)
      let agent_type2 = site_address2.agent_type in
      let site2       = site_address2.site in
      let store_binding1, store_binding2 = store_result in
      (*new: A bond to B, B bond to A*)
      (*add site1 into set*)
      let error, set_site1 =
        BSet.add_set parameter error site1 BSet.empty_set
      in
        let error, set_site2 =
        BSet.add_set parameter error site2 BSet.empty_set
      in
      let store_binding1 =
        add_link (agent_type1, set_site1, agent_type2) set_site2 store_binding1
      in
      (*B bond to A*)
      let store_binding2 =
        add_link (agent_type2, set_site2, agent_type1) set_site1 store_binding2
      in
      error, (store_binding1, store_binding2)
    ) (error, store_result) rule.actions.bind
  in
  (*result*)
  let store_binding1 =
    Int2Map_CM_Set.map (fun (l, x) -> l, x) store_binding1
  in
  let store_binding2 =
    Int2Map_CM_Set.map (fun (l, x) -> l, x) store_binding2
  in
  error, (store_binding1, store_binding2)

(*****************************************************************************************)
(*compute the binding information with precise information with state information.

  More precisely it will search in the contact map and the binding rhs and
  return the result of binding on the rhs.

  For instance:
  r1: A(x), B(x) -> A(x!1), B(x!1)
  r2: A(y!1), C(x!1) -> A(y), C(x)

  The result of 
  - Contact map is: (with state information)
  [A bond to B; B bond to A], and [A bond to C; C bond to A]
  - Binding on rhs is: (without state information)
  [A bond to B; B bond to A]
  - Precise binding dual is: (with state information)
  [A bond to B; B bond to A]
*)

(*FIXME: check the state*)
(*TODO*)

let precise_binding_dual parameter error handler rule store_result =
  let result_binding = Int2Map_CM_state.empty, Int2Map_CM_state.empty in (*FIXME*)
  (*add_link*)
  let add_link (a, b, s) (c, d, s') store_result =
    let l, old =
      try Int2Map_CM_state.find (a, b, s) store_result
      with Not_found -> [],[]
    in
    Int2Map_CM_state.add (a, b, s) (l, ((c, d, s') :: old)) store_result
  in  
  (*return the site name of site: this of type string*)
  (*folding this solution with the information in dual*)
  let error, result_binding =
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameter error
      (fun parameter error (agent, (site, state)) (agent', site', state')
	_result_binding ->
        (*binding on the rhs*)
        List.fold_left (fun (error, sol2) (site_address1, site_address2) ->
          let agent_type1 = site_address1.agent_type in
          let site1       = site_address1.site in
          (*second*)
          let agent_type2 = site_address2.agent_type in
          let site2       = site_address2.site in
          (*matching binding on the rhs and relation in a contact map*)
          if agent_type1 = agent  && site1 = site &&
             agent_type2 = agent' && site2 = site'
          then
            (*if true return the solution*)
            error, sol2
          else
            (*if not true, add those links*)
            let sol2_forward, sol2_reverse = sol2 in
            (*A bond to B*)
            let result_binding_forward =
              add_link 
                (agent_type1, site1, state)
                (agent_type2, site2, state')
                sol2_forward
            in
            (*B bond to A*)
            let result_binding_reverse =
              add_link
                (agent_type2, site2, state')
                (agent_type1, site1, state)
                sol2_reverse
            in
            (*store*)
            error, (result_binding_forward, result_binding_reverse)
        ) (error, store_result) rule.actions.bind
      ) handler.dual result_binding
  in
  let result_binding_forward, result_binding_reverse = result_binding in
  (*return the result of this contact map*)
  let result_binding_forward =
    Int2Map_CM_state.map (fun (l, x) -> List.rev l, x) result_binding_forward
  in
  let result_binding_reverse =
    Int2Map_CM_state.map (fun (l, x) -> List.rev l, x) result_binding_reverse
  in
  (result_binding_forward, result_binding_reverse)
