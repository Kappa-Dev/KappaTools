(**
  * bdu_side_effects.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 30th of September
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Int_storage
open Bdu_analysis_type
open Bdu_contact_map
open SetMap

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU side effects") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*compute side effects, return a list of pair [rule_id * binding_state].
  For example:
  'r0' A() ->
  'r1' A(x) ->
  'r2' A(x!B.x) -> A(x)
  'r3' A(x,y), C(y) -> A(x,y!1), C(y!1)
  
  The result of this function is:
  - A(x): [(r0, _); (r2, B.x)]
  - A(y): [(r0, _); (r1, _)]
*)

(*compute half-break action: rule r2 is a half-break action.*)

let half_break_action parameter error handler rule_id half_break store_result =
  (*module (agent_type, site) -> (rule_id, binding_state) list*)
  let add_link (a, b) (r, state) store_result =
    let (l, old) =
      Int2Map_HalfBreak_effect.Map.find_default ([],[]) (a, b) store_result in
    let result =
      Int2Map_HalfBreak_effect.Map.add
        (a, b) (l, (r, state) :: old) store_result
    in
    error, result
  in
  let error, store_result =
    List.fold_left (fun (error, store_result) (site_address, state_op) ->
      (*site_address: {agent_index, site, agent_type}*)
      let agent_type = site_address.agent_type in
      let site = site_address.site in
      (*state*)
      let error, (state_min, state_max) =
        match state_op with
        | None ->
          begin
            let error, state_value =
              Misc_sa.unsome
                (Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter
                   error
                   (agent_type, site)
                   handler.states_dic)
                (fun error -> warn parameter error (Some "line 54") Exit
                  (Dictionary_of_States.init()))
            in
            let error, last_entry =
              Dictionary_of_States.last_entry parameter error state_value in
            error, (1, last_entry)
          end
        | Some interval -> error, (interval.min, interval.max)
      in
      (*return result*)
      let error, store_result =
        add_link (agent_type, site) (rule_id, state_min) store_result
      in
      error, store_result  
  ) (error, store_result) half_break
  in
  (*map function*)
  let store_result =
    Int2Map_HalfBreak_effect.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*compute remove action: r0 and r1 are remove action *)

let remove_action parameter error rule_id remove store_result =
  let add_link (a, b) r store_result =
    let (l, old) =
      Int2Map_Remove_effect.Map.find_default ([],[]) (a, b) store_result in
    let result =
      Int2Map_Remove_effect.Map.add (a, b) (l, r :: old) store_result
    in
    error, result
  in
  let error, store_result =
    List.fold_left (fun (error, store_result) (agent_index, agent, list_undoc) ->
      let agent_type = agent.agent_name in
      (*NOTE: if it is a site_free then do not consider this case.*)
      (*result*)
      let store_result =
        List.fold_left (fun store_result site ->
          let error, result =
            add_link (agent_type, site) rule_id store_result
          in
          result
        ) store_result list_undoc
      in
      error, store_result
    ) (error, store_result) remove
  in
  let store_result =
    Int2Map_Remove_effect.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*compute side effects: this is an update before discover bond function *)

let collect_side_effects parameter error handler rule_id half_break remove store_result =
  let store_half_break_action, store_remove_action = store_result in
  (*if there is a half_break action*)
  let error, store_half_break_action =
    half_break_action
      parameter
      error
      handler
      rule_id
      half_break
      store_half_break_action
  in
  (*if there is a remove action*)
  let error, store_remove_action =
    remove_action
      parameter
      error
      rule_id
      remove
      store_remove_action
  in
  error, (store_half_break_action, store_remove_action)
