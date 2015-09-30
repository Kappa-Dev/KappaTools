(**
  * bdu_side_effects.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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
  List.fold_left (fun (error, store_result) (site_address, state_op) ->
    (*site_address: {agent_index, site, agent_type}*)
    let agent_type = site_address.agent_type in
    let site = site_address.site in
    (*state*)
    let error, (min, max) =
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
    (*old*)
    let error, old_list =
      match AgentMap.unsafe_get parameter error agent_type store_result with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    (*new*)
    let new_list = List.concat [[rule_id, site, min]; old_list] in
    (*store*)
    let error, store_result =
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_list)
        store_result
    in
    error, store_result    
  ) (error, store_result) half_break

(************************************************************************************)
(*compute remove action: r0 and r1 are remove action *)

let site_free_of_port port = port.site_free

let remove_action parameter error rule_id remove store_result =
  List.fold_left (fun (error, store_result) (agent_index, agent, list_undoc) ->
    let store_remove_with_info, store_remove_without_info = store_result in
    let agent_type = agent.agent_name in
    (*-------------------------------------------------------------------------*)
    (*remove with document site: r1 is the rule with this property*)
    let triple_list =
      Site_map_and_set.fold_map
        (fun site port current_list ->
          let site_free = site_free_of_port port in
          let list = (rule_id, site, site_free) :: current_list in
          list
        ) agent.agent_interface []
    in
    (*old*)
    let error, old_list =
      match AgentMap.unsafe_get parameter error agent_type store_remove_with_info with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    (*new*)
    let new_list = List.concat [triple_list; old_list] in
    (*-------------------------------------------------------------------------*)
    (*remove with no information: r0 is the rule with this property*)
    let pair_list =
      List.fold_left (fun current_list site ->
        let list = (rule_id, site) :: current_list in
        list
      ) [] list_undoc
    in
    (*old*)
    let error, old_pair_list =
      match AgentMap.unsafe_get parameter error agent_type store_remove_without_info with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    (*new*)
    let new_pair_list = List.concat [pair_list; old_pair_list] in
    (*-------------------------------------------------------------------------*)
    (*store*)
    let error, store_remove_with_info =
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_list)
        store_remove_with_info
    in
    let error, store_remove_without_info =
      AgentMap.set
        parameter
        error
        agent_type
        (List.rev new_pair_list)
        store_remove_without_info
    in
    error, (store_remove_with_info, store_remove_without_info)
  ) (error, store_result) remove

(************************************************************************************)
(*compute side effects*)

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
