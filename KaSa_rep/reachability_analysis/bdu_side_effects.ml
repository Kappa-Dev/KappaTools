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
(*return a potential sites of side effects in the case of half break action*)

let store_potential_half_break parameter error handler rule_id half_break store_result =
  (*map of potential partner that is bond/free*)
  let add_link (agent_type, site_type) (rule_id, state) store_result =
    let old =
      Int2Map_potential_effect.Map.find_default [] (agent_type, site_type) 
        store_result
    in
    let result =
      Int2Map_potential_effect.Map.add
        (agent_type, site_type) ((rule_id, state) :: old) store_result
    in
    error, result
  in
  List.fold_left (fun (error, store_modif_minus) (add, state_op) ->
    let agent_type = add.agent_type in
    let site = add.site in
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
              (fun error -> warn parameter error (Some "line 109") Exit 
                (Dictionary_of_States.init()))
               in
              let error, last_entry =
                Dictionary_of_States.last_entry parameter error state_value
              in
              error, (1, last_entry)
        end
      | Some interval -> error, (interval.min, interval.max)
    in
    (*potential partner*)
    match Handler.dual parameter error handler agent_type site state_min with
      | error, None -> error, store_result
      | error, Some (agent_type2, site2, state2) ->
        let error, store_potential_free =
          add_link (agent_type2, site2) (rule_id, 0) (fst store_result)
        in
        let error, store_potential_bind =
          add_link (agent_type2, site2) (rule_id, state2) (snd store_result)
        in
        error, (store_potential_free, store_potential_bind)
  ) (error, store_result) half_break

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
(*potential partner of remove action*)

let store_potential_remove parameter error handler rule_id remove store_result =
  let add_link (agent_type, site_type) (rule_id, state) store_result =
    let old =
      Int2Map_potential_effect.Map.find_default [] (agent_type, site_type) 
        store_result
    in
    let result =
      Int2Map_potential_effect.Map.add
        (agent_type, site_type) ((rule_id, state) :: old) store_result
    in
    error, result
  in
  List.fold_left (fun (error, store_result) (agent_index, agent, list_undoc) ->
    let agent_type = agent.agent_name in
    let error, store_result =
      List.fold_left (fun (error, store_result) site ->
        let error, is_binding =
          Handler.is_binding_site parameter error handler agent_type site
        in
        if is_binding
        then
          begin
            let error, state_dic =
              Misc_sa.unsome
                (Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter error (agent_type, site) handler.states_dic)
                (fun error -> warn parameter error (Some "line 196") Exit
                  (Dictionary_of_States.init()))
            in
            let error, last_entry =
              Dictionary_of_States.last_entry parameter error state_dic
            in
            (*potential partner*)
            match Handler.dual parameter error handler agent_type site 1 with
            | error, None -> error, store_result
            | error, Some (agent_type2, site2, state2) ->
              let error, store_potential_free =
                add_link (agent_type2, site2) (rule_id, 0) (fst store_result)
              in
              let error, store_potential_bind =
                add_link (agent_type2, site2) (rule_id, state2) (snd store_result)
              in
              error, (store_potential_free, store_potential_bind)
          end          
        else
          error, store_result
      ) (error, store_result) list_undoc
    in
    error, store_result
  ) (error, store_result) remove

(************************************************************************************)

let store_potential_side_effects parameter error handler rule_id half_break remove store_result =
  let error, store_potential_half_break =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (fst store_result)
  in
  let error, store_potential_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (snd store_result)
  in
  error, (store_potential_half_break, store_potential_remove)

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
