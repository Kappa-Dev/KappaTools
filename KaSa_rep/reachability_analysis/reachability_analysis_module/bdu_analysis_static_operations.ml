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
open Covering_classes_type
open Covering_classes
open Bdu_analysis_static_type

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis static operations") message exn
    (fun () -> default)
    
let trace = false

(************************************************************************************)
(*a pair (agent_type_cv, site_cv) in covering classes
  return a list of covering_classes_id*)

let site_covering_classes parameter error covering_classes =
  let add_link (agent_type, site_type) cv_id store_result =
    let error, (l, old) =
      match Int2Map_CV.Map.find_option_without_logs
        parameter error (agent_type, site_type) store_result 
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      Int2Map_CV.Map.add_or_overwrite parameter error
        (agent_type, site_type) (l, cv_id :: old) store_result
    in
    error, result
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    AgentMap.fold parameter error
      (fun parameter error agent_type_cv remanent store_result ->
        (*get a list of covering_class_id from remanent*)
        let cv_dic = remanent.store_dic in
        (*fold a dictionary*)
        let error, store_result =
          Dictionary_of_Covering_class.fold
            (fun list_of_site_type ((),()) cv_id (error, store_result) ->
              (*get site_cv in value*)
              List.fold_left (fun (error, store_result) site_type_cv ->
                let error, result =
                  add_link (agent_type_cv, site_type_cv) cv_id store_result
                in 
                error, result
              ) (error, store_result) list_of_site_type
            ) cv_dic (error, store_result)
        in
        error, store_result
      (*REMARK: when it is folding inside a list, start with empty result,
        because the add_link function has already called the old result.*)
      ) covering_classes Int2Map_CV.Map.empty
  in
  let store_result =
    Int2Map_CV.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

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
  let add_link (a, b) (r, state) store_result =
    let error, (l, old) =
      match Int2Map_HalfBreak_effect.Map.find_option_without_logs parameter error
        (a, b) store_result
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      Int2Map_HalfBreak_effect.Map.add_or_overwrite
        parameter error (a, b) (l, (r, state) :: old)
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
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
      (*-------------------------------------------------------------------------------*)
      (*return result*)
      let error, store_result =
        add_link (agent_type, site) (rule_id, state_min) store_result
      in
      error, store_result  
  ) (error, store_result) half_break
  in
  (*-------------------------------------------------------------------------------*)
  (*map function*)
  let store_result =
    Int2Map_HalfBreak_effect.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*compute remove action: r0 and r1 are remove action *)

let remove_action parameter error rule_id remove store_result =
  let add_link (a, b) r store_result =
    let error, (l, old) =
      match Int2Map_Remove_effect.Map.find_option_without_logs
        parameter error (a, b) store_result
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      Int2Map_Remove_effect.Map.add_or_overwrite
        parameter error (a, b) (l, r :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
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
  (*-------------------------------------------------------------------------------*)
  let store_result =
    Int2Map_Remove_effect.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*compute side effects: this is an update before discover bond function *)

let collect_side_effects parameter error handler rule_id rule store_result =
  let store_half_break_action, store_remove_action = store_result in
  (*if there is a half_break action*)
  let error, store_half_break_action =
    half_break_action
      parameter
      error
      handler
      rule_id
      rule.actions.half_break
      store_half_break_action
  in
  (*if there is a remove action*)
  let error, store_remove_action =
    remove_action
      parameter
      error
      rule_id
      rule.actions.remove
      store_remove_action
  in
  error, (store_half_break_action, store_remove_action)

(************************************************************************************)
(*return a potential sites of side effects in the case of half break action*)

let store_potential_half_break parameter error handler rule_id half_break store_result =
  (*map of potential partner that is bond/free*)
  let add_link (agent_type, rule_id) (site, state) store_result =
    let error, old =
      match Int2Map_potential_effect.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, result =
      Int2Map_potential_effect.Map.add_or_overwrite parameter error
        (agent_type, rule_id) ((site, state) :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
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
    (*-------------------------------------------------------------------------------*)
    let rec aux k (error, store_result) =
      if k > state_max
      then
        error, store_result
      else
        (*potential partner*)
        match Handler.dual parameter error handler agent_type site k with
        | error, None -> error, store_result
        | error, Some (agent_type2, site2, state2) ->
          let error, store_potential_free =
            add_link (agent_type2, rule_id) (site2, 0) (fst store_result)
          in
          let error, store_potential_bind =
            add_link (agent_type2, rule_id) (site2, state2) (snd store_result)
          in
          error, (store_potential_free, store_potential_bind)
    in aux state_min (error, store_result)
  ) (error, store_result) half_break

(************************************************************************************)
(*potential partner of remove action*)

let store_potential_remove parameter error handler rule_id remove store_result =
  let add_link (agent_type, rule_id) (site, state) store_result =
    let error, old =
      match Int2Map_potential_effect.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, result =
      Int2Map_potential_effect.Map.add_or_overwrite parameter error (agent_type, rule_id)
        ((site, state) :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
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
            (*---------------------------------------------------------------------------*)
            let rec aux k (error, store_result) =
              if k > last_entry
              then error, store_result
              else
                (*potential partner*)
                match Handler.dual parameter error handler agent_type site k with
                | error, None -> error, store_result
                | error, Some (agent_type2, site2, state2) ->
                  (*---------------------------------------------------------------------*)
                  let error, store_potential_free =
                    add_link (agent_type2, rule_id) (site2, 0) (fst store_result)
                  in
                  let error, store_potential_bind =
                    add_link (agent_type2, rule_id) (site2, state2) (snd store_result)
                  in
                  error, (store_potential_free, store_potential_bind)
            in
            aux 1 (error, store_result)
          end          
        else
          error, store_result
      ) (error, store_result) list_undoc
    in
    error, store_result
  ) (error, store_result) remove

(************************************************************************************)
(*potential side effects*)

let collect_potential_side_effects_free parameter error handler rule_id 
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (Int2Map_potential_effect.Map.empty, Int2Map_potential_effect.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Int2Map_potential_effect.Map.empty, Int2Map_potential_effect.Map.empty)
  in
  (*-------------------------------------------------------------------------------*)
  let add_link error (agent_type, rule_id) l store_result =
    let error, old =
      match Int2Map_potential_effect.Map.find_option_without_logs parameter error
        (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = List.concat [l; old] in
    let error, result =
      Int2Map_potential_effect.Map.add parameter error (agent_type, rule_id)
        new_list store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  Int2Map_potential_effect.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, rule_id) l1 store_result ->
      let error, store_result =
        add_link error (agent_type, rule_id) l1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun paramter error (agent_type, rule_id) l2 store_result ->
      let error, store_result =
        add_link error (agent_type, rule_id) l2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, rule_id) l1 l2 store_result ->
      let concat = List.concat [l1; l2] in
      let error, store_result =
        add_link error (agent_type, rule_id) concat store_result
      in
      error, store_result
    )
    (fst store_result_hb)
    (fst store_result_remove)
    store_result_map

(************************************************************************************)

let collect_potential_side_effects_bind parameter error handler rule_id 
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (Int2Map_potential_effect.Map.empty, Int2Map_potential_effect.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Int2Map_potential_effect.Map.empty, Int2Map_potential_effect.Map.empty)
  in
  (*-------------------------------------------------------------------------------*)
  let add_link error (agent_type, rule_id) l store_result =
    let error, old =
      match Int2Map_potential_effect.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result 
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = List.concat [l; old] in
    let error, result =
      Int2Map_potential_effect.Map.add parameter error (agent_type, rule_id)
        new_list store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  Int2Map_potential_effect.Map.fold2
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, rule_id) l1 store_result ->
      let error, store_result =
        add_link error (agent_type, rule_id) l1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun paramter error (agent_type, rule_id) l2 store_result ->
      let error, store_result =
        add_link error (agent_type, rule_id) l2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, rule_id) l1 l2 store_result ->
      let concat = List.concat [l1; l2] in
      let error, store_result =
        add_link error (agent_type, rule_id) concat store_result
      in
      error, store_result
    )
    (snd store_result_hb)
    (snd store_result_remove)
    store_result_map

(************************************************************************************)

let collect_potential_side_effects parameter error handler rule_id rule store_result =
  let error, store_result_free =
    collect_potential_side_effects_free
      parameter
      error
      handler
      rule_id
      rule.actions.half_break
      rule.actions.remove
      (fst store_result)
  in
  let error, store_result_bind =
    collect_potential_side_effects_bind
      parameter
      error
      handler
      rule_id
      rule.actions.half_break
      rule.actions.remove
      (snd store_result)
  in
  error, (store_result_free, store_result_bind)

(************************************************************************************)
(* return a list of rule_id has sites that are modified.
   For example:
   'r0' A() ->
   'r1' A(x) ->
   'r2' A(x!B.x) -> A(x)
   'r3' A(x), B(x) -> A(x!1), B(x!1)
   'r4' A(x,y), C(x) -> A(x, y!1), C(x!1)

   result:
   - A(x): [r2; r3]
   - A(y): [r4]
   - B(x): [r3]
   - C(x): [r4]
*)

let collect_modification_sites parameter error rule_id rule store_result =
  (*from a pair of Map (agent_id, agent_type, site) -> rule_id :: old_result)*)
  let add_link error (agent_id, agent_type, site_type) rule_id store_result =
    let error, (l, old) =
      match Int2Map_Modif.Map.find_option_without_logs parameter error
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', current_set = Site_map_and_set.Set.add parameter error rule_id 
      Site_map_and_set.Set.empty 
    in
    let error = Exception.check warn parameter error error' (Some "line 65") Exit in
    let error, result =
      Int2Map_Modif.Map.add_or_overwrite parameter error (agent_id, agent_type, site_type) 
        (l, current_set) store_result
    in
    error, result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent_modif store_result ->
        if Site_map_and_set.Map.is_empty agent_modif.agent_interface
        then error, store_result
        else
          let agent_type = agent_modif.agent_name in
          (*return*)
          let error, store_result =
            Site_map_and_set.Map.fold
              (fun site_type _ (error, store_result) ->
                let error, store_result =
                  add_link error (agent_id, agent_type, site_type) rule_id store_result
                in
                error, store_result
              ) agent_modif.agent_interface (error, store_result)
          in
          error, store_result
      ) rule.diff_direct store_result
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*collect a set of rule_id of test rule and modification *)

let collect_test_sites parameter error rule_id rule store_result =
  let add_link (agent_id, agent_type, site_type) rule_id store_result =
    let error, (l, old) =
      match Int2Map_Modif.Map.find_option_without_logs parameter error
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error', current_set =
      Site_map_and_set.Set.add parameter error rule_id Site_map_and_set.Set.empty
    in
    let error = Exception.check warn parameter error error' (Some "line 137") Exit in
    let error, result =
      Int2Map_Modif.Map.add_or_overwrite parameter error (agent_id, agent_type, site_type)
        (l, current_set) store_result
    in
    error, result
  in
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent store_result ->
       match agent with
       | Unknown_agent _ | Ghost -> error, store_result
       | Dead_agent (agent,_,_,_)
       | Agent agent ->
         let agent_type = agent.agent_name in
         let error, store_result_test =
           Site_map_and_set.Map.fold
             (fun site_type _ (error, store_result) ->
               let error, store_result_test =
                 add_link (agent_id, agent_type, site_type) rule_id store_result
               in
               error, store_result_test
             ) agent.agent_interface (error, store_result)
         in
         error, store_result_test
      ) rule.rule_lhs.views store_result
  in
  let store_result =
    Int2Map_Modif.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*TODO: modification and test rule that has rule_id union together.
For example:
modification: agent_type:0:site_type:0:[5;6]
test: agent_type:0:site_type:0:[4;5;6;7]
=> result: agent_type:0:site_type:0:[4;5;6;7]
*)

let collect_test_modification_sites
    parameter error store_modification_sites store_test_sites store_result =
  let add_link error (agent_id, agent_type, site_type) rule_id_set store_result =
    let error, (l, old) =
      match Int2Map_Modif.Map.find_option_without_logs parameter error 
        (agent_id, agent_type, site_type) store_result
      with
      | error, None -> error, ([], Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
    let error, union = Site_map_and_set.Set.union parameter error old rule_id_set in
    let error, result =
      Int2Map_Modif.Map.add_or_overwrite parameter error (agent_id, agent_type, site_type)
        (l, union) store_result
    in
    error, result
  in
  Int2Map_Modif.Map.fold2
    parameter error
    (*exists in 'a t*)
    (fun parameter error (agent_id, agent_type, site_type) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_id, agent_type, site_type) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_id, agent_type, site_type) (l1, s1) (l2, s2) store_result ->
      let error',union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 212") Exit in   
      let error, store_result =
        add_link error (agent_id, agent_type, site_type) union store_result
      in
      error, store_result
    ) store_modification_sites store_test_sites store_result

(************************************************************************************)
(*update of the views due to modification without agent_id*)

let collect_modif_map parameter error store_modification_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type))
    parameter
    error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    ) store_modification_sites

(************************************************************************************)
(*valuations of the views that are created without agent_id*)

let collect_test_map parameter error store_test_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type))
    parameter
    error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    ) store_test_sites

(************************************************************************************)
(*valuations of the views that are created without agent_id*)

let collect_test_modif_map parameter error store_test_modification_sites =
  Project2_modif.monadic_proj
    (fun parameter error (agent_id, agent_type, site_type) ->
      error, (agent_type, site_type)
    )
    parameter error
    ([], Site_map_and_set.Set.empty)
    (fun parameter error (l1, s1) (l2, s2) ->
      let error', new_set = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 118") Exit in
      error, (List.concat [l1; l2], new_set)
    )
    store_test_modification_sites
