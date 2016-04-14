(**
  * common_static_type.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 18th of Feburary
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU side effects") message exn (fun () -> default)  

let trace = false

type half_break_action = 
  (int list * (Ckappa_sig.c_rule_id * Ckappa_sig.c_state) list) Ckappa_sig.AgentSite_map_and_set.Map.t

type remove_action =
  (int list * Ckappa_sig.c_rule_id list) Ckappa_sig.AgentSite_map_and_set.Map.t

type free_partner = 
  (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list Ckappa_sig.AgentRule_map_and_set.Map.t

type bind_partner = (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list
  Ckappa_sig.AgentRule_map_and_set.Map.t

type potential_partner_free = free_partner
type potential_partner_bind = bind_partner

type bdu_common_static =
  {
    store_agent_name             : Ckappa_sig.c_agent_name Ckappa_sig.RuleAgent_map_and_set.Map.t;
    store_side_effects           : half_break_action * remove_action; 
    store_potential_side_effects : potential_partner_free *  potential_partner_bind;
    (*bond in the rhs and in the lhs*)
    store_bonds_rhs : Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
    store_bonds_lhs : Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
  }

(**********************************************************************************)
(*get agent_name from (rule_id and agent_id) in the lhs*)

let collect_agent_name parameter error rule_id rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Ghost
        | Cckappa_sig.Unknown_agent _ -> error, store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _)
        | Cckappa_sig.Agent agent -> 
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, store_result =
            Ckappa_sig.RuleAgent_map_and_set.Map.add_or_overwrite
              parameter
              error
              (rule_id, agent_id)
              agent_type
              store_result
          in
          error, store_result        
      ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

(**********************************************************************************)
(*Implementation*)

let half_break_action parameter error handler rule_id half_break store_result =
  (*module (agent_type, site) -> (rule_id, binding_state) list*)
  let add_link (agent_type, site_type) (r, state) store_result =
    let error, (l, old) =
      match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameter error
        (agent_type, site_type) store_result
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
        parameter error (agent_type, site_type) (l, (r, state) :: old)
        store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_result =
    List.fold_left (fun (error, store_result) (site_address, state_op) ->
      (*site_address: {agent_index, site, agent_type}*)
      let agent_type = site_address.Cckappa_sig.agent_type in
      let site_type = site_address.Cckappa_sig.site in
      (*state*)
      let error, (state_min, state_max) =
        match state_op with
        | None ->
          begin
            let error, state_value =
              Misc_sa.unsome
                (
                  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter
                   error
                   (agent_type, site_type)
                   handler.Cckappa_sig.states_dic)
                (fun error -> warn parameter error (Some "line 84") Exit
                  (Ckappa_sig.Dictionary_of_States.init()))
            in
            let error, last_entry =
              Ckappa_sig.Dictionary_of_States.last_entry parameter error state_value in
            error, (Ckappa_sig.dummy_state_index_1, last_entry)
          end
        | Some interval -> error, (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
      in
      (*-------------------------------------------------------------------------------*)
      (*return result*)
      let error, store_result =
        add_link (agent_type, site_type) (rule_id, state_min) store_result
      in
      (*let _ =
        Printf.fprintf stdout "HALF ACTION: agent_type:%i:site_type:%i:rule_id:%i:state:%i\n"
          agent_type site_type rule_id state_min
      in*)
      error, store_result  
  ) (error, store_result) half_break
  in
  (*-------------------------------------------------------------------------------*)
  (*map function*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*compute remove action: r0 and r1 are remove action *)

(*FIXME: state = 0 or there is no state?*)

let remove_action parameter error rule_id remove store_result =
  let add_link (agent_type, site_type) r store_result =
    let error, (l, old) =
      match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
        parameter error (agent_type, site_type) store_result
      with
      | error, None -> error, ([], [])
      | error, Some (l, l') -> error, (l, l')
    in
    let error, result =
      Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
        parameter error (agent_type, site_type) (l, r :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  let error, store_result =
    List.fold_left (fun (error, store_result) (agent_index, agent, list_undoc) ->
      let agent_type = agent.Cckappa_sig.agent_name in
      (*NOTE: if it is a site_free then do not consider this case.*)
      (*result*)
      let store_result =
        List.fold_left (fun store_result site_type ->
          let error, result =
            add_link (agent_type, site_type) rule_id store_result
          in
          (*let _ =
            Printf.fprintf stdout "REMOVE ACTION: agent_type:%i:site_type:%i:rule_id:%i\n"
              agent_type site_type rule_id
          in*)
          result
        ) store_result list_undoc
      in
      error, store_result
    ) (error, store_result) remove
  in
  (*-------------------------------------------------------------------------------*)
  let store_result =
    Ckappa_sig.AgentSite_map_and_set.Map.map (fun (l, x) -> List.rev l, x) store_result
  in
  error, store_result

(************************************************************************************)
(*return a potential sites of side effects in the case of half break action*)

let store_potential_half_break parameter error handler rule_id half_break store_result =
  (*map of potential partner that is bond/free*)
  let add_link (agent_type, rule_id) (site_type, state) store_result =
    let error, old =
      match Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, result =
      Ckappa_sig.AgentRule_map_and_set.Map.add_or_overwrite parameter error
        (agent_type, rule_id) ((site_type, state) :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  List.fold_left (fun (error, store_modif_minus) (add, state_op) ->
    let agent_type = add.Cckappa_sig.agent_type in
    let site_type = add.Cckappa_sig.site in
    (*state*)
    let error, (state_min, state_max) =
      match state_op with
      | None -> 
        begin
          let error, state_value =
            Misc_sa.unsome
              (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                 parameter
                 error
                 (agent_type, site_type)
                 handler.Cckappa_sig.states_dic)
              (fun error -> warn parameter error (Some "line 109") Exit 
                (Ckappa_sig.Dictionary_of_States.init()))
               in
              let error, last_entry =
                Ckappa_sig.Dictionary_of_States.last_entry parameter error state_value
              in
              error, (Ckappa_sig.dummy_state_index_1, last_entry)
        end
      | Some interval -> error, (interval.Cckappa_sig.min, interval.Cckappa_sig.max)
    in
    (*-------------------------------------------------------------------------------*)
    let rec aux k (error, store_result) =
      if k > state_max
      then
        error, store_result
      else
        (*potential partner*)
        match Handler.dual parameter error handler agent_type site_type k with
        | error, None -> error, store_result
        | error, Some (agent_type2, site2, state2) ->
          let error, store_potential_free =
            add_link (agent_type2, rule_id) (site2, Ckappa_sig.dummy_state_index) (fst store_result)
          in
          (*Print*)
          (*let _ =
            AgentRule_map_and_set.Map.iter (fun (agent_type, rule_id) l ->
              List.iter (fun (site_type, state) ->
                Printf.fprintf stdout "FREE:rule_id:%i:agent_type:%i:site_type:%i:state:%i\n" 
                  rule_id agent_type site_type state
              ) l
            ) store_potential_free
          in*)
          let error, store_potential_bind =
            add_link (agent_type2, rule_id) (site2, state2) (snd store_result)
          in
          (*Print*)
          (*let _ =
            AgentRule_map_and_set.Map.iter (fun (agent_type, rule_id) l ->
              List.iter (fun (site_type, state) ->
                Printf.fprintf stdout "BIND:rule_id:%i:agent_type:%i:site_type:%i:state:%i\n" 
                  rule_id agent_type site_type state
              ) l
            ) store_potential_bind
          in*)
          error, (store_potential_free, store_potential_bind)
    in aux state_min (error, store_result)
  ) (error, store_result) half_break

(************************************************************************************)
(*potential partner of remove action*)

let store_potential_remove parameter error handler rule_id remove store_result =
  let add_link (agent_type, rule_id) (site, state) store_result =
    let error, old =
      match Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, result =
      Ckappa_sig.AgentRule_map_and_set.Map.add_or_overwrite parameter error 
        (agent_type, rule_id)
        ((site, state) :: old) store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  List.fold_left (fun (error, store_result) (agent_index, agent, list_undoc) ->
    let agent_type = agent.Cckappa_sig.agent_name in
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
                (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter
                    error 
                    (agent_type, site)
                    handler.Cckappa_sig.states_dic)
                (fun error -> warn parameter error (Some "line 196") Exit
                  (Ckappa_sig.Dictionary_of_States.init()))
            in
            let error, last_entry =
              Ckappa_sig.Dictionary_of_States.last_entry parameter error state_dic
            in
            (*---------------------------------------------------------------------------*)
            let rec aux k (error, store_result) =
              if (Ckappa_sig.int_of_state_index k) > (Ckappa_sig.int_of_state_index last_entry)
              then error, store_result
              else
                (*potential partner*)
                match Handler.dual parameter error handler agent_type site k with
                | error, None -> error, store_result
                | error, Some (agent_type2, site2, state2) ->
                  (*---------------------------------------------------------------------*)
                  let error, store_potential_free =
                    add_link (agent_type2, rule_id) (site2, Ckappa_sig.dummy_state_index) (fst store_result)
                  in
                  (*Print*)
                  (*let _ =
                    AgentRule_map_and_set.Map.iter (fun (agent_type, rule_id) l ->
                      List.iter (fun (site_type, state) ->
                        Printf.fprintf stdout "REMOVE: FREE\n";
                        Printf.fprintf stdout "rule_id:%i:agent_type:%i:site_type:%i:state:%i\n" 
                          rule_id agent_type site_type state
                      ) l
                    ) store_potential_free
                  in*)
                  let error, store_potential_bind =
                    add_link (agent_type2, rule_id) (site2, state2) (snd store_result)
                  in
                  (*Print*)
                  (*let _ =
                    AgentRule_map_and_set.Map.iter (fun (agent_type, rule_id) l ->
                      List.iter (fun (site_type, state) ->
                        Printf.fprintf stdout "REMOVE: BIND\n";
                        Printf.fprintf stdout "rule_id:%i:agent_type:%i:site_type:%i:state:%i\n" 
                          rule_id agent_type site_type state
                      ) l
                    ) store_potential_bind
                  in*)
                  error, (store_potential_free, store_potential_bind)
            in
            aux Ckappa_sig.dummy_state_index_1 (error, store_result)
          end          
        else
          error, store_result
      ) (error, store_result) list_undoc
    in
    error, store_result
  ) (error, store_result) remove

(************************************************************************************)

let collect_potential_side_effects_free parameter error handler rule_id 
    half_break remove store_result_map =
  let error, store_result_hb =
    store_potential_half_break
      parameter
      error
      handler
      rule_id
      half_break
      (Ckappa_sig.AgentRule_map_and_set.Map.empty, 
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Ckappa_sig.AgentRule_map_and_set.Map.empty, 
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  (*-------------------------------------------------------------------------------*)
  let add_link error (agent_type, rule_id) l store_result =
    let error, old =
      match Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs parameter error
        (agent_type, rule_id) store_result
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = List.concat [l; old] in
    let error, result =
      Ckappa_sig.AgentRule_map_and_set.Map.add parameter error (agent_type, rule_id)
        new_list store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  Ckappa_sig.AgentRule_map_and_set.Map.fold2
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
      (Ckappa_sig.AgentRule_map_and_set.Map.empty, 
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  let error, store_result_remove =
    store_potential_remove
      parameter
      error
      handler
      rule_id
      remove
      (Ckappa_sig.AgentRule_map_and_set.Map.empty,
       Ckappa_sig.AgentRule_map_and_set.Map.empty)
  in
  (*-------------------------------------------------------------------------------*)
  let add_link error (agent_type, rule_id) l store_result =
    let error, old =
      match Ckappa_sig.AgentRule_map_and_set.Map.find_option_without_logs
        parameter error (agent_type, rule_id) store_result 
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let new_list = List.concat [l; old] in
    let error, result =
      Ckappa_sig.AgentRule_map_and_set.Map.add parameter error (agent_type, rule_id)
        new_list store_result
    in
    error, result
  in
  (*-------------------------------------------------------------------------------*)
  Ckappa_sig.AgentRule_map_and_set.Map.fold2
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

let collect_potential_side_effects parameter error handler rule_id half_break remove
    store_result =
  let error, store_result_free =
    collect_potential_side_effects_free
      parameter
      error
      handler
      rule_id
      half_break
      remove
      (fst store_result)
  in
  let error, store_result_bind =
    collect_potential_side_effects_bind
      parameter
      error
      handler
      rule_id
      half_break
      remove
      (snd store_result)
  in
  error, (store_result_free, store_result_bind)

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

(**********************************************************************************)
(*collect bonds in the rhs and lhs*)

let collect_agent_type_state parameter error agent site_type =
  match agent with
  | Cckappa_sig.Ghost
  | Cckappa_sig.Unknown_agent _ -> error, (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Dead_agent _ ->
    warn parameter error (Some "line 127") Exit (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_state_index)
  | Cckappa_sig.Agent agent1 ->
    let agent_type1 = agent1.Cckappa_sig.agent_name in
    let error, state1 =
      match Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
        parameter
        error
        site_type
        agent1.Cckappa_sig.agent_interface
      with
      | error, None ->  warn parameter error (Some "line 228") Exit Ckappa_sig.dummy_state_index
      | error, Some port ->
        let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
        if (Ckappa_sig.int_of_state_index state) > 0
        then error, state
        else warn parameter error (Some "line 196") Exit Ckappa_sig.dummy_state_index
    in
    error, (agent_type1, state1) 

(**************************************************************************)

let add_link_set parameter error rule_id (x, y) store_result =
  let error, old_set =
    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error 
      rule_id store_result
    with
    | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.empty
    | error, Some p -> error, p
  in
  let error', set = 
    Ckappa_sig.PairAgentSiteState_map_and_set.Set.add_when_not_in
      parameter error 
      (x, y)
      old_set
  in    
  let error = Exception.check warn parameter error error' (Some "line 246") Exit in
  let error'', union_set =
    Ckappa_sig.PairAgentSiteState_map_and_set.Set.union parameter error set old_set 
  in
  let error = Exception.check warn parameter error error'' (Some "line 250") Exit in
  let error, store_result =
    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id union_set store_result
  in
  error, store_result

(**************************************************************************)

let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
  let error, pair =
    let agent_index_target = site_add.Cckappa_sig.agent_index in
    let site_type_target = site_add.Cckappa_sig.site in
    let error, agent_source = 
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_id views
      with
      | error, None -> warn parameter error (Some "line 267") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, agent_target =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_index_target views
      with
      | error, None -> warn parameter error (Some "line 275") Exit Cckappa_sig.Ghost
      | error, Some agent -> error, agent
    in
    let error, (agent_type1, state1) =
      collect_agent_type_state
        parameter
        error
        agent_source
        site_type_source
    in
    let error, (agent_type2, state2) =
      collect_agent_type_state
        parameter
        error
        agent_target
        site_type_target
    in
    let pair = ((agent_type1, site_type_source, state1), 
                (agent_type2, site_type_target, state2))
    in
    error, pair
  in
  error, pair

(**************************************************************************)

let collect_bonds parameter error rule_id bonds views store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id bonds_map store_result ->
        let error, store_result =
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun site_type_source site_add (error, store_result) ->
              let error, pair =
                collect_pair_of_bonds
                  parameter
                  error
                  site_add
                  agent_id
                  site_type_source
                  views
              in
              let error, store_result =
                add_link_set parameter error rule_id pair store_result 
              in
              error, store_result
            ) bonds_map (error, store_result)
        in
        error, store_result
      ) bonds store_result
  in
  error, store_result

(**************************************************************************)
(*collect bonds lhs*)

let collect_bonds_rhs parameter error rule_id rule store_result =
  let views = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let bonds = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  let error, store_result =
    collect_bonds 
      parameter 
      error
      rule_id
      bonds
      views
      store_result
  in
  error, store_result

let collect_bonds_lhs parameter error rule_id rule store_result =
  let views = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let bonds = rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds in
  let error, store_result =
    collect_bonds parameter error rule_id bonds views store_result
  in
  error, store_result

(************************************************************************************)

let scan_rule parameter error handler_kappa rule_id rule store_result =
  (*------------------------------------------------------------------------------*)
  (*get agent_name*)
  let error, store_agent_name =
    collect_agent_name
      parameter
      error
      rule_id
      rule
      store_result.store_agent_name
  in
  (*------------------------------------------------------------------------------*)
  (*side effects*)
  let error, store_side_effects =
    collect_side_effects
      parameter
      error
      handler_kappa
      rule_id 
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_side_effects
  in 
  (*------------------------------------------------------------------------------*)
  (*potential partner side effects*)
  let error, store_potential_side_effects =
    collect_potential_side_effects
      parameter
      error
      handler_kappa
      rule_id
      rule.Cckappa_sig.actions.Cckappa_sig.half_break
      rule.Cckappa_sig.actions.Cckappa_sig.remove
      store_result.store_potential_side_effects
  in
  (*------------------------------------------------------------------------------*)
  (*bonds in the rhs and lhs*)
  let error, store_bonds_rhs =
    collect_bonds_rhs
      parameter
      error
      rule_id
      rule
      store_result.store_bonds_rhs
  in
  let error, store_bonds_lhs =
    collect_bonds_lhs
      parameter
      error
      rule_id
      rule
      store_result.store_bonds_lhs
  in
  error,
  {
    store_agent_name = store_agent_name;
    store_side_effects = store_side_effects;
    store_potential_side_effects = store_potential_side_effects;
    store_bonds_rhs = store_bonds_rhs;
    store_bonds_lhs = store_bonds_lhs
  }

(************************************************************************************)

let init_bdu_common_static =
  let init_agent_name = Ckappa_sig.RuleAgent_map_and_set.Map.empty in
  let init_half_break     = Ckappa_sig.AgentSite_map_and_set.Map.empty  in
  let init_remove         = Ckappa_sig.AgentSite_map_and_set.Map.empty  in
  let init_potential_free = Ckappa_sig.AgentRule_map_and_set.Map.empty in
  let init_potential_bind = Ckappa_sig.AgentRule_map_and_set.Map.empty in
  let init_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_bonds_lhs = Ckappa_sig.Rule_map_and_set.Map.empty in
  let init_common_static =
    {
      store_agent_name              = init_agent_name;
      store_side_effects            = (init_half_break, init_remove);
      store_potential_side_effects  = (init_potential_free, init_potential_bind); 
      store_bonds_rhs = init_bonds_rhs;
      store_bonds_lhs = init_bonds_lhs;
    }
  in
  init_common_static

(************************************************************************************)

let scan_rule_set parameter error handler_kappa compil =
  let error, store_result =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        scan_rule
          parameter
          error
          handler_kappa
          rule_id
          rule.Cckappa_sig.e_rule_c_rule
          store_result
      ) compil.Cckappa_sig.rules init_bdu_common_static
  in
  error, store_result
