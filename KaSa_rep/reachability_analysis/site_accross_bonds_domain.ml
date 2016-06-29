(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  (* the type of the struct that contains all static information as in the previous version of the analysis *)

  (* domain specific info: *)
  (* collect the set of tuples (A,x,y,B,z,t) such that there exists a rule with a bond connecting the site A.x and B.z and that the agent of type A document a site y <> x, and the agent of type B document a site t <> z *)

  (* for each tuple, collect three maps -> (A,x,y,B,z,t) -> Ag_id list
     RuleIdMap to explain in which rule and which agent_id the site y can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and which agent_id the site t can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and which agent_id (A) a site x of A may become bound to a site z of B *)

  type local_static_information =
    {
      store_basic_static_information: Site_accross_bonds_domain_static.basic_static_information;
      dummy:unit;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  (*--------------------------------------------------------------*)
  (* a triple of maps : mvbdu_of_association_list*)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables that describes the relation between the state of y and the state of t, when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of y when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables that decribes the range of t when both agents are connected via x and z *)

  type local_dynamic_information =
    {
      store_init : Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t;
      (**)
      store_tuple_pair_state_aux : (Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.t*Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.t) Ckappa_sig.Rule_map_and_set.Map.t;
      store_tuple_pair_state:
        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      (**)
      store_tuple_pair_binding_state_aux:
        (Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.t*Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.t) Ckappa_sig.Rule_map_and_set.Map.t;
      store_tuple_pair_binding_state:
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t;
      store_tuple_pair_binding_and_state : Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.t;
      (**)
      store_tuple_pair_question_marks_state_aux:
        (Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.t * Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.t) Ckappa_sig.Rule_map_and_set.Map.t;
      store_tuple_pair_question_marks_state:
        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t * Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.t;

      (*store_relation_mvbdu :
        Ckappa_sig.Views_bdu.mvbdu
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
        (*range of the second site of the first agent, when both agents are connected via the first site*)
        store_range_site_first_agent:
        Ckappa_sig.Views_bdu.mvbdu
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
        store_range_site_second_agent:
        Ckappa_sig.Views_bdu.mvbdu
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;*)
    }

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value of type static_information. Kappa handler is static and thus it should never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*-------------------------------------------------------------*)

  let get_local_static_information static = static.local_static_information

  let set_local_static_information local static =
    {
      static with
      local_static_information = local
    }

  let get_basic_static_information static =
    (get_local_static_information static).store_basic_static_information

  let set_basic_static_information domain static =
    set_local_static_information
      {
        (get_local_static_information static) with
      store_basic_static_information = domain
      } static

  let get_views_rhs static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_views_rhs

  let set_views_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_views_rhs = r
      } static

  let get_bonds_rhs static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_bonds_rhs

  let set_bonds_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_bonds_rhs = r
      } static

  let get_created_bond static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_created_bond

  let set_created_bond r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bond = r
      } static

  let get_modified_internal_state_and_bond static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_modified_internal_state_and_bond

  let set_modified_internal_state_and_bond r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_modified_internal_state_and_bond = r
      } static

  let get_question_marks_rhs static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_question_marks_rhs

  let set_question_marks_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_question_marks_rhs = r
      } static

  let get_modified_map static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_modified_map

  let set_modified_map r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_modified_map = r
      } static

  let get_tuple_pair static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_tuple_pair

  let set_tuple_pair r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_tuple_pair = r
      } static

  let get_explicit_rule static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_explicit_static

  let set_explicit_rule r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_explicit_static = r
      } static

  let get_implicit_rule static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_implicit_static

  let set_implicit_rule r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_implicit_static = r
      } static

  (*------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    {
      dynamic with global = gdynamic
    }

  (*handler*)
  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global = Analyzer_headers.set_mvbdu_handler handler (get_global_dynamic_information dynamic)
    }

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  let get_init dynamic =
    (get_local_dynamic_information dynamic).store_init

  let set_init init dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_init = init
      } dynamic

  let get_tuple_pair_state dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_state

  let set_tuple_pair_state tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_state = tuple
      } dynamic

  let get_tuple_pair_state_aux dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_state_aux

  let set_tuple_pair_state_aux tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_state_aux = tuple
      } dynamic

  let get_tuple_pair_binding_state_aux dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_binding_state_aux

  let set_tuple_pair_binding_state_aux tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_binding_state_aux = tuple
      } dynamic

  let get_tuple_pair_binding_state dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_binding_state

  let set_tuple_pair_binding_state tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_binding_state = tuple
      } dynamic

  let get_tuple_pair_binding_and_state dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_binding_and_state

  let set_tuple_pair_binding_and_state tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_binding_and_state = tuple
      } dynamic

  let get_tuple_pair_question_marks_state_aux dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_question_marks_state_aux

  let set_tuple_pair_question_marks_state_aux tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_question_marks_state_aux = tuple
      } dynamic

  let get_tuple_pair_question_marks_state dynamic =
    (get_local_dynamic_information dynamic).store_tuple_pair_question_marks_state

  let set_tuple_pair_question_marks_state tuple dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_tuple_pair_question_marks_state = tuple
      } dynamic

(*
  let get_relation_mvbdu dynamic =
    (get_local_dynamic_information dynamic).store_relation_mvbdu

  let set_relation_mvbdu mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_relation_mvbdu = mvbdu
      } dynamic

  let get_range_site_first_agent dynamic =
    (get_local_dynamic_information dynamic).store_range_site_first_agent

  let set_range_site_first_agent mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_range_site_first_agent = mvbdu
      } dynamic

  let get_range_site_second_agent dynamic =
    (get_local_dynamic_information dynamic).store_range_site_second_agent

  let set_range_site_second_agent mvbdu dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_range_site_second_agent = mvbdu
      } dynamic
    *)

  (*-------------------------------------------------------------*)

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

  (****************************************************************)
  (*return a set of two different agents, each agent has two different sites *)

  let collect_pair_sites parameter error rule_id store_pair_rhs store_result =
    let error, store_pair_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_pair_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, store_result =
      (*fold over this set*)
      let error, lists = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
          (fun x (error, current_set) ->
             let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
             (*fold again*)
             let error, pair_set =
               Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
                 (fun z (error, current_set) ->
                    let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                    if agent_id <> agent_id'
                    then
                      let error, pair_set =
                        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                          ((agent_id, agent_type, site_type, site_type2, state, state2),
                           (agent_id', agent_type', site_type', site_type2', state', state2'))
                          current_set
                      in
                      error, pair_set
                    else error, current_set
                 ) store_pair_set (error, current_set)
             in
             error, pair_set
          ) store_pair_set (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
      in
      Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error
        rule_id lists store_result
    in
    error, store_result

  (****************************************************************)

  let scan_rule parameter error rule_id rule static =
    let kappa_handler = get_kappa_handler static in
    (*------------------------------------------------------------*)
    (*views on the right hand side*)
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      Site_accross_bonds_domain_static.collect_views_rhs parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    (*------------------------------------------------------------*)
    (*bonds on the right hand side*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, store_bonds_rhs =
      Site_accross_bonds_domain_static.collect_bonds_rhs parameter error rule_id rule store_bonds_rhs
    in
    let static = set_bonds_rhs store_bonds_rhs static in
    (*------------------------------------------------------------*)
    (*tuple pair*)
    let store_views_rhs = get_views_rhs static in
    let error, store_test =
      Site_accross_bonds_domain_static.collect_pair_sites_aux parameter error rule_id store_views_rhs
    in
    let store_tuple_pair = get_tuple_pair static in
    let error, store_tuple_pair =
      Site_accross_bonds_domain_static.collect_tuple_pair parameter error rule_id store_test store_tuple_pair
    in
    let static = set_tuple_pair store_tuple_pair static in
    (*------------------------------------------------------------*)
    (*modification*)
    let store_modified_map = get_modified_map static in
    let error, store_modified_map =
      Site_accross_bonds_domain_static.collect_site_modified parameter error rule_id rule store_modified_map
    in
    let static = set_modified_map store_modified_map static in
    (*------------------------------------------------------------*)
    (*created bond*)
    let store_tuple_pair = get_tuple_pair static in
    let store_created_bond = get_created_bond static in
    let error, store_created_bond =
      Site_accross_bonds_domain_static.collect_created_bond parameter error rule_id rule store_tuple_pair store_created_bond
    in
    let static = set_created_bond store_created_bond static in
    (*------------------------------------------------------------*)
    (*internal state*)
    let store_bonds_rhs = get_bonds_rhs static in
    let store_modified_map = get_modified_map static in
    let store_modified_internal_state_and_bond = get_modified_internal_state_and_bond static in
    let error, store_modified_internal_state_and_bond =
      Site_accross_bonds_domain_static.collect_modified_internal_and_bond parameter error rule_id store_tuple_pair store_bonds_rhs store_modified_map store_modified_internal_state_and_bond
    in
    let static = set_modified_internal_state_and_bond store_modified_internal_state_and_bond static in
    (*------------------------------------------------------------*)
    (*question marks on the right hand side*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let error, store_question_marks_rhs =
      Site_accross_bonds_domain_static.collect_question_marks_rhs parameter error rule_id kappa_handler rule store_modified_map store_question_marks_rhs
    in
    let static = set_question_marks_rhs store_question_marks_rhs static in
    (*------------------------------------------------------------*)
    (*implicit static information*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_implicit_static =
      Site_accross_bonds_domain_static.collect_implicit_static parameter error store_tuple_pair store_question_marks_rhs
    in
    let static = set_implicit_rule store_implicit_static static in
    (*------------------------------------------------------------*)
    (*explicit static information*)
    let store_modified_internal_state_and_bond = get_modified_internal_state_and_bond static in
    let store_created_bond = get_created_bond static in
    let store_explicit_static = get_explicit_rule static in
    let error, store_explicit_static =
      Site_accross_bonds_domain_static.collect_explicit_static parameter error store_created_bond store_modified_internal_state_and_bond store_explicit_static
    in
    let static = set_explicit_rule store_explicit_static static in
    error, static

  let scan_rule_set static dynamic error =
  let parameter = get_parameter static in
  let compil = get_compil static in
  let error, static =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error rule_id rule static ->
         let error, static =
           scan_rule
             parameter error rule_id
             rule.Cckappa_sig.e_rule_c_rule static
         in
         error, static
      ) compil.Cckappa_sig.rules static
  in
  error, static, dynamic

  (****************************************************************)
  (** [get_scan_rule_set static] *)

  (* todo *)
  let initialize static dynamic error =
    let init_local_static_information =
      {
        store_basic_static_information =
          Site_accross_bonds_domain_static.init_basic_static_information;
        dummy = ()
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static_information;
      }
    in
    let init_local_dynamic_information =
      {
        store_init = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty;
        store_tuple_pair_state_aux = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_tuple_pair_state = Ckappa_sig.Rule_map_and_set.Map.empty;
        store_tuple_pair_binding_state_aux =
          Ckappa_sig.Rule_map_and_set.Map.empty;
        store_tuple_pair_binding_state =
          Ckappa_sig.Rule_map_and_set.Map.empty;
        store_tuple_pair_binding_and_state =
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty;
        store_tuple_pair_question_marks_state_aux =
          Ckappa_sig.Rule_map_and_set.Map.empty;
        store_tuple_pair_question_marks_state =
          Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty , Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty;
        (*store_relation_mvbdu = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
          store_range_site_first_agent = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
          store_range_site_second_agent = Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;*)
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_local_dynamic_information;
      }
    in
    let error, static, dynamic =
      scan_rule_set
        init_global_static_information
        init_global_dynamic_information
        error
    in
    error, static, dynamic

  (******************************************************************)
  (*initial states*)

  (*collect init where it has the first site can be bound and the second site is different than the first one*)

  let collect_views_init parameter error init_state =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id agent store_result ->
         match agent with
         | Cckappa_sig.Ghost
         | Cckappa_sig.Unknown_agent _ -> error, store_result
         | Cckappa_sig.Dead_agent (agent, _, _, _)
         | Cckappa_sig.Agent agent ->
           let agent_type = agent.Cckappa_sig.agent_name in
           let error, set =
             Ckappa_sig.Site_map_and_set.Map.fold
               (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, state)
                      store_set
                  in
                  error, store_set
               ) agent.Cckappa_sig.agent_interface (error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty)
           in
           let error', new_set =
             Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.union parameter error store_result set
           in
           let error = Exception.check warn parameter error error'
               (Some "line 904") Exit
           in
           error, new_set
      ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty

  let collect_pair_sites_init_aux parameter error store_views_init =
    let error, store_result =
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id, agent_type, site_type, state) (error, store_result) ->
          (*fold again views in the initial states*)
          let error, pair_set =
            Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold_inv (fun (agent_id', _, site_type', state') (error, current_set) ->
                if agent_id = agent_id' && site_type <> site_type'
                then
                  let error, pair_set =
                    Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in
                      parameter error
                      (agent_id, agent_type, site_type, site_type', state, state')
                      current_set
                  in
                  error, pair_set
                else error, current_set
              ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
          in
          let error, new_set =
            Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error store_result pair_set
          in
          error, new_set
        ) store_views_init (error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
    in
    error, store_result

  let collect_pair_sites_init parameter error store_pair_init_aux =
    let error, store_result =
      Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
        (fun x (error, current_set) ->
           let (agent_id, agent_type, site_type, site_type2, state, state2) = x in (*A*)
           (*fold again*)
           let error, pair_set =
             Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold_inv
               (fun z (error, current_set) ->
                  let (agent_id', agent_type', site_type', site_type2', state', state2') = z in
                  if agent_id <> agent_id'
                  then
                    let error, pair_set =
                      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                        ((agent_id, agent_type, site_type, site_type2, state, state2),
                         (agent_id', agent_type', site_type', site_type2', state', state2'))
                        current_set
                    in
                    error, pair_set
                  else error, current_set
               ) store_pair_init_aux (error, current_set)
           in
           error, pair_set
        ) store_pair_init_aux (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
    in
    error, store_result

  let collect_pair_of_bonds parameter error site_add agent_id site_type_source views =
    let error, pair =
      let agent_index_target = site_add.Cckappa_sig.agent_index in
      let site_type_target = site_add.Cckappa_sig.site in
      let error, agent_source =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_id views
        with
        | error, None -> warn parameter error (Some "line 632") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, agent_target =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameter error agent_index_target views
        with
        | error, None -> warn parameter error (Some "line 640") Exit Cckappa_sig.Ghost
        | error, Some agent -> error, agent
      in
      let error, (agent_type1, state1) =
        Site_accross_bonds_domain_static.collect_agent_type_state
          parameter
          error
          agent_source
          site_type_source
      in
      let error, (agent_type2, state2) =
        Site_accross_bonds_domain_static.collect_agent_type_state
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

  let collect_bonds_init parameter error init_state =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error agent_id bonds_map store_result ->
         let error, store_result =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site_type_source site_add (error, store_result) ->
                let error,
                    ((agent_type_source, site_type_source, state_source),
                     (agent_type_target, site_type_target, state_target)) =
                  collect_pair_of_bonds
                    parameter error site_add agent_id site_type_source
                    init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views
                in
                let pair =
                  ((agent_id, agent_type_source, site_type_source, state_source),
                   (site_add.Cckappa_sig.agent_index, agent_type_target, site_type_target, state_target))
                in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.add_when_not_in
                    parameter
                    error
                    pair
                    store_result
                in
                error, store_result
             ) bonds_map (error, store_result)
         in
         error, store_result
      ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty

  let collect_init parameter error init_state store_result =
    (*a set of site that can be bounds*)
    let error, store_bonds_init =
      collect_bonds_init parameter error init_state in
    (*views in the initial state that has two agents and their sites are different*)
    let error, store_views_init =
      collect_views_init parameter error init_state in
    let error, store_pair_sites_init_aux =
      collect_pair_sites_init_aux parameter error store_views_init in
    let error, store_pair_sites_init =
      collect_pair_sites_init parameter error store_pair_sites_init_aux in
    (*fold over a set of pair and check the first site whether or not it belongs to a set of sites that can be bound*)
    let error, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold_inv
        (fun (x, y) (error, store_result) ->
           let (agent_id, agent_type, site_type, _, state, _) = x in
           let (agent_id', agent_type', site_type', _, state', _) = y in
           if Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
               ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state'))
               store_bonds_init
           then
             let error, pair_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error
                 (x, y)
                 Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
             in
             let error', new_set =
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set store_result
             in
             let error = Exception.check warn parameter error error'
                 (Some "line 1060") Exit
             in
             error, new_set
           else
             error, store_result
        ) store_pair_sites_init (error, store_result)
    in
    error, store_result

  (*------------------------------------------------------------*)
  (* take into account bounds that may occur in initial states *)

  let add_initial_state static dynamic error species =
    let parameter = get_parameter static in
    let store_result = get_init dynamic in
    let error, store_init =
      collect_init parameter error species store_result
    in
    let dynamic = set_init store_init dynamic in
    let event_list = [] in
    error, dynamic, event_list

  (*------------------------------------------------------------*)
  (*implementation dynamic information*)

  (*first map, (A,x,y,B,z,t) to a mvbdu: describes the relation between the state of y and the state of t, when both agents are connected via x.z*)
  (*
  let collect_realtion_mvbdu static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_relation_mvbdu dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via x.z*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold (fun _ set (error, handler, store_result) ->
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
            (fun (x, y) (error, handler, store_result)->
               (*build the mvbdu of two sites y(site_type2) and z(site_type2')*)
               let (_, _, _, site_type2, _, state2) = x in
               let (_, _, _, site_type2', _, state2') = y in
               (*build (key * value) list *)
               let pair_list =
                 [(site_type2, state2);(site_type2', state2')]
               in
               let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
               let error, store_result =
                 Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x,y) mvbdu store_result
               in
               error, handler, store_result
            ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    (*store handler and the result of the relation *)
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_relation_mvbdu store_result dynamic in
    error, dynamic
           *)
  (*------------------------------------------------------------*)
  (*range of the second site of the first agent, when both agents are connected via the first site*)

  (*
  let collect_range_site_first_agent static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_range_site_first_agent dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via the first site*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun _ set (error, handler, store_result) ->
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, handler, store_result) ->
                (*build the mvbdu of the site y (site_type2)*)
                let (_, _, _, site_type2, _, state2) = x in
                (*build (key * value) list*)
                let pair_list = [(site_type2, state2)] in
                let error, handler, mvbdu =
                  Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x, y) mvbdu store_result
                in
                error, handler, store_result
             ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_range_site_first_agent store_result dynamic in
    error, dynamic
           *)

  (*
  let collect_range_site_second_agent static dynamic error =
    let parameter = get_parameter static in
    let store_result = get_range_site_second_agent dynamic in
    let handler = get_mvbdu_handler dynamic in
    (*get the set of tuple when both agents are connected via the first site*)
    let store_first_site_is_bound = get_first_site_is_bound static in
    let error, handler, store_result =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun _ set (error, handler, store_result) ->
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, handler, store_result) ->
                (*build the mvbdu of the site y (site_type2)*)
                let (_, _, _, site_type2, _, state2) = y in
                (*build (key * value) list*)
                let pair_list = [(site_type2, state2)] in
                let error, handler, mvbdu =
                  Ckappa_sig.Views_bdu.mvbdu_of_association_list parameter handler error pair_list in
                let error, store_result =
                  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.add_or_overwrite parameter error (x, y) mvbdu store_result
                in
                error, handler, store_result
             ) set (error, handler, store_result)
        ) store_first_site_is_bound (error, handler, store_result)
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_range_site_second_agent store_result dynamic in
    error, dynamic

           *)
  (* to do *)
  (* check for each bond that occur in the lhs, whether
     the constraints in the lhs are consistent *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (*************************************************************)
  (*return a pair of tuple in the case of explicit static information: created a bond, the second site can be modified and the first site is bound. *)

  let collect_tuple_pair_internal_state_aux error static dynamic rule_id =
    (*get init information*)
    let store_result = get_tuple_pair_state_aux dynamic in
    let parameter = get_parameter static in
    let store_views_rhs = get_views_rhs static in
    let store_explicit_static = get_explicit_rule static in
    let error, views_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, tuple_pair_explicit_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_explicit_static with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*------------------------------------------------------------*)
    let error, store_result =
      (*fold over a views rhs set, to test each rule*)
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
        (fun x (error, store_result) ->
           (*------------------------------------------------------*)
           let (_, agent_type, site_type, state) = x in
           (*fold over an explicit tuple pair*)
           let error, store_result = Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold (fun (t, z) (error, store_result) ->
               let (agent_id1, agent_type1, site_type1, site_type2) = t in
               let (agent_id1', agent_type1', site_type1', site_type2') = z in
               (*--------------------------------------------------*)
               (*check if the site of the views on the rhs belongs to the second site of the pair, if yes then return its state*)
               let error, first_agent =
                 if agent_type = agent_type1 && site_type = site_type2
                 then
                   let internal_state =
                     (agent_id1, agent_type1, site_type1, site_type2, state)
                   in
                   let error, set =
                     Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.add_when_not_in parameter error
                       internal_state
                       Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
                   in
                   error, set
                 else
                   error, Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
               in
               (*--------------------------------------------------*)
               (*second agent*)
               let error, second_agent =
                 if agent_type = agent_type1' && site_type = site_type2'
                 then
                   let internal_state =
                     (agent_id1', agent_type1', site_type1', site_type2', state)
                   in
                   let error, set =
                     Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.add_when_not_in parameter error
                       internal_state
                       Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
                   in
                   error, set
                 else
                   error, Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty
               in
               (*---------------------------------------------------*)
               let error, (old_set1, old_set2) =
                 match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                 | error, None -> error, (Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty,Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.empty)
                 | error, Some (s1, s2) -> error, (s1, s2)
               in
               let error, new_set1 = Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.union parameter error first_agent old_set1 in
               let error, new_set2 = Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.union parameter error second_agent old_set2 in
               let error, store_result =
                 Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id (new_set1, new_set2) store_result
               in
               error, store_result
             ) tuple_pair_explicit_set (error, store_result)
           in
           error, store_result
        ) views_set (error, store_result)
    in
    error, store_result

  (****************************************************************)
  (*return the internal state of the second site in the tuple pair*)

  let collect_tuple_pair_internal_state parameter error dynamic =
    let store_tuple_pair_state_aux = get_tuple_pair_state_aux dynamic in
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun (set1, set2) ->
           let _, set =
             Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold (fun (agent_id, agent_type, site_type, site_type2, state2) (error, store_set) ->
                 Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold
                   (fun (agent_id', agent_type', site_type', site_type2', state2') (error, store_set) ->
                      if agent_type <> agent_type'
                      then
                        let pair =
                          (agent_id, agent_type, site_type, site_type2, state2),
                          (agent_id', agent_type', site_type', site_type2', state2')
                        in
                        let error, new_set =
                          Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in
                            parameter error pair store_set
                        in
                        error, new_set
                      else
                        error, store_set
                   ) set2 (error, store_set)
               ) set1 (error, Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty)
           in
           set
        ) store_tuple_pair_state_aux
    in
    store_result

  (****************************************************************)
  (*check the first site on the rhs belong to the first site of the tuple pair*)

  let collect_tuple_pair_binding_state_aux parameter error rule_id static dynamic =
    let store_result = get_tuple_pair_binding_state_aux dynamic in
    let store_views_rhs = get_views_rhs static in
    let store_tuple_pair_state = get_tuple_pair_state dynamic in
    (*------------------------------------------------*)
    let error, views_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*------------------------------------------------*)
    let error, tuple_pair_internal_state_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_tuple_pair_state with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*------------------------------------------------*)
    let error, store_result =
      (*fold over the views on the rhs*)
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
        (fun x (error, store_result) ->
           let (_, agent_type, site_type, state) = x in
           (*fold over a tuple pair which the information of the state of the second site*)
           let error, store_result =
             Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
               (fun (t, z) (error, store_result) ->
                  let (agent_id1, agent_type1, site_type1, site_type2, state2) = t in
                  let (agent_id1', agent_type1', site_type1', site_type2', state2') = z in
                  (*------------------------------------------------*)
                  (*check the site on the rhs belongs to the first site of the tuple pair*)
                  let error, first_agent =
                    if agent_type = agent_type1 && site_type = site_type1
                    then
                      let binding_state =
                        (agent_id1, agent_type1, site_type1, site_type2, state, state2)
                      in
                      let error, set =
                        Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in parameter error binding_state
                          Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
                      in
                      error, set
                    else
                      error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
                  in
                  (*------------------------------------------------*)
                  let error, second_agent =
                    if agent_type = agent_type1' && site_type = site_type1'
                    then
                      let binding_state =
                        (agent_id1', agent_type1', site_type1', site_type2', state, state2')
                      in
                      let error, set =
                        Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.add_when_not_in parameter error binding_state
                          Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
                      in
                      error, set
                    else
                      error, Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty
                  in
                  (*------------------------------------------------*)
                  let error, (old_set1, old_set2) =
                    match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                    | error, None -> error, (Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty,Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.empty)
                    | error, Some (s1, s2) -> error, (s1, s2)
                  in
                  let error, new_set1 = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error first_agent old_set1 in
                  let error, new_set2 = Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.union parameter error second_agent old_set2 in
                  let error, store_result =
                    Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id (new_set1, new_set2) store_result
                  in
                  error, store_result
               ) tuple_pair_internal_state_set (error, store_result)
           in
           error, store_result
        ) views_set (error, store_result)
    in
    error, store_result

(****************************************************************)
(**)

  let collect_tuple_pair_binding_state parameter error dynamic =
    let store_tuple_pair_binding_state_aux = get_tuple_pair_binding_state_aux dynamic in
    let store_result =
      Ckappa_sig.Rule_map_and_set.Map.map
        (fun (set1, set2) ->
           let _, set =
             Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold (fun (agent_id, agent_type, site_type, site_type2, state, state2) (error, store_set) ->
                 Site_accross_bonds_domain_type.AgentsSitesStates_map_and_set.Set.fold
                   (fun (agent_id', agent_type', site_type', site_type2', state', state2') (error, store_set) ->
                      if agent_type <> agent_type'
                      then
                        let pair =
                          ((agent_id, agent_type, site_type, site_type2, state, state2),
                           (agent_id', agent_type', site_type', site_type2', state', state2'))
                        in
                        let error, set =
                          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.add_when_not_in parameter error pair store_set
                        in
                        error, set
                      else error, store_set
                   ) set2 (error, store_set)
               ) set1 (error, Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty)
           in
           set
        ) store_tuple_pair_binding_state_aux
    in
    store_result

(****************************************************************)
(*return a set of tuple pair statisfy the condition of explicit static information*)

  let collect_tuple_pair_binding_and_state parameter error dynamic =
    (*add information about initial state*)
    let store_binding_and_state = get_tuple_pair_binding_and_state dynamic in
    let store_tuple_pair_binding_state = get_tuple_pair_binding_state dynamic in
    let store_init = get_init dynamic in
    let error, store_binding_and_state =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun _ pair_set (error, store_result) ->
           let error, store_result =
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error pair_set store_result
             in
             error, store_result
        ) store_tuple_pair_binding_state (error, store_binding_and_state)
    in
    (*FIXME: is it union?*)
    let error, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.union parameter error store_init store_binding_and_state
    in
    error, store_result

(****************************************************************)
  (*check in the case of implicit static information: question marks, and has the second site can be modified*)

  let collect_tuple_pair_question_marks_state_aux error rule_id static dynamic =
    let store_result = get_tuple_pair_question_marks_state_aux dynamic in
    let parameter = get_parameter static in
    let store_views_rhs = get_views_rhs static in
    let store_implicit_static = get_implicit_rule static in
    let error, views_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_views_rhs with
      | error, None -> error, Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, implicit_static_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_implicit_static with
      | error, None -> error, Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, store_result =
      (*fold over a views rhs, to test each rule and get the information of internal state*)
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold (fun x (error, store_result) ->
          let (_, agent_type, site_type, state) = x in
          (*fold over implicit static tuple pair*)
          let error, store_result =
            Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.fold (fun (t, z) (error, store_result) ->
                let (agent_id1, agent_type1, site_type1, site_type2) = t in
                let (agent_id1', agent_type1', site_type1', site_type2') = z in
                (*check if the second site of the tuple is the site on the rhs: return the internal state of B(t~)*)
                let error, first_agent =
                  if agent_type = agent_type1 && site_type = site_type2
                  then
                    let internal_state =
                      (agent_id1, agent_type1, site_type1, site_type2, state), z
                    in
                    let error, set =
                      Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.add_when_not_in parameter error
                        internal_state
                        Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty
                    in
                    error, set
                  else error, Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty
                in
                (*--------------------------------------------------*)
                let error, second_agent =
                  if agent_type = agent_type1' && site_type = site_type2'
                  then
                    let internal_state =
                      t, (agent_id1', agent_type1', site_type1', site_type2', state)
                    in
                    let error, set =
                    Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.add_when_not_in parameter error
                      internal_state
                      Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty
                  in
                  error, set
                  else error, Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty
                in
                (*------------------------------------------------*)
                let error, (old_set1, old_set2) =
                match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_result with
                | error, None -> error, (Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty,Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty)
                | error, Some (s1, s2) -> error, (s1, s2)
              in
              let error, new_set1 = Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.union parameter error first_agent old_set1 in
              let error, new_set2 = Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.union parameter error second_agent old_set2 in
              let error, store_result =
                Ckappa_sig.Rule_map_and_set.Map.add_or_overwrite parameter error rule_id (new_set1, new_set2) store_result
              in
              error, store_result
              ) implicit_static_set (error, store_result)
          in
          error, store_result
        ) views_set (error, store_result)
    in
    error, store_result

(****************************************************************)
(*get the internal state in the initial state, combine with the tuple pair of the implicit static information*)

  let collect_tuple_pair_question_marks_state parameter error rule_id dynamic =
    let store_result = get_tuple_pair_question_marks_state dynamic in
    let store_tuple_pair_question_marks_state_aux =
      get_tuple_pair_question_marks_state_aux dynamic in
    let error, (pair_set1, _pair_set2) =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs parameter error rule_id store_tuple_pair_question_marks_state_aux with
      | error, None -> error, (Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.empty, Site_accross_bonds_domain_type.PairAgentsSites_SitesState_map_and_set.Set.empty)
      | error, Some (s1, s2) -> error, (s1, s2)
    in
    let store_init = get_init dynamic in
    (*fold over a set of tuple pair in implicit static information*)
    let error, store_result =
      Site_accross_bonds_domain_type.PairAgentsSitesState_Sites_map_and_set.Set.fold
        (fun (x, y) (error, store_result) ->
           (*B question mark*)
           let (agent_id, agent_type, site_type, site_type2, state2) =
             x in
           let (agent_id', agent_type', site_type', site_type2') = y in
           (*fold over the initial*)
           let error, store_result =
             Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
               (fun (z, t) (error, store_result) ->
                  let (store_result1, store_result2) = store_result in
                  let (agent_id_init, agent_type_init, site_type_init, site_type_init2, _, state_init2) = z in
                  let (agent_id_init', agent_type_init', site_type_init', site_type_init2', _, state_init2') = t in
                  (*check the information in y with z first*)
                  let error, first_agent =
                    if (agent_type' = agent_type_init &&
                        site_type' = site_type_init &&
                        site_type2' = site_type_init2)
                    then
                      let pair =
                        x,
                        (agent_id_init, agent_type_init, site_type_init, site_type_init2, state_init2)
                      in
                      let error, set =
                        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in parameter error pair
                          store_result1
                      in
                      error, set
                    else error, store_result1
                  in
                  (**)
                  let error, second_agent =
                    if (agent_type' = agent_type_init' &&
                        site_type' = site_type_init' &&
                        site_type2' = site_type_init2')
                    then
                      let pair =
                        (agent_id_init', agent_type_init', site_type_init', site_type_init2', state_init2'), x
                      in
                      let error, set =
                        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.add_when_not_in parameter error pair
                          store_result2
                      in
                      error, set
                    else error, store_result2
                  in
                  (**)
                  error, (first_agent, second_agent)
               ) store_init (error, store_result)
           in
           error, store_result
        ) pair_set1 (error, store_result)
    in
    error, store_result


  (****************************************************************)
  (* to do : add information of init ?*)

  let apply_rule static dynamic error rule_id precondition =
    let parameter  = get_parameter static in
    (*------------------------------------------------*)
    let error, store_tuple_pair_state_aux =
      collect_tuple_pair_internal_state_aux error static dynamic rule_id
    in
    let dynamic = set_tuple_pair_state_aux store_tuple_pair_state_aux dynamic in
    (*------------------------------------------------*)
    let pair_tuple_set =
      collect_tuple_pair_internal_state parameter error dynamic
    in
    let dynamic = set_tuple_pair_state pair_tuple_set dynamic in
    (*------------------------------------------------*)
    let error, store_tuple_pair_binding_state_aux =
      collect_tuple_pair_binding_state_aux parameter error rule_id static dynamic in
    let dynamic = set_tuple_pair_binding_state_aux store_tuple_pair_binding_state_aux dynamic in
    let pair_tuple_binding =
      collect_tuple_pair_binding_state parameter error dynamic
    in
    let dynamic = set_tuple_pair_binding_state pair_tuple_binding dynamic in
    (*------------------------------------------------*)
    (* look into the static information which tuples may be affected *)
    (* take into account all cases  *)
    (* modification of y and/or t accross a bond that is preserved *)
    (* creation of a bond with/without modification of z,t *)
    let error, store_tuple_pair_binding_and_state =
      collect_tuple_pair_binding_and_state parameter error dynamic
    in
    let dynamic = set_tuple_pair_binding_and_state store_tuple_pair_binding_and_state dynamic in
    (*------------------------------------------------*)
    (* modification of y and/or t and we do not know whether the agents are bound : implicit case*)
    let error, store_tuple_pair_question_marks_state_aux =
      collect_tuple_pair_question_marks_state_aux error rule_id static dynamic in
    let dynamic = set_tuple_pair_question_marks_state_aux store_tuple_pair_question_marks_state_aux dynamic in
    let error, store_tuple_pair_question_marks_state =
      collect_tuple_pair_question_marks_state parameter error rule_id dynamic in
    let dynamic = set_tuple_pair_question_marks_state store_tuple_pair_question_marks_state dynamic in
    (* use the method in precondition, to ask about information captured by the view domains *)


    let event_list = [] in
    error, dynamic, (precondition, event_list)


  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  (* to do *)
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (****************************************************************)
(* to do *)

  let print static dynamic error loggers =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let log = Remanent_parameters.get_logger parameter in    (*--------------------------------------------------------*)
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Site accross bonds domain\n";
          Loggers.fprintf log
            "------------------------------------------------------------\n"
        in
        error
      else error
    in
    (*--------------------------------------------------------*)
    (*print basic static information*)
    let store_views_rhs = get_views_rhs static in
    let store_bonds_rhs = get_bonds_rhs static in
    let store_modified_map = get_modified_map static in
    let store_created_bond = get_created_bond static in
    let store_modified_internal_state_and_bond =
      get_modified_internal_state_and_bond static in
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_explicit_static = get_explicit_rule static in
    let store_implicit_static = get_implicit_rule static in
    let error =
      Site_accross_bonds_domain_static.print_basic_static_information
        parameter error handler_kappa log
        store_views_rhs
        store_bonds_rhs
        store_modified_map
        store_created_bond
        store_modified_internal_state_and_bond
        store_question_marks_rhs
        store_explicit_static
        store_implicit_static
    in
    (*--------------------------------------------------------*)
    (*get internal state of implicit static information*)
    let store_tuple_pair_question_marks_state_aux = get_tuple_pair_question_marks_state_aux dynamic in
    let () =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair_question_marks_state_aux parameter error handler_kappa log store_tuple_pair_question_marks_state_aux
        in ()
    in
    (*--------------------------------------------------------*)
    let store_tuple_pair_question_marks_state = get_tuple_pair_question_marks_state dynamic in
    let () =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair_question_marks_state parameter error handler_kappa log store_tuple_pair_question_marks_state
        in ()
    in
    (*--------------------------------------------------------*)
    let store_init = get_init dynamic in
    let () =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let ()
          =
          Print_site_accross_bonds_domain.print_init parameter error handler_kappa log store_init
        in ()
      else ()
    in
    (*--------------------------------------------------------*)
    (*let store_tuple_pair_state = get_tuple_pair_state dynamic in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair_internal_state parameter error handler_kappa log store_tuple_pair_state
        in error
      else error
    in*)
    (*--------------------------------------------------------*)
    let store_tuple_pair_binding_state = get_tuple_pair_binding_state dynamic in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair_binding_state parameter error handler_kappa log store_tuple_pair_binding_state
        in error
      else error
    in
    (*--------------------------------------------------------*)
    let store_tuple_pair_binding_and_state = get_tuple_pair_binding_and_state dynamic in
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_tuple_pair_binding_and_state parameter error handler_kappa log store_tuple_pair_binding_and_state
        in error
      else error
    in
    (*--------------------------------------------------------*)
    (*dynamic information*)
    (*let store_relation_mvbdu = get_relation_mvbdu dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_relation_mvbdu parameter error handler_kappa log store_relation_mvbdu
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_range_site_first_agent = get_range_site_first_agent dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_range_site_first_agent parameter error handler_kappa log store_range_site_first_agent
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_range_site_second_agent = get_range_site_second_agent dynamic in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_range_site_second_agent parameter error handler_kappa log store_range_site_second_agent
        in error
      else error
      in
      (*--------------------------------------------------------*)
      let store_result = get_site_accross_bonds_rhs static in
      let error =
      if Remanent_parameters.get_dump_reachability_analysis_site_accross_bonds parameter
      then
        let () =
          Print_site_accross_bonds_domain.print_site_accross_bonds_domain parameter error handler_kappa log store_result
        in error
      else error
      in*)
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
