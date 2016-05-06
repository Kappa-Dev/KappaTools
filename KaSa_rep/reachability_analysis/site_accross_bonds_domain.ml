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

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  (* domain specific info: *)
  (* collect the set of tuples (A,x,y,B,z,t) such that there exists a rule
     with a bond connecting the site A.x and B.z and that the agent of type A
     document a site y <> x, and the agent of type B document a site t <> z *)

  (* for each tuple, collect three maps -> (A,x,y,B,z,t) -> Ag_id list
     RuleIdMap to explain in which rule and which agent_id the site y can be
     modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
     which agent_id the site t can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
     which agent_id (A) a site x of A may become bound to a site z of
     B *)

  type local_static_information =
    {
      store_site_accross_bonds_rhs: Ckappa_sig.PairAgentSiteState_map_and_set.Set.t
          Ckappa_sig.Rule_map_and_set.Map.t;
      dummy:unit;
    }

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      local_static_information : local_static_information
    }

  (*--------------------------------------------------------------------*)
  (* a triple of maps *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables
     that describes the relation between the state of y and the state of t,
     when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
     that decribes the range of y when both agents are connected via x and
     z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
     that decribes the range of t when both agents are connected via x and
     z *)

  type local_dynamic_information = unit (* to be replaced *)

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  let get_bonds_rhs static = lift Analyzer_headers.get_bonds_rhs static

  (*--------------------------------------------------------------------*)

  let get_local_static_information static = static.local_static_information

  let set_local_static_information local static =
    {
      static with
        local_static_information = local
    }

  let get_site_accross_bonds_rhs static =
    (get_local_static_information static).store_site_accross_bonds_rhs

  let set_site_accross_bonds_rhs site_accross static =
    set_local_static_information
      {
        (get_local_static_information static) with
          store_site_accross_bonds_rhs = site_accross
      } static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  (*--------------------------------------------------------------------*)

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

  (**************************************************************************)
  (*Implementation of local static information*)

  (*let collect_site_accross_bonds_rhs parameter error rule_id rule store_bonds_rhs
      store_result =
    (*look inside the views rhs*)
    let error, store_result =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
        parameter error
        (fun parameter error agent_id agent store_result ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, store_result
          | Cckappa_sig.Dead_agent (agent,_,_,_)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a set of pair (site, state) of this the agent interface*)
            let error, pair_set =
              Ckappa_sig.Site_map_and_set.Map.fold
                (fun site_type port (error, store_set) ->
                  let state = port.Cckappa_sig.site_state.Cckappa_sig.max in
                  let error, store_set =
                    Ckappa_sig.SiteState_map_and_set.Set.add_when_not_in
                      parameter
                      error
                      (site_type, state)
                      store_set
                  in
                  error, store_set
                ) agent.Cckappa_sig.agent_interface
                (error, Ckappa_sig.SiteState_map_and_set.Set.empty)
            in
            (*from the first agent, get their partner that is bond in the rhs.
              get a pair of site, state that is bond in their rhs*)
            let error, pair_bonds_rhs_set =
              match
                Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
                  parameter
                  error
                  rule_id
                  store_bonds_rhs
              with
              | error, None -> error, Ckappa_sig.PairAgentSiteState_map_and_set.Set.t
              | error, Some s -> error, s
            in
            (*get the partner of binding information*)
            let error, (agent_type2, site_type2, state2) =
              Ckappa_sig.PairAgentSiteState_map_and_set.Set.fold
                (fun ((agent_type1, site_type1, state1), (agent_type2, site_type2, state2))
                (error, result_triple) ->
                    (*if the set of pair of the agent interface belong to first pair*)
                  if agent_type1 = agent_type &&
                    (Ckappa_sig.SiteState_map_and_set.Set.mem
                       (site_type1, state1)
                       pair_set)
                  then error, (agent_type2, site_type2, state2)
                  else error, result_triple
                ) pair_bonds_rhs_set (error, (0,0,0))
            in
            (*TODO:get agent of the first*)



        ) rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_result
    in
    error, store_result*)

  let scan_rule_set static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
        parameter
        error
        (fun parameter error rule_id rule static ->
          (*let store_site_accross_bonds_rhs = get_site_accross_bonds_rhs static in
          let error, store_site_accross_bonds_rhs =
            collect_site_accross_bonds_rhs
              parameter
              error
              rule_id
              rule.Cckappa_sig.e_rule_c_rule
              store_site_accross_bonds_rhs
          in
          let static = set_site_accross_bonds_rhs store_site_accross_bonds_rhs static in*)
          error, static
        ) compil.Cckappa_sig.rules static
    in
    error, static, dynamic

  (**************************************************************************)
  (** [get_scan_rule_set static] *)

  (* todo *)
  let initialize static dynamic error =
    let init_local_static_information =
      {
        store_site_accross_bonds_rhs = Ckappa_sig.Rule_map_and_set.Map.empty;
        dummy = ()
      }
    in
    let init_global_static_information =
      {
        global_static_information = static;
        local_static_information = init_local_static_information;
      }
    in
    (*  let kappa_handler = Analyzer_headers.get_kappa_handler static in
        let parameter = Analyzer_headers.get_parameter static in*)
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = () ;
      }
    in
    let error, static, dynamic =
      scan_rule_set
        init_global_static_information
        init_global_dynamic_information
        error
    in
    error, static, dynamic


  (* to do *)
  (* take into account bounds that may occur in initial states *)
  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* check for each bond that occur in the lhs, and if half bond, whether
     the constraints in the lhs are consistent *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (* to do *)
  let apply_rule static dynamic error rule_id precondition =
    (* look into the static information which tuples may be affected *)
    (* use the method in precondition, to ask about information captured by the view domains *)
    (* take into account all cases  *)
    (* modification of y and/or t and we do not know whether the agents are bound *)
    (* modification of y and/or t accross a bond that is preserved *)
    (* creation of a bond with/without modification of z,t *)
    let event_list = [] in
    (*  let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
        let compil = get_compil static in*)
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  (* to do *)
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  (* to do *)
  let print static dynamic error loggers =
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
