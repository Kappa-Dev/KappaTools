(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Jul 12 2016>
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
     RuleIdMap to explain in which rule and which agent_id the site y can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
        which agent_id the site t can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
        which agent_id (A) a site x of A may become bound to a site z of
        B *)

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

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables
     that describes the relation between the state of y and the state of t,
     when both agents are connected via x and z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
     that decribes the range of y when both agents are connected via x and
     z *)

  (* one maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
     that decribes the range of t when both agents are connected via x and
     z *)

  type local_dynamic_information =
    {
      dumy: unit;
      store_value : Ckappa_sig.Views_bdu.mvbdu
          Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.t;
    }

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*------------------------------------------------------------*)
  (** global static information.  explain how to extract the handler for
      kappa expressions from a value of type static_information. Kappa
      handler is static and thus it should never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*-------------------------------------------------------------*)
  (*static information*)
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

  (*-------------------------------------------------------------*)

  let get_views_rhs static =
    (get_basic_static_information static).Site_accross_bonds_domain_static.store_views_rhs

  let set_views_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_views_rhs = r
      } static

  (*sites that can be bound on the rhs*)
  let get_bonds_rhs static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_bonds_rhs

  let set_bonds_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_bonds_rhs = r
      } static

  (*sites that can be modified*)

  let get_modified_map static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_modified_map

  let set_modified_map r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_modified_map = r
      } static

  (*tuple pair*)

  let get_tuple_pair static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_tuple_pair

  let set_tuple_pair r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_tuple_pair = r
      } static

  (*rule that can created a bond *)

  let get_created_bond static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_created_bond

  let set_created_bond r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bond = r
      } static

  (**)

  let get_modified_internal_state_and_bond static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_modified_internal_state_and_bond

  let set_modified_internal_state_and_bond r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_modified_internal_state_and_bond = r
      } static
      
  (*question marks on the rhs*)

  let get_question_marks_rhs static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_question_marks_rhs

  let set_question_marks_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_question_marks_rhs = r
      } static

  (*implicit rule*)

  let get_implicit_rule static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_implicit_static

  let set_implicit_rule r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_implicit_static = r
      } static

  (*explicit rule*)
  let get_explicit_rule static =
    (get_basic_static_information 
       static).Site_accross_bonds_domain_static.store_explicit_static

  let set_explicit_rule r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_explicit_static = r
      } static


  (*------------------------------------------------------------*)
  (** dynamic information*)
  (*------------------------------------------------------------*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    {
      dynamic with global = gdynamic
    }

  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global = Analyzer_headers.set_mvbdu_handler handler
          (get_global_dynamic_information dynamic)
    }

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  let get_value dynamic =
    (get_local_dynamic_information dynamic).store_value

  let set_value value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value = value
      } dynamic

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

  let scan_rule parameter error rule_id rule static =
    let kappa_handler = get_kappa_handler static in
    (*------------------------------------------------------------*)
    (*views on the right hand side*)
    let store_views_rhs = get_views_rhs static in
    let error, store_views_rhs =
      Site_accross_bonds_domain_static.collect_views_rhs
        parameter error rule_id rule store_views_rhs
    in
    let static = set_views_rhs store_views_rhs static in
    (*------------------------------------------------------------*)
    (*bonds on the right hand side*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, store_bonds_rhs =
      Site_accross_bonds_domain_static.collect_bonds_rhs
        parameter error rule_id rule store_bonds_rhs
    in
    let static = set_bonds_rhs store_bonds_rhs static in
    (*------------------------------------------------------------*)
    (*tuple pair*)
    let store_views_rhs = get_views_rhs static in
    let error, store_test =
      Site_accross_bonds_domain_static.collect_pair_sites_aux
        parameter error rule_id store_views_rhs
    in
    let store_tuple_pair = get_tuple_pair static in
    let error, store_tuple_pair =
      Site_accross_bonds_domain_static.collect_tuple_pair
        parameter error rule_id store_test store_tuple_pair
    in
    let static = set_tuple_pair store_tuple_pair static in
    (*------------------------------------------------------------*)
    (*modification*)
    let store_modified_map = get_modified_map static in
    let error, store_modified_map =
      Site_accross_bonds_domain_static.collect_site_modified
        parameter error rule_id rule store_modified_map
    in
    let static = set_modified_map store_modified_map static in
    (*------------------------------------------------------------*)
    (*created bond*)
    let store_tuple_pair = get_tuple_pair static in
    let store_created_bond = get_created_bond static in
    let error, store_created_bond =
      Site_accross_bonds_domain_static.collect_created_bond
        parameter error rule_id rule store_tuple_pair store_created_bond
    in
    let static = set_created_bond store_created_bond static in
    (*------------------------------------------------------------*)
    (*internal state*)
    let store_bonds_rhs = get_bonds_rhs static in
    let store_modified_map = get_modified_map static in
    let store_modified_internal_state_and_bond =
      get_modified_internal_state_and_bond static in
    let error, store_modified_internal_state_and_bond =
      Site_accross_bonds_domain_static.collect_modified_internal_and_bond
        parameter error rule_id
        store_tuple_pair
        store_bonds_rhs
        store_modified_map
        store_modified_internal_state_and_bond
    in
    let static = set_modified_internal_state_and_bond
        store_modified_internal_state_and_bond static in
    (*------------------------------------------------------------*)
    (*question marks on the right hand side*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let error, store_question_marks_rhs =
      Site_accross_bonds_domain_static.collect_question_marks_rhs
        parameter error rule_id kappa_handler rule
        store_modified_map
        store_question_marks_rhs
    in
    let static = set_question_marks_rhs store_question_marks_rhs static in
    (*------------------------------------------------------------*)
    (*implicit static information*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_implicit_static =
      Site_accross_bonds_domain_static.collect_implicit_static
        parameter error store_tuple_pair store_question_marks_rhs
    in
    let static = set_implicit_rule store_implicit_static static in
    (*------------------------------------------------------------*)
    (*explicit static information*)
    let store_modified_internal_state_and_bond =
      get_modified_internal_state_and_bond static in
    let store_created_bond = get_created_bond static in
    let store_explicit_static = get_explicit_rule static in
    let error, store_explicit_static =
      Site_accross_bonds_domain_static.collect_explicit_static
        parameter error store_created_bond
        store_modified_internal_state_and_bond store_explicit_static
    in
    let static = set_explicit_rule store_explicit_static static in
    error, static

  (****************************************************************)

  let scan_rules static dynamic error =
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
        dumy = ();
       store_value =
         Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.empty;
         
      }
    in
    let init_global_dynamic_information =
      {
        global = dynamic;
        local = init_local_dynamic_information;
      }
    in
    let error, static, dynamic =
      scan_rules
        init_global_static_information
        init_global_dynamic_information
        error
    in
    error, static, dynamic

  (*------------------------------------------------------------*)
  (* take into account bounds that may occur in initial states *)

  let add_initial_state static dynamic error species =
    let parameter = get_parameter static in
    let store_result = get_pair_tuple_init dynamic in
    let error, store_tuple_pair_init =
      Site_accross_bonds_domain_dynamic.collect_pair_tuple_init
        parameter error species store_result
    in
    let dynamic = set_pair_tuple_init store_tuple_pair_init dynamic in
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* check for each bond that occur in the lhs, whether
     the constraints in the lhs are consistent *)
  let is_enabled _static dynamic error (_rule_id:Ckappa_sig.c_rule_id) precondition =
    error, dynamic, Some precondition

  (****************************************************************)
  (* to do : add information of init ?*)

  let apply_rule static dynamic error rule_id precondition =
    let parameter  = get_parameter static in
    let event_list = [] in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export _static dynamic error kasa_state =
    error, dynamic, kasa_state

  (****************************************************************)
(* to do *)

  let print static dynamic error _loggers =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let log = Remanent_parameters.get_logger parameter in
    (*--------------------------------------------------------*)
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_result parameter
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
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, ()

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
