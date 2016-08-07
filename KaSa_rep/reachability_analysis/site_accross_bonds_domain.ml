(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 07 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

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
      store_value :
        Ckappa_sig.Views_bdu.mvbdu
          Site_accross_bonds_domain_type.PairAgentSites_map_and_set.Map.t;
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

  let get_rule parameter error static r_id =
    let compil = get_compil static in
    let error, rule  =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
        parameter
        error
        r_id
        compil.Cckappa_sig.rules
    in
    error, rule

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

  let get_bonds_lhs static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_bonds_lhs

  let set_bonds_lhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_bonds_lhs = r
      } static

  let get_bonds_rhs_set static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_bonds_rhs_set

  let set_bonds_rhs_set r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_bonds_rhs_set = r
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

  let get_potential_tuple_pair static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_potential_tuple_pair

  let set_potential_tuple_pair r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_potential_tuple_pair = r
      } static

  let get_potential_tuple_pair_set static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_potential_tuple_pair_set

  let set_potential_tuple_pair_set r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_potential_tuple_pair_set = r
      } static

  let get_created_bonds static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_created_bonds

  let set_created_bonds r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bonds = r
      } static

  let get_potential_tuple_pair_created_bonds static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds

  let set_potential_tuple_pair_created_bonds r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds = r
      } static

  let get_potential_tuple_pair_bonds_rhs static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_potential_tuple_pair_bonds_rhs

  let set_potential_tuple_pair_bonds_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_potential_tuple_pair_bonds_rhs = r
      } static

(*projection*)

  (*let get_potential_tuple_pair_created_bonds_proj1 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds_proj1

let set_potential_tuple_pair_created_bonds_proj1 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds_proj1 = r
    } static

let get_potential_tuple_pair_created_bonds_proj2 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds_proj2

let set_potential_tuple_pair_created_bonds_proj2 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_potential_tuple_pair_created_bonds_proj2 = r
    } static*)

(*projection map*)

let get_proj_map1 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_proj_map1

let set_proj_map1 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_proj_map1 = r
    } static

let get_proj_reverse_map1 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_proj_reverse_map1

let set_proj_reverse_map1 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_proj_reverse_map1 = r
    } static

let get_proj_map2 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_proj_map2

let set_proj_map2 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_proj_map2 = r
    } static

let get_proj_reverse_map2 static =
  (get_basic_static_information
     static).Site_accross_bonds_domain_static.store_proj_reverse_map2

let set_proj_reverse_map2 r static =
  set_basic_static_information
    {
      (get_basic_static_information static) with
      Site_accross_bonds_domain_static.store_proj_reverse_map2 = r
    } static

  (*rule that can created a bond *)
  (*
  let get_created_bond_with_potential_pair static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_created_bond_with_potential_pair

  let set_created_bond_with_potential_pair r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bond_with_potential_pair = r
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
      } static*)

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
    (*bonds on the left hand side*)
    let store_bonds_lhs = get_bonds_lhs static in
    let error, store_bonds_lhs =
      Site_accross_bonds_domain_static.collect_bonds_lhs
        parameter error rule_id rule store_bonds_lhs
    in
    let static = set_bonds_lhs store_bonds_lhs static in
    (*------------------------------------------------------------*)
    (*bonds set without rule_id*)
    let store_bonds_rhs = get_bonds_rhs static in
    (*let store_bonds_rhs_set = get_bonds_rhs_set static in*)
    let error, store_bonds_rhs_set =
      Site_accross_bonds_domain_static.collect_bonds_rhs_set
        parameter error
        rule_id
        store_bonds_rhs
    in
    let static = set_bonds_rhs_set store_bonds_rhs_set static in
    (*------------------------------------------------------------*)
    (*tuple pair*)
    let store_views_rhs = get_views_rhs static in
    let error, store_test =
      Site_accross_bonds_domain_static.collect_pair_sites_aux
        parameter error rule_id store_views_rhs
    in
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let error, store_potential_tuple_pair =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair
        parameter error
        kappa_handler
        rule_id
        store_test
        store_potential_tuple_pair
    in
    let static = set_potential_tuple_pair store_potential_tuple_pair static in
    (*------------------------------------------------------------*)
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let error, store_potential_tuple_pair_set =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair_set
        parameter error
        rule_id
        store_potential_tuple_pair
    in
    let static =
      set_potential_tuple_pair_set store_potential_tuple_pair_set static
    in
    (*------------------------------------------------------------*)
    (*created a bond*)
    let store_created_bonds = get_created_bonds static in
    let error, store_created_bonds =
      Site_accross_bonds_domain_static.collect_created_bonds
        parameter
        error
        rule
        rule_id
        store_created_bonds
    in
    let static = set_created_bonds store_created_bonds static in
    (*------------------------------------------------------------*)
    (*potential tuple pair and created bonds*)
    let store_created_bonds = get_created_bonds static in
    let store_potential_tuple_pair_created_bonds =
      get_potential_tuple_pair_created_bonds static
    in
    let error, store_potential_tuple_pair_created_bonds =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair_created_bonds
        parameter
        error
        rule_id
        store_created_bonds
        store_potential_tuple_pair
        store_potential_tuple_pair_created_bonds
    in
    let static =
      set_potential_tuple_pair_created_bonds store_potential_tuple_pair_created_bonds
        static
    in
    (*------------------------------------------------------------*)
    (*1.b bonds on the rhs*)
    let store_bonds_rhs = get_bonds_rhs static in
    let store_potential_tuple_pair_bonds_rhs =
      get_potential_tuple_pair_bonds_rhs static
    in
    let error, store_potential_tuple_pair_bonds_rhs =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair_bonds_rhs
        parameter error
        rule_id
        store_bonds_rhs
        store_potential_tuple_pair
        store_potential_tuple_pair_bonds_rhs
    in
    let static =
      set_potential_tuple_pair_bonds_rhs
        store_potential_tuple_pair_bonds_rhs
        static
    in
    (*------------------------------------------------------------*)
    (*projection set*)
    (*let store_potential_tuple_pair_created_bonds =
      get_potential_tuple_pair_created_bonds static
    in
    let error, store_potential_tuple_pair_created_bonds_proj1 =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair_created_bonds_proj1
        parameter error
        rule_id
        store_potential_tuple_pair_created_bonds
    in
    let static =
      set_potential_tuple_pair_created_bonds_proj1
        store_potential_tuple_pair_created_bonds_proj1 static
    in
    (*------------------------------------------------------------*)
    (*projection*)
    let error, store_potential_tuple_pair_created_bonds_proj2 =
      Site_accross_bonds_domain_static.collect_potential_tuple_pair_created_bonds_proj2
        parameter error
        kappa_handler
        rule_id
        store_potential_tuple_pair_created_bonds
    in
    let static =
      set_potential_tuple_pair_created_bonds_proj2
        store_potential_tuple_pair_created_bonds_proj2
        static
    in*)
    (*------------------------------------------------------------*)
    (*modification*)
    let store_modified_map = get_modified_map static in
    let error, store_modified_map =
      Site_accross_bonds_domain_static.collect_site_modified
        parameter error
        rule_id
        rule
        store_modified_map
    in
    let static = set_modified_map store_modified_map static in
    (*------------------------------------------------------------*)
    (*the first site of potential pair can be a created bond*)
    (*let store_tuple_pair = get_tuple_pair static in
    let store_created_bond_with_potential_pair =
      get_created_bond_with_potential_pair static
    in
    let error, store_created_bond_with_potential_pair =
      Site_accross_bonds_domain_static.collect_created_bond_with_potential_pair
        parameter error
        rule_id
        rule
        store_tuple_pair
        store_created_bond_with_potential_pair
    in
    let static =
      set_created_bond_with_potential_pair
        store_created_bond_with_potential_pair static
    in
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
        parameter error
        kappa_handler
        rule_id
        rule
        store_modified_map
        store_question_marks_rhs
    in
    let static = set_question_marks_rhs store_question_marks_rhs static in*)
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
    (*------------------------------------------------------------*)
    (*project map*)
    let store_potential_tuple_pair_set = get_potential_tuple_pair_set static in
    let error, store_proj_map1 =
      Site_accross_bonds_domain_static.collect_proj_map1
        parameter error
        store_potential_tuple_pair_set
    in
    let static = set_proj_map1 store_proj_map1 static in
    (*------------------------------------------------------------*)
    let error, store_proj_reverse_map1 =
      Site_accross_bonds_domain_static.collect_proj_reverse_map1
        parameter error
        store_potential_tuple_pair_set
    in
    let static = set_proj_reverse_map1 store_proj_reverse_map1 static in
    (*------------------------------------------------------------*)
    let error, store_proj_map2 =
      Site_accross_bonds_domain_static.collect_proj_map2
        parameter error
        store_potential_tuple_pair_set
    in
    let static = set_proj_map2 store_proj_map2 static in
    (*------------------------------------------------------------*)
    let error, store_proj_reverse_map2 =
      Site_accross_bonds_domain_static.collect_proj_reverse_map2
        parameter error
        store_potential_tuple_pair_set
    in
    let static = set_proj_map2 store_proj_reverse_map2 static in
    (*------------------------------------------------------------*)
    (*implicit static information*)
    (*  let kappa_handler = get_kappa_handler static in*)
    (*let store_tuple_pair = get_tuple_pair static in
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_implicit_static =
      Site_accross_bonds_domain_static.collect_implicit_static
        parameter error
        store_tuple_pair
        store_question_marks_rhs
    in
    let static = set_implicit_rule store_implicit_static static in*)
    (*------------------------------------------------------------*)
    (*explicit static information*)
    (*let store_modified_internal_state_and_bond =
      get_modified_internal_state_and_bond static in
      let store_created_bond = get_created_bond static in
      let store_explicit_static = get_explicit_rule static in*)
    (*let error, store_explicit_static =
      Site_accross_bonds_domain_static.collect_explicit_static
        parameter error store_created_bond
        store_modified_internal_state_and_bond store_explicit_static
      in
      let static = set_explicit_rule store_explicit_static static in*)
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
          Site_accross_bonds_domain_type.PairAgentSites_map_and_set.Map.empty;

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

  let get_mvbdu_false global_static dynamic error =
    let parameter = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_false =
      Ckappa_sig.Views_bdu.mvbdu_false
        parameter handler_bdu error
    in
    error, set_mvbdu_handler handler_bdu dynamic, bdu_false

  let add_initial_state static dynamic error species =
    let parameter = get_parameter static in
    (*views in the initial state that has two agents and their sites are different*)
    let error, store_views_init =
      Site_accross_bonds_domain_static.collect_views_init
        parameter error species in
    let error, store_sites_init =
      Site_accross_bonds_domain_static.collect_sites_init
        parameter error store_views_init in
    let error, store_pair_sites_init =
      Site_accross_bonds_domain_static.collect_pair_sites_init
        parameter error store_sites_init in
    (*a set of site that can be bounds*)
    let error, store_bonds_init =
      Site_accross_bonds_domain_static.collect_bonds_init
        parameter error species
    in
    (*collect the first site bound, and the second site different than the
      first site, return the information of its state, result*)
    let store_result = get_value dynamic in
    let handler = get_mvbdu_handler dynamic in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, handler, store_result =
      Site_accross_bonds_domain_static.collect_pair_tuple_init
        parameter
        error
        bdu_false
        handler
        kappa_handler
        species
        store_bonds_init
        store_pair_sites_init
        store_result
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_value store_result dynamic in
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* check for each bond that occur in the lhs, whether
     the constraints in the lhs are consistent *)
  let is_enabled _static dynamic error (_rule_id:Ckappa_sig.c_rule_id) precondition =
    (*let parameter = get_parameter static in
    let store_bonds_lhs = get_bonds_lhs static in
    let error, bonds_lhs_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_bonds_lhs
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let store_value = get_value dynamic in
    let error, bool =
      if
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.elements
        bonds_lhs_set

    in
    *)
    error, dynamic, Some precondition

  (****************************************************************)

  let get_state_of_site_in_precondition parameter error dynamic rule agent_id
      site_type precondition =
    let path =
      {
        Communication.defined_in = Communication.LHS rule ;
        Communication.agent_id = agent_id;
        Communication.relative_address = [];
        Communication.site = site_type;
      }
    in
    (*get a list of site_type2 state in the precondition*)
    let error, global_dynamic, precondition, state_list_lattice =
      Communication.get_state_of_site
        error
        (get_global_dynamic_information dynamic)
        precondition
        path
    in
    let error, state_list =
      match state_list_lattice with
      | Usual_domains.Val l -> error, l
      | Usual_domains.Any | Usual_domains.Undefined ->
        Exception.warn parameter error __POS__ Exit []
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

  let context rule_id agent_id site_type =
    " rule "^(Ckappa_sig.string_of_rule_id rule_id)^
    " agent_id "^(Ckappa_sig.string_of_agent_id agent_id)^
    " site_type "^(Ckappa_sig.string_of_site_name site_type)

  let apply_rule static dynamic error rule_id precondition =
    let parameter  = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    (*-----------------------------------------------------------*)
    let error, rule = get_rule parameter error static rule_id in
    match rule with
    | None ->
      let error, () =
        Exception.warn  parameter error __POS__ Exit ()
      in
      error, dynamic, (precondition, [])
    | Some rule ->
    let parameter =
      Remanent_parameters.update_prefix parameter "                " in
    (*------------------------------------------------------*)
    (*get the project of the agent_id and second site in
      potential tuple pair - created bonds *)
    let store_potential_tuple_pair_created_bonds =
      get_potential_tuple_pair_created_bonds static in
    let error, potential_tuple_pair_created_bonds_set =
      Site_accross_bonds_domain_static.get_set
        parameter
        error
        rule_id
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
        store_potential_tuple_pair_created_bonds
    in
    (*------------------------------------------------------*)
    (*1. created bonds*)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun (x, y) (error, dynamic, precondition) ->
           let proj (a, _, _, d, _, _) = (a, d) in
           (*get a list of state in the second site*)
           let ((agent_id, site_type'),
                (agent_id1, site_type1')) = proj x, proj y
           in
           let error', dynamic, precondition, state_list =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               rule
               agent_id
               site_type'
               precondition
           in
           let error =
           Exception.check_point
             Exception.warn parameter error error'
            __POS__ Exit
           in
           let error', dynamic, precondition, state_list' =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               rule
               agent_id1
               site_type1'
               precondition
           in
           let error =
             Exception.check_point
               Exception.warn parameter error error'
               __POS__ Exit
           in
           (*------------------------------------------------------*)
           let error, potential_list =
             List.fold_left (fun (error, current_list) pre_state' ->
                 List.fold_left (fun (error, current_list) pre_state1' ->
                     let potential_list =
                       ((site_type', pre_state'),
                        (site_type1', pre_state1')) :: current_list
                     in
                     error, potential_list
                   ) (error, current_list) state_list'
               ) (error, []) state_list
           in
           (*------------------------------------------------------*)
           let error, dynamic, precondition =
             List.fold_left
               (fun (error, dynamic, precondition) (t, u) ->
                  let handler = get_mvbdu_handler dynamic in
                  let store_result = get_value dynamic in
                  let (site_type', pre_state') = t in
                  let (site_type1', pre_state1') = u in
                  let pair_list =
                    [Ckappa_sig.fst_site, pre_state';
                     Ckappa_sig.snd_site, pre_state1']
                  in
                  let proj1 (_, a, b, _, _, _) = (a, b) in
                  let ((agent_type, site_type),
                       (agent_type1, site_type1)) = proj1 x, proj1 y
                  in
                  let pair =
                    (agent_type, site_type, site_type'),
                    (agent_type1, site_type1, site_type1')
                  in
                  let error, handler, mvbdu =
                    Ckappa_sig.Views_bdu.mvbdu_of_association_list
                      parameter handler error pair_list
                  in
                  (*----------------------------------------------------*)
                  let error, handler, store_result =
                    Site_accross_bonds_domain_type.add_link
                      parameter error bdu_false handler
                      kappa_handler
                      pair
                      mvbdu
                      store_result
                  in
                  let dynamic = set_value store_result dynamic in
                  let dynamic = set_mvbdu_handler handler dynamic in
                  error, dynamic, precondition
               ) (error, dynamic, precondition) potential_list
           in
           error, dynamic, precondition
        ) potential_tuple_pair_created_bonds_set (error, dynamic, precondition)
    in

    (*------------------------------------------------------*)
    let event_list = [] in
    error, dynamic, (precondition, event_list)

(* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in

    error, dynamic, event_list

  let stabilize _static dynamic error =
    error, dynamic, ()

  let export _static dynamic error kasa_state =
    error, dynamic, kasa_state

  (****************************************************************)
  (* to do *)

  let print static dynamic (error:Exception.method_handler) loggers =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let handler = get_mvbdu_handler dynamic in
    let log = loggers in
    (*--------------------------------------------------------*)
    let error, handler =
      if Remanent_parameters.get_dump_reachability_analysis_result parameter
      then
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Site accross bonds domain\n";
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Static information\n";
        in
        (*--------------------------------------------------------*)
        (*print created bonds*)
        (*let store_created_bonds = get_created_bonds static in
        let proj (_, b, c, e) = (b, c, e) in
        let error =
          Ckappa_sig.Rule_map_and_set.Map.fold
            (fun rule_id set error ->
               Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
                 (fun (x, y) error ->
                    let (x', y') = proj x, proj y in
                    let error, (agent, site, state) =
                      Site_accross_bonds_domain_type.convert_single
                        parameter
                        error
                        kappa_handler
                        x'
                    in
                    let error, (agent1, site1, state1) =
                      Site_accross_bonds_domain_type.convert_single
                        parameter
                        error
                        kappa_handler
                        y'
                    in
                    let _ =
                      Loggers.fprintf log
                        "At rule_id:%i there is an action binding between the site %s of %s and the site %s of %s. %s:(%s:%s); %s:(%s:%s)\n"
                        (Ckappa_sig.int_of_rule_id rule_id)
                        site agent
                        site1 agent1
                        (**)
                        agent site state
                        agent1 site1 state1
                    in
                    error
                 ) set error
            ) store_created_bonds error
        in
        let () = Loggers.print_newline log in*)
        (*--------------------------------------------------------*)
        (*tuple pair where the first site belongs to the created bond*)
        let store_potential_tuple_pair_created_bonds =
          get_potential_tuple_pair_created_bonds static
        in
        let proj (_, b, c, d, e, f) = (b,c, d, e, f) in
        let error =
          Ckappa_sig.Rule_map_and_set.Map.fold
            (fun rule_id set error ->
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
                 (fun (x, y) error ->
                    let error,
                        (agent, site, site', state, state',
                         agent1, site1, site1', state1, state1') =
                      Site_accross_bonds_domain_type.convert_tuple_full
                        parameter error kappa_handler (proj x, proj y)
                    in
                    let () =
                      Loggers.fprintf log
                        "At rule_id:%i, the potential tuple pair when there is an action binding between the site %s of %s and the site %s of %s is: %s(%s:%s, %s:%s); %s(%s:%s, %s:%s)\n"
                        (Ckappa_sig.int_of_rule_id rule_id)
                        site agent site1 agent1
                        agent site state site' state'
                        agent1 site1 state1 site1' state1'
                    in
                    error
                 ) set error
            ) store_potential_tuple_pair_created_bonds error
        in
        let () = Loggers.print_newline log in
        (*--------------------------------------------------------*)
        (*t1.b uple pair where the first site belongs to a bond on rhs§*)
        let store_potential_tuple_pair_bonds_rhs =
          get_potential_tuple_pair_bonds_rhs static
        in
        let proj (_, b, c, d, e, f) = (b,c, d, e, f) in
        let error =
          Ckappa_sig.Rule_map_and_set.Map.fold
            (fun rule_id set error ->
               Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
                 (fun (x, y) error ->
                    let error,
                        (agent, site, site', state, state',
                         agent1, site1, site1', state1, state1') =
                      Site_accross_bonds_domain_type.convert_tuple_full
                        parameter error kappa_handler (proj x, proj y)
                    in
                    let () =
                      Loggers.fprintf log
                        "At rule_id:%i: the potential tuple pair when there is a bond on the rhs between the site %s of %s and the site %s of %s is: %s(%s:%s, %s:%s); %s(%s:%s, %s:%s)\n"
                        (Ckappa_sig.int_of_rule_id rule_id)
                        site agent site1 agent1
                        agent site state site' state'
                        agent1 site1 state1 site1' state1'
                    in
                    error
                 ) set error
            ) store_potential_tuple_pair_bonds_rhs error
        in
        let () = Loggers.print_newline log in
        (*--------------------------------------------------------*)
        (*print result*)
        let () =
          Loggers.fprintf log "* Dynamic information\n"
        in
        let store_value = get_value dynamic in
        let error, handler =
          Site_accross_bonds_domain_type.PairAgentSites_map_and_set.Map.fold
            (fun (x, y) mvbdu (error, handler) ->
               Site_accross_bonds_domain_type.print_site_accross_domain
                 ~sparse:true
                 ~final_resul:true
                 ~dump_any:true parameter error kappa_handler handler (x, y) mvbdu
            ) store_value (error, handler)
        in
        error, handler
      else error, handler
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, ()

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
