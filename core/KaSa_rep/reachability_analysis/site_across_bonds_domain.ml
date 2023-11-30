(**
   * site_across_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Dec 22 2018>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

module Domain = struct
  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  (*************************************************************************)
  (* Domain specific info:
     Collect the set of tuples (A,x,y,B,z,t) such that there exists a rule
     with a bond connecting the site A.x and B.z and that the agent of type A
     document a site y <> x, and the agent of type B document a site t <> z

     - For each tuple, collect three maps -> (A,x,y,B,z,t) -> Ag_id list
     RuleIdMap to explain in which rule and which agent_id the site y can be modified.

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
        which agent_id the site t can be modified

     -> (A,x,y,B,z,t) -> Ag_id list RuleIdMap to explain in which rule and
        which agent_id (A) a site x of A may become bound to a site z of
        B *)
  (*************************************************************************)

  type local_static_information = {
    store_basic_static_information:
      Site_across_bonds_domain_static.basic_static_information;
    dummy: unit;
  }

  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
    local_static_information: local_static_information;
  }

  (*--------------------------------------------------------------*)
  (* A triple of maps : mvbdu_of_association_list
     - One maps each such tuples (A,x,y,B,z,t) to a mvbdu with two variables
      that describes the relation between the state of y and the state of t,
      when both agents are connected via x and z.
     - One maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
      that decribes the range of y when both agents are connected via x and
      z.
     - One maps each such tuples (A,x,y,B,z,t) to a mvbdu with one variables
      that decribes the range of t when both agents are connected via x and
      z. *)
  (*--------------------------------------------------------------*)

  type local_dynamic_information = {
    dummy: unit;
    store_value:
      Ckappa_sig.Views_bdu.mvbdu
      Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Map.t;
  }

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  let domain_name = "Site accross bonds domain"

  (*------------------------------------------------------------*)
  (** global static information.  Explain how to extract the handler for
      kappa expressions from a value of type static_information. Kappa
      handler is static and thus it should never updated. *)

  let get_global_static_information static = static.global_static_information
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static
  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_potential_side_effects static =
    lift Analyzer_headers.get_potential_side_effects_per_rule static

  let get_compil static = lift Analyzer_headers.get_cc_code static
  let get_views_rhs static = lift Analyzer_headers.get_views_rhs static
  let get_views_lhs static = lift Analyzer_headers.get_views_lhs static

  let get_action_binding static =
    lift Analyzer_headers.get_action_binding static

  let get_modified_map static = lift Analyzer_headers.get_modified_map static
  let get_bonds_rhs static = lift Analyzer_headers.get_bonds_rhs static
  let get_bonds_lhs static = lift Analyzer_headers.get_bonds_lhs static

  let get_rule parameters error static r_id =
    let compil = get_compil static in
    let error, rule =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error r_id
        compil.Cckappa_sig.rules
    in
    error, rule

  (*static information*)

  let get_local_static_information static = static.local_static_information

  let set_local_static_information local static =
    { static with local_static_information = local }

  (***************************************************************************)
  (*STATIC INFORMATION*)
  (***************************************************************************)

  let get_basic_static_information static =
    (get_local_static_information static).store_basic_static_information

  let set_basic_static_information domain static =
    set_local_static_information
      {
        (get_local_static_information static) with
        store_basic_static_information = domain;
      }
      static

  (***************************************************************************)
  (*POTENTIAL TUPLE PAIR ON VIEWS*)
  (***************************************************************************)

  let get_potential_tuple_pair_views static =
    (get_basic_static_information static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair_views

  let set_potential_tuple_pair_views domain static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_across_bonds_domain_static.store_potential_tuple_pair_views =
          domain;
      }
      static

  let get_potential_tuple_pair static =
    (get_potential_tuple_pair_views static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair

  let set_potential_tuple_pair r static =
    set_potential_tuple_pair_views
      {
        (get_potential_tuple_pair_views static) with
        Site_across_bonds_domain_static.store_potential_tuple_pair = r;
      }
      static

  let get_potential_tuple_pair_lhs static =
    (get_potential_tuple_pair_views static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair_lhs

  let set_potential_tuple_pair_lhs l static =
    set_potential_tuple_pair_views
      {
        (get_potential_tuple_pair_views static) with
        Site_across_bonds_domain_static.store_potential_tuple_pair_lhs = l;
      }
      static

  let get_potential_tuple_pair_rule_rhs static =
    (get_potential_tuple_pair_views static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair_rule_rhs

  let set_potential_tuple_pair_rule_rhs r static =
    set_potential_tuple_pair_views
      {
        (get_potential_tuple_pair_views static) with
        Site_across_bonds_domain_static.store_potential_tuple_pair_rule_rhs = r;
      }
      static

  (***************************************************************************)
  (*CREATED A NEW BINDING*)
  (***************************************************************************)

  let get_potential_tuple_pair_creation_bonds static =
    (get_basic_static_information static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair_creation_bonds

  let set_potential_tuple_pair_creation_bonds domain static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_across_bonds_domain_static
        .store_potential_tuple_pair_creation_bonds = domain;
      }
      static

  let get_partition_created_bonds_map static =
    (get_potential_tuple_pair_creation_bonds static)
      .Site_across_bonds_domain_static.store_partition_created_bonds_map

  let set_partition_created_bonds_map r static =
    set_potential_tuple_pair_creation_bonds
      {
        (get_potential_tuple_pair_creation_bonds static) with
        Site_across_bonds_domain_static.store_partition_created_bonds_map = r;
      }
      static

  let get_partition_created_bonds_map_1 static =
    (get_potential_tuple_pair_creation_bonds static)
      .Site_across_bonds_domain_static.store_partition_created_bonds_map_1

  let set_partition_created_bonds_map_1 r static =
    set_potential_tuple_pair_creation_bonds
      {
        (get_potential_tuple_pair_creation_bonds static) with
        Site_across_bonds_domain_static.store_partition_created_bonds_map_1 = r;
      }
      static

  let get_partition_created_bonds_map_2 static =
    (get_potential_tuple_pair_creation_bonds static)
      .Site_across_bonds_domain_static.store_partition_created_bonds_map_2

  let set_partition_created_bonds_map_2 r static =
    set_potential_tuple_pair_creation_bonds
      {
        (get_potential_tuple_pair_creation_bonds static) with
        Site_across_bonds_domain_static.store_partition_created_bonds_map_2 = r;
      }
      static

  let get_rule_partition_created_bonds_map_1 static =
    (get_potential_tuple_pair_creation_bonds static)
      .Site_across_bonds_domain_static.store_rule_partition_created_bonds_map_1

  let set_rule_partition_created_bonds_map_1 r static =
    set_potential_tuple_pair_creation_bonds
      {
        (get_potential_tuple_pair_creation_bonds static) with
        Site_across_bonds_domain_static.store_rule_partition_created_bonds_map_1 =
          r;
      }
      static

  let get_rule_partition_created_bonds_map_2 static =
    (get_potential_tuple_pair_creation_bonds static)
      .Site_across_bonds_domain_static.store_rule_partition_created_bonds_map_2

  let set_rule_partition_created_bonds_map_2 r static =
    set_potential_tuple_pair_creation_bonds
      {
        (get_potential_tuple_pair_creation_bonds static) with
        Site_across_bonds_domain_static.store_rule_partition_created_bonds_map_2 =
          r;
      }
      static

  (***************************************************************************)
  (*MODIFICATION*)
  (***************************************************************************)

  let get_potential_tuple_pair_modification static =
    (get_basic_static_information static)
      .Site_across_bonds_domain_static.store_potential_tuple_pair_modification

  let set_potential_tuple_pair_modification domain static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_across_bonds_domain_static.store_potential_tuple_pair_modification =
          domain;
      }
      static

  let get_partition_modified_map_1 static =
    (get_potential_tuple_pair_modification static)
      .Site_across_bonds_domain_static.store_partition_modified_map_1

  let set_partition_modified_map_1 r static =
    set_potential_tuple_pair_modification
      {
        (get_potential_tuple_pair_modification static) with
        Site_across_bonds_domain_static.store_partition_modified_map_1 = r;
      }
      static

  let get_partition_modified_map_2 static =
    (get_potential_tuple_pair_modification static)
      .Site_across_bonds_domain_static.store_partition_modified_map_2

  let set_partition_modified_map_2 r static =
    set_potential_tuple_pair_modification
      {
        (get_potential_tuple_pair_modification static) with
        Site_across_bonds_domain_static.store_partition_modified_map_2 = r;
      }
      static

  let get_rule_partition_modified_map_1 static =
    (get_potential_tuple_pair_modification static)
      .Site_across_bonds_domain_static.store_rule_partition_modified_map_1

  let set_rule_partition_modified_map_1 r static =
    set_potential_tuple_pair_modification
      {
        (get_potential_tuple_pair_modification static) with
        Site_across_bonds_domain_static.store_rule_partition_modified_map_1 = r;
      }
      static

  let get_rule_partition_modified_map_2 static =
    (get_potential_tuple_pair_modification static)
      .Site_across_bonds_domain_static.store_rule_partition_modified_map_2

  let set_rule_partition_modified_map_2 r static =
    set_potential_tuple_pair_modification
      {
        (get_potential_tuple_pair_modification static) with
        Site_across_bonds_domain_static.store_rule_partition_modified_map_2 = r;
      }
      static

  (***************************************************************************)
  (** DYNAMIC INFORMATION*)
  (***************************************************************************)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global =
        Analyzer_headers.set_mvbdu_handler handler
          (get_global_dynamic_information dynamic);
    }

  let get_local_dynamic_information dynamic = dynamic.local
  let set_local_dynamic_information local dynamic = { dynamic with local }
  let get_value dynamic = (get_local_dynamic_information dynamic).store_value

  let set_value value dynamic =
    set_local_dynamic_information
      { (get_local_dynamic_information dynamic) with store_value = value }
      dynamic

  (** profiling *)
  let get_log_info dynamic =
    Analyzer_headers.get_log_info (get_global_dynamic_information dynamic)

  let set_log_info log_info dynamic =
    {
      dynamic with
      global =
        Analyzer_headers.set_log_info log_info
          (get_global_dynamic_information dynamic);
    }

  (***************************************************************************)
  (*TYPE*)
  (***************************************************************************)

  type 'a zeroary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * dynamic_information * 'd

  (****************************************************************)
  (*RULE*)
  (***************************************************************************)

  let scan_rule parameters error kappa_handler rule_id _rule static =
    (*------------------------------------------------------------*)
    (*collect potential tuple pair on the rhs views*)
    let store_views_rhs = get_views_rhs static in
    let store_bonds_rhs = get_bonds_rhs static in
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let error, store_potential_tuple_pair =
      Site_across_bonds_domain_static.collect_potential_tuple_pair parameters
        error kappa_handler rule_id store_bonds_rhs store_views_rhs
        store_potential_tuple_pair
    in
    let static = set_potential_tuple_pair store_potential_tuple_pair static in
    (*------------------------------------------------------------*)
    (*potential tuple pair on the rhs depend on rule_id*)
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let store_potential_tuple_pair_rule_rhs =
      get_potential_tuple_pair_rule_rhs static
    in
    let error, store_potential_tuple_pair_rule_rhs =
      Site_across_bonds_domain_static.collect_potential_tuple_pair_rule_rhs
        parameters error rule_id store_potential_tuple_pair
        store_potential_tuple_pair_rule_rhs
    in
    let static =
      set_potential_tuple_pair_rule_rhs store_potential_tuple_pair_rule_rhs
        static
    in
    (*------------------------------------------------------------*)
    (*potential tuple pair on lhs views *)
    let store_views_lhs = get_views_lhs static in
    let store_bonds_lhs = get_bonds_lhs static in
    let store_potential_tuple_pair_lhs = get_potential_tuple_pair_lhs static in
    let error, store_potential_tuple_pair_lhs =
      Site_across_bonds_domain_static.collect_potential_tuple_pair_lhs
        parameters error kappa_handler rule_id store_bonds_lhs store_views_lhs
        store_potential_tuple_pair_lhs
    in
    let static =
      set_potential_tuple_pair_lhs store_potential_tuple_pair_lhs static
    in
    error, static

  (****************************************************************)
  (*RULES*)
  (****************************************************************)

  let scan_rules static dynamic error =
    let parameters = get_parameter static in
    let compil = get_compil static in
    let kappa_handler = get_kappa_handler static in
    let error, static =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
        (fun parameters error rule_id rule static ->
          let error, static =
            scan_rule parameters error kappa_handler rule_id
              rule.Cckappa_sig.e_rule_c_rule static
          in
          error, static)
        compil.Cckappa_sig.rules static
    in
    (*------------------------------------------------------------*)
    (*partition map with key is the pair of the bonds in the rhs*)
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let error, store_partition_created_bonds_map =
      Site_across_bonds_domain_static.collect_partition_created_bonds_map
        parameters error store_potential_tuple_pair
    in
    let static =
      set_partition_created_bonds_map store_partition_created_bonds_map static
    in
    (*------------------------------------------------------------*)
    (*a site is modified explicitly*)
    let error, store_partition_modified_map_1 =
      Site_across_bonds_domain_static.collect_partition_modified_map_1
        parameters error store_potential_tuple_pair
    in
    let static =
      set_partition_modified_map_1 store_partition_modified_map_1 static
    in
    let error, store_partition_modified_map_2 =
      Site_across_bonds_domain_static.collect_partition_modified_map_2
        parameters error store_potential_tuple_pair
    in
    let static =
      set_partition_modified_map_2 store_partition_modified_map_2 static
    in
    (*------------------------------------------------------------*)
    (*potential tuple pair that are modified depend on rule_id*)
    let store_potential_tuple_pair_rule_rhs =
      get_potential_tuple_pair_rule_rhs static
    in
    let store_rule_partition_modified_map_1 =
      get_rule_partition_modified_map_1 static
    in
    let error, store_rule_partition_modified_map_1 =
      Site_across_bonds_domain_static.collect_rule_partition_modified_map_1
        parameters error store_potential_tuple_pair_rule_rhs
        store_rule_partition_modified_map_1
    in
    let static =
      set_rule_partition_modified_map_1 store_rule_partition_modified_map_1
        static
    in
    (*------------------------------------------------------------*)
    let store_rule_partition_modified_map_2 =
      get_rule_partition_modified_map_2 static
    in
    let error, store_rule_partition_modified_map_2 =
      Site_across_bonds_domain_static.collect_rule_partition_modified_map_2
        parameters error store_potential_tuple_pair_rule_rhs
        store_rule_partition_modified_map_2
    in
    let static =
      set_rule_partition_modified_map_2 store_rule_partition_modified_map_2
        static
    in
    (*------------------------------------------------------------*)
    let store_partition_created_bonds_map =
      get_partition_created_bonds_map static
    in
    let store_partition_created_bonds_map_1 =
      get_partition_created_bonds_map_1 static
    in
    let error, store_partition_created_bonds_map_1 =
      Site_across_bonds_domain_static.collect_partition_created_bonds_map_1
        parameters error store_partition_created_bonds_map
        store_partition_created_bonds_map_1
    in
    let static =
      set_partition_created_bonds_map_1 store_partition_created_bonds_map_1
        static
    in
    (* *)
    let store_partition_created_bonds_map_2 =
      get_partition_created_bonds_map_2 static
    in
    let error, store_partition_created_bonds_map_2 =
      Site_across_bonds_domain_static.collect_partition_created_bonds_map_2
        parameters error store_partition_created_bonds_map
        store_partition_created_bonds_map_2
    in
    let static =
      set_partition_created_bonds_map_2 store_partition_created_bonds_map_2
        static
    in
    (*------------------------------------------------------------*)
    let store_rule_partition_created_bonds_map_1 =
      get_rule_partition_created_bonds_map_1 static
    in
    let error, store_rule_partition_created_bonds_map_1 =
      Site_across_bonds_domain_static.collect_rule_partition_created_bonds_map_1
        parameters error store_potential_tuple_pair_rule_rhs
        store_rule_partition_created_bonds_map_1
    in
    let static =
      set_rule_partition_created_bonds_map_1
        store_rule_partition_created_bonds_map_1 static
    in
    let store_rule_partition_created_bonds_map_2 =
      get_rule_partition_created_bonds_map_2 static
    in
    let error, store_rule_partition_created_bonds_map_2 =
      Site_across_bonds_domain_static.collect_rule_partition_created_bonds_map_1
        parameters error store_potential_tuple_pair_rule_rhs
        store_rule_partition_created_bonds_map_2
    in
    let static =
      set_rule_partition_created_bonds_map_2
        store_rule_partition_created_bonds_map_2 static
    in
    (*------------------------------------------------------------*)
    (* Restrict tuples in lhs to the tuples of interest *)
    let store_potential_tuple_pair_lhs = get_potential_tuple_pair_lhs static in
    let store_potential_tuple_pair_lhs =
      Ckappa_sig.Rule_map_and_set.Map.map
        (Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set
         .filter (fun ((a, b, c, d, _), (a', b', c', d', _)) ->
             Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set
             .mem
               ((a, b, c, d), (a', b', c', d'))
               store_potential_tuple_pair))
        store_potential_tuple_pair_lhs
    in
    let static =
      set_potential_tuple_pair_lhs store_potential_tuple_pair_lhs static
    in
    error, static, dynamic

  (***************************************************************************)
  (*INITIAL STATES*)
  (***************************************************************************)

  let initialize static dynamic error =
    let parameters = Analyzer_headers.get_parameter static in
    let log_info = Analyzer_headers.get_log_info dynamic in
    let error, log_info =
      StoryProfiling.StoryStats.add_event parameters error
        (StoryProfiling.Domain_initialization domain_name) None log_info
    in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let init_local_static_information =
      {
        store_basic_static_information =
          Site_across_bonds_domain_static.init_basic_static_information;
        dummy = ();
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
        dummy = ();
        store_value =
          Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Map
          .empty;
      }
    in
    let init_global_dynamic_information =
      { global = dynamic; local = init_local_dynamic_information }
    in
    let error, static, dynamic =
      scan_rules init_global_static_information init_global_dynamic_information
        error
    in
    let log_info = get_log_info dynamic in
    let error, log_info =
      StoryProfiling.StoryStats.close_event parameters error
        (StoryProfiling.Domain_initialization domain_name) None log_info
    in
    let dynamic = set_log_info log_info dynamic in
    error, static, dynamic, []

  (***************************************************************************)
  (*IMPEMENTATION*)
  (***************************************************************************)

  let add_rules_tuples_into_wake_up_relation parameters error rule_tuples
      wake_up =
    Ckappa_sig.Rule_map_and_set.Map.fold
      (fun rule_id tuple_pairs (error, wake_up) ->
        Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.fold
          (fun (x, y) (error, wake_up) ->
            let agent_type, site_type1, site_type2, _ = x in
            let agent_type', site_type1', site_type2', _ = y in
            let error, wake_up =
              Common_static.add_dependency_site_rule parameters error agent_type
                site_type1 rule_id wake_up
            in
            let error, wake_up =
              Common_static.add_dependency_site_rule parameters error agent_type
                site_type2 rule_id wake_up
            in
            let error, wake_up =
              Common_static.add_dependency_site_rule parameters error
                agent_type' site_type1' rule_id wake_up
            in
            let error, wake_up =
              Common_static.add_dependency_site_rule parameters error
                agent_type' site_type2' rule_id wake_up
            in
            error, wake_up)
          tuple_pairs (error, wake_up))
      rule_tuples (error, wake_up)

  (* fold over all the rules, all the tuples of interest, all the sites in these
     tuples, and apply the function Common_static.add_dependency_site_rule to
       update the wake_up relation *)

  let complete_wake_up_relation static error wake_up =
    let parameters = get_parameter static in
    (*dealing with create a binding sites *)
    let store_rule_partition_created_bonds_map_1 =
      get_rule_partition_created_bonds_map_1 static
    in
    let store_rule_partition_created_bonds_map_2 =
      get_rule_partition_created_bonds_map_2 static
    in
    let store_rule_partition_modified_map_1 =
      get_rule_partition_modified_map_1 static
    in
    let store_rule_partition_modified_map_2 =
      get_rule_partition_modified_map_2 static
    in
    let store_potential_side_effects = get_potential_side_effects static in
    (*----------------------------------------------------*)
    let error, wake_up =
      add_rules_tuples_into_wake_up_relation parameters error
        store_rule_partition_created_bonds_map_1 wake_up
    in
    let error, wake_up =
      add_rules_tuples_into_wake_up_relation parameters error
        store_rule_partition_created_bonds_map_2 wake_up
    in
    (*----------------------------------------------------*)
    (*dealing with site that is modified*)
    let error, wake_up =
      add_rules_tuples_into_wake_up_relation parameters error
        store_rule_partition_modified_map_1 wake_up
    in
    let error, wake_up =
      add_rules_tuples_into_wake_up_relation parameters error
        store_rule_partition_modified_map_2 wake_up
    in
    (*----------------------------------------------------*)
    (*dealing with side effects*)
    let error, wake_up =
      Ckappa_sig.Rule_map_and_set.Map.fold
        (fun rule_id list (error, wake_up) ->
          List.fold_left
            (fun (error, wake_up) (_, (agent_type, site_type, _)) ->
              (* TO TO BETTER *)
              Common_static.add_dependency_site_rule parameters error agent_type
                site_type rule_id wake_up)
            (error, wake_up) list)
        store_potential_side_effects (error, wake_up)
    in
    error, wake_up

  let get_mvbdu_false global_static dynamic error =
    let parameters = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_false =
      Ckappa_sig.Views_bdu.mvbdu_false parameters handler_bdu error
    in
    error, set_mvbdu_handler handler_bdu dynamic, bdu_false

  (***************************************************************************)
  (*ADD INTITIAL STATE*)
  (***************************************************************************)

  let add_initial_state static dynamic error species =
    let parameters = get_parameter static in
    (*views in the initial state that has two agents and their sites are
      different*)
    let kappa_handler = get_kappa_handler static in
    let views = species.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views in
    let init = true in
    let error, store_views_init =
      Common_static.collect_views_pattern_aux ~init parameters kappa_handler
        error views Ckappa_sig.Agent_id_map_and_set.Map.empty
    in
    let error, store_bonds_init =
      Common_static.collect_bonds_pattern parameters error views
        species.Cckappa_sig.e_init_c_mixture.Cckappa_sig.bonds
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
    in
    (*collect the first site bound, and the second site different than the
      first site, return the information of its state, result*)
    let store_result = get_value dynamic in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let handler = get_mvbdu_handler dynamic in
    let error, tuple_init =
      Site_across_bonds_domain_static.build_potential_tuple_pair_set parameters
        error kappa_handler store_bonds_init store_views_init
    in
    let error, handler, store_result =
      Site_across_bonds_domain_static.collect_potential_tuple_pair_init
        parameters error bdu_false handler kappa_handler tuple_init store_result
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let dynamic = set_value store_result dynamic in
    let event_list = [] in
    error, dynamic, event_list

  (* check for each bond that occur in the lhs, whether
     the constraints in the lhs are consistent *)
  (* For each bond in the lhs of the rule rule_id *)
  (* For each tuple (x,y) of interest that gives information about this kind of
     bonds *)
  (* Fetch the state of the two other sites in the lhs and in the precondition
     if they are not available (take the meet)*)
  (* Check that there exists at least one such pair of state in the image of
     the pair (x,y) in dynamic *)

  (***************************************************************************)
  (*IS ENABLED *)
  (***************************************************************************)

  let build_mvbdu_range_list parameters error dynamic tuple mvbdu_value =
    let (_, _, _, _, pair_of_state2), (_, _, _, _, pair_of_state2') = tuple in
    let pair_list =
      [
        Ckappa_sig.fst_site, pair_of_state2;
        Ckappa_sig.snd_site, pair_of_state2';
      ]
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, mvbdu =
      Ckappa_sig.Views_bdu.mvbdu_of_range_list parameters handler error
        pair_list
    in
    (*intersection*)
    let error, handler, new_mvbdu =
      Ckappa_sig.Views_bdu.mvbdu_and parameters handler error mvbdu mvbdu_value
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, new_mvbdu

  let common_scan parameters error bdu_false dynamic store_value list =
    let rec scan list dynamic error =
      match list with
      | [] -> error, true, dynamic
      | tuple :: tail ->
        let proj (b, c, d, e, _) = b, c, d, e in
        let proj2 (x, y) = proj x, proj y in
        let tuple' = proj2 tuple in
        let error, mvbdu_value =
          Site_across_bonds_domain_type.get_mvbdu_from_tuple_pair parameters
            error tuple' bdu_false store_value
        in
        let error, dynamic, new_mvbdu =
          build_mvbdu_range_list parameters error dynamic tuple mvbdu_value
        in
        if Ckappa_sig.Views_bdu.equal new_mvbdu bdu_false then
          error, false, dynamic
        else
          scan tail dynamic error
    in
    scan list dynamic error

  let whether_or_not_it_has_precondition parameters error bdu_false tuple_set
      dynamic precondition =
    let list =
      Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set
      .elements tuple_set
    in
    let store_value = get_value dynamic in
    (*check if this pattern belong to the set of the patterns in the result*)
    let error, bool, dynamic =
      common_scan parameters error bdu_false dynamic store_value list
    in
    if bool then
      error, dynamic, Some precondition
    else
      error, dynamic, None

  let is_enabled static dynamic error (rule_id : Ckappa_sig.c_rule_id)
      precondition =
    let parameters = get_parameter static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    (*look into the lhs whether or not there exists a site across pattern or
      not *)
    let store_potential_tuple_pair_lhs = get_potential_tuple_pair_lhs static in
    let error, tuple_set =
      Common_map.get_rule_id_map_and_set parameters error rule_id
        Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set
        .empty store_potential_tuple_pair_lhs
    in
    whether_or_not_it_has_precondition parameters error bdu_false tuple_set
      dynamic precondition

  (***************************************************************************)
  (*MAY BE REACHABLE*)
  (***************************************************************************)

  (* the flag can be safely ignored for this abstract domain *)

  let maybe_reachable static dynamic error _flag (pattern : Cckappa_sig.mixture)
      precondition =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, bonds_lhs =
      Common_static.collect_bonds_pattern parameters error
        pattern.Cckappa_sig.views pattern.Cckappa_sig.bonds
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty
    in
    let error, views_lhs =
      Common_static.collect_views_pattern_aux parameters kappa_handler error
        pattern.Cckappa_sig.views Ckappa_sig.Agent_id_map_and_set.Map.empty
    in
    let error, tuple_set =
      Site_across_bonds_domain_static.build_potential_tuple_pair_set parameters
        error kappa_handler bonds_lhs views_lhs
    in
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let tuple_set =
      Site_across_bonds_domain_type.PairAgentSitesPStates_map_and_set.Set.filter
        (fun ((a, b, c, d, _), (a', b', c', d', _)) ->
          Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.mem
            ((a, b, c, d), (a', b', c', d'))
            store_potential_tuple_pair)
        tuple_set
    in
    whether_or_not_it_has_precondition parameters error bdu_false tuple_set
      dynamic precondition

  (****************************************************************)

  let context rule_id agent_id site_type =
    " rule "
    ^ Ckappa_sig.string_of_rule_id rule_id
    ^ " agent_id "
    ^ Ckappa_sig.string_of_agent_id agent_id
    ^ " site_type "
    ^ Ckappa_sig.string_of_site_name site_type

  (***************************************************************************)
  (*APPLY RULE*)
  (***************************************************************************)

  let check_association_list parameters error bdu_false pair check dynamic =
    let store_result = get_value dynamic in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, mvbdu =
      Ckappa_sig.Views_bdu.mvbdu_of_association_list parameters handler error
        check
    in
    let error, handler, bool =
      Site_across_bonds_domain_type.check parameters error bdu_false handler
        pair mvbdu store_result
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bool
  (*there is an action binding in the domain of a rule*)

  let build_mvbdu_association_list parameters error bdu_false kappa_handler
      dump_title bool modified_sites pair pair_list dynamic =
    let store_result = get_value dynamic in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, mvbdu =
      Ckappa_sig.Views_bdu.mvbdu_of_association_list parameters handler error
        pair_list
    in
    let error, bool, handler, modified_sites, store_result =
      Site_across_bonds_domain_type.add_link_and_check parameters error
        bdu_false handler kappa_handler bool dump_title pair mvbdu
        modified_sites store_result
    in
    let dynamic = set_value store_result dynamic in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, bool, dynamic, modified_sites

  let apply_rule_created_bonds static dynamic error bool dump_title rule_id rule
      precondition modified_sites =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    (*------------------------------------------------------*)
    (*let store_created_bonds = get_created_bonds static in*)
    let store_created_bonds = get_action_binding static in
    let error, created_bonds_set =
      Common_map.get_rule_id_map_and_set parameters error rule_id
        Ckappa_sig.PairAgentsSiteState_map_and_set.Set.empty store_created_bonds
    in
    let error, created_bonds_set =
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
        (fun (t, u) (error, modified_sites) ->
          Ckappa_sig.PairAgentsSiteState_map_and_set.Set.add_when_not_in
            parameters error (u, t) modified_sites)
        created_bonds_set (error, created_bonds_set)
    in
    let store_partition_created_bonds_map =
      get_partition_created_bonds_map static
    in
    (*------------------------------------------------------*)
    let error, bool, dynamic, precondition, modified_sites =
      Ckappa_sig.PairAgentsSiteState_map_and_set.Set.fold
        (fun (t, u) (error, bool, dynamic, precondition, modified_sites) ->
          let agent_id_t, agent_type_t, site_type_t, state_t = t in
          let agent_id_u, agent_type_u, site_type_u, state_u = u in
          let error, potential_tuple_pair_set =
            match
              Site_across_bonds_domain_type.PairAgentSiteState_map_and_set.Map
              .find_option_without_logs parameters error
                ( (agent_type_t, site_type_t, state_t),
                  (agent_type_u, site_type_u, state_u) )
                store_partition_created_bonds_map
            with
            | error, None ->
              ( error,
                Site_across_bonds_domain_type.PairAgentSitesState_map_and_set
                .Set
                .empty )
            | error, Some s -> error, s
          in
          (*-----------------------------------------------------------*)
          (*project the second site*)
          (*MOVE this function in static*)
          let proj (_, _, d, _) = d in
          let proj2 (x, y) = proj x, proj y in
          let error, proj_potential_tuple_pair_set =
            Site_across_bonds_domain_type.Proj_potential_tuple_pair_set.proj_set
              proj2 parameters error potential_tuple_pair_set
          in
          (*-----------------------------------------------------------*)
          Site_across_bonds_domain_type.PairSite_map_and_set.Set.fold
            (fun (site_type'_x, site_type'_y)
                 (error, bool, dynamic, precondition, modified_sites) ->
              let error', dynamic, precondition, state'_list_x =
                Communication.get_state_of_site_in_postcondition
                  get_global_static_information get_global_dynamic_information
                  set_global_dynamic_information error static dynamic
                  (rule_id, rule) agent_id_t site_type'_x precondition
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  ~message:(context rule_id agent_id_t site_type'_x)
                  __POS__ Exit
              in
              let error', dynamic, precondition, state'_list_y =
                Communication.get_state_of_site_in_postcondition
                  get_global_static_information get_global_dynamic_information
                  set_global_dynamic_information error static dynamic
                  (rule_id, rule) agent_id_u site_type'_y precondition
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  ~message:(context rule_id agent_id_u site_type'_y)
                  __POS__ Exit
              in
              (*-----------------------------------------------------------*)
              let error, bool, dynamic, precondition, modified_sites =
                match state'_list_x, state'_list_y with
                (* _::_::_, _::_::_ ->
                   (*we know for sure that none of the two sites have been
                       modified*)
                       error, bool, dynamic, precondition, modified_sites*)
                | [], _ | _, [] ->
                  let error, () =
                    Exception.warn parameters error __POS__
                      ~message:
                        "empty list in potential states in post condition" Exit
                      ()
                  in
                  error, bool, dynamic, precondition, modified_sites
                | _ :: _, _ :: _ ->
                  (*general case*)
                  List.fold_left
                    (fun (error, bool, dynamic, precondition, modified_sites)
                         state'_x ->
                      List.fold_left
                        (fun (error, bool, dynamic, precondition, modified_sites)
                             state'_y ->
                          let pair_list =
                            [
                              Ckappa_sig.fst_site, state'_x;
                              Ckappa_sig.snd_site, state'_y;
                            ]
                          in
                          let pair =
                            ( (agent_type_t, site_type_t, site_type'_x, state_t),
                              (agent_type_u, site_type_u, site_type'_y, state_u)
                            )
                          in
                          let error, bool, dynamic, modified_sites =
                            build_mvbdu_association_list parameters error
                              bdu_false kappa_handler dump_title bool
                              modified_sites pair pair_list dynamic
                          in
                          error, bool, dynamic, precondition, modified_sites)
                        (error, bool, dynamic, precondition, modified_sites)
                        state'_list_y)
                    (error, bool, dynamic, precondition, modified_sites)
                    state'_list_x
              in
              error, bool, dynamic, precondition, modified_sites)
            proj_potential_tuple_pair_set
            (error, bool, dynamic, precondition, modified_sites))
        created_bonds_set
        (error, bool, dynamic, precondition, modified_sites)
    in
    error, bool, dynamic, precondition, modified_sites

  (*a site is modified (explicitly)*)

  let get_state_of_site_in_pre_post_condition_2 error static dynamic agent_id_t
      (site_type_x, agent_type_y, site_type_y) site_type'_y defined_in
      precondition =
    (*CHECK ME*)
    let step =
      {
        Communication.site_out = site_type_x;
        Communication.site_in = site_type_y;
        Communication.agent_type_in = agent_type_y;
      }
    in
    let path =
      {
        Communication.defined_in;
        path =
          {
            Communication.agent_id = agent_id_t;
            Communication.relative_address = [ step ];
            Communication.site = site_type'_y;
          };
      }
    in
    let error, global_dynamic, precondition, state_list_lattice =
      Communication.get_state_of_site error precondition
        (get_global_static_information static)
        (get_global_dynamic_information dynamic)
        path
    in
    let error, state_list =
      match state_list_lattice with
      | Usual_domains.Val l -> error, l
      | Usual_domains.Undefined -> error, []
      | Usual_domains.Any ->
        let parameter = get_parameter static in
        let error, () =
          if Remanent_parameters.get_view_analysis parameter then
            Exception.warn (get_parameter static) error __POS__ Exit ()
          else
            error, ()
        in
        let kappa_handler = get_kappa_handler static in
        Handler.state_list parameter kappa_handler error agent_type_y
          site_type_y
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

  let get_state_of_site_in_precondition_2 error static dynamic rule agent_id
      agent_type site precondition =
    let path =
      {
        Communication.defined_in = Communication.LHS rule;
        path =
          {
            Communication.agent_id;
            Communication.relative_address = [];
            Communication.site;
          };
      }
    in
    let error, global_dynamic, precondition, state_list_lattice =
      Communication.get_state_of_site error precondition
        (get_global_static_information static)
        (get_global_dynamic_information dynamic)
        path
    in
    let error, state_list =
      match state_list_lattice with
      | Usual_domains.Val l -> error, l
      | Usual_domains.Undefined -> error, []
      | Usual_domains.Any ->
        let parameter = get_parameter static in
        let error, () =
          if Remanent_parameters.get_view_analysis parameter then
            Exception.warn (get_parameter static) error __POS__ Exit ()
          else
            error, ()
        in
        let kappa_handler = get_kappa_handler static in
        Handler.state_list parameter kappa_handler error agent_type site
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

  let get_state_of_site_in_postcondition_2 error static dynamic rule agent_id
      (site_type_x, agent_type_y, site_type_y) site_type'_y precondition =
    let defined_in = Communication.RHS rule in
    get_state_of_site_in_pre_post_condition_2 error static dynamic agent_id
      (site_type_x, agent_type_y, site_type_y)
      site_type'_y defined_in precondition

  type pos = Fst | Snd

  let get_partition_modified pos static =
    match pos with
    | Fst -> get_partition_modified_map_1 static
    | Snd -> get_partition_modified_map_2 static

  let get_state_of_site_in_postcondition_gen pos error static dynamic rule
      agent_id_mod (agent_type_x, site_type_x, site_type'_x, _)
      (agent_type_y, site_type_y, site_type'_y, _) precondition =
    match pos with
    | Fst ->
      get_state_of_site_in_postcondition_2 error static dynamic rule
        agent_id_mod
        (site_type_x, agent_type_y, site_type_y)
        site_type'_y precondition
    | Snd ->
      get_state_of_site_in_postcondition_2 error static dynamic rule
        agent_id_mod
        (site_type_y, agent_type_x, site_type_x)
        site_type'_x precondition

  let get_potential_tuple_pair parameters error (agent, site) empty_map map =
    let error, result =
      match
        Site_across_bonds_domain_type.AgentSite_map_and_set.Map
        .find_option_without_logs parameters error (agent, site) map
      with
      | error, None -> error, empty_map
      | error, Some s -> error, s
    in
    error, result

  let apply_rule_modified_explicity_gen ~pos bdu_false parameters error
      kappa_handler bool dump_title static dynamic rule_id rule precondition
      modified_set modified_sites =
    let store_partition_modified_map = get_partition_modified pos static in
    (*------------------------------------------------------*)
    Ckappa_sig.AgentsSiteState_map_and_set.Set.fold
      (fun mod_tuple (error, bool, dynamic, precondition, modified_sites) ->
        let agent_id_mod, agent_type_mod, site_type_mod, state_mod =
          mod_tuple
        in
        let error, potential_tuple_pair_set =
          get_potential_tuple_pair parameters error
            (agent_type_mod, site_type_mod)
            Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set
            .empty store_partition_modified_map
        in
        (*-----------------------------------------------------------*)
        Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.fold
          (fun (x, y) (error, bool, dynamic, precondition, modified_sites) ->
            let agent_type_x, site_type_x, site_type'_x, state_x = x in
            let agent_type_y, site_type_y, site_type'_y, state_y = y in
            let error', dynamic, precondition, state'_list_other =
              get_state_of_site_in_postcondition_gen pos error static dynamic
                (rule_id, rule) agent_id_mod x y precondition
            in
            let error', (agent_y, site_y) =
              Site_across_bonds_domain_type.convert_single_without_state
                parameters error' kappa_handler
                (agent_type_y, site_type_y)
            in
            let error', (agent_x, site_x) =
              Site_across_bonds_domain_type.convert_single_without_state
                parameters error' kappa_handler
                (agent_type_x, site_type_x)
            in
            let () =
              if error' == error then
                ()
              else
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "\n\
                   WRONG TUPLE: !!! \n\
                  \ Rule %i agent_id_t: %i%s:%s( site_type_x: %i:%s), \
                   agent_type_y:%i:%s: (site_type_y:%i:%s) \n"
                  (Ckappa_sig.int_of_rule_id rule_id)
                  (Ckappa_sig.int_of_agent_id agent_id_mod)
                  (match pos with
                  | Fst -> "->"
                  | Snd -> "<-")
                  agent_x
                  (Ckappa_sig.int_of_site_name site_type_x)
                  site_x
                  (Ckappa_sig.int_of_agent_name agent_type_y)
                  agent_y
                  (Ckappa_sig.int_of_site_name site_type_y)
                  site_y
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                ~message:
                  (context rule_id agent_id_mod
                     (match pos with
                     | Fst -> site_type'_x
                     | Snd -> site_type'_x))
                __POS__ Exit
            in
            let error', dynamic, precondition, state_list_other =
              let error, agent =
                match
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameters error agent_id_mod
                    rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs
                      .Cckappa_sig.views
                with
                | error, None ->
                  Exception.warn parameters error __POS__ Exit Cckappa_sig.Ghost
                | error, Some a -> error, a
              in
              match agent with
              | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _
              | Cckappa_sig.Dead_agent _ ->
                error, dynamic, precondition, []
              | Cckappa_sig.Agent _ ->
                get_state_of_site_in_precondition_2 error static dynamic
                  (rule_id, rule) agent_id_mod agent_type_mod site_type_mod
                  precondition
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                ~message:(context rule_id agent_id_mod site_type_mod)
                __POS__ Exit
            in
            (*-----------------------------------------------------------*)
            let error, bool, dynamic, precondition, modified_sites =
              let not_modified =
                match state'_list_other with
                | [] | _ :: _ :: _ ->
                  true
                  (* we know for sure that the site has not been modified *)
                | [ _ ] -> false
              in
              List.fold_left
                (fun (error, bool, dynamic, precondition, modified_sites)
                     state'_other ->
                  let pair_list =
                    match pos with
                    | Fst ->
                      [
                        Ckappa_sig.fst_site, state_mod;
                        Ckappa_sig.snd_site, state'_other;
                      ]
                    | Snd ->
                      [
                        Ckappa_sig.fst_site, state'_other;
                        Ckappa_sig.snd_site, state_mod;
                      ]
                  in
                  let pair =
                    ( (agent_type_x, site_type_x, site_type'_x, state_x),
                      (agent_type_y, site_type_y, site_type'_y, state_y) )
                  in
                  let check =
                    match state_list_other, pos with
                    | ([] | _ :: _ :: _), _ -> []
                    | [ a ], Fst -> [ Ckappa_sig.fst_site, a ]
                    | [ a ], Snd -> [ Ckappa_sig.snd_site, a ]
                  in
                  let check =
                    if not_modified then (
                      match pos with
                      | Fst ->
                        (* to do: add info about the other site *)
                        (* if the bond between site_type_x and site_type_y
                           has not been created by the rule *)
                        (* this is the state before the modification *)
                        (* otherwise, nothing to check *)
                        (Ckappa_sig.snd_site, state'_other) :: check
                      | Snd ->
                        (* to do: add info about the other site *)
                        (* if the bond between site_type_x and site_type_y
                           has not been created by the rule *)
                        (* this is the state before the modification *)
                        (* this is the state before the modification *)
                        (Ckappa_sig.fst_site, state'_other) :: check
                    ) else
                      check
                  in
                  let error, dynamic, unmodified_sites_ok =
                    check_association_list parameters error bdu_false pair check
                      dynamic
                  in
                  if unmodified_sites_ok then (
                    let error, bool, dynamic, modified_sites =
                      build_mvbdu_association_list parameters error bdu_false
                        kappa_handler dump_title bool modified_sites pair
                        pair_list dynamic
                    in
                    error, bool, dynamic, precondition, modified_sites
                  ) else
                    error, bool, dynamic, precondition, modified_sites)
                (error, bool, dynamic, precondition, modified_sites)
                state'_list_other
            in
            error, bool, dynamic, precondition, modified_sites)
          potential_tuple_pair_set
          (error, bool, dynamic, precondition, modified_sites))
      modified_set
      (error, bool, dynamic, precondition, modified_sites)

  let apply_rule_modified_explicity static dynamic error bool dump_title rule_id
      rule precondition modified_sites =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store_modified_map = get_modified_map static in
    let error, modified_set =
      Common_map.get_rule_id_map_and_set parameters error rule_id
        Ckappa_sig.AgentsSiteState_map_and_set.Set.empty store_modified_map
    in
    (*---------------------------------------------------------------*)
    let error, bool, dynamic, precondition, modified_sites =
      apply_rule_modified_explicity_gen ~pos:Fst bdu_false parameters error
        kappa_handler bool dump_title static dynamic rule_id rule precondition
        modified_set modified_sites
    in
    let error, bool, dynamic, precondition, modified_sites =
      apply_rule_modified_explicity_gen ~pos:Snd bdu_false parameters error
        kappa_handler bool dump_title static dynamic rule_id rule precondition
        modified_set modified_sites
    in
    error, bool, dynamic, precondition, modified_sites

  (*Apply rule in the case of side effects*)

  let free_site_gen ~pos static dynamic error bool dump_title agent' site_name'
      state' modified_sites =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let store_partition_modified_map = get_partition_modified pos static in
    let error, potential_tuple_pair_set =
      get_potential_tuple_pair parameters error (agent', site_name')
        Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty
        store_partition_modified_map
    in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    (*-----------------------------------------------------------*)
    Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Set.fold
      (fun (x, y) (error, bool, dynamic, modified_sites) ->
        let handler = get_mvbdu_handler dynamic in
        let result = get_value dynamic in
        let error, mvbdu_opt =
          Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Map
          .find_option_without_logs parameters error (x, y) result
        in
        match mvbdu_opt with
        | None -> error, bool, dynamic, modified_sites
        | Some mvbdu ->
          let var =
            match pos with
            | Fst -> Ckappa_sig.fst_site
            | Snd -> Ckappa_sig.snd_site
          in
          let error, handler, cap =
            Ckappa_sig.Views_bdu.mvbdu_of_association_list parameters handler
              error
              [ var, state' ]
          in
          let error, handler, redefine =
            Ckappa_sig.Views_bdu.build_association_list parameters handler error
              [ var, Ckappa_sig.dummy_state_index ]
          in
          let error, handler, mvbdu_cap =
            Ckappa_sig.Views_bdu.mvbdu_and parameters handler error mvbdu cap
          in
          let error, handler, mvbdu' =
            Ckappa_sig.Views_bdu.mvbdu_redefine parameters handler error
              mvbdu_cap redefine
          in
          (*check the freshness of the value *)
          let error, bool, handler, modified_sites, result =
            Site_across_bonds_domain_type.add_link_and_check parameters error
              bdu_false handler kappa_handler bool dump_title (x, y) mvbdu'
              modified_sites result
          in
          let dynamic = set_mvbdu_handler handler dynamic in
          let dynamic = set_value result dynamic in
          error, bool, dynamic, modified_sites)
      potential_tuple_pair_set
      (error, bool, dynamic, modified_sites)

  let free_site static dynamic error bool dump_title agent' site_name' state'
      modified_sites =
    let error, bool, dynamic, modified_sites =
      free_site_gen ~pos:Fst static dynamic error bool dump_title agent'
        site_name' state' modified_sites
    in
    free_site_gen ~pos:Snd static dynamic error bool dump_title agent'
      site_name' state' modified_sites

  let apply_rule static dynamic error rule_id precondition =
    let parameters = get_parameter static in
    let event_list = [] in
    (*-----------------------------------------------------------*)
    let error, rule = get_rule parameters error static rule_id in
    match rule with
    | None ->
      let error, () = Exception.warn parameters error __POS__ Exit () in
      error, dynamic, (precondition, [])
    | Some rule ->
      (*------------------------------------------------------*)
      (*1.a bonds on the rhs: not sure you need this test, it will be
        covered by 1.c and 1.d *)
      (*------------------------------------------------------*)
      (* let error, dynamic, precondition =
         apply_rule_bonds_rhs
           static dynamic error rule_id rule precondition
             in*)
      (*------------------------------------------------------*)
      (*1.b created bonds *)
      (*------------------------------------------------------*)
      let parameters = Remanent_parameters.update_prefix parameters "\t\t" in
      let dump_title () =
        if
          local_trace
          || Remanent_parameters.get_dump_reachability_analysis_diff parameters
        then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%sUpdate information about potential sites across domain"
              (Remanent_parameters.get_prefix parameters)
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        ) else
          ()
      in
      let error, modified_sites =
        Communication.init_sites_working_list parameters error
      in
      (*-----------------------------------------------------------*)
      let error, bool, dynamic, precondition, event_list, modified_sites =
        (* deal with create a binding sites *)
        let error, bool, dynamic, precondition, modified_sites =
          apply_rule_created_bonds static dynamic error false dump_title rule_id
            rule precondition modified_sites
        in
        (*-----------------------------------------------------------*)
        (*new event*)
        error, bool, dynamic, precondition, event_list, modified_sites
      in
      (*-----------------------------------------------------------*)
      (*1.c a site is modified (explicitly) *)
      let error, bool, dynamic, precondition, modified_sites =
        apply_rule_modified_explicity static dynamic error bool dump_title
          rule_id rule precondition modified_sites
      in
      (*-----------------------------------------------------------*)
      (*new event*)
      (*-----------------------------------------------------------*)
      (*1.d a site is modified by side effect *)
      let () =
        if
          bool
          && (local_trace
             || Remanent_parameters.get_dump_reachability_analysis_diff
                  parameters)
        then (
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        )
      in
      let error, event_list =
        Communication.fold_sites parameters error
          (fun _ error s _ event_list ->
            error, Communication.Modified_sites s :: event_list)
          modified_sites event_list
      in
      (*-----------------------------------------------------------*)
      error, dynamic, (precondition, event_list)

  (***************************************************************************)
  (* events enable communication between domains. At this moment, the
         global domain does not collect information *)
  (***************************************************************************)

  let apply_one_side_effect static dynamic error _
      (_, (agent_name, site, state)) precondition =
    let parameters = get_parameter static in
    let dump_title () =
      if
        local_trace
        || Remanent_parameters.get_dump_reachability_analysis_diff parameters
      then (
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%sUpdate information about potential sites across domain"
            (Remanent_parameters.get_prefix parameters)
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      ) else
        ()
    in
    let error, modified_sites =
      Communication.init_sites_working_list parameters error
    in
    let error, bool, dynamic, modified_sites =
      free_site static dynamic error false dump_title agent_name site state
        modified_sites
    in
    let error, event_list =
      Communication.fold_sites parameters error
        (fun _ error s _ event_list ->
          error, Communication.Modified_sites s :: event_list)
        modified_sites []
    in
    let () =
      if
        bool
        && (local_trace
           || Remanent_parameters.get_dump_reachability_analysis_diff parameters
           )
      then (
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      )
    in
    error, dynamic, (precondition, event_list)

  (****************************************************************)
  (*APPLY A LIST OF EVENT*)
  (****************************************************************)

  let apply_event_list _static dynamic error _event_list = error, dynamic, []
  let stabilize _static dynamic error = error, dynamic, ()

  (****************************************************************)
  (*EXPORT*)
  (****************************************************************)

  let export_aux ?(final_result = false) static dynamic error kasa_state =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let handler = get_mvbdu_handler dynamic in
    let store_value = get_value dynamic in
    let domain_name = "Connected agents" in
    let error, (handler, current_list) =
      Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Map.fold
        (fun tuple mvbdu (error, (handler, current_list)) ->
          let ( (agent_type1, site_type1, site_type1', _),
                (agent_type2, site_type2, site_type2', _) ) =
            tuple
          in
          let error, (agent1, site1, site1', _, agent2, site2, site2', _) =
            Site_across_bonds_domain_type.convert_tuple parameters error
              kappa_handler tuple
          in
          (*this test remove the relation: B.A*)
          if compare (agent1, site1, site1') (agent2, site2, site2') > 0 then
            error, (handler, current_list)
          else (
            (*this test remove the relation when their states are not the
              same: for instance: A(x~p), B(x~u)*)
            let error, handler, non_relational =
              if final_result then
                (*at the final result needs to check the non_relational
                  condition*)
                Translation_in_natural_language.non_relational parameters
                  handler error mvbdu
              else
                (*other cases will by pass this test*)
                error, handler, false
            in
            if non_relational then
              error, (handler, current_list)
            else (
              (*----------------------------------------------------*)
              let error, handler, pair_list =
                Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler
                  error mvbdu
              in
              match Remanent_parameters.get_backend_mode parameters with
              | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
                let pattern = Site_graphs.KaSa_site_graph.empty in
                let error, agent_id1, pattern =
                  Site_graphs.KaSa_site_graph.add_agent parameters error
                    kappa_handler agent_type1 pattern
                in
                let error, agent_id2, pattern =
                  Site_graphs.KaSa_site_graph.add_agent parameters error
                    kappa_handler agent_type2 pattern
                in
                let error, pattern =
                  Site_graphs.KaSa_site_graph.add_bond parameters error
                    kappa_handler agent_id1 site_type1 agent_id2 site_type2
                    pattern
                in
                (*---------------------------------------------------*)
                (*internal constraint list*)
                let error, refine =
                  Ckappa_site_graph.internal_pair_list_to_list parameters error
                    kappa_handler pattern agent_id1 site_type1' agent_id2
                    site_type2' pair_list
                in
                let lemma_internal =
                  { Public_data.hyp = pattern; Public_data.refinement = refine }
                in
                let current_list = lemma_internal :: current_list in
                (*---------------------------------------------------*)
                error, (handler, current_list)
              | Remanent_parameters_sig.Natural_language ->
                error, (handler, current_list)
            )
          ))
        store_value
        (error, (handler, []))
    in
    (*------------------------------------------------------------------*)
    let dynamic = set_mvbdu_handler handler dynamic in
    (*------------------------------------------------------------------*)
    (*internal constraint list*)
    let internal_constraints_list =
      Remanent_state.get_internal_constraints_list kasa_state
    in
    let error, internal_constraints_list =
      match internal_constraints_list with
      | None -> Exception.warn parameters error __POS__ Exit []
      | Some l -> error, l
    in
    let pair_list =
      (domain_name, List.rev current_list) :: internal_constraints_list
    in
    let kasa_state =
      Remanent_state.set_internal_constraints_list pair_list kasa_state
    in
    error, dynamic, kasa_state

  let export static dynamic error kasa_state =
    export_aux ~final_result:true static dynamic error kasa_state

  (****************************************************************)
  (*PRINT*)
  (****************************************************************)

  let print ?dead_rules static dynamic (error : Exception.method_handler)
      loggers =
    let _ = dead_rules in
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let handler = get_mvbdu_handler dynamic in
    let log = loggers in
    (*--------------------------------------------------------*)
    let error, handler =
      if Remanent_parameters.get_dump_reachability_analysis_result parameters
      then (
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------";
          Loggers.print_newline log;
          Loggers.fprintf log "* Properties in connected agents";
          Loggers.print_newline log;

          Loggers.fprintf log
            "------------------------------------------------------------";
          Loggers.print_newline log
        in
        (*--------------------------------------------------------*)
        (*print result*)
        let store_value = get_value dynamic in
        let error, handler =
          Site_across_bonds_domain_type.PairAgentSitesState_map_and_set.Map.fold
            (fun (x, y) mvbdu (error, handler) ->
              Site_across_bonds_domain_type.print_site_across_domain
                ~verbose:true ~sparse:true ~final_result:true ~dump_any:true
                parameters error kappa_handler handler (x, y) mvbdu)
            store_value (error, handler)
        in
        error, handler
      ) else
        error, handler
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, ()

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
