(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 12 2016>
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
          Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Map.t;
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

  (*-------------------------------------------------------------*)
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

  (*-------------------------------------------------------------*)

  let get_bonds_rhs_set static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_bonds_rhs_set

  let set_bonds_rhs_set r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_bonds_rhs_set = r
      } static

  let get_partition_bonds_rhs_map static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_partition_bonds_rhs_map

  let set_partition_bonds_rhs_map r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_partition_bonds_rhs_map = r
      } static

  (*
  let get_partition_bonds_rhs_map' static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_partition_bonds_rhs_map'

  let set_partition_bonds_rhs_map' r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_partition_bonds_rhs_map' = r
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
    *)
  (*------------------------------------------------------------*)

  let get_created_bonds static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_created_bonds

  let set_created_bonds r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bonds = r
      } static

  (*
  let get_created_bonds_set static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_created_bonds_set

  let set_created_bonds_set r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_created_bonds_set = r
      } static

  let get_partition_created_bonds_map static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_partition_created_bonds_map

  let set_partition_created_bonds_map r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_partition_created_bonds_map = r
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
    *)
  (*------------------------------------------------------------*)

  let get_modified_map static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_modified_map

  let set_modified_map r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_modified_map = r
      } static

  (*------------------------------------------------------------*)

  let get_question_marks_rhs static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_question_marks_rhs

  let set_question_marks_rhs r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_question_marks_rhs = r
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
    (*bonds on the left hand side*)
    let store_bonds_lhs = get_bonds_lhs static in
    let error, store_bonds_lhs =
      Site_accross_bonds_domain_static.collect_bonds_lhs
        parameter error rule_id rule store_bonds_lhs
    in
    let static = set_bonds_lhs store_bonds_lhs static in
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
    (*question marks on the right hand side*)
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_modified_map = get_modified_map static in
    let error, store_question_marks_rhs =
      Site_accross_bonds_domain_static.collect_question_marks_rhs
        parameter error
        kappa_handler
        rule_id
        rule
        store_modified_map
        store_question_marks_rhs
    in
    let static = set_question_marks_rhs store_question_marks_rhs static in
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
    (*projection of bonds on the rhs*)
    (*------------------------------------------------------------*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, store_bonds_rhs_set =
      Site_accross_bonds_domain_static.collect_bonds_rhs_set
        parameter error
        store_bonds_rhs
    in
    let static = set_bonds_rhs_set store_bonds_rhs_set static in
    (*------------------------------------------------------------*)
    (*partition map with key is the pair in bonds rhs*)
    let store_potential_tuple_pair = get_potential_tuple_pair static in
    let error, store_partition_bonds_rhs_map =
      Site_accross_bonds_domain_static.collect_partition_bonds_rhs_map
        parameter error
        store_potential_tuple_pair
    in
    let static =
      set_partition_bonds_rhs_map
        store_partition_bonds_rhs_map
        static
    in
    (*------------------------------------------------------------*)
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
          Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Map.empty;

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
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let handler = get_mvbdu_handler dynamic in
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
  (* For each bond in the lhs of the rule rule_id *)
  (* For each tuple (x,y) of interest that gives information about this kind of bonds *)
  (* Fetch the state of the two other sites in the lhs and in the precondition if they are not available (take the meet)*)
  (* Check that there exists at least one such pair of state in the image of the pair (x,y) in dynamic *)
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

  let get_state_of_site_in_pre_post_condition
      parameter error dynamic agent_id
      site_type defined_in precondition =
    let path =
      {
        Communication.defined_in = defined_in ;
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

(*use this function before apply a rule, like in is_enabled*)

  let get_state_of_site_in_precondition
      parameter error dynamic rule agent_id site_type precondition =
    let defined_in = Communication.LHS rule in
    get_state_of_site_in_pre_post_condition
      parameter error dynamic agent_id
      site_type defined_in precondition

  let get_state_of_site_in_postcondition
      parameter error dynamic rule agent_id site_type precondition =
    let defined_in = Communication.RHS rule in
    get_state_of_site_in_pre_post_condition
      parameter error dynamic agent_id
      site_type defined_in precondition

  let context rule_id agent_id site_type =
    " rule "^(Ckappa_sig.string_of_rule_id rule_id)^
    " agent_id "^(Ckappa_sig.string_of_agent_id agent_id)^
    " site_type "^(Ckappa_sig.string_of_site_name site_type)

  (* You shoull first itterates over t *)
  (* Then, use the appropriate map in static (add it if it is missing) *)
  (* to get the appropriate x that satisfied the constraint: *)
  (* agent_type = agent_type_q && site_type = site_type_q && site_type' = site_type_q'*)


  let apply_rule_bonds_rhs static dynamic error rule_id rule precondition =
    (* there is a bond in the domain of a rule *)
    let parameter  = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    (*------------------------------------------------------*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, bonds_rhs_set =
      Site_accross_bonds_domain_static.get_set parameter error
        rule_id
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
        store_bonds_rhs
    in
    let store_partition_bonds_rhs_map = get_partition_bonds_rhs_map static in
    (*------------------------------------------------------*)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
        (fun (t, u) (error, dynamic, precondition) ->
           let (agent_id_t, agent_type_t, site_type_t, state_t) = t in
           let (agent_id_u, agent_type_u, site_type_u, state_u) = u in
           let error, potential_tuple_pair_set =
             match
               Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Map.find_option_without_logs
                 parameter error
                 ((agent_type_t, site_type_t, state_t),
                  (agent_type_u, site_type_u, state_u))
                 store_partition_bonds_rhs_map
             with
             | error, None ->
               error,
               Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.empty
             | error, Some s -> error, s
           in
           (*project this set*)
           let proj (_,_, d, _) = d in
           let proj2 (x, y) = proj x, proj y in
           let error, proj_potential_tuple_pair_set =
             Site_accross_bonds_domain_type.Proj_potential_tuple_pair_set.proj_set
               proj2
               parameter
               error
               potential_tuple_pair_set
           in
           Site_accross_bonds_domain_type.PairSite_map_and_set.Set.fold
             (fun (site_type'_x, site_type'_y) (error, dynamic, precondition) ->
                let error', dynamic, precondition, state'_list_x =
                  get_state_of_site_in_postcondition
                    parameter error dynamic
                    rule
                    agent_id_t
                    site_type'_x
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:(context rule_id agent_id_t site_type'_x)
                    __POS__ Exit
                in
                let error', dynamic, precondition, state'_list_y =
                  get_state_of_site_in_postcondition
                    parameter error
                    dynamic
                    rule
                    agent_id_u
                    site_type'_y
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:(context rule_id agent_id_u site_type'_y)
                    __POS__ Exit
                in
                let error, dynamic, precondition =
                  match state'_list_x, state'_list_y with
                  | _::_::_, _::_::_ ->
                    (*we know for sure that none of the two sites have been
                      modified*)
                    error, dynamic, precondition
                  | [], _ | _, [] ->
                    let error, () =
                      Exception.warn parameter error __POS__
                        ~message: "empty list in potential states in post condition" Exit ()
                    in
                    error, dynamic, precondition
                  | [_], _ | _, [_] -> (*general case*)
                    List.fold_left (fun (error, dynamic, precondition) state'_x ->
                        List.fold_left (fun (error, dynamic, precondition) state'_y ->
                            let store_result = get_value dynamic in
                            let pair_list =
                              [Ckappa_sig.fst_site, state'_x;
                               Ckappa_sig.snd_site, state'_y]
                            in
                            let pair =
                              (agent_type_t, site_type_t, site_type'_x, state_t),
                              (agent_type_u, site_type_u, site_type'_y, state_u)
                            in
                            let handler = get_mvbdu_handler dynamic in
                            let error, handler, mvbdu =
                              Ckappa_sig.Views_bdu.mvbdu_of_association_list
                                parameter handler error pair_list
                            in
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
                          ) (error, dynamic, precondition) state'_list_y
                      ) (error, dynamic, precondition) state'_list_x
                in
                error, dynamic, precondition
             ) proj_potential_tuple_pair_set (error, dynamic, precondition)
        ) bonds_rhs_set (error, dynamic, precondition)
    in
    error, dynamic, precondition
           (* once t and u are fixed, you should apply the appropriate map *)
           (* to get the set of potential tuple pairs *)
           (* ((agent_type_x, site_type_x, site_type'_x, state_x, state'_x),
              (agent_type_y, site_type_y, site_type'_y, state_y, state'_y)) *)
           (* such that
                   agent_type_x = agent_type_t &&
                    site_type_x = site_type_t &&
                        state_x = state_t &&
                   agent_type_y = agent_type_u &&
                    site_type_y = site_type_u &&
                        state_y = state_u *)
           (* If you do not have this function, add it the struct static *)
           (* and compute it in your scan_rules function, after the iteration over all the rules, thanks to the partioning function *)
           (* then fold over this set instead of the full one *)
  (*Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, dynamic, precondition) ->
                let (_agent_type_x, _site_type_x, site_type'_x, _state_x, _state'_x) = x in (* I do not understand what is _state'_x *)
                let (_agent_type_y, _site_type_y, site_type'_y, _state_y, _state'_y) = y    (* I do not understand what is _state'_y *)
                in
                (* the following test is temporary, it will disapear when the fold will be done directly on the subset that satsifies these constraints *)
                if _agent_type_x = agent_type_t &&
                   _site_type_x = site_type_t &&
                   _state_x = state_t &&
                   _agent_type_y = agent_type_u &&
                   _site_type_y = site_type_u &&
                   _state_y = state_u
                then
                  let error', dynamic, precondition, state'_list_x =
                    get_state_of_site_in_postcondition
                      parameter error
                      dynamic
                      rule
                      agent_id_t
                      site_type'_x
                      precondition
                  in
                  let error =
                    Exception.check_point
                      Exception.warn
                      parameter error error'
                      ~message:(context rule_id agent_id_t site_type'_x)
                      __POS__ Exit
                  in
                  let error', dynamic, precondition, state'_list_y =
                    get_state_of_site_in_postcondition
                      parameter error
                      dynamic
                      rule
                      agent_id_u
                      site_type'_y
                      precondition
                  in
                  let error =
                    Exception.check_point
                      Exception.warn
                      parameter error error'
                      ~message:(context rule_id agent_id_u site_type'_y)
                      __POS__ Exit
                  in
                  (*------------------------------------------------------*)
                  let error, dynamic, precondition =
                    match state'_list_x, state'_list_x with
                    | _::_::_, _::_::_ ->
                      error, dynamic, precondition (* we know for sure that none of the two sites have been modified *)
                    | [], _ | _, [] ->
                      let error, () =
                        Exception.warn
                          parameter error __POS__
                          ~message:"empty list in potential states in post condition"
                          Exit ()
                      in
                      error, dynamic, precondition
                    | [_],_ | _,[_] -> (* general case *)
                      List.fold_left (fun (error, dynamic, precondition) state'_x ->
                          List.fold_left
                            (fun (error, dynamic, precondition) state'_y ->
                               let store_result = get_value dynamic in
                               let pair_list =
                                 [Ckappa_sig.fst_site, state'_x;
                                  Ckappa_sig.snd_site, state'_y]
                               in
                               let pair =
                                 (agent_type_t, site_type_t, site_type'_x, state_t),
                                 (agent_type_u, site_type_u, site_type'_y,
                                  state_u)
                               in
                               let handler = get_mvbdu_handler dynamic in
                               let error, handler, mvbdu =
                                 Ckappa_sig.Views_bdu.mvbdu_of_association_list
                                   parameter handler error pair_list
                               in
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
                            ) (error, dynamic, precondition) state'_list_y
                        ) (error, dynamic, precondition) state'_list_x
                  in
                  error, dynamic, precondition
                else error, dynamic, precondition
             ) store_potential_tuple_pair_bonds_rhs
             (error, dynamic, precondition)
        ) bonds_rhs_set (error, dynamic, precondition)
    in
    error, dynamic, precondition*)

    (*
  let apply_rule_bonds_rhs' static dynamic error rule_id rule precondition =
    (* there is a bond in the domain of a rule *)
    let parameter  = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store_potential_tuple_pair_bonds_rhs =
      get_potential_tuple_pair_bonds_rhs static
    in (* this set is to big *)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, bonds_rhs_set =
      Site_accross_bonds_domain_static.get_set parameter error
        rule_id
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
        store_bonds_rhs
    in
    (*------------------------------------------------------*)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
        (fun (t, u) (error, dynamic, precondition) ->
        let (agent_id_t, agent_type_t, site_type_t, state_t) = t in
        let (agent_id_u, agent_type_u, site_type_u, state_u) = u in
           (* once t and u are fixed, you should apply the appropriate map *)
           (* to get the set of potential tuple pairs *)
           (* ((agent_type_x, site_type_x, site_type'_x, state_x, state'_x),
            (agent_type_y, site_type_y, site_type'_y, state_y, state'_y)) *)
           (* such that
                   agent_type_x = agent_type_t &&
                    site_type_x = site_type_t &&
                        state_x = state_t &&
                   agent_type_y = agent_type_u &&
                    site_type_y = site_type_u &&
                        state_y = state_u *)
           (* If you do not have this function, add it the struct static *)
           (* and compute it in your scan_rules function, after the iteration over all the rules, thanks to the partioning function *)
        (* then fold over this set instead of the full one *)
           Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, dynamic, precondition) ->
                let (_agent_type_x, _site_type_x, site_type'_x, _state_x, _state'_x) = x in (* I do not understand what is _state'_x *)
                let (_agent_type_y, _site_type_y, site_type'_y, _state_y, _state'_y) = y    (* I do not understand what is _state'_y *)
                in
                (* the following test is temporary, it will disapear when the fold will be done directly on the subset that satsifies these constraints *)
                if _agent_type_x = agent_type_t &&
                   _site_type_x = site_type_t &&
                  _state_x = state_t &&
                _agent_type_y = agent_type_u &&
                 _site_type_y = site_type_u &&
                   _state_y = state_u
                then
                let error', dynamic, precondition, state'_list_x =
                  get_state_of_site_in_postcondition
                    parameter error
                    dynamic
                    rule
                    agent_id_t
                    site_type'_x
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:(context rule_id agent_id_t site_type'_x)
                    __POS__ Exit
                in
                let error', dynamic, precondition, state'_list_y =
                  get_state_of_site_in_postcondition
                    parameter error
                    dynamic
                    rule
                    agent_id_u
                    site_type'_y
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:(context rule_id agent_id_u site_type'_y)
                    __POS__ Exit
                in
                (*------------------------------------------------------*)
                let error, dynamic, precondition =
                  match state'_list_x, state'_list_x with
                  | _::_::_, _::_::_ ->
                    error, dynamic, precondition (* we know for sure that none of the two sites have been modified *)
                  | [], _ | _, [] ->
                    let error, () =
                      Exception.warn
                        parameter error __POS__
                        ~message:"empty list in potential states in post condition"
                        Exit ()
                    in
                    error, dynamic, precondition
                  | [_],_ | _,[_] -> (* general case *)
                  List.fold_left (fun (error, dynamic, precondition) state'_x ->
                      List.fold_left
                        (fun (error, dynamic, precondition) state'_y ->
                           let store_result = get_value dynamic in
                             let pair_list =
                               [Ckappa_sig.fst_site, state'_x;
                                Ckappa_sig.snd_site, state'_y]
                             in
                             let pair =
                               (agent_type_t, site_type_t, site_type'_x, state_t),
                               (agent_type_u, site_type_u, site_type'_y,
                                state_u)
                             in
                             let handler = get_mvbdu_handler dynamic in
                             let error, handler, mvbdu =
                               Ckappa_sig.Views_bdu.mvbdu_of_association_list
                                 parameter handler error pair_list
                             in
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
                        ) (error, dynamic, precondition) state'_list_y
                    ) (error, dynamic, precondition) state'_list_x
                in
                error, dynamic, precondition
                else error, dynamic, precondition
             ) store_potential_tuple_pair_bonds_rhs
             (error, dynamic, precondition)
        ) bonds_rhs_set (error, dynamic, precondition)
    in
    error, dynamic, precondition
                    *)
  (*
  let apply_rule_created_bonds static dynamic error rule_id rule precondition =
    let parameter  = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store_potential_tuple_pair_created_bonds =
      get_potential_tuple_pair_created_bonds static
    in
    let store_created_bonds = get_created_bonds static in
    let error, created_bonds_set =
      Site_accross_bonds_domain_static.get_set parameter error
        rule_id
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
        store_created_bonds
    in
    (*------------------------------------------------------*)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.fold
        (fun (t, u) (error, dynamic, precondition) ->
           Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
             (fun (x, y) (error, dynamic, precondition) ->
                let (agent_type, site_type, site_type', state, state') = x in
                let (agent_type1, site_type1, site_type1', state1, state1') = y
                in
                let (agent_id, _, site_type_t, _) = t in
                let (agent_id1, _, site_type_u, _) = u in
                let error', dynamic, precondition, state_list =
                  get_state_of_site_in_precondition
                    parameter error
                    dynamic
                    rule
                    agent_id
                    site_type_t
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:("rule :"^(Ckappa_sig.string_of_rule_id rule_id)^" agent_id: "^(Ckappa_sig.string_of_agent_id agent_id)^" site: "^(Ckappa_sig.string_of_site_name site_type_t))
                    __POS__ Exit
                in
                let error', dynamic, precondition, state_list' =
                  get_state_of_site_in_postcondition
                    parameter error
                    dynamic
                    rule
                    agent_id1
                    site_type_u
                    precondition
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error'
                    ~message:("rule :"^(Ckappa_sig.string_of_rule_id rule_id)^" agent_id: "^(Ckappa_sig.string_of_agent_id agent_id1)^" site: "^(Ckappa_sig.string_of_site_name site_type_u))
                    __POS__ Exit
                in
                (*------------------------------------------------------*)
                let error, dynamic, precondition =
                  List.fold_left (fun (error, dynamic, precondition) pre_state ->
                      List.fold_left
                        (fun (error, dynamic, precondition) pre_state1 ->
                           let store_result = get_value dynamic in
                           if pre_state = state && pre_state1 = state1
                           then
                             let pair_list =
                               [Ckappa_sig.fst_site, state';
                                Ckappa_sig.snd_site, state1']
                             in
                             let pair =
                               (agent_type, site_type, site_type', state),
                               (agent_type1, site_type1, site_type1', state1)
                             in
                             let handler = get_mvbdu_handler dynamic in
                             let error, handler, mvbdu =
                               Ckappa_sig.Views_bdu.mvbdu_of_association_list
                                 parameter handler error pair_list
                             in
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
                           else error, dynamic, precondition
                        ) (error, dynamic, precondition) state_list'
                    ) (error, dynamic, precondition) state_list
                in
                error, dynamic, precondition
             ) store_potential_tuple_pair_created_bonds
             (error, dynamic, precondition)
        ) created_bonds_set (error, dynamic, precondition)
    in
    error, dynamic, precondition*)

  let apply_rule static dynamic error rule_id precondition =
    let parameter  = get_parameter static in
    (*let kappa_handler = get_kappa_handler static in
      let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in*)
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
      let _log = Remanent_parameters.get_logger parameter in
      (*------------------------------------------------------*)
      (*1.a bonds on the rhs: not sure you need this test, it will be
        covered by 1.c and 1.d *)
      (*------------------------------------------------------*)
      let error, dynamic, precondition =
        apply_rule_bonds_rhs
          static dynamic error rule_id rule precondition
      in
      (*------------------------------------------------------*)
      (*1.b created bonds *)
      (*------------------------------------------------------*)
      (*let error, dynamic, precondition =
        apply_rule_created_bonds
          static dynamic error rule_id rule precondition
      in*)
      (*1.c a site is modified (explicitly) *)
      (*1.d a site is modified by side effect *)
      (*-----------------------------------------------------------*)
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
        (*tuple pair where the first site belongs to the created bond*)
        (*let store_potential_tuple_pair_created_bonds =
          get_potential_tuple_pair_created_bonds static
        in
        let error =
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
            (fun (x, y) error ->
               let error,
                   (agent, site, site', state, state',
                    agent1, site1, site1', state1, state1') =
                 Site_accross_bonds_domain_type.convert_tuple_full
                   parameter error kappa_handler (x, y)
               in
               let () =
                 Loggers.fprintf log
                   "The potential tuple pair when there is an action binding between the site %s of %s and the site %s of %s is: %s(%s:%s, %s:%s); %s(%s:%s, %s:%s)\n"
                   site agent site1 agent1
                   agent site state site' state'
                   agent1 site1 state1 site1' state1'
               in
               error
            ) store_potential_tuple_pair_created_bonds error
        in
        let () = Loggers.print_newline log in
        (*--------------------------------------------------------*)
        (*1.a uple pair where the first site belongs to a bond on rhs§*)
        let store_potential_tuple_pair_bonds_rhs =
          get_potential_tuple_pair_bonds_rhs static
        in
        let error =
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Set.fold
            (fun (x, y) error ->
               let error,
                   (agent, site, site', state, state',
                    agent1, site1, site1', state1, state1') =
                 Site_accross_bonds_domain_type.convert_tuple_full
                   parameter error kappa_handler (x, y)
               in
               let () =
                 Loggers.fprintf log
                   "The potential tuple pair when there is a bond on the rhs between the site %s of %s and the site %s of %s is: %s(%s:%s, %s:%s); %s(%s:%s, %s:%s)\n"
                   site agent site1 agent1
                   agent site state site' state'
                   agent1 site1 state1 site1' state1'
               in
               error
            ) store_potential_tuple_pair_bonds_rhs error
        in
        let () = Loggers.print_newline log in*)
        (*--------------------------------------------------------*)
        (**)
        (*let store_potential_tuple_pair = get_potential_tuple_pair static in
        let error =
          Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Set.fold
            (fun (x, y) error ->
               let error, (agent, site, site', state,
                           agent1, site1, site1', state1) =
                 Site_accross_bonds_domain_type.convert_tuple
                   parameter error kappa_handler (x, y)
               in
               let () =
                 Loggers.fprintf log
                   "The potential tuple pair between the site %s of agent %s and the site %s of agent %s is: %s(%s:%s, %s); %s(%s:%s, %s)\n"
                   site agent site1 agent1
                   agent site state site'
                   agent1 site1 state1 site1'
               in
               error
            ) store_potential_tuple_pair error
          in
          let () = Loggers.print_newline log in*)
        (*--------------------------------------------------------*)
        (*question mark on the rhs*)
        (*let store_question_marks_rhs = get_question_marks_rhs static in
          let error =
          Ckappa_sig.Rule_map_and_set.Map.fold
            (fun rule_id set error ->
               Site_accross_bonds_domain_type.AgentsSitesState_map_and_set.Set.fold
                 (fun x error ->
                    let (_, agent_type, site_type', site_type_m, state_m) = x in
                    let error, (agent, sitem, statem) =
                      Site_accross_bonds_domain_type.convert_single
                        parameter error kappa_handler
                        (agent_type, site_type_m, state_m)
                    in
                    let error, (_, site) =
                      Site_accross_bonds_domain_type.convert_single_without_state
                        parameter error kappa_handler
                        (agent_type, site_type')
                    in
                    let () =
                      Loggers.fprintf log
                        "At rule_id:%i, there is a binding with a question mark of the site %s of %s. The potential tuple pair is: %s(:%i:%s, %s:%s)\n"
                        (Ckappa_sig.int_of_rule_id rule_id)
                        site agent
                        agent (Ckappa_sig.int_of_site_name site_type') site sitem statem
                    in
                    error
                 ) set error
            ) store_question_marks_rhs error
          in
          let () = Loggers.print_newline log in*)
        (*--------------------------------------------------------*)
        (*let store_potential_tuple_pair_modification =
          get_potential_tuple_pair_modification static
          in
          let error =
          Ckappa_sig.Rule_map_and_set.Map.fold
            (fun rule_id set error ->
               Site_accross_bonds_domain_type.PAgentsSitesStates_map_and_set.Set.fold
                 (fun (x, y) error ->
                    let proj (_, b, c, d, e,f) = b, c, d, e, f in
                    let error, (agent, site, site', state, state') =
                      Site_accross_bonds_domain_type.convert_double
                        parameter error kappa_handler
                        (proj x)
                    in
                    let () =
                      Loggers.fprintf log
                        "rule_id:%i There is a modification of the site %s of agent %s. The potential tuple pair is: %s(%s:%s, %s:%s) \n"
                        (Ckappa_sig.int_of_rule_id rule_id)
                        site' agent
                        agent site state site' state'
                   (*agent1 site1 state1 site1' state1'*)
                    in
                    error
                 ) set error
            ) store_potential_tuple_pair_modification error
          in
          let () = Loggers.print_newline log in*)
        (*--------------------------------------------------------*)
        (*print result*)
        let () =
          Loggers.fprintf log "* Dynamic information\n"
        in
        let store_value = get_value dynamic in
        let error, handler =
          Site_accross_bonds_domain_type.PairAgentSitesState_map_and_set.Map.fold
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
