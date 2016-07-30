(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification: Time-stamp: <Aug 06 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Site_accross_bonds_domain") message exn
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
      store_value :
        Ckappa_sig.Views_bdu.mvbdu
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Map.t;
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

  let get_tuple_pair static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_tuple_pair

  let set_tuple_pair r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_tuple_pair = r
      } static

(*TODO*)

  let get_tuple_pair_rule static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_tuple_pair_rule

  let set_tuple_pair_rule r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_tuple_pair_rule = r
      } static

  let get_agent_id_list_from_tuple static =
      (get_basic_static_information
         static).Site_accross_bonds_domain_static.store_agent_id_list_from_tuple

  let set_agent_id_list_from_tuple r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_agent_id_list_from_tuple = r
      } static

  let get_pair_tuple_set static =
    (get_basic_static_information
       static).Site_accross_bonds_domain_static.store_pair_tuple_set

  let set_pair_tuple_set r static =
    set_basic_static_information
      {
        (get_basic_static_information static) with
        Site_accross_bonds_domain_static.store_pair_tuple_set = r
      } static

  (*rule that can created a bond *)

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
    (*bonds set without rule_id*)
    let store_bonds_rhs_set = get_bonds_rhs_set static in
    let error, store_bonds_rhs_set =
      Site_accross_bonds_domain_static.collect_bonds_rhs_set
        parameter error
        rule
        store_bonds_rhs_set
    in
    let static = set_bonds_rhs_set store_bonds_rhs_set static in
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
        parameter error
        kappa_handler
        rule_id
        store_test
        store_tuple_pair
    in
    let static = set_tuple_pair store_tuple_pair static in
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
    let store_tuple_pair = get_tuple_pair static in
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
    (*implicit static information*)
    (*  let kappa_handler = get_kappa_handler static in*)
    let store_tuple_pair = get_tuple_pair static in
    let store_question_marks_rhs = get_question_marks_rhs static in
    let store_implicit_static =
      Site_accross_bonds_domain_static.collect_implicit_static
        parameter error
        store_tuple_pair
        store_question_marks_rhs
    in
    let static = set_implicit_rule store_implicit_static static in
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
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Map.empty;

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

  (*
  let print_result prefix parameter error kappa_handler handler
      (x,y) mvbdu =
    let (agent_type, _, _, _, _) = x in
    let (agent_type1, _, site_type1, stateb1, _) = y in
    let error,
        (agent, site, site', _, _, (*A,x,y*)
         agent1, site1, site1', _, _) =
      Site_accross_bonds_domain_type.convert_tuple
        parameter error kappa_handler
        (x,y)
    in
    let error, handler, pair_list =
      Ckappa_sig.Views_bdu.extensional_of_mvbdu
        parameter handler error mvbdu
    in
    List.fold_left (fun (error, handler) l ->
        let rec aux acc =
          match acc with
          | []
          | (_, _) :: [] -> error, handler
          | (sitex, statex) :: (sitey, statey) :: tl ->
            (*do not print free and binding state, TODO: check the reserved *)
            if sitex = site_type1 && statex = stateb1 ||
               sitex = site_type1 &&
               (Ckappa_sig.int_of_state_index statex) = 0
            then error, handler
            else
            (*the first one for agent, the second element for agent1*)
            let error, (_, _, statex) =
              Site_accross_bonds_domain_type.convert_single
                parameter error kappa_handler
                (agent_type, sitex, statex)
            in
            (**)
            let error, (_, _, statey) =
              Site_accross_bonds_domain_type.convert_single
                parameter error kappa_handler
                (agent_type1, sitey, statey)
            in
            (*note the statex/statey is always the state of t*)
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "\n%sWhenever the site %s of %s and the site %s of %s are bound together, then the site %s of %s and %s of %s can have the following respective states: %s, %s\n"
                prefix site agent site1 agent1
                site' agent site1' agent1
                statex statey
            in
            aux tl
        in aux l
      ) (error, handler) pair_list
    *)

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
        warn parameter error (Some "line 512") Exit []
    in
    let dynamic = set_global_dynamic_information global_dynamic dynamic in
    error, dynamic, precondition, state_list

  let context line rule_id agent_id site_type =
    "line: "^(string_of_int line)^
    " rule "^(Ckappa_sig.string_of_rule_id rule_id)^
    " agent_id "^(Ckappa_sig.string_of_agent_id agent_id)^
    " site_type "^(Ckappa_sig.string_of_site_name site_type)



  let apply_rule static dynamic error rule_id precondition =
    let parameter  = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    (*-----------------------------------------------------------*)
    let error, rule = get_rule parameter error static rule_id in
    match
      rule
    with
    | None ->
      let error, () = warn parameter error (Some "line 713") Exit () in
      error, dynamic, (precondition, [])
    | Some rule ->
    let parameter =
      Remanent_parameters.update_prefix parameter "                " in
    (*-----------------------------------------------------------*)
    (*rule rhs has bound*)
    let store_bonds_rhs = get_bonds_rhs static in
    let error, bonds_rhs_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_bonds_rhs
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*-----------------------------------------------------------*)
    let store_bonds_rhs_set = get_bonds_rhs_set static in
    (*-----------------------------------------------------------*)
    (*modifying the site knowing the bonds*)
    (*let store_modif_bond_map = get_modified_internal_state_and_bond static in
    let error, store_modif_bond_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_modif_bond_map
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.empty
      | error, Some s -> error, s
    in*)
    (*-----------------------------------------------------------*)
    (*created bonds*)
    let store_created_bond_with_potential_pair =
      get_created_bond_with_potential_pair static
    in
    let error, created_bonds_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id
              store_created_bond_with_potential_pair
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*-----------------------------------------------------------*)
    (*question marks on the rhs*)
    let store_implicit_static = get_implicit_rule static in
    let error, question_marks_set =
      match Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
              parameter error rule_id store_implicit_static
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store_tuple_pair = get_tuple_pair static in
    let store_modif = get_modified_map static in
    let error, modified_set =
      match
        Ckappa_sig.Rule_map_and_set.Map.find_option_without_logs
          parameter error rule_id store_modif
      with
      | error, None ->
        error,
        Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*-----------------------------------------------------------*)
    let error, dynamic, precondition =
      (*1. the second site in a rule can be modified, then check its first site whether or not it can be bound.*)
      (*fold over a set of modified sites*)
      Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.fold
        (fun m (error, dynamic, precondition) ->
           let (agent_id_m, agent_type_m, site_type_m, state_m) = m in
           (*fold over a tuple pair*)
           Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
             (fun (x, y) (error, dynamic, precondition) ->
                (* JF: the list of the good tuple, should be stored in a map in static *)
                (* JF: Iterating over the full set of tuples, and filtering afterward is costly *)
                let (agent_id, agent_type, site_type, site_type', _) = x in
                let (agent_id1, agent_type1, site_type1, site_type1', _) = y in
                (*get the list of state in the precondition of the first site*)
                let error', dynamic, precondition, state_list =
                  get_state_of_site_in_precondition
                    parameter error
                    dynamic
                    rule
                    agent_id
                    site_type
                    precondition
                in
                let error = Exception.check warn parameter error error' (Some (context 766 rule_id agent_id site_type)) Exit in
                let error', dynamic, precondition, state_list' =
                  get_state_of_site_in_precondition
                    parameter error
                    dynamic
                    rule
                    agent_id1
                    site_type1
                    precondition
                in
                let error = Exception.check warn parameter error error' (Some (context 775 rule_id agent_id1 site_type1)) Exit in
                (*----------------------------------------------------*)
                let error, potential_list =
                  List.fold_left (fun (error, current_list) pre_state ->
                      List.fold_left (fun (error, current_list) pre_state' ->
                          (*check if the second site belong to modif site*)

                          (* JF: this test must be done much earlier *)
                          (* JF: the predicate depends neither on pre_state nor on pre_state' *)

                        if agent_type = agent_type_m && site_type' = site_type_m
                             ||
                             agent_type1 = agent_type_m && site_type1' = site_type_m
                          then
                            let potential_list =
                              ((agent_id, agent_type, site_type, site_type', pre_state, state_m),
                               (agent_id1, agent_type1, site_type1, site_type1', pre_state', state_m)) :: current_list
                            in
                            error, potential_list
                          else error, current_list
                        ) (error, current_list) state_list'
                    ) (error, []) state_list
                in
                (*----------------------------------------------------*)
                let error, dynamic, precondition =
                  List.fold_left (fun (error, dynamic, precondition) (t, u) ->
                      let handler = get_mvbdu_handler dynamic in
                      let store_result = get_value dynamic in
                      let (agent_id, agent_type, site_type, _, state, state') = t in
                      let (agent_id1, agent_type1, site_type1, _, state1, state1') = u in
                      let pair_bond = (agent_type, site_type, state),
                                      (agent_type1, site_type1, state1)
                      in
                      (*----------------------------------------------------*)
                      (*check if the first site is bound*)
                      if
                        Site_accross_bonds_domain_type.PairAgentSiteState_map_and_set.Set.mem
                          pair_bond
                          store_bonds_rhs_set
                      then
                        let pair_list =
                          [(Ckappa_sig.fst_site, state');
                           (Ckappa_sig.snd_site, state1')]
                        in
                        let error, handler, mvbdu =
                          Ckappa_sig.Views_bdu.mvbdu_of_association_list
                            parameter handler error pair_list
                        in
                        let pair =
                          Site_accross_bonds_domain_type.project2 (t, u)
                        in
                        (*----------------------------------------------------*)
                        (*PRINT*)
                        (*let error, handler =
                          print_result prefix parameter error
                            kappa_handler handler
                            pair
                            mvbdu
                        in*)
                        (*----------------------------------------------------*)
                        let error, handler, store_result =
                          Site_accross_bonds_domain_type.add_link
                            parameter error bdu_false handler
                            kappa_handler pair mvbdu store_result
                        in
                        let dynamic = set_value store_result dynamic in
                        let dynamic = set_mvbdu_handler handler dynamic in
                        error, dynamic, precondition
                      else
                        error, dynamic, precondition
                    ) (error, dynamic, precondition) potential_list
                in
                error, dynamic, precondition
             ) store_tuple_pair (error, dynamic, precondition)
        ) modified_set (error, dynamic, precondition)
    in
    (*-----------------------------------------------------------*)
    (*1.a fold over a pair where the first site is bound and the second site is modified*)
    (*
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.fold
        (fun (x, y) (error, dynamic, precondition) ->
        let store_result = get_value dynamic in
        let handler = get_mvbdu_handler dynamic in
           let (agent_id, _, site_type_b, _, _, _) = x in
           let (agent_id1, _, site_type_b1, _, _, _) = y in
           let pair = Site_accross_bonds_domain_type.project2 (x, y) in
           let ((agent_type, _, site_type', _, state'),
                (agent_type1, _, site_type1', _, state1')) = pair
           in
           (*----------------------------------------------------------*)
           (*the first site 1 is indicate for the first agent, and the
             number 2 for the second agent*)
           let pair_list = [(Ckappa_sig.site_name_of_int 1, state');
                            (Ckappa_sig.site_name_of_int 2, state1')]
           in
           (*--------------------------------------------------------*)
           let error, handler, mvbdu =
             Ckappa_sig.Views_bdu.mvbdu_of_association_list
               parameter handler error pair_list
           in
           (*-----------------------------------------------------------*)
           (*PRINT*)
           let error, handler =
             print_result prefix parameter error
               kappa_handler handler
               pair
               mvbdu
           in
           (*-----------------------------------------------------------*)
           let error, handler, store_result =
           Site_accross_bonds_domain_type.add_link
             parameter error bdu_false handler kappa_handler pair mvbdu store_result
           in
           let dynamic = set_value store_result dynamic in
           let dynamic = set_mvbdu_handler handler dynamic in
           (*------------------------------------------------------*)
           (*in case we do not know the first site is bound or not,
             get the information of its precondition, if it belongs
             to a bound rhs, then return the internal state of
             the second site, if not return the result*)
           let error, dynamic, precondition, state_list =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               agent_id (*A*)
               site_type_b (*x*)
               precondition
           in
           let error, dynamic, precondition, state_list' =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               agent_id1 (*B*)
               site_type_b1 (*x*)
               precondition
           in
           (*------------------------------------------------------*)
           (*check inside this state list whether or not it belongs to
           rhs bound*)
           let error, potential_list =
             List.fold_left (fun (error, current_list) pre_state ->
                 List.fold_left (fun (error, current_list) pre_state' ->
                     let potential_list =
                       ((agent_id, agent_type, site_type_b, site_type',
                         pre_state, state'),
                        (agent_id1, agent_type1, site_type_b1, site_type1',
                         pre_state', state1')) :: current_list
                     in error, potential_list
                   ) (error, current_list) state_list'
               ) (error, []) state_list
           in
           (*------------------------------------------------------*)
           (*check the the first site belongs to bound rhs*)
           let error, dynamic, precondition =
             List.fold_left (fun (error, dynamic, precondition) (x', y') ->
             let store_result = get_value dynamic in
             let handler = get_mvbdu_handler dynamic in
                 let (agent_id, agent_type, site_type, _, state, _) = x' in
                 let (agent_id1, agent_type1, site_type1, _, state1, _) = y' in
                 let pair_bond =
                   (agent_id, agent_type, site_type, state),
                   (agent_id1, agent_type1, site_type1, state1)
                 in
                 if
                   Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
                     pair_bond
                     bonds_rhs_set
                 then
                   (*store_the value*)
                   let
                     ((_, _, _, _, state'),
                      (_, _, _, _, state1')) = pair
                   in
                   let pair_list =
                     [(Ckappa_sig.site_name_of_int 1, state');
                      (Ckappa_sig.site_name_of_int 2, state1')]
                   in
                   let handler = get_mvbdu_handler dynamic in
                   let error, handler, mvbdu =
                     Ckappa_sig.Views_bdu.mvbdu_of_association_list
                       parameter handler error pair_list
                   in
                   (*-----------------------------------------------------------*)
                   (*PRINT*)
                   let error, handler =
                     print_result prefix parameter error
                       kappa_handler handler
                       pair
                       mvbdu
                   in
                   (*-----------------------------------------------------------*)
                   let error, handler, store_result =
                     Site_accross_bonds_domain_type.add_link
                       parameter error bdu_false handler
                       kappa_handler pair mvbdu store_result
                   in
                   let dynamic = set_value store_result dynamic in
                   let dynamic = set_mvbdu_handler handler dynamic in
                   error, dynamic, precondition
                 else
                   error, dynamic, precondition
               ) (error, dynamic, precondition) potential_list
             in
           error, dynamic, precondition
        ) store_modif_bond_set (error, dynamic, precondition)
    in*)
    (*-----------------------------------------------------------*)
    (*2. created a bound *)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
        (fun (x, y) (error, dynamic, precondition) ->
           (*site_type, site_type1 are site that created a bound*)
           let (agent_id, agent_type, site_type, site_type', state) = x in
           let (agent_id1, agent_type1, site_type1, site_type1', state1) = y in
           (*get the precondition of site_type', site_type1', the internal state list*)
           let error', dynamic, precondition, state_list =
             get_state_of_site_in_precondition
               parameter
               error
               dynamic
               rule
               agent_id (*A*)
               site_type' (*y*)
               precondition
           in
           let error = Exception.check warn parameter error error' (Some (context 995 rule_id agent_id site_type')) Exit in
           let error', dynamic, precondition, state_list' =
             get_state_of_site_in_precondition
               parameter
               error
               dynamic
               rule
               agent_id1 (*B*)
               site_type1' (*y*)
               precondition
           in
           let error = Exception.check warn parameter error error' (Some (context 1006 rule_id agent_id1 site_type1')) Exit in
           (*------------------------------------------------------*)
           (*return the pre_state*)
           let error, potential_list =
             List.fold_left (fun (error, current_list) pre_state ->
                 List.fold_left (fun (error, current_list) pre_state' ->
                     let potential_list =
                       ((agent_id, agent_type, site_type, site_type', state, pre_state),
                        (agent_id1, agent_type1, site_type1, site_type1', state1, pre_state')) :: current_list
                     in
                     error, potential_list
                   ) (error, current_list) state_list'
               ) (error, []) state_list
           in
           (*-----------------------------------------------------------*)
           (*build the pair_list*)
           let error, dynamic, precondition =
             List.fold_left (fun (error, dynamic, precondition) (x', y') ->
                 let store_result = get_value dynamic in
                 let handler = get_mvbdu_handler dynamic in
                 let (_agent_id, agent_type, site_type, site_type', state,
                      state') = x'
                 in
                 let (_agent_id1, agent_type1, site_type1, site_type1',
                      state1, state1') = y'
                 in
                 let pair =
                   ((agent_type, site_type, site_type', state, state'),
                    (agent_type1, site_type1, site_type1', state1,
                     state1'))
                 in
                 let pair_list =
                   [
                     Ckappa_sig.fst_site, state';
                     Ckappa_sig.snd_site, state1'
                   ]
                 in
                 let error, handler, mvbdu =
                   Ckappa_sig.Views_bdu.mvbdu_of_association_list
                     parameter handler error pair_list
                 in
                 (*-----------------------------------------------------------*)
                 (*PRINT*)
                 (*let error, handler =
                   print_result prefix parameter error
                     kappa_handler handler
                     pair
                     mvbdu
                 in*)
                 (*-----------------------------------------------------------*)
                 let error, handler, store_result =
                   Site_accross_bonds_domain_type.add_link
                     parameter error bdu_false handler
                     kappa_handler pair mvbdu store_result
                 in
                 let dynamic = set_value store_result dynamic in
                 let dynamic = set_mvbdu_handler handler dynamic in
                 error, dynamic, precondition
               ) (error, dynamic, precondition) potential_list
             in
             error, dynamic, precondition
        ) created_bonds_set (error, dynamic, precondition)
    in
    (*-----------------------------------------------------------*)
    (*3. the first site does not know if it can be bound or not, the second site can be modified *)
    let error, dynamic, precondition =
      Site_accross_bonds_domain_type.PairAgentsSitesState_map_and_set.Set.fold
        (fun (x, y) (error, dynamic, precondition) ->
           let (agent_id, agent_type, site_type, site_type', state') = x in
           let (agent_id1, agent_type1, site_type1, site_type1', state1') =
             y
           in
           (*get a list of state of the first site in the precondition*)
           let error', dynamic, precondition, state_list =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               rule
               agent_id (*B*)
               site_type (*x*)
               precondition
           in
           let error = Exception.check warn parameter error error' (Some (context 1085 rule_id agent_id site_type)) Exit in
           let error, dynamic, precondition, state_list' =
             get_state_of_site_in_precondition
               parameter error
               dynamic
               rule
               agent_id1 (*A*)
               site_type1 (*x*)
               precondition
           in
           let error = Exception.check warn parameter error error' (Some (context 1094 rule_id agent_id1 site_type1)) Exit in
           let error, potential_list =
             List.fold_left (fun (error, current_list) pre_state ->
                 List.fold_left (fun (error, current_list) pre_state' ->
                     let potential_list =
                       ((agent_id, agent_type, site_type, site_type', pre_state, state'),
                        (agent_id1, agent_type1, site_type1, site_type1', pre_state', state1')) :: current_list
                     in
                     error, potential_list
                   ) (error, current_list) state_list'
               ) (error, []) state_list
           in
           let error, dynamic, precondition =
             List.fold_left (fun (error, dynamic, precondition) (x', y') ->
             let store_result = get_value dynamic in
             let handler = get_mvbdu_handler dynamic in
                 (*check whether or not the first site belongs to
                   bonds rhs*)
                 let (agent_id, agent_type, site_type, site_type', state,
                      state') = x' in
                 let (agent_id1, agent_type1, site_type1, site_type1',
                      state1, state1') = y' in
                 let pair_bond =
                   (agent_id, agent_type, site_type, state),
                   (agent_id1, agent_type1, site_type1, state1)
                 in
                 (*-----------------------------------------------------*)
                 if
                   Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.mem
                     pair_bond
                     bonds_rhs_set
                 then
                   (*store_value*)
                   let pair =
                     (agent_type, site_type, site_type', state, state'),
                     (agent_type1, site_type1, site_type1', state1, state1')
                   in
                   let pair_list =
                     [
                       Ckappa_sig.fst_site, state';
                       Ckappa_sig.snd_site, state1'
                     ]
                   in
                   let error, handler, mvbdu =
                     Ckappa_sig.Views_bdu.mvbdu_of_association_list
                       parameter handler error pair_list
                   in
                   (*-----------------------------------------------------------*)
                   (*PRINT*)
                   (*let error, handler =
                     print_result prefix parameter error
                       kappa_handler handler
                       pair
                       mvbdu
                   in*)
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
               ) (error, dynamic, precondition) potential_list
           in
           error, dynamic, precondition
        ) question_marks_set (error, dynamic, precondition)
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
            "------------------------------------------------------------\n"
        in
        (*--------------------------------------------------------*)
        (*print result*)
        let store_value = get_value dynamic in
        (*let error, handler =
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Map.fold
            (fun (x, y) mvbdu (error, handler) ->
               Site_accross_bonds_domain_type.print_site_accross_domain
                 ~verbose:true
                 ~sparse:true
                 ~final_resul:true
                 ~dump_any:true parameter error kappa_handler handler (x, y) mvbdu
            ) store_value (error, handler)
        in*)
        let error, handler =
          Site_accross_bonds_domain_type.PairAgentSitesStates_map_and_set.Map.fold
            (fun (x, y) mvbdu (error, handler) ->
               let (agent_type, _, site_type, _stateb, _) = x in
               let (agent_type1, _, site_type1, _stateb1, _) = y in
               let error,
                   (agent, site, site', _state, _state', (*A,x,y*)
                    agent1, site1, site1', _state1, _state1') =
                 Site_accross_bonds_domain_type.convert_tuple
                   parameter error kappa_handler
                   (x,y)
               in
               let error, handler, pair_list =
                 Ckappa_sig.Views_bdu.extensional_of_mvbdu
                   parameter handler error mvbdu
               in
               if pair_list = [] && compare site site' > 0
               then error, handler
               else
               let error =
                 List.fold_left (fun error l ->
                     (* JF: l is an association list
                        fst_site -> state of site_type
                        snd_site -> state of site_type1 *)
                     match
                       l
                     with
                     | [siteone, statex ; sitetwo, statey]
                       when siteone == Ckappa_sig.fst_site
                         && sitetwo == Ckappa_sig.snd_site
                       ->
                          (*do not print free and binding state*)
                         (*if sitex = site_type1 && statex = stateb1 ||
                            sitex = site_type1 &&
                            (Ckappa_sig.int_of_state_index statex) = 0
                         then error
                           else*)
                         (*the first one for agent, the second element for agent1*)
                         let error, (_agentx, _sitex, statex) =
                           Site_accross_bonds_domain_type.convert_single
                             parameter error kappa_handler
                             (agent_type, site_type, statex)
                         in
                         (**)
                         let error, (_agenty, _sitey, statey) =
                           Site_accross_bonds_domain_type.convert_single
                             parameter error kappa_handler
                             (agent_type1, site_type1, statey)
                         in
                         let () =
                           Loggers.fprintf (Remanent_parameters.get_logger parameter)
                             "Whenever the site %s of %s and the site %s of %s are bound together, then the site %s of %s and %s of %s can have the following respective states: %s, %s\n"
                             site agent site1 agent1
                             site' agent site1' agent1
                             statex statey
                         in
                         error
                     | [] | _::_ ->
                       let error, () =
                         warn parameter error (Some "1282") Exit ()
                       in
                       error
                   ) error pair_list
               in
               error, handler
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
