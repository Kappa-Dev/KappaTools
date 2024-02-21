(**
   * analyzer_sig.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 30th of January
   * Last modification: Time-stamp: <Dec 22 2018>
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(* Before properly achieving separation of concepts. We introduce one
   monolithic domain that collect everything (as in the previous analyzer).*)

let domain_name = "View domain"
let local_trace = false

module Domain = struct
  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
    domain_static_information: Bdu_static_views.bdu_analysis_static;
    domain_static_information_pattern:
      Bdu_static_views.bdu_analysis_static_pattern;
    domain_static_information_covering_class:
      Covering_classes_type.predicate_covering_classes;
  }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)

  module AgentCV_map_and_set = Covering_classes_type.AgentCV_map_and_set

  type local_dynamic_information = {
    fixpoint_result: Ckappa_sig.Views_bdu.mvbdu AgentCV_map_and_set.Map.t;
    domain_dynamic_information: Bdu_dynamic_views.bdu_analysis_dynamic;
    subviews: unit option;
    ranges:
      Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t
      option;
    separating_edges: (string * string) list Mods.IntMap.t option;
    transition_system_length: int list option;
  }

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  (** bdu analysis static in static information*)
  let set_domain_static domain static =
    { static with domain_static_information = domain }

  let _get_domain_static static = static.domain_static_information

  let set_domain_static_pattern domain static =
    { static with domain_static_information_pattern = domain }

  let get_domain_static_pattern static =
    static.domain_static_information_pattern

  let _get_store_proj_bdu_test_restriction_pattern static =
    (get_domain_static_pattern static)
      .Bdu_static_views.store_proj_bdu_test_restriction_pattern

  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static
  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_test_modif_map static =
    lift Analyzer_headers.get_test_modif_map static

  let get_compil static = lift Analyzer_headers.get_cc_code static
  let get_agent_name static = lift Analyzer_headers.get_agent_name static

  let get_agent_name_from_pattern static =
    lift Analyzer_headers.get_agent_name_from_pattern static

  let get_potential_side_effects static =
    lift Analyzer_headers.get_potential_side_effects static

  let get_predicate_covering_classes static =
    static.domain_static_information_covering_class

  let get_covering_classes static =
    (get_predicate_covering_classes static)
      .Covering_classes_type.store_covering_classes_predicate

  let get_list_of_site_type_in_covering_classes static =
    (get_predicate_covering_classes static)
      .Covering_classes_type.store_list_of_site_type_in_covering_classes

  let get_covering_classes_id static =
    (get_predicate_covering_classes static)
      .Covering_classes_type.store_covering_classes_id

  let get_site_correspondence_array static =
    (get_predicate_covering_classes static)
      .Covering_classes_type.site_correspondence

  let get_remanent_triple static =
    (get_predicate_covering_classes static)
      .Covering_classes_type.store_remanent_triple

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  (** handler *)
  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global =
        Analyzer_headers.set_mvbdu_handler handler
          (get_global_dynamic_information dynamic);
    }

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

  (** local dynamic information*)

  let get_local_dynamic_information dynamic = dynamic.local
  let set_local_dynamic_information local dynamic = { dynamic with local }

  (** fixpoint result local dynamic information*)
  let get_fixpoint_result dynamic =
    (get_local_dynamic_information dynamic).fixpoint_result

  let set_fixpoint_result result dynamic =
    set_local_dynamic_information
      { (get_local_dynamic_information dynamic) with fixpoint_result = result }
      dynamic

  let get_ranges dynamic = (get_local_dynamic_information dynamic).ranges

  let set_ranges ranges dynamic =
    set_local_dynamic_information
      { (get_local_dynamic_information dynamic) with ranges = Some ranges }
      dynamic

  let get_separating_transitions dynamic =
    (get_local_dynamic_information dynamic).separating_edges

  let set_separating_transitions sep_edges dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        separating_edges = Some sep_edges;
      }
      dynamic

  let set_transition_system_length lengths dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        transition_system_length = Some lengths;
      }
      dynamic

  (** bdu analysis dynamic in local dynamic information*)
  let get_domain_dynamic_information dynamic =
    (get_local_dynamic_information dynamic).domain_dynamic_information

  let set_domain_dynamic_information domain dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        domain_dynamic_information = domain;
      }
      dynamic

  let get_store_dual_contact_map dynamic =
    (get_domain_dynamic_information dynamic)
      .Bdu_dynamic_views.store_dual_contact_map

  (****************************************************************)

  (** get type bdu_analysis_static*)
  let get_bdu_analysis_static static error =
    let result = static.domain_static_information in
    error, result

  (**get type bdu_analysis_dynamic*)
  let get_store_proj_bdu_test_restriction static error =
    let error, result_static = get_bdu_analysis_static static error in
    error, result_static.Bdu_static_views.store_proj_bdu_test_restriction

  let get_store_proj_bdu_creation_restriction static error =
    let error, result_static = get_bdu_analysis_static static error in
    ( error,
      result_static.Bdu_static_views.store_proj_bdu_creation_restriction_map )

  let get_store_proj_bdu_potential_restriction static error =
    let error, result_static = get_bdu_analysis_static static error in
    ( error,
      result_static.Bdu_static_views.store_proj_bdu_potential_restriction_map )

  let get_store_modif_list_restriction_map static error =
    let error, result_static = get_bdu_analysis_static static error in
    error, result_static.Bdu_static_views.store_modif_list_restriction_map

  let get_site_to_renamed_site_list static error =
    let error, result_static = get_bdu_analysis_static static error in
    error, result_static.Bdu_static_views.site_to_renamed_site_list
  (*--------------------------------------------------------------------*)

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

  (*----------------------------------------------------------------------*)
  (*Instantiate of functions that store the static and dynamic information
    accordingly from the previous analyzer *)

  (**[get_bdu_false/true] from dynamic*)
  let get_mvbdu_false global_static dynamic error =
    let parameters = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_false =
      Ckappa_sig.Views_bdu.mvbdu_false parameters handler_bdu error
    in
    error, set_mvbdu_handler handler_bdu dynamic, bdu_false

  (** the initial build for mvbdu_true*)
  let get_mvbdu_true global_static dynamic error =
    let parameters = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true parameters handler_bdu error
    in
    error, set_mvbdu_handler handler_bdu dynamic, bdu_true

  (**************************************************************************)
  (** [scan_rule_set static] *)

  let scan_rule_set_static static dynamic error =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compiled = get_compil static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let potential_side_effects = get_potential_side_effects static in
    let log_info = get_log_info dynamic in
    let remanent_triple = get_remanent_triple static in
    let error, (handler_bdu, log_info, result) =
      Bdu_static_views.scan_rule_set parameters log_info handler_bdu error
        kappa_handler compiled potential_side_effects remanent_triple
    in
    let dynamic = set_log_info log_info dynamic in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let static = set_domain_static result static in
    (*-----------------------------------------------------------------------*)
    (*pattern*)
    (*-----------------------------------------------------------------------*)
    let error, result =
      Bdu_static_views.scan_rule_set_pattern parameters error remanent_triple
        compiled
    in
    let static = set_domain_static_pattern result static in
    error, static, dynamic

  let scan_rule_set_dynamic static dynamic error =
    let parameters = get_parameter static in
    let compiled = get_compil static in
    let kappa_handler = get_kappa_handler static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let store_test_modif_map = get_test_modif_map static in
    let covering_classes = get_covering_classes static in
    let covering_classes_id = get_covering_classes_id static in
    let potential_side_effects = get_potential_side_effects static in
    let log_info = get_log_info dynamic in
    let error, (handler_bdu, log_info, store_result) =
      Bdu_dynamic_views.scan_rule_set_dynamic parameters log_info error compiled
        kappa_handler handler_bdu store_test_modif_map covering_classes
        covering_classes_id potential_side_effects
    in
    let dynamic = set_log_info log_info dynamic in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let dynamic = set_domain_dynamic_information store_result dynamic in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let parameters = Analyzer_headers.get_parameter static in
    let log_info = Analyzer_headers.get_log_info dynamic in
    let error, log_info =
      StoryProfiling.StoryStats.add_event parameters error
        (StoryProfiling.Domain_initialization domain_name) None log_info
    in
    let compil = Analyzer_headers.get_cc_code static in
    let handler_kappa = Analyzer_headers.get_kappa_handler static in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let error, init_bdu_analysis_static =
      Bdu_static_views.init_bdu_analysis_static parameters error
    in
    let init_bdu_analysis_static_pattern =
      Bdu_static_views.init_bdu_analysis_static_pattern
    in
    let error, init_covering_class =
      Covering_classes_main.scan_predicate_covering_classes parameters error
        handler_kappa compil
    in
    let init_global_static =
      {
        global_static_information = static;
        domain_static_information = init_bdu_analysis_static;
        domain_static_information_pattern = init_bdu_analysis_static_pattern;
        domain_static_information_covering_class = init_covering_class;
      }
    in
    let init_fixpoint = AgentCV_map_and_set.Map.empty in
    let init_bdu_analysis_dynamic =
      Bdu_dynamic_views.init_bdu_analysis_dynamic
    in
    let init_global_dynamic =
      {
        global = dynamic;
        local =
          {
            fixpoint_result = init_fixpoint;
            domain_dynamic_information = init_bdu_analysis_dynamic;
            subviews = None;
            ranges = None;
            separating_edges = None;
            transition_system_length = None;
          };
      }
    in
    let error, init_static, init_dynamic =
      scan_rule_set_static init_global_static init_global_dynamic error
    in
    let error, static, dynamic =
      scan_rule_set_dynamic init_static init_dynamic error
    in
    let log_info = get_log_info dynamic in
    let error, log_info =
      StoryProfiling.StoryStats.close_event parameters error
        (StoryProfiling.Domain_initialization domain_name) None log_info
    in
    let dynamic = set_log_info log_info dynamic in
    error, static, dynamic, []

  let add_wake_up_common parameters error rule_id (agent_type, cv_id)
      store_list_of_site_type_in_covering_classes wake_up =
    let error, list_of_site_type =
      match
        Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
          parameters error (agent_type, cv_id)
          store_list_of_site_type_in_covering_classes
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, wake_up =
      List.fold_left
        (fun (error, wake_up) site_type ->
          Common_static.add_dependency_site_rule parameters error agent_type
            site_type rule_id wake_up)
        (error, wake_up) list_of_site_type
    in
    error, wake_up

  let complete_wake_up_relation static error wake_up =
    let parameters = get_parameter static in
    let store_list_of_site_type_in_covering_classes =
      get_list_of_site_type_in_covering_classes static
    in
    let error, store_modif_list_restriction_map =
      get_store_modif_list_restriction_map static error
    in
    let error, wake_up =
      Covering_classes_type.AgentsRuleCV_map_and_set.Map.fold
        (fun (_, agent_type, rule_id, cv_id) _ (error, wake_up) ->
          let error, wake_up =
            add_wake_up_common parameters error rule_id (agent_type, cv_id)
              store_list_of_site_type_in_covering_classes wake_up
          in
          error, wake_up)
        store_modif_list_restriction_map (error, wake_up)
    in
    let error, store_proj_bdu_potential_restriction_map =
      get_store_proj_bdu_potential_restriction static error
    in
    let error, wake_up =
      Ckappa_sig.Rule_setmap.Map.fold
        (fun rule_id map (error, wake_up) ->
          Covering_classes_type.AgentSiteCV_setmap.Map.fold
            (fun (agent_type, _, cv_id) _ (error, wake_up) ->
              let error, wake_up =
                add_wake_up_common parameters error rule_id (agent_type, cv_id)
                  store_list_of_site_type_in_covering_classes wake_up
              in
              error, wake_up)
            map (error, wake_up))
        store_proj_bdu_potential_restriction_map (error, wake_up)
    in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static error
    in
    let error, wake_up =
      Ckappa_sig.Rule_setmap.Map.fold
        (fun rule_id map (error, wake_up) ->
          Covering_classes_type.AgentsCV_setmap.Map.fold
            (fun (_, agent_type, cv_id) _ (error, wake_up) ->
              let error, wake_up =
                add_wake_up_common parameters error rule_id (agent_type, cv_id)
                  store_list_of_site_type_in_covering_classes wake_up
              in
              error, wake_up)
            map (error, wake_up))
        store_proj_bdu_test_restriction (error, wake_up)
    in
    error, wake_up

  (**************************************************************************)
  (**get type bdu_analysis_dynamic*)

  let get_store_covering_classes_modification_update_full dynamic error =
    let result = get_domain_dynamic_information dynamic in
    error, result.Bdu_dynamic_views.store_update

  (**************************************************************************)

  let travel_in_site_correspondence parameters error cv_id site_correspondence =
    let error, site_correspondence =
      let rec aux list =
        match list with
        | [] -> Exception.warn parameters error __POS__ Exit []
        | (h, list, _) :: _ when h = cv_id -> error, list
        | _ :: tail -> aux tail
      in
      aux site_correspondence
    in
    error, site_correspondence

  let get_site_correspondence parameters error agent_type site_correspondence =
    let error, site_correspondence =
      match
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
          parameters error agent_type site_correspondence
      with
      | error, None -> Exception.warn parameters error __POS__ Exit []
      | error, Some a -> error, a
    in
    error, site_correspondence

  let get_list_of_sites_correspondence parameters error agent_type cv_id
      site_correspondence =
    let error, site_correspondence =
      get_site_correspondence parameters error agent_type site_correspondence
    in
    let error, site_correspondence =
      travel_in_site_correspondence parameters error cv_id site_correspondence
    in
    error, site_correspondence

  let get_list_of_sites_correspondence_map parameters error agent_type cv_id
      site_correspondence =
    let error, cv_id_array_opt =
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
        parameters error agent_type site_correspondence
    in
    match cv_id_array_opt with
    | None ->
      let error, map1 =
        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      in
      let error, map2 =
        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      in
      Exception.warn parameters error __POS__ Exit (map1, map2)
    | Some cv_id_array ->
      let error, pair_opt =
        Covering_classes_type.Cv_id_nearly_Inf_Int_storage_Imperatif.get
          parameters error cv_id cv_id_array
      in
      (match pair_opt with
      | None ->
        let error, map1 =
          Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create
            parameters error 0
        in
        let error, map2 =
          Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create
            parameters error 0
        in
        Exception.warn parameters error __POS__ Exit (map1, map2)
      | Some (map1, map2) -> error, (map1, map2))

  let dump_cv_label static error bool (agent_type, cv_id) =
    (*TODO: put title*)
    let parameters = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let site_correspondence = get_remanent_triple static in
    (*get_store_remanent_triple static error*)
    if local_trace || Remanent_parameters.get_trace parameters || bool then (
      let log = Remanent_parameters.get_logger parameters in
      let parameter_cv =
        Remanent_parameters.update_prefix parameters "Updating the views for"
      in
      let prefix = Remanent_parameters.get_prefix parameter_cv in
      (*---------------------------------------------------------*)
      let error, agent_string =
        try Handler.string_of_agent parameters error handler_kappa agent_type
        with _ ->
          Exception.warn parameters error __POS__ Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      (*get a list of sites in a covering class *)
      let error, site_correspondence =
        get_list_of_sites_correspondence parameters error agent_type cv_id
          site_correspondence
      in
      let () = Loggers.print_newline log in
      let () = Loggers.fprintf log "\t\t%s %s(" prefix agent_string in
      let error, _ =
        List.fold_left
          (fun (error, bool) site_type ->
            let error, site_string =
              try
                Handler.string_of_site_update_views parameters error
                  handler_kappa agent_type site_type
              with _ ->
                Exception.warn parameters error __POS__ Exit
                  (Ckappa_sig.string_of_site_name site_type)
            in
            let () =
              Loggers.fprintf log "%s%s"
                (if bool then
                   ","
                 else
                   "")
                site_string
            in
            error, true)
          (error, false) site_correspondence
      in
      let () = Loggers.fprintf log ")" in
      let () = Loggers.print_newline log in
      error
    ) else
      error

  (**************************************************************************)

  let discover_a_modify_sites parameters error covering_classes_modified_map
      store_list_of_site_type_in_covering_classes modified_sites =
    (*in a covering classes of modified site, return the list of list*)
    Covering_classes_type.AgentCV_map_and_set.Map.fold
      (fun (agent_type, cv_id) _rule_id_set (error, modified_sites) ->
        let error, list_of_site_type =
          match
            Covering_classes_type.AgentCV_map_and_set.Map
            .find_option_without_logs parameters error (agent_type, cv_id)
              store_list_of_site_type_in_covering_classes
          with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        List.fold_left
          (fun (error, modified_sites) site ->
            Communication.add_site parameters error agent_type site
              modified_sites)
          (error, modified_sites) list_of_site_type)
      covering_classes_modified_map (error, modified_sites)

  let updates_list2event_list ?title:(_title = "") static dynamic error
      event_list =
    let parameters = get_parameter static in
    (*a covering classses that contains modified sites*)
    let error, store_covering_classes_modification_update_full =
      get_store_covering_classes_modification_update_full dynamic error
    in
    (*a list of site_type in a covering classes*)
    let store_list_of_site_type_in_covering_classes =
      get_list_of_site_type_in_covering_classes static
    in
    let error, modified_sites =
      Communication.init_sites_working_list parameters error
    in
    let error, modified_sites =
      discover_a_modify_sites parameters error
        store_covering_classes_modification_update_full
        store_list_of_site_type_in_covering_classes modified_sites
    in
    let error, event_list =
      Communication.fold_sites parameters error
        (fun _ error s _ event_list ->
          error, Communication.Modified_sites s :: event_list)
        modified_sites event_list
    in
    error, event_list

  (***************************************************************)

  let dump_view_diff static dynamic error (agent_type, cv_id) bdu_old bdu_union
      =
    let parameters = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let site_correspondence = get_site_correspondence_array static in
    if
      local_trace
      || Remanent_parameters.get_dump_reachability_analysis_diff parameters
      || Remanent_parameters.get_trace parameters
    then (
      let prefix = Remanent_parameters.get_prefix parameters in
      let handler = get_mvbdu_handler dynamic in
      let error, handler, bdu_diff =
        Ckappa_sig.Views_bdu.mvbdu_xor parameters handler error bdu_old
          bdu_union
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      (*-----------------------------------------------------------------*)
      let error, agent_string =
        try Handler.string_of_agent parameters error handler_kappa agent_type
        with _ ->
          Exception.warn parameters error __POS__ Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      (*------------------------------------------------------------------*)
      (*list of sites in a covering class*)
      let error, (_, map2) =
        get_list_of_sites_correspondence_map parameters error agent_type cv_id
          site_correspondence
      in
      (*-------------------------------------------------------------------*)
      let log = Remanent_parameters.get_logger parameters in
      let error, dynamic =
        if local_trace || Remanent_parameters.get_trace parameters then (
          let () = Loggers.fprintf log "%sINTENSIONAL DESCRIPTION:" prefix in
          let () = Loggers.print_newline log in
          (*print bdu different: this will print in a format of bdu*)
          let () = Ckappa_sig.Views_bdu.print parameters bdu_diff in
          (*print a list of relations: this will print in a format readable*)
          let () = Loggers.fprintf log "%sEXTENSIONAL DESCRIPTION:" prefix in
          let () = Loggers.print_newline log in
          error, dynamic
        ) else
          error, dynamic
      in
      (*this is a function to convert a bdu of diff into a list.
        return a pair: (bdu, and a pair of (site, state) list of list)*)
      let handler = get_mvbdu_handler dynamic in
      let error, handler, list =
        Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error
          bdu_diff
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      (*----------------------------------------------------*)
      (*print function for extentional description*)
      let error =
        List.fold_left
          (fun error l ->
            let error, bool =
              List.fold_left
                (fun (error, bool) (site_type, state) ->
                  let error, site_type =
                    match
                      Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.get
                        parameters error site_type map2
                    with
                    | error, None ->
                      Exception.warn parameters error __POS__ Exit
                        Ckappa_sig.dummy_site_name
                    | error, Some i -> error, i
                  in
                  (*----------------------------------------------------*)
                  let error, site_string =
                    try
                      Handler.string_of_site parameters error handler_kappa
                        ~state agent_type site_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_site_name site_type)
                  in
                  (*-----------------------------------------------------*)
                  let () =
                    if bool then
                      Loggers.fprintf log ","
                    else
                      Loggers.fprintf log "\t\t%s%s(" prefix agent_string
                  in
                  let () = Loggers.fprintf log "%s" site_string in
                  error, true)
                (error, false) l
            in
            (*-----------------------------------------------------------*)
            let () =
              if bool then (
                let () = Loggers.fprintf log ")" in
                Loggers.print_newline log
              )
            in
            error)
          error list
      in
      let () =
        if list = [] then
          ()
        else
          Loggers.print_newline log
      in
      error, dynamic
    ) else
      error, dynamic

  (**************************************************************************)

  let add_link ?title error static dynamic (agent_type, cv_id) bdu event_list =
    let parameters = get_parameter static in
    let log = Remanent_parameters.get_logger parameters in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store = get_fixpoint_result dynamic in
    let error, bdu_old =
      match
        Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
          parameters error (agent_type, cv_id) store
      with
      | error, None -> error, bdu_false
      | error, Some bdu -> error, bdu
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_union =
      Ckappa_sig.Views_bdu.mvbdu_or parameters handler error bdu_old bdu
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let updates_list = [] in
    (*-----------------------------------------------------------*)
    let error, dynamic, _title, is_new_views, _updates_list =
      if Ckappa_sig.Views_bdu.equal bdu_old bdu_union then
        error, dynamic, title, false, updates_list
      else (
        (*print different views*)
        let () =
          match title with
          | None -> ()
          | Some s ->
            if
              local_trace
              || Remanent_parameters.get_dump_reachability_analysis_diff
                   parameters
              || Remanent_parameters.get_trace parameters
            then
              if s = "" then
                Loggers.print_newline log
              else (
                let () = Loggers.fprintf log "\t%s" s in
                let () = Loggers.print_newline log in
                let () = Loggers.print_newline log in
                ()
              )
        in
        let error, dynamic =
          dump_view_diff static dynamic error (agent_type, cv_id) bdu_old
            bdu_union
        in
        let error, store =
          Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite
            parameters error (agent_type, cv_id) bdu_union store
        in
        let dynamic = set_fixpoint_result store dynamic in
        error, dynamic, None, true, (agent_type, cv_id) :: updates_list
      )
    in
    (*-----------------------------------------------------------------------*)
    (*add updates_list into event list*)
    if is_new_views then (
      (*print*)
      let error, event_list =
        updates_list2event_list static dynamic error event_list
      in
      error, dynamic, event_list
    ) else
      error, dynamic, event_list

  (***************************************************************************)
  (*build bdu restriction for initial state *)

  let bdu_build static dynamic error
      (pair_list : (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list) =
    let parameters = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_of_association_list parameters handler error
        pair_list
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (**************************************************************************)

  let build_init_restriction static dynamic error init_state =
    let parameters = get_parameter static in
    let store_remanent_triple = get_remanent_triple static in
    let error, (dynamic, event_list) =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun parameters error _agent_id agent (dynamic, event_list) ->
          match agent with
          | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost ->
            error, (dynamic, event_list)
          | Cckappa_sig.Dead_agent _ ->
            Exception.warn parameters error __POS__ Exit (dynamic, event_list)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*-------------------------------------------------------------*)
            let error, (dynamic, event_list) =
              match
                Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
                .unsafe_get parameters error agent_type store_remanent_triple
              with
              | error, Some triple_list ->
                let error, get_pair_list =
                  Bdu_static_views
                  .get_pair_cv_map_with_missing_association_creation parameters
                    error agent triple_list
                in
                let error, (dynamic, event_list) =
                  List.fold_left
                    (fun (error, (dynamic, event_list)) (cv_id, map_res) ->
                      let error, pair_list =
                        Ckappa_sig.Site_map_and_set.Map.fold
                          (fun site' state (error, current_list) ->
                            let pair_list = (site', state) :: current_list in
                            error, pair_list)
                          map_res (error, [])
                      in
                      let error, dynamic, bdu_init =
                        bdu_build static dynamic error pair_list
                      in
                      (*----------------------------------------------------*)
                      let error, dynamic, event_list =
                        add_link ~title:"Views in initial state:" error static
                          dynamic (agent_type, cv_id) bdu_init event_list
                      in
                      error, (dynamic, event_list))
                    (error, (dynamic, event_list))
                    get_pair_list
                in
                error, (dynamic, event_list)
              | error, None -> error, (dynamic, event_list)
            in
            error, (dynamic, event_list))
        init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views (dynamic, [])
    in
    error, dynamic, event_list

  (**************************************************************)
  (**add initial state of kappa*)

  let add_initial_state static dynamic error init_state =
    let error, dynamic, event_list =
      build_init_restriction static dynamic error init_state
    in
    error, dynamic, event_list

  (****************************************************************)

  exception False of Exception.method_handler * dynamic_information

  (****************************************************************)
  (*compute condition of bdu whether or not it is enable by doing the
    intersection of bdu_test and bdu_X*)

  let collect_bdu_enabled parameters error dynamic bdu_false fixpoint_result
      proj_bdu_test_restriction =
    let error, dynamic, _ =
      Covering_classes_type.AgentsCV_setmap.Map.fold
        (fun (agent_id, agent_type, cv_id) bdu_test (error, dynamic, map) ->
          (*------------------------------------------------------*)
          (*for each (agent_id, cv_id) a bdu*)
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map
              .find_option_without_logs parameters error (agent_type, cv_id)
                fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          (*----------------------------------------------------------------*)
          (*bdu intersection*)
          let handler = get_mvbdu_handler dynamic in
          let error, handler, bdu_inter =
            Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu_test
              bdu_X
          in
          let dynamic = set_mvbdu_handler handler dynamic in
          if Ckappa_sig.Views_bdu.equal bdu_inter bdu_false then
            raise (False (error, dynamic))
          else (
            let error, map =
              Covering_classes_type.AgentIDCV_map_and_set.Map.add parameters
                error (agent_id, cv_id) bdu_inter map
            in
            error, dynamic, map
          ))
        proj_bdu_test_restriction
        (error, dynamic, Covering_classes_type.AgentIDCV_map_and_set.Map.empty)
    in
    error, dynamic

  (*****************************************************************)
  (*MOVE in static?*)

  let get_new_site_name parameters error site_name
      (*store_new_index_pair_map*)
        map1 =
    let error, new_site_name =
      match
        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.get parameters
          error site_name map1
      with
      | error, None ->
        Exception.warn parameters error __POS__ Exit Ckappa_sig.dummy_site_name
      | error, Some i -> error, i
    in
    error, new_site_name

  (*MOVE?*)
  let is_new_site_name parameters error site_name map1 =
    match
      Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
        parameters error site_name map1
    with
    | error, None -> error, false
    | error, Some _ -> error, true

  (*****************************************************************)

  let step_list_empty _kappa_handler dynamic parameters error agent_id
      agent_type site_name cv_list fixpoint_result proj_bdu_test_restriction
      bdu_false bdu_true site_correspondence =
    (*------------------------------------------------------------*)
    let error, dynamic, bdu =
      List.fold_left
        (fun (error, dynamic, bdu) cv_id ->
          let error, (map1, _) =
            get_list_of_sites_correspondence_map parameters error agent_type
              cv_id site_correspondence
          in
          let error, new_site_name =
            get_new_site_name parameters error site_name map1
          in
          (*--------------------------------------------------------------*)
          (* fetch the bdu for the agent type and the cv_id in
             the current state of the iteration *)
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map
              .find_option_without_logs parameters error (agent_type, cv_id)
                fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          (*get bdu test*)
          let error, bdu_test =
            match
              Covering_classes_type.AgentsCV_setmap.Map.find_option
                (agent_id, agent_type, cv_id)
                proj_bdu_test_restriction
            with
            | None -> error, bdu_true
            | Some bdu -> error, bdu
          in
          (*Bdu_X and Bdu_test*)
          let handler = Analyzer_headers.get_mvbdu_handler dynamic in
          let error, handler, bdu_test_X =
            Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu_X
              bdu_test
          in
          (* compute the projection over new_site_name *)
          let error, handler, singleton =
            Ckappa_sig.Views_bdu.build_variables_list parameters handler error
              [ new_site_name ]
          in
          let error, handler, bdu_proj =
            Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameters handler
              error bdu_test_X singleton
          in
          (* rename new_site_name into 1 *)
          let error, handler, new_site_name_1 =
            Ckappa_sig.Views_bdu.build_renaming_list parameters handler error
              [ new_site_name, site_name ]
          in
          let error, handler, bdu_renamed =
            Ckappa_sig.Views_bdu.mvbdu_rename parameters handler error bdu_proj
              new_site_name_1
          in
          (* conjunction between bdu and bdu'*)
          let error, handler, bdu =
            Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu
              bdu_renamed
          in
          let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
          error, dynamic, bdu)
        (error, dynamic, bdu_true) cv_list
    in
    (*---------------------------------------------------------------------*)
    let handler = Analyzer_headers.get_mvbdu_handler dynamic in
    let error, handler, list =
      Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error bdu
    in
    let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
    (*---------------------------------------------------------------------*)
    let error, state_list =
      List.fold_left
        (fun (error, output) list ->
          match list with
          | [ (_, state) ] ->
            (* the site name is fictitious, do not take it *)
            error, state :: output
          | _ ->
            Exception.warn parameters error __POS__ ~message:"state is empty"
              Exit output)
        (error, []) list
    in
    error, dynamic, Usual_domains.Val (List.rev state_list)

  (**************************************************************************)
  (*empty case of step list*)

  let precondition_empty_step_list kappa_handler parameters error dynamic
      rule_id path store_agent_name bdu_false bdu_true store_covering_classes_id
      site_correspondence fixpoint_result proj_bdu_test_restriction =
    let error, agent_type =
      match
        Ckappa_sig.RuleAgent_map_and_set.Map.find_option_without_logs parameters
          error
          (rule_id, path.Communication.agent_id)
          store_agent_name
      with
      | error, None ->
        Exception.warn parameters error __POS__ ~message:"unknown agent type"
          Exit Ckappa_sig.dummy_agent_name
      | error, Some a -> error, a
    in
    (*------------------------------------------------------------*)
    (* let error, site_correspondence =
       match Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
               parameters
               error
               agent_type
               site_correspondence
       with
       | error, None ->
         Exception.warn  parameters error __POS__ Exit []
       | error, Some l -> error, l
         in*)
    (* compute the list of cv_id documenting site_name *)
    let error, cv_list =
      match
        Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameters
          error
          (agent_type, path.Communication.site) (*site:v*)
          store_covering_classes_id
      with
      | error, None ->
        Exception.warn parameters error __POS__
          ~message:"the site does not belong to the last agent type of a path"
          Exit []
      | error, Some l -> error, l
    in
    (*---------------------------------------------------------------------*)
    let error, dynamic, new_answer =
      step_list_empty kappa_handler dynamic parameters error
        path.Communication.agent_id agent_type path.Communication.site cv_list
        fixpoint_result proj_bdu_test_restriction bdu_false bdu_true
        (*store_new_index_pair_map*)
        site_correspondence
    in
    error, dynamic, new_answer

  (***************************************************************)
  (*let compute precondition*)

  (* checking the binding information whether or not path belong to the
     current contact map*)

  (* if not an empty list *)
  (* follow the path while it is within the pattern *)
  (* 1) if you remain inside the pattern for ever, identify the
     target agent and apply the the [] case *)
  (* 2) the path is inconsistent with the pattern, return the empty
     list *)
  (* 3) the path goes outside of the pattern, grab the agent
     type of the target and the site type of the target, and
     gather information (in the views) *)
  (*-------------------------------------------------------------*)
  (*compute the pattern: lhs of the rule*)
  (* Where do you test whether the target of the bond belong to the pattern *)
  (* If it is the case, you should apply your fonction recursively to
     agent_id: target_agent;
     relative_address: tl;
     site: path.site *)
  (* It not, take the last element of the relative_address,
     if you could take the agent type of the target, take
     site, and collect the information you have about the
     potential state of this site in agents of this type.*)

  (*typing*)

  let precondition_typing parameters error kappa_handler rule_id step_list path
      store_agent_name dual_contact_map =
    let rec aux acc error =
      match acc with
      | [] -> error, Usual_domains.Any
      | step :: tl ->
        let error, agent_type =
          match
            Ckappa_sig.RuleAgent_map_and_set.Map.find_option_without_logs
              parameters error
              (rule_id, path.Communication.agent_id)
              store_agent_name
          with
          | error, None -> error, Ckappa_sig.dummy_agent_name
          | error, Some a -> error, a
        in
        (*---------------------------------------------------------*)
        (*get state information from (agent_type, site)*)
        let error, state_dic =
          Misc_sa.unsome
            (Ckappa_sig
             .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
             .get parameters error
               (agent_type, step.Communication.site_out) (*A.x*)
               kappa_handler.Cckappa_sig.states_dic)
            (fun error ->
              Exception.warn parameters error __POS__ Exit
                (Ckappa_sig.Dictionary_of_States.init ()))
        in
        (*---------------------------------------------------------*)
        (*Binding state: B.y*)
        let state =
          Ckappa_sig.C_Lnk_type
            (step.Communication.agent_type_in, step.Communication.site_in)
        in
        (*check if whether or not state is defined*)
        let error, b =
          Ckappa_sig.Dictionary_of_States.member parameters error
            (Ckappa_sig.Binding state) state_dic
        in
        if b then (
          (*-----------------------------------------------------*)
          (*state is defined*)
          let error, answer_contact_map =
            match
              Ckappa_sig.Dictionary_of_States.allocate parameters error
                Ckappa_sig.compare_unit_state_index (Ckappa_sig.Binding state)
                () Misc_sa.const_unit state_dic
            with
            | error, None ->
              Exception.warn parameters error __POS__ Exit
                Usual_domains.Undefined
            | error, Some (state, _, _, _) ->
              (match
                 Ckappa_sig.AgentSiteState_map_and_set.Map
                 .find_option_without_logs parameters error
                   (agent_type, step.Communication.site_out, state)
                   dual_contact_map
               with
              | error, None ->
                Exception.warn parameters error __POS__ Exit
                  Usual_domains.Undefined
              | error, Some _ -> (*recursive call*) aux tl error)
          in
          error, answer_contact_map
        ) else
          (*state is not defined*)
          Exception.warn parameters error __POS__ Exit Usual_domains.Undefined
    in
    aux step_list error

  (*-----------------------------------------------------------*)
  (*outside the pattern*)

  let get_tuple_pattern error path agent_type =
    let error, (agent_type', site_out, site_in, agent_type_in) =
      (*revert the path, and get the three cases*)
      match List.rev path.Communication.relative_address with
      | [] ->
        (*return dummy*)
        ( error,
          ( Ckappa_sig.dummy_agent_name,
            Ckappa_sig.dummy_site_name,
            Ckappa_sig.dummy_site_name,
            Ckappa_sig.dummy_agent_name ) )
      | [ x ] ->
        (*agent_type' (h') is agent_type inside the pattern*)
        (*get information of the triple*)
        let site_in = x.Communication.site_in in
        let agent_type_in = x.Communication.agent_type_in in
        let site_out = x.Communication.site_out in
        error, (agent_type, site_out, site_in, agent_type_in)
      | h :: h' :: _ ->
        (*from h get the information of the triple
          (site_in, agent_type_in, site_out)*)
        (*get information of the triple*)
        let site_in = h.Communication.site_in in
        let agent_type_in = h.Communication.agent_type_in in
        let site_out = h.Communication.site_out in
        let agent_type' = h'.Communication.agent_type_in in
        error, (agent_type', site_out, site_in, agent_type_in)
    in
    error, (agent_type', site_out, site_in, agent_type_in)

  let precondition_outside_pattern parameters error dynamic kappa_handler path
      agent_type (*agent_type inside the pattern*) bdu_false bdu_true
      _site_correspondence site_correspondence_map store_covering_classes_id
      fixpoint_result =
    (*get the information of the path,
      A - x - y - B - z
      (agent_type', site_out, site_in, agent_type_in, site_path)
      where (agent_type_in, site_in) is the information of the last agent,
      and (agent_type', site_in) is the information of the agent of the pattern
    *)
    let error, (agent_type', site_out, site_in, agent_type_in) =
      get_tuple_pattern error path agent_type
    in
    (*get the site path*)
    let site_path = path.Communication.site in
    (*get the information of state of the last agent
      (A.x:agent_type,site_out)*)
    let error, state_dic_A_x =
      Misc_sa.unsome
        (Ckappa_sig
         .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
         .get parameters error (agent_type', site_out)
           kappa_handler.Cckappa_sig.states_dic)
        (fun error ->
          Exception.warn parameters error __POS__ Exit
            (Ckappa_sig.Dictionary_of_States.init ()))
    in
    let error, state_dic_B_y =
      Misc_sa.unsome
        (Ckappa_sig
         .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
         .get parameters error (agent_type_in, site_in)
           kappa_handler.Cckappa_sig.states_dic)
        (fun error ->
          Exception.warn parameters error __POS__ Exit
            (Ckappa_sig.Dictionary_of_States.init ()))
    in
    (*check binding state B,y *)
    let state_A_x = Ckappa_sig.C_Lnk_type (agent_type_in, site_in) in
    let error, b_A_x =
      Ckappa_sig.Dictionary_of_States.member parameters error
        (Ckappa_sig.Binding state_A_x) state_dic_A_x
    in
    let state_B_y = Ckappa_sig.C_Lnk_type (agent_type', site_out) in
    let error, b_B_y =
      Ckappa_sig.Dictionary_of_States.member parameters error
        (Ckappa_sig.Binding state_B_y) state_dic_B_y
    in
    if b_A_x && b_B_y then (
      (*state is defined*)
      let error, (dynamic, new_answer) =
        match
          Ckappa_sig.Dictionary_of_States.allocate parameters error
            Ckappa_sig.compare_unit_state_index (Ckappa_sig.Binding state_B_y)
            () Misc_sa.const_unit state_dic_B_y
        with
        | error, None ->
          (*inconsistent*)
          error, (dynamic, Usual_domains.Undefined)
        | error, Some (state, _, _, _) ->
          let error, cv_list =
            match
              Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameters error (agent_type_in, site_path)
                store_covering_classes_id
            with
            | error, None -> Exception.warn parameters error __POS__ Exit []
            | error, Some l -> error, l
          in
          let error, dynamic, bdu =
            List.fold_left
              (fun (error, dynamic, bdu) cv_id ->
                let error, (map1, _) =
                  get_list_of_sites_correspondence_map parameters error
                    agent_type_in cv_id site_correspondence_map
                in
                let error, b = is_new_site_name parameters error site_in map1 in
                let error, bdu_X =
                  match
                    Covering_classes_type.AgentCV_map_and_set.Map
                    .find_option_without_logs parameters error
                      (agent_type_in, cv_id) fixpoint_result
                  with
                  | error, None -> error, bdu_false
                  | error, Some bdu -> error, bdu
                in
                let handler = Analyzer_headers.get_mvbdu_handler dynamic in
                let error, new_site_name_z =
                  get_new_site_name parameters error site_path map1
                in
                let error, handler, singleton =
                  Ckappa_sig.Views_bdu.build_variables_list parameters handler
                    error [ new_site_name_z ]
                in
                let error, handler, new_bdu =
                  if b then (
                    (*site y is in CV, *)
                    let error, new_site_name_y =
                      get_new_site_name parameters error site_in map1
                    in
                    let error, handler, mvbdu_B_y =
                      Ckappa_sig.Views_bdu
                      .mvbdu_of_reverse_sorted_association_list parameters
                        handler error
                        [ new_site_name_y, state ]
                    in
                    Ckappa_sig.Views_bdu.mvbdu_and parameters handler error
                      bdu_X mvbdu_B_y
                  ) else
                    error, handler, bdu_X
                in
                let error, handler, bdu_proj =
                  Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameters
                    handler error new_bdu singleton
                in
                let error, handler, new_site_name_1 =
                  Ckappa_sig.Views_bdu.build_renaming_list parameters handler
                    error
                    [ new_site_name_z, site_path ]
                in
                let error, handler, bdu_renamed =
                  Ckappa_sig.Views_bdu.mvbdu_rename parameters handler error
                    bdu_proj new_site_name_1
                in
                let error, handler, bdu =
                  Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu
                    bdu_renamed
                in
                let dynamic =
                  Analyzer_headers.set_mvbdu_handler handler dynamic
                in
                error, dynamic, bdu)
              (error, dynamic, bdu_true) cv_list
          in
          let handler = Analyzer_headers.get_mvbdu_handler dynamic in
          let error, handler, list =
            Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error
              bdu
          in
          let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
          let error, state_list =
            List.fold_left
              (fun (error, output) list ->
                match list with
                | [ (_, state) ] -> error, state :: output
                | _ -> Exception.warn parameters error __POS__ Exit output)
              (error, []) list
          in
          error, (dynamic, Usual_domains.Val (List.rev state_list))
      in
      error, (dynamic, new_answer)
    ) else
      (*state is not defined*)
      Exception.warn parameters error __POS__ Exit
        (dynamic, Usual_domains.Undefined)

  (*---------------------------------------------------------------*)
  (*inside the pattern*)

  let precondition_inside_pattern parameters error dynamic kappa_handler path
      agent_type aux rule site_correspondence site_correspondence_map
      store_covering_classes_id fixpoint_result bdu_false bdu_true =
    (*---------------------------------------------------------*)
    (*inside the pattern, check the binding information in the lhs of the
      current agent*)
    let error, output =
      Communication.follow_path_inside_cc parameters error kappa_handler
        rule.Cckappa_sig.rule_lhs path
    in
    match output with
    | Communication.Cannot_exist -> error, (dynamic, Usual_domains.Undefined)
    | Communication.May_exist _ ->
      (*-----------------------------------------------------*)
      let error', (dynamic, new_answer) =
        precondition_outside_pattern parameters error dynamic kappa_handler path
          agent_type bdu_false bdu_true site_correspondence
          site_correspondence_map store_covering_classes_id fixpoint_result
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, (dynamic, new_answer)
    (*--------------------------------------------------------*)
    | Communication.Located agent_id ->
      (*search inside this map which agent and site, A.x bind to.*)
      let next_path =
        {
          Communication.site = path.Communication.site;
          Communication.agent_id;
          Communication.relative_address = [];
        }
      in
      let error, dynamic, new_answer = aux dynamic next_path in
      error, (dynamic, new_answer)

  (*-------------------------------------------------------------*)

  let scan_bot ~(also_scan_top : bool) pos parameters error elt string =
    match elt with
    | Usual_domains.Undefined ->
      let error, () =
        Exception.warn parameters error pos
          ~message:
            ("bot generated while fetching the potential state of a site"
           ^ string)
          Exit ()
      in
      error
    | Usual_domains.Any when also_scan_top ->
      let error, () =
        Exception.warn parameters error pos
          ~message:
            ("top generated while fetching the potential state of a site"
           ^ string)
          Exit ()
      in
      error
    | Usual_domains.Val _ | Usual_domains.Any -> error

  let scan_bot_warn ~(also_scan_top : bool) (a, _, _, _) parameters error elt
      string =
    let () =
      match elt with
      | Usual_domains.Undefined ->
        if Remanent_parameters.get_dump_reachability_analysis_wl parameters then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s%sbot generated while fetching the potential state of a site \
               %s"
              (Remanent_parameters.get_prefix parameters)
              a string
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        )
      | Usual_domains.Any when also_scan_top ->
        if Remanent_parameters.get_dump_reachability_analysis_wl parameters then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%stop generated while fetching the potential state of a site %s"
              (Remanent_parameters.get_prefix parameters)
              string
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        )
      | Usual_domains.Val _ | Usual_domains.Any -> ()
    in
    error

  let compute_pattern_navigation parameters error kappa_handler aux dynamic path
      rule step bdu_false bdu_true site_correspondence site_correspondence_map
      store_covering_classes_id fixpoint_result =
    let error, agent =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameters error path.Communication.agent_id (*#1:A*)
          rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      with
      | error, None ->
        Exception.warn parameters error __POS__ Exit Cckappa_sig.Ghost
      | error, Some a -> error, a
    in
    let error, (dynamic, new_answer) =
      match agent with
      | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _
      | Cckappa_sig.Dead_agent _ ->
        Exception.warn parameters error __POS__ Exit
          (dynamic, Usual_domains.Undefined)
      | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*search inside the pattern, check whether or not it is out
          of the pattern or in the pattern.*)
        let error, (dynamic, new_answer) =
          match
            Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters
              error step.Communication.site_out (*A.x: state*)
              agent.Cckappa_sig.agent_interface
          with
          | error, None ->
            (*------------------------------------------------------*)
            (*out of the pattern, take the last element in the
              relative_address, if one can take the agent type of the
              target, take site and collect the information one has about
              the potential state of this site in agents of this type. *)
            let error', (dynamic, new_answer) =
              precondition_outside_pattern parameters error dynamic
                kappa_handler path agent_type (*agent_type inside the pattern*)
                bdu_false bdu_true site_correspondence site_correspondence_map
                store_covering_classes_id fixpoint_result
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error =
              scan_bot ~also_scan_top:false __POS__ parameters error new_answer
                " in precondition outside a pattern"
            in
            error, (dynamic, new_answer)
          (*----------------------------------------------------*)
          (*There is some states, inside the pattern. Check
            port whether or not it is free/bound?*)
          | error, Some port ->
            (*check if it is free?*)
            (match port.Cckappa_sig.site_free with
            | Some true ->
              (*then it is inconsistent, undefined*)
              let () =
                if
                  local_trace
                  || Remanent_parameters.get_dump_reachability_analysis_wl
                       parameters
                  || Remanent_parameters.get_trace parameters
                then (
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "Try to navigate through a free site: bottom reduction"
                  in
                  Loggers.print_newline
                    (Remanent_parameters.get_logger parameters)
                )
              in
              error, (dynamic, Usual_domains.Undefined)
            | None | Some false ->
              (*it is not free, check if it is fully defined, or incompelete,
                by looking into the bonds on the lhs*)
              (*get the information of the agent partner *)
              let agent_type_partner = step.Communication.agent_type_in in
              let site_x_partner = step.Communication.site_in in
              (*get information of the agent*)
              let agent_id = path.Communication.agent_id in
              let agent_type = agent.Cckappa_sig.agent_name in
              let site_x = step.Communication.site_out in
              (*looking into the bonds of the agent*)
              (match
                 Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif
                 .unsafe_get parameters error agent_id
                   rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
               with
              | error, None ->
                (*it is incompelete, then it is outside the pattern*)
                let error', (dynamic, new_answer) =
                  precondition_outside_pattern parameters error dynamic
                    kappa_handler path agent_type bdu_false bdu_true
                    site_correspondence site_correspondence_map
                    store_covering_classes_id fixpoint_result
                in
                let error =
                  Exception.check_point Exception.warn parameters error error'
                    __POS__ Exit
                in
                error, (dynamic, new_answer)
              | error, Some map ->
                (match
                   Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
                     parameters error site_x map
                 with
                | error, None ->
                  (*outside the pattern*)
                  let error', (dynamic, new_answer) =
                    precondition_outside_pattern parameters error dynamic
                      kappa_handler path agent_type bdu_false bdu_true
                      site_correspondence site_correspondence_map
                      store_covering_classes_id fixpoint_result
                  in
                  let error =
                    Exception.check_point Exception.warn parameters error error'
                      __POS__ Exit
                  in
                  error, (dynamic, new_answer)
                | error, Some site_add ->
                  (*there is a bond, check the following pattern is it well
                    defined*)
                  let agent_type' = site_add.Cckappa_sig.agent_type in
                  (*B*)
                  let site_type' = site_add.Cckappa_sig.site in
                  (*z?*)
                  (*if A.x is bound to B.x*)
                  if
                    agent_type' = agent_type_partner
                    && site_type' = site_x_partner
                  then (
                    (*inside the pattern*)
                    let error', (dynamic, new_answer) =
                      precondition_inside_pattern parameters error dynamic
                        kappa_handler path agent_type aux rule
                        site_correspondence site_correspondence_map
                        store_covering_classes_id fixpoint_result bdu_false
                        bdu_true
                    in
                    let error =
                      Exception.check_point Exception.warn parameters error
                        error' __POS__ Exit
                    in
                    error, (dynamic, new_answer)
                  ) else (
                    (*outsite the pattern*)
                    let error', (dynamic, new_answer) =
                      precondition_outside_pattern parameters error dynamic
                        kappa_handler path agent_type bdu_false bdu_true
                        site_correspondence site_correspondence_map
                        store_covering_classes_id fixpoint_result
                    in
                    let error =
                      Exception.check_point Exception.warn parameters error
                        error' __POS__ Exit
                    in
                    error, (dynamic, new_answer)
                  ))))
        in
        error, (dynamic, new_answer)
    in
    error, (dynamic, new_answer)

  (*------------------------------------------------------------*)

  let compute_precondition_enable error kappa_handler rule rule_id precondition
      bdu_false bdu_true dual_contact_map store_agent_name site_correspondence
      site_correspondence_map store_covering_classes_id fixpoint_result
      proj_bdu_test_restriction =
    let precondition =
      Communication.refine_information_about_state_of_sites_in_precondition
        precondition
        (fun
          parameters
          error
          dynamic
          (current_path : Communication.path)
          former_answer
        ->
          (*-----------------------------------------------------*)
          (*typing*)
          let error =
            scan_bot ~also_scan_top:false __POS__ parameters error former_answer
              " from overlying domain"
          in
          let error, answer_contact_map =
            precondition_typing parameters error kappa_handler rule_id
              current_path.Communication.relative_address current_path
              store_agent_name dual_contact_map
          in
          let error =
            scan_bot_warn ~also_scan_top:false __POS__ parameters error
              answer_contact_map " in the contact map"
          in
          (*-----------------------------------------------------*)
          (* The output should be more precise than former_answer:
             If the former_answer is any, do not change anything,
             If the former_answer is Val l,
             then the answer must be Val l', with l' a sublist of l *)
          let error, dynamic, new_answer =
            let rec aux dynamic path =
              let step_list = path.Communication.relative_address in
              match step_list with
              | step :: _ ->
                (*------------------------------------------------*)
                (*pattern navigation*)
                let error, (dynamic, new_answer) =
                  compute_pattern_navigation parameters error kappa_handler aux
                    dynamic path rule step bdu_false bdu_true
                    site_correspondence site_correspondence_map
                    store_covering_classes_id fixpoint_result
                in
                let error =
                  scan_bot ~also_scan_top:false __POS__ parameters error
                    new_answer " while navigating"
                in
                let update_answer =
                  Usual_domains.glb_list new_answer former_answer
                in
                error, dynamic, update_answer
              (*--------------------------------------------------*)
              (*empty relative_adress*)
              | [] ->
                let error, dynamic, new_answer =
                  precondition_empty_step_list kappa_handler parameters error
                    dynamic rule_id path store_agent_name bdu_false bdu_true
                    store_covering_classes_id site_correspondence_map
                    fixpoint_result proj_bdu_test_restriction
                in
                let error =
                  scan_bot ~also_scan_top:false __POS__ parameters error
                    new_answer " while navigating (empty path)"
                in
                (*do I need to do the intersection with former answer?*)
                let update_answer =
                  Usual_domains.glb_list new_answer former_answer
                in
                error, dynamic, update_answer
            in
            aux dynamic current_path
          in
          (*final intersection with contact map*)
          let update_answer =
            Usual_domains.glb_list answer_contact_map new_answer
          in
          error, dynamic, update_answer)
    in
    error, precondition

  (****************************************************************)

  let is_enable_aux static dynamic error rule_id precondition =
    let parameters = get_parameter static in
    let compil = get_compil static in
    let kappa_handler = get_kappa_handler static in
    let rules = compil.Cckappa_sig.rules in
    let error, rule =
      match
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.unsafe_get parameters
          error rule_id rules
      with
      | error, None ->
        let error, rule = Preprocess.empty_rule parameters error in
        Exception.warn parameters error __POS__ ~message:"unknown rule" Exit
          rule
      | error, Some rule -> error, rule.Cckappa_sig.e_rule_c_rule
    in
    (*-----------------------------------------------------------*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
    (*------------------------------------------------------------*)
    let fixpoint_result = get_fixpoint_result dynamic in
    let dual_contact_map = get_store_dual_contact_map dynamic in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static error
    in
    let store_agent_name = get_agent_name static in
    let site_correspondence = get_remanent_triple static in
    let site_correspondence_map = get_site_correspondence_array static in
    let store_covering_classes_id = get_covering_classes_id static in
    let error, proj_bdu_test_restriction =
      match
        Ckappa_sig.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Covering_classes_type.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*---------------------------------------------------------*)
    try
      (*check the condition whether or not the bdu is enabled, do the
        intersection of bdu_test and bdu_X.*)
      let error, dynamic =
        collect_bdu_enabled parameters error dynamic bdu_false fixpoint_result
          proj_bdu_test_restriction
      in
      (*-----------------------------------------------------*)
      (*get a set of sites in a covering class: later with state list*)
      let error, precondition =
        compute_precondition_enable error kappa_handler rule rule_id
          precondition bdu_false bdu_true dual_contact_map store_agent_name
          site_correspondence site_correspondence_map store_covering_classes_id
          fixpoint_result proj_bdu_test_restriction
      in
      error, (dynamic, precondition), true
    with False (error, dynamic) -> error, (dynamic, precondition), false

  (************************************************************)
  (*get contact_map from dynamic*)
  (* then use the functions get_potential_partner and/or
     fold_over_potential_partners in the views domain to use the incremental
     (dynamic) contact map *)
  (* instead of the static one *)

  let is_enabled static dynamic error rule_id precondition =
    let error, (dynamic, precondition), is_enable =
      is_enable_aux static dynamic error rule_id precondition
    in
    if is_enable then
      error, dynamic, Some precondition
    else
      error, dynamic, None

  (***********************************************************)
  (*Precondition inside pattern*)
  (***********************************************************)

  let precondition_step_pattern parameters error step aux dynamic path pattern
      kappa_handler bdu_false bdu_true site_correspondence
      site_correspondence_map store_covering_classes_id fixpoint_result =
    let error, agent =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameters error path.Communication.agent_id (*#1:A*)
          pattern.Cckappa_sig.views
      with
      | error, None ->
        Exception.warn parameters error __POS__ Exit Cckappa_sig.Ghost
      | error, Some a -> error, a
    in
    let error, (dynamic, new_answer) =
      match agent with
      | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _
      | Cckappa_sig.Dead_agent _ ->
        Exception.warn parameters error __POS__ Exit
          (dynamic, Usual_domains.Undefined)
      | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*search inside the pattern, check whether or not it is out
            of the pattern or in the pattern.*)
        let error, (dynamic, new_answer) =
          match
            Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters
              error step.Communication.site_out (*A.x: state*)
              agent.Cckappa_sig.agent_interface
          with
          (*---------------------------------------------------------*)
          | error, None ->
            let error, (dynamic, new_answer) =
              precondition_outside_pattern parameters error dynamic
                kappa_handler path agent_type bdu_false bdu_true
                site_correspondence site_correspondence_map
                store_covering_classes_id fixpoint_result
            in
            let error =
              scan_bot ~also_scan_top:false __POS__ parameters error new_answer
                " in precondition outside a pattern"
            in
            error, (dynamic, new_answer)
          (*---------------------------------------------------------*)
          | error, Some port ->
            (match port.Cckappa_sig.site_free with
            | Some true ->
              let () =
                if
                  local_trace
                  || Remanent_parameters.get_dump_reachability_analysis_wl
                       parameters
                  || Remanent_parameters.get_trace parameters
                then (
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "Try to navigate through a free site: bottom reduction"
                  in
                  Loggers.print_newline
                    (Remanent_parameters.get_logger parameters)
                )
              in
              error, (dynamic, Usual_domains.Undefined)
            | None | Some false ->
              let agent_type_partner = step.Communication.agent_type_in in
              let site_x_partner = step.Communication.site_in in
              let agent_id = path.Communication.agent_id in
              let site_x = step.Communication.site_out in
              (*---------------------------------------------------------*)
              (match
                 Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif
                 .unsafe_get parameters error agent_id pattern.Cckappa_sig.bonds
               with
              | error, None ->
                let error', (dynamic, new_answer) =
                  precondition_outside_pattern parameters error dynamic
                    kappa_handler path agent_type bdu_false bdu_true
                    site_correspondence site_correspondence_map
                    store_covering_classes_id fixpoint_result
                in
                let error =
                  Exception.check_point Exception.warn parameters error error'
                    __POS__ Exit
                in
                error, (dynamic, new_answer)
              (*---------------------------------------------------------*)
              | error, Some map ->
                (match
                   Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
                     parameters error site_x map
                 with
                | error, None ->
                  let error', (dynamic, new_answer) =
                    precondition_outside_pattern parameters error dynamic
                      kappa_handler path agent_type bdu_false bdu_true
                      site_correspondence site_correspondence_map
                      store_covering_classes_id fixpoint_result
                  in
                  let error =
                    Exception.check_point Exception.warn parameters error error'
                      __POS__ Exit
                  in
                  error, (dynamic, new_answer)
                | error, Some site_add ->
                  let agent_type' = site_add.Cckappa_sig.agent_type in
                  let site_type' = site_add.Cckappa_sig.site in
                  if
                    agent_type' = agent_type_partner
                    && site_type' = site_x_partner
                  then (
                    let error, output =
                      Communication.follow_path_inside_cc parameters error
                        kappa_handler pattern path
                    in
                    match output with
                    | Communication.Cannot_exist ->
                      error, (dynamic, Usual_domains.Undefined)
                    | Communication.May_exist _ ->
                      let error', (dynamic, new_answer) =
                        precondition_outside_pattern parameters error dynamic
                          kappa_handler path agent_type bdu_false bdu_true
                          site_correspondence site_correspondence_map
                          store_covering_classes_id fixpoint_result
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      error, (dynamic, new_answer)
                    | Communication.Located agent_id ->
                      let next_path =
                        {
                          Communication.site = path.Communication.site;
                          Communication.agent_id;
                          Communication.relative_address = [];
                        }
                      in
                      let error, dynamic, new_answer = aux dynamic next_path in
                      error, (dynamic, new_answer)
                  ) else (
                    let error', (dynamic, new_answer) =
                      precondition_outside_pattern parameters error dynamic
                        kappa_handler path agent_type bdu_false bdu_true
                        site_correspondence site_correspondence_map
                        store_covering_classes_id fixpoint_result
                    in
                    let error =
                      Exception.check_point Exception.warn parameters error
                        error' __POS__ Exit
                    in
                    error, (dynamic, new_answer)
                  ))))
        in
        error, (dynamic, new_answer)
    in
    error, (dynamic, new_answer)

  (************************************************************************)

  let build_bdu_test_pattern parameters error pattern site_correspondence
      dynamic =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error _agent_id agent (dynamic, current_list) ->
        match agent with
        | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost
        | Cckappa_sig.Dead_agent _ ->
          Exception.warn parameters error __POS__ Exit (dynamic, current_list)
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, get_pair_list =
            Bdu_static_views.get_pair_cv_map_with_restriction_views parameters
              error agent site_correspondence
          in
          (*build bdu_test*)
          let error, (dynamic, list) =
            List.fold_left
              (fun (error, (dynamic, current_list)) (_cv_id, map_res) ->
                if Ckappa_sig.Site_map_and_set.Map.is_empty map_res then
                  error, (dynamic, current_list)
                else (
                  let error, pair_list =
                    Ckappa_sig.Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
                        let pair_list =
                          (site', (state.Cckappa_sig.min, state.Cckappa_sig.max))
                          :: current_list
                        in
                        error, pair_list)
                      map_res (error, [])
                  in
                  let handler = Analyzer_headers.get_mvbdu_handler dynamic in
                  let error, handler, bdu_test =
                    Ckappa_sig.Views_bdu.mvbdu_of_reverse_sorted_range_list
                      parameters handler error pair_list
                  in
                  let dynamic =
                    Analyzer_headers.set_mvbdu_handler handler dynamic
                  in
                  error, (dynamic, (agent_type, bdu_test) :: current_list)
                ))
              (error, (dynamic, current_list))
              get_pair_list
          in
          error, (dynamic, list))
      pattern.Cckappa_sig.views (dynamic, [])

  let precondition_empty_aux parameters error path pattern dynamic bdu_false
      bdu_true site_correspondence site_correspondence_map fixpoint_result
      cv_list =
    let error, (dynamic, list) =
      build_bdu_test_pattern parameters error pattern site_correspondence
        dynamic
    in
    (*--------------------------------------------------*)
    let error, dynamic, bdu =
      List.fold_left
        (fun (error, dynamic, bdu) (agent_type, bdu_test) ->
          List.fold_left
            (fun (error, dynamic, bdu) cv_id ->
              let site_name = path.Communication.site in
              let error, (map1, _) =
                get_list_of_sites_correspondence_map parameters error agent_type
                  cv_id site_correspondence_map
              in
              let error, new_site_name =
                get_new_site_name parameters error site_name map1
              in
              let error, bdu_X =
                match
                  Covering_classes_type.AgentCV_map_and_set.Map
                  .find_option_without_logs parameters error (agent_type, cv_id)
                    fixpoint_result
                with
                | error, None -> error, bdu_false
                | error, Some bdu -> error, bdu
              in
              let handler = Analyzer_headers.get_mvbdu_handler dynamic in
              let error, handler, bdu_test_X =
                Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu_X
                  bdu_test
              in
              let error, handler, singleton =
                Ckappa_sig.Views_bdu.build_variables_list parameters handler
                  error [ new_site_name ]
              in
              let error, handler, bdu_proj =
                Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameters handler
                  error bdu_test_X singleton
              in
              (* rename new_site_name into 1 *)
              let error, handler, new_site_name_1 =
                Ckappa_sig.Views_bdu.build_renaming_list parameters handler
                  error
                  [ new_site_name, site_name ]
              in
              let error, handler, bdu_renamed =
                Ckappa_sig.Views_bdu.mvbdu_rename parameters handler error
                  bdu_proj new_site_name_1
              in
              (* conjunction between bdu and bdu'*)
              let error, handler, bdu =
                Ckappa_sig.Views_bdu.mvbdu_and parameters handler error bdu
                  bdu_renamed
              in
              let dynamic =
                Analyzer_headers.set_mvbdu_handler handler dynamic
              in
              error, dynamic, bdu)
            (error, dynamic, bdu) cv_list)
        (error, dynamic, bdu_true) list
    in
    let handler = Analyzer_headers.get_mvbdu_handler dynamic in
    let error, handler, list =
      Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error bdu
    in
    let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
    let error, state_lists =
      List.fold_left
        (fun (error, output) list ->
          match list with
          | [ (_, state) ] -> error, state :: output
          | _ ->
            Exception.warn parameters error __POS__ ~message:"state is empty"
              Exit output)
        (error, []) list
    in
    error, (dynamic, Usual_domains.Val (List.rev state_lists))

  let precondition_empty_pattern parameters error path pattern dynamic bdu_false
      bdu_true store_agent_name_from_pattern site_correspondence
      site_correspondence_map store_covering_classes_id fixpoint_result =
    let error, agent_type =
      match
        Ckappa_sig.Agent_id_map_and_set.Map.find_option_without_logs parameters
          error path.Communication.agent_id store_agent_name_from_pattern
      with
      | error, None ->
        Exception.warn parameters error __POS__ ~message:"unknown agent type"
          Exit Ckappa_sig.dummy_agent_name
      | error, Some a -> error, a
    in
    let error, site_correspondence =
      match
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
          parameters error agent_type site_correspondence
      with
      | error, None -> Exception.warn parameters error __POS__ Exit []
      | error, Some l -> error, l
    in
    let error, cv_list =
      match
        Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs parameters
          error
          (agent_type, path.Communication.site)
          store_covering_classes_id
      with
      | error, None ->
        Exception.warn parameters error __POS__
          ~message:"the site does not belong to the last agent type of a path"
          Exit []
      | error, Some l -> error, l
    in
    (*step list empty*)
    let error, (dynamic, new_answer) =
      precondition_empty_aux parameters error path pattern dynamic bdu_false
        bdu_true site_correspondence site_correspondence_map fixpoint_result
        cv_list
    in
    error, dynamic, new_answer

  (************************************************************************)

  let maybe_reachable_aux static dynamic error (pattern : Cckappa_sig.mixture)
      precondition =
    let parameters = get_parameter static in
    (*-----------------------------------------------------------*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
    (*-----------------------------------------------------------*)
    let kappa_handler = get_kappa_handler static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let dual_contact_map = get_store_dual_contact_map dynamic in
    let store_agent_name_from_pattern = get_agent_name_from_pattern static in
    let site_correspondence = get_remanent_triple static in
    let site_correspondence_map = get_site_correspondence_array static in
    let store_covering_classes_id = get_covering_classes_id static in
    (*---------------------------------------------------------*)
    (* Why an arbitrary patterns would be stored in that map *)
    (* For each view, you have to collect the set of sites *)
    (* Then to collect the set of covering class that overlaps with at least one
       site (you should have the relation site -> cv_id list somewhere in static) *)
    (* Then, for each covering class, you have to compute the bdu *)
    (* If it doens not overlap with the corresponding bdu in the fixpoint *)
    (* Then, answer false *)
    (* Othewise keep on iterating *)
    try
      let error, dynamic =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error _agent_id agent dynamic ->
            match agent with
            | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost
            | Cckappa_sig.Dead_agent (_, _, _, _) ->
              error, dynamic
            | Cckappa_sig.Agent agent ->
              let agent_type = agent.Cckappa_sig.agent_name in
              let interface = agent.Cckappa_sig.agent_interface in
              if Ckappa_sig.Site_map_and_set.Map.is_empty interface then
                error, dynamic
              else (
                let error, site_correspondence =
                  match
                    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
                    .get parameters error agent_type site_correspondence
                  with
                  | error, None ->
                    Exception.warn parameters error __POS__ Exit []
                  | error, Some l -> error, l
                in
                let error, get_pair_list =
                  Bdu_static_views.get_pair_cv_map_with_restriction_views
                    parameters error agent site_correspondence
                in
                (*build bdu_test*)
                let error, dynamic =
                  List.fold_left
                    (fun (error, dynamic) (cv_id, map_res) ->
                      if Ckappa_sig.Site_map_and_set.Map.is_empty map_res then
                        error, dynamic
                      else (
                        let error, pair_list =
                          Ckappa_sig.Site_map_and_set.Map.fold
                            (fun site' state (error, current_list) ->
                              let pair_list =
                                ( site',
                                  (state.Cckappa_sig.min, state.Cckappa_sig.max)
                                )
                                :: current_list
                              in
                              error, pair_list)
                            map_res (error, [])
                        in
                        let handler = get_mvbdu_handler dynamic in
                        let error, handler, bdu_test =
                          Ckappa_sig.Views_bdu
                          .mvbdu_of_reverse_sorted_range_list parameters handler
                            error pair_list
                        in
                        let dynamic = set_mvbdu_handler handler dynamic in
                        (*check bdu_test with bdu in result*)
                        let error, bdu_X =
                          match
                            Covering_classes_type.AgentCV_map_and_set.Map
                            .find_option_without_logs parameters error
                              (agent_type, cv_id) fixpoint_result
                          with
                          | error, None -> error, bdu_false
                          | error, Some bdu -> error, bdu
                        in
                        let handler = get_mvbdu_handler dynamic in
                        (*if it does not overlap then answer false, otherwise
                          continue*)
                        let error, handler, bdu_inter =
                          Ckappa_sig.Views_bdu.mvbdu_and parameters handler
                            error bdu_test bdu_X
                        in
                        let dynamic = set_mvbdu_handler handler dynamic in
                        (*check if it is overlap or not?*)
                        if Ckappa_sig.Views_bdu.equal bdu_inter bdu_false then
                          raise (False (error, dynamic))
                        else
                          (*continue to iterate*)
                          error, dynamic
                      ))
                    (error, dynamic) get_pair_list
                in
                error, dynamic
              ))
          pattern.Cckappa_sig.views dynamic
      in
      let precondition =
        Communication.refine_information_about_state_of_sites_in_precondition
          precondition
          (fun
            parameters
            error
            (dynamic : Analyzer_headers.global_dynamic_information)
            (current_path : Communication.path)
            former_answer
          ->
            let error =
              scan_bot ~also_scan_top:false __POS__ parameters error
                former_answer " from overlying domain"
            in
            let error, answer_contact_map =
              let rec aux acc error =
                match acc with
                | [] -> error, Usual_domains.Any
                | step :: tl ->
                  let error, agent_type =
                    match
                      Ckappa_sig.Agent_id_map_and_set.Map
                      .find_option_without_logs parameters error
                        current_path.Communication.agent_id
                        store_agent_name_from_pattern
                    with
                    | error, None -> error, Ckappa_sig.dummy_agent_name
                    | error, Some a -> error, a
                  in
                  let error, state_dic =
                    Misc_sa.unsome
                      (Ckappa_sig
                       .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                       .get parameters error
                         (agent_type, step.Communication.site_out)
                         kappa_handler.Cckappa_sig.states_dic)
                      (fun error ->
                        Exception.warn parameters error __POS__ Exit
                          (Ckappa_sig.Dictionary_of_States.init ()))
                  in
                  let state =
                    Ckappa_sig.C_Lnk_type
                      ( step.Communication.agent_type_in,
                        step.Communication.site_in )
                  in
                  let error, b =
                    Ckappa_sig.Dictionary_of_States.member parameters error
                      (Ckappa_sig.Binding state) state_dic
                  in
                  if b then (
                    let error, answer_contact_map =
                      match
                        Ckappa_sig.Dictionary_of_States.allocate parameters
                          error Ckappa_sig.compare_unit_state_index
                          (Ckappa_sig.Binding state) () Misc_sa.const_unit
                          state_dic
                      with
                      | error, None ->
                        Exception.warn parameters error __POS__ Exit
                          Usual_domains.Undefined
                      | error, Some (state, _, _, _) ->
                        (match
                           Ckappa_sig.AgentSiteState_map_and_set.Map
                           .find_option_without_logs parameters error
                             (agent_type, step.Communication.site_out, state)
                             dual_contact_map
                         with
                        | error, None ->
                          Exception.warn parameters error __POS__ Exit
                            Usual_domains.Undefined
                        | error, Some _ -> aux tl error)
                    in
                    error, answer_contact_map
                  ) else
                    Exception.warn parameters error __POS__ Exit
                      Usual_domains.Undefined
              in
              aux current_path.Communication.relative_address error
            in
            let error =
              scan_bot_warn ~also_scan_top:false __POS__ parameters error
                answer_contact_map " in the contact map"
            in
            let error, dynamic, new_answer =
              let rec aux dynamic path =
                let step_list = path.Communication.relative_address in
                match step_list with
                | step :: _ ->
                  let error, (dynamic, new_answer) =
                    precondition_step_pattern parameters error step aux dynamic
                      path pattern kappa_handler bdu_false bdu_true
                      site_correspondence site_correspondence_map
                      store_covering_classes_id fixpoint_result
                  in
                  let error =
                    scan_bot ~also_scan_top:false __POS__ parameters error
                      new_answer " whie navigating"
                  in
                  let update_answer =
                    Usual_domains.glb_list new_answer former_answer
                  in
                  error, dynamic, update_answer
                | [] ->
                  let error, dynamic, new_answer =
                    precondition_empty_pattern parameters error path pattern
                      dynamic bdu_false bdu_true store_agent_name_from_pattern
                      site_correspondence site_correspondence_map
                      store_covering_classes_id fixpoint_result
                  in
                  let error =
                    scan_bot ~also_scan_top:false __POS__ parameters error
                      new_answer " while navigating (empty path)"
                  in
                  let update_answer =
                    Usual_domains.glb_list new_answer former_answer
                  in
                  error, dynamic, update_answer
              in
              aux dynamic current_path
            in
            (*---------------------------------------------------------*)
            let update_answer =
              Usual_domains.glb_list answer_contact_map new_answer
            in
            error, dynamic, update_answer)
      in
      error, (dynamic, precondition), true
    with False (error, dynamic) -> error, (dynamic, precondition), false

  (* the flag can be safely ignored in this abstract domain *)
  let maybe_reachable static dynamic error _flag pattern precondition =
    let error, (dynamic, precondition), maybe_reachable =
      maybe_reachable_aux static dynamic error pattern precondition
    in
    if maybe_reachable then
      error, dynamic, Some precondition
    else
      error, dynamic, None

  (***********************************************************)
  (*deal with views*)

  let compute_bdu_update_aux static dynamic error bdu_test list_a bdu_X =
    let parameters = get_parameter static in
    let parameter_views =
      Remanent_parameters.update_prefix parameters "\t\t\t"
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_inter =
      Ckappa_sig.Views_bdu.mvbdu_and parameter_views handler error bdu_X
        bdu_test
    in
    (*redefine with modification list*)
    let error, handler, bdu_redefine =
      Ckappa_sig.Views_bdu.mvbdu_redefine parameter_views handler error
        bdu_inter list_a
    in
    (*do the union of bdu_redefine and bdu_X*)
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_or parameter_views handler error bdu_redefine
        bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (*************************************************************)

  let compute_bdu_update_views static dynamic error bdu_test list_a bdu_X =
    let error, dynamic, bdu_result =
      compute_bdu_update_aux static dynamic error bdu_test list_a bdu_X
    in
    error, dynamic, bdu_result

  (***************************************************************)

  let compute_bdu_update_creation static dynamic error bdu_creation bdu_X =
    let parameters = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_or parameters handler error bdu_creation bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (***************************************************************)

  let compute_bdu_update_side_effects static dynamic error bdu_test list_a bdu_X
      =
    let parameters = get_parameter static in
    let parameter_views =
      Remanent_parameters.update_prefix parameters "\t\t\t"
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_inter =
      Ckappa_sig.Views_bdu.mvbdu_and parameter_views handler error bdu_X
        bdu_test
    in
    (*redefine with modification list*)
    let error, handler, bdu_redefine =
      Ckappa_sig.Views_bdu.mvbdu_redefine parameter_views handler error
        bdu_inter list_a
    in
    (*do the union of bdu_redefine and bdu_X*)
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_or parameter_views handler error bdu_redefine
        bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (****************************************************************)

  let compute_views_test_enabled static dynamic error rule_id event_list =
    let parameters = get_parameter static in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static error
    in
    let error, proj_bdu_test_restriction =
      match
        Ckappa_sig.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Covering_classes_type.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*------------------------------------------------------------*)
    let error, dynamic, event_list =
      (*-----------------------------------------------------------*)
      (*deal with views*)
      Covering_classes_type.AgentsCV_setmap.Map.fold
        (fun (agent_id, agent_type, cv_id) _ (error, dynamic, event_list) ->
          let error, dynamic, bdu_false =
            get_mvbdu_false static dynamic error
          in
          let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
          let error, store_modif_list_restriction_map =
            get_store_modif_list_restriction_map static error
          in
          (*print*)
          let error =
            dump_cv_label static error
              (Remanent_parameters.get_dump_reachability_analysis_diff
                 parameters)
              (agent_type, cv_id)
          in
          (*-----------------------------------------------------*)
          let store_result = get_fixpoint_result dynamic in
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map
              .find_option_without_logs parameters error (agent_type, cv_id)
                store_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, bdu_test =
            match
              Covering_classes_type.AgentsCV_setmap.Map.find_option
                (agent_id, agent_type, cv_id)
                proj_bdu_test_restriction
            with
            | None -> error, bdu_true
            | Some bdu -> error, bdu
          in
          let error, dynamic, bdu_update =
            match
              Covering_classes_type.AgentsRuleCV_map_and_set.Map
              .find_option_without_logs parameters error
                (agent_id, agent_type, rule_id, cv_id)
                store_modif_list_restriction_map
            with
            | error, None -> error, dynamic, bdu_X
            | error, Some list_a ->
              let error, dynamic, bdu_update =
                compute_bdu_update_views static dynamic error bdu_test list_a
                  bdu_X
              in
              error, dynamic, bdu_update
          in
          let error, dynamic, event_list =
            add_link ~title:"" error static dynamic (agent_type, cv_id)
              bdu_update event_list
          in
          error, dynamic, event_list)
        proj_bdu_test_restriction
        (error, dynamic, event_list)
    in
    error, dynamic, event_list

  (**************************************************************************)
  (*deal with creation*)

  let compute_views_creation_enabled static dynamic error rule_id event_list =
    let parameters = get_parameter static in
    let error, store_bdu_creation =
      get_store_proj_bdu_creation_restriction static error
    in
    let error, bdu_creation_map =
      match
        Ckappa_sig.Rule_setmap.Map.find_option rule_id store_bdu_creation
      with
      | None -> error, Covering_classes_type.AgentCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      Covering_classes_type.AgentCV_setmap.Map.fold
        (fun (agent_type, cv_id) bdu_creation (error, dynamic, event_list) ->
          let error, dynamic, bdu_false =
            get_mvbdu_false static dynamic error
          in
          let fixpoint_result = get_fixpoint_result dynamic in
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map
              .find_option_without_logs parameters error (agent_type, cv_id)
                fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, dynamic, bdu_update =
            compute_bdu_update_creation static dynamic error bdu_creation bdu_X
          in
          let error, dynamic, event_list =
            add_link ~title:"Dealing with creation" error static dynamic
              (agent_type, cv_id) bdu_update event_list
          in
          error, dynamic, event_list)
        bdu_creation_map
        (error, dynamic, event_list)
    in
    error, dynamic, event_list

  (**************************************************************************)

  (**************************************************************************)
  (*apply rules in different cases*)

  let can_we_prove_this_is_the_first_application precondition =
    match Communication.is_the_rule_applied_for_the_first_time precondition with
    | Usual_domains.Sure_value b ->
      if b then
        true
      else
        false
    | Usual_domains.Maybe -> false

  let compute_views_enabled static dynamic error rule_id precondition =
    (*-----------------------------------------------------------------------*)
    (*deal with views*)
    let error, dynamic, event_list =
      compute_views_test_enabled static dynamic error rule_id []
    in
    (*-----------------------------------------------------------------------*)
    (*deal with creation*)
    let error, dynamic, event_list =
      let b = can_we_prove_this_is_the_first_application precondition in
      if b then (
        (*if Sure_value is true then compute creation_enabled*)
        let error, dynamic, event_list =
          compute_views_creation_enabled static dynamic error rule_id event_list
        in
        error, dynamic, event_list
      ) else
        (*Sure_value is false*)
        error, dynamic, event_list
    in
    error, dynamic, event_list

  (**************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let error, dynamic, event_list =
      compute_views_enabled static dynamic error rule_id precondition
    in
    error, dynamic, (precondition, event_list)

  let apply_one_side_effect static dynamic error _rule_id
      (_, (agent_name, site, state)) precondition =
    let parameters = get_parameter static in
    let error, site_to_site_list = get_site_to_renamed_site_list static error in
    let error, site_list_opt =
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
      .unsafe_get parameters error (agent_name, site) site_to_site_list
    in
    let site_list =
      match site_list_opt with
      | None -> []
      | Some l -> l
    in
    let error, dynamic, event_list =
      List.fold_left
        (fun (error, dynamic, event_list) (cv_id, site') ->
          let list_test = [ site', state ] in
          let list_modif = [ site', Ckappa_sig.dummy_state_index ] in
          let handler = get_mvbdu_handler dynamic in
          let error, handler, bdu_test =
            Ckappa_sig.Views_bdu.mvbdu_of_reverse_sorted_association_list
              parameters handler error list_test
          in
          let error, handler, list_modif =
            Ckappa_sig.Views_bdu.build_association_list parameters handler error
              list_modif
          in
          let dynamic = set_mvbdu_handler handler dynamic in
          let fixpoint_result = get_fixpoint_result dynamic in
          let error, dynamic, bdu_false =
            get_mvbdu_false static dynamic error
          in
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map
              .find_option_without_logs parameters error (agent_name, cv_id)
                fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, dynamic, bdu_update =
            compute_bdu_update_side_effects static dynamic error bdu_test
              list_modif bdu_X
          in
          let error, dynamic, event_list =
            add_link ~title:"Dealing with side effects" error static dynamic
              (agent_name, cv_id) bdu_update event_list
          in
          error, dynamic, event_list)
        (error, dynamic, []) site_list
    in
    error, dynamic, (precondition, event_list)

  (**************************************************************************)
  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event_list _static dynamic error _event_list = error, dynamic, []

  (**************************************************************************)
  (*main print of fixpoint*)

  let print_bdu_update_map parameters error handler_kappa result =
    AgentCV_map_and_set.Map.fold
      (fun (agent_type, cv_id) bdu_update error ->
        let error', agent_string =
          Handler.string_of_agent parameters error handler_kappa agent_type
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "agent_type:%i:%s:cv_id:%i"
            (Ckappa_sig.int_of_agent_name agent_type)
            agent_string
            (Covering_classes_type.int_of_cv_id cv_id)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        let () = Ckappa_sig.Views_bdu.print parameters bdu_update in
        error)
      result error

  (**************************************************************************)

  let smash_map decomposition ~show_dep_with_dimmension_higher_than:_dim_min
      parameters handler error _handler_kappa
      (*store_new_index_pair_map*)
        site_correspondence result =
    let error', handler, mvbdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    Covering_classes_type.AgentCV_map_and_set.Map.fold
      (fun (agent_type, cv_id) bdu (error, handler, output) ->
        let error, handler, list = decomposition parameters handler error bdu in
        let error, (_, map2) =
          get_list_of_sites_correspondence_map parameters error agent_type cv_id
            site_correspondence
        in
        let rename_site parameters error site_type =
          let error, site_type =
            match
              Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.get
                parameters error site_type map2
            with
            | error, None ->
              Exception.warn parameters error __POS__ Exit
                Ckappa_sig.dummy_site_name_minus1
            | error, Some i -> error, i
          in
          error, site_type
        in
        List.fold_left
          (fun (error, handler, output) bdu ->
            let error, handler, lvar =
              Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameters handler
                error bdu
            in
            (*list: ckappa_sig.c_site_name list*)
            let error, handler, list =
              Ckappa_sig.Views_bdu.extensional_of_variables_list parameters
                handler error lvar
            in
            (*asso take (key * key) list *)
            let error, asso =
              List.fold_left
                (fun (error, list) i ->
                  let error, new_name = rename_site parameters error i in
                  error, (i, new_name) :: list)
                (error, []) (List.rev list)
            in
            let error, handler, hconsed_asso =
              Ckappa_sig.Views_bdu.build_renaming_list parameters handler error
                asso
            in
            let error, handler, renamed_mvbdu =
              Ckappa_sig.Views_bdu.mvbdu_rename parameters handler error bdu
                hconsed_asso
            in
            let error, handler, hconsed_vars =
              Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameters handler
                error renamed_mvbdu
            in
            let error, cv_map_opt =
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error agent_type output
            in
            let error, cv_map =
              match cv_map_opt with
              | None -> error, Wrapped_modules.LoggedIntMap.empty
              | Some map -> error, map
            in
            let error, handler, cv_map' =
              Ckappa_sig.Views_bdu.store_by_variables_list
                Wrapped_modules.LoggedIntMap.find_default_without_logs
                Wrapped_modules.LoggedIntMap.add_or_overwrite mvbdu_true
                Ckappa_sig.Views_bdu.mvbdu_and parameters handler error
                hconsed_vars renamed_mvbdu cv_map
            in
            let error, output =
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                parameters error agent_type cv_map' output
            in
            error, handler, output)
          (error, handler, output) list)
      result
      (let error, agent_map =
         Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
           parameters error 0
       in
       error, handler, agent_map)

  (**************************************************************************)

  let stabilise_bdu_update_map_gen_decomposition decomposition ~smash
      ~show_dep_with_dimmension_higher_than:dim_min parameters handler error
      handler_kappa site_correspondence result =
    let log = Remanent_parameters.get_logger parameters in
    if smash then (
      let error', handler, output =
        smash_map decomposition ~show_dep_with_dimmension_higher_than:dim_min
          parameters handler error handler_kappa site_correspondence result
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      let error, (handler, list) =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error agent_type map (handler, list) ->
            let error, agent_string =
              try
                Handler.string_of_agent parameters error handler_kappa
                  agent_type
              with _ ->
                Exception.warn parameters error __POS__ Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
            in
            (*-----------------------------------------------------------*)
            Wrapped_modules.LoggedIntMap.fold
              (fun _ mvbdu (error, (handler, list)) ->
                let error, handler =
                  if local_trace || Remanent_parameters.get_trace parameters
                  then (
                    let () = Loggers.fprintf log "INTENSIONAL DESCRIPTION:" in
                    let () = Loggers.print_newline log in
                    let () = Ckappa_sig.Views_bdu.print parameters mvbdu in
                    let () = Loggers.fprintf log "EXTENSIONAL DESCRIPTION:" in
                    let () = Loggers.print_newline log in
                    error, handler
                  ) else
                    error, handler
                in
                let error, (handler, translation) =
                  Translation_in_natural_language.translate parameters handler
                    error
                    (fun _ e i -> e, i)
                    mvbdu
                in
                (*-------------------------------------------------------*)
                ( error,
                  ( handler,
                    (agent_string, agent_type, mvbdu, translation) :: list ) ))
              map
              (error, (handler, list)))
          output (handler, [])
      in
      error, handler, List.rev list
    ) else (
      let error, (handler, list) =
        Covering_classes_type.AgentCV_map_and_set.Map.fold
          (fun (agent_type, cv_id) bdu_update (error, (handler, list)) ->
            let error, agent_string =
              try
                Handler.string_of_agent parameters error handler_kappa
                  agent_type
              with _ ->
                Exception.warn parameters error __POS__ Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
            in
            (*-----------------------------------------------------------*)
            let () =
              if local_trace || Remanent_parameters.get_trace parameters then (
                let () =
                  Loggers.fprintf log "agent_type:%i:%s:cv_id:%i"
                    (Ckappa_sig.int_of_agent_name agent_type)
                    agent_string
                    (Covering_classes_type.int_of_cv_id cv_id)
                in
                Loggers.print_newline log
              )
            in
            (*------------------------------------------------------------*)
            let error, (_, map2) =
              get_list_of_sites_correspondence_map parameters error agent_type
                cv_id site_correspondence
            in
            (*-----------------------------------------------------------*)
            let error, handler, list' =
              decomposition parameters handler error bdu_update
            in
            (*-----------------------------------------------------------*)
            let error, (handler, list) =
              List.fold_left
                (fun (error, (handler, list)) mvbdu ->
                  let error, handler =
                    if local_trace || Remanent_parameters.get_trace parameters
                    then (
                      let () = Loggers.fprintf log "INTENSIONAL DESCRIPTION:" in
                      let () = Loggers.print_newline log in
                      let () = Ckappa_sig.Views_bdu.print parameters mvbdu in
                      let () = Loggers.fprintf log "EXTENSIONAL DESCRIPTION:" in
                      let () = Loggers.print_newline log in
                      error, handler
                    ) else
                      error, handler
                  in
                  let rename_site parameters error site_type =
                    let error, site_type =
                      match
                        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif
                        .get parameters error site_type map2
                      with
                      | error, None ->
                        Exception.warn parameters error __POS__ Exit
                          Ckappa_sig.dummy_site_name_minus1
                      | error, Some i -> error, i
                    in
                    error, site_type
                  in
                  let error, (handler, translation) =
                    Translation_in_natural_language.translate parameters handler
                      error rename_site mvbdu
                  in
                  (*----------------------------------------------------*)
                  ( error,
                    ( handler,
                      (agent_string, agent_type, mvbdu, translation) :: list ) ))
                (error, (handler, list))
                list'
            in
            error, (handler, list))
          result
          (error, (handler, []))
      in
      error, handler, List.rev list
    )

  let print_bdu_update_map_gen_decomposition decomposition ~sort ~smash
      ~show_dep_with_dimmension_higher_than:dim_min parameters handler error
      handler_kappa site_correspondence result =
    let error, handler, list =
      stabilise_bdu_update_map_gen_decomposition decomposition ~smash
        ~show_dep_with_dimmension_higher_than:dim_min parameters handler error
        handler_kappa site_correspondence result
    in
    let error, list =
      if sort then
        Tools_kasa.sort_list
          (fun parameters error (agent_string, agent_id, _, translation) ->
            match translation with
            | Translation_in_natural_language.Range (x, _) ->
              let error, x =
                Handler.string_of_site_contact_map parameters error
                  handler_kappa agent_id x
              in
              error, (agent_string, x)
            | Translation_in_natural_language.Equiv _
            | Translation_in_natural_language.No_known_translation _
            | Translation_in_natural_language.Partition _
            | Translation_in_natural_language.Imply _ ->
              error, (agent_string, ""))
          parameters error list
      else
        error, list
    in

    let error =
      List.fold_left
        (fun error (agent_string, agent_type, _, translation) ->
          Translation_in_natural_language.print
            ~show_dep_with_dimmension_higher_than:dim_min parameters
            handler_kappa error agent_string agent_type translation)
        error list
    in
    error, handler

  (****************************************************************)
  (*Print for NON-Relational properties*)

  let print_bdu_update_map_cartesian_abstraction parameters handler error
      handler_kappa =
    print_bdu_update_map_gen_decomposition ~sort:true ~smash:true
      ~show_dep_with_dimmension_higher_than:1
      Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction parameters handler error
      handler_kappa

  (*****************************************************************)
  (*Print for relational properties*)

  let print_bdu_update_map_cartesian_decomposition parameters handler error
      handler_kappa =
    print_bdu_update_map_gen_decomposition ~smash:true
      ~show_dep_with_dimmension_higher_than:
        (if
           Remanent_parameters
           .get_hide_one_d_relations_from_cartesian_decomposition parameters
         then
           2
         else
           1)
      Ckappa_sig.Views_bdu.mvbdu_full_cartesian_decomposition parameters handler
      error handler_kappa

  (*****************************************************************)
  let print_separating_edges parameters handler error compil _handler_kappa list
      =
    let log = Remanent_parameters.get_logger parameters in
    let () = Loggers.print_newline log in
    let () =
      Loggers.fprintf log
        "------------------------------------------------------------"
    in
    let () = Loggers.print_newline log in
    let error, handler =
      if Mods.IntMap.is_empty list then (
        let () = Loggers.fprintf log "There may be no separating transitions" in
        error, handler
      ) else (
        let () =
          Loggers.fprintf log "The following transitions are separating:"
        in
        let () = Loggers.print_newline log in
        let () = Loggers.print_newline log in
        let error =
          Mods.IntMap.fold
            (fun r_id l error ->
              let error, r =
                Handler.string_of_rule parameters error compil
                  (Ckappa_sig.rule_id_of_int r_id)
              in
              let () = Loggers.fprintf log "  * %s:" r in
              let () = Loggers.print_newline log in
              let error =
                List.fold_left
                  (fun error (s1, s2) ->
                    let () = Loggers.fprintf log "      %s -> %s" s1 s2 in
                    let () = Loggers.print_newline log in
                    error)
                  error l
              in
              let () = Loggers.print_newline log in
              error)
            list error
        in
        let () = Loggers.print_newline log in
        error, handler
      )
    in
    error, handler

  let print_result_fixpoint_aux parameters handler error handler_kappa
      (*store_new_index_pair_map*)
        site_correspondence result =
    let log = Remanent_parameters.get_logger parameters in
    if Remanent_parameters.get_dump_reachability_analysis_result parameters then (
      let error =
        if local_trace || Remanent_parameters.get_trace parameters then (
          let () = Loggers.fprintf log "" in
          let () = Loggers.print_newline log in
          let () =
            Loggers.fprintf log
              "------------------------------------------------------------"
          in
          let () = Loggers.print_newline log in
          let () = Loggers.fprintf log "* Fixpoint iteration :" in
          let () = Loggers.print_newline log in
          let () =
            Loggers.fprintf log
              "------------------------------------------------------------"
          in
          let () = Loggers.print_newline log in
          let error =
            print_bdu_update_map parameters error handler_kappa result
          in
          error
        ) else
          error
      in
      let () = Loggers.print_newline log in
      let () =
        Loggers.fprintf log
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline log in
      let () = Loggers.fprintf log "* Non relational properties:" in
      let () = Loggers.print_newline log in
      let () =
        Loggers.fprintf log
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline log in
      let error, handler =
        print_bdu_update_map_cartesian_abstraction parameters handler error
          handler_kappa site_correspondence result
      in
      let () = Loggers.print_newline log in
      let () =
        Loggers.fprintf log
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline log in
      let () = Loggers.fprintf log "* Relational properties:" in
      let () = Loggers.print_newline log in
      let () =
        Loggers.fprintf log
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline log in
      let error, handler =
        print_bdu_update_map_cartesian_decomposition ~sort:false parameters
          handler error handler_kappa site_correspondence result
      in
      error, handler
    ) else
      error, handler

  (************************************************************************)

  let print_fixpoint_result static dynamic error _loggers =
    let parameters = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let site_correspondence = get_site_correspondence_array static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let handler = get_mvbdu_handler dynamic in
    let compil = get_compil static in
    let error, handler =
      if
        local_trace
        || Remanent_parameters.get_compute_separating_transitions parameters
      then (
        match get_separating_transitions dynamic with
        | None -> error, handler
        | Some l ->
          print_separating_edges parameters handler error compil kappa_handler l
      ) else
        error, handler
    in
    let error, handler =
      if
        local_trace
        || Remanent_parameters.get_dump_reachability_analysis_result parameters
      then
        print_result_fixpoint_aux parameters handler error kappa_handler
          site_correspondence fixpoint_result
      else
        error, handler
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, ()

  (**************************************************************************)

  let print ?dead_rules static dynamic error loggers =
    let dead_rules =
      match dead_rules with
      | None -> Analyzer_headers.dummy_dead_rules
      | Some f -> f
    in
    let parameters = get_parameter static in
    let error, dynamic =
      (* local traces *)
      if
        Remanent_parameters.get_compute_local_traces parameters
        || Remanent_parameters.get_compute_separating_transitions parameters
      then (
        let handler_kappa = get_kappa_handler static in
        let handler = get_mvbdu_handler dynamic in
        let compil = get_compil static in
        let site_correspondence = get_site_correspondence_array static in
        let error, handler, output =
          smash_map
            (fun _parameters handler error a -> error, handler, [ a ])
            parameters handler error ~show_dep_with_dimmension_higher_than:1
            handler_kappa site_correspondence
            (get_fixpoint_result dynamic)
        in
        let error, log_info, handler, bridges, transition_systems_length =
          if
            Remanent_parameters.get_compute_separating_transitions parameters
            && Remanent_parameters.get_use_macrotransitions_in_local_traces
                 parameters
          then (
            let parameters' =
              Remanent_parameters.set_use_macrotransitions_in_local_traces
                parameters false
            in
            let error, log_info, handler, bridges, _ =
              Agent_trace.agent_trace parameters' (get_log_info dynamic) error
                dead_rules handler
                (get_global_static_information static)
                handler_kappa compil output
            in
            let parameters' =
              Remanent_parameters.set_compute_separating_transitions parameters
                false
            in
            let error, log_info, handler, _, transition_systems_length =
              Agent_trace.agent_trace parameters' log_info error dead_rules
                handler
                (get_global_static_information static)
                handler_kappa compil output
            in
            error, log_info, handler, bridges, transition_systems_length
          ) else
            Agent_trace.agent_trace parameters (get_log_info dynamic) error
              dead_rules handler
              (get_global_static_information static)
              handler_kappa compil output
        in
        let dynamic =
          match bridges with
          | None -> dynamic
          | Some bridges -> set_separating_transitions bridges dynamic
        in
        let dynamic =
          match transition_systems_length with
          | None -> dynamic
          | Some transition_system_length ->
            set_transition_system_length transition_system_length dynamic
        in
        error, set_mvbdu_handler handler (set_log_info log_info dynamic)
      ) else
        error, dynamic
    in
    let error, dynamic, () =
      print_fixpoint_result static dynamic error loggers
    in
    error, dynamic, ()

  (*-----------------------------------------------*)

  let export_contact_map static dynamic error kasa_state =
    match get_ranges dynamic with
    | None -> error, dynamic, kasa_state
    | Some ranges ->
      let kappa_handler = get_kappa_handler static in
      let handler = get_mvbdu_handler dynamic in
      let parameters = get_parameter static in
      let contact_map = Preprocess.init_contact_map in
      (*-----------------------------------------------*)
      let error, (handler, contact_map) =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error ag_type site_map (handler, contact_map) ->
            let error, contact_map =
              Preprocess.declare_agent parameters error ag_type contact_map
            in
            Wrapped_modules.LoggedIntMap.fold
              (fun _ mvbdu (error, (handler, contact_map)) ->
                let error, handler, list_of_states =
                  Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler
                    error mvbdu
                in
                match list_of_states with
                | [] -> error, (handler, contact_map)
                | [ (site_type, _) ] :: _ ->
                  let error, contact_map =
                    Preprocess.declare_site parameters error ag_type site_type
                      contact_map
                  in
                  let error, site =
                    Handler.translate_site parameters error kappa_handler
                      ag_type site_type
                  in
                  (match site with
                  | Ckappa_sig.Internal _ ->
                    let error, contact_map =
                      List.fold_left
                        (fun (error, contact_map) l ->
                          match l with
                          | [ (site_type', state) ] when site_type' = site_type
                            ->
                            Preprocess.add_internal_state_in_contact_map
                              parameters error (ag_type, site_type) state
                              contact_map
                          | [] | _ :: _ ->
                            Exception.warn parameters error __POS__ Exit
                              contact_map)
                        (error, contact_map) list_of_states
                    in
                    error, (handler, contact_map)
                  | Ckappa_sig.Counter _ -> error, (handler, contact_map)
                  | Ckappa_sig.Binding _ ->
                    let error, contact_map =
                      List.fold_left
                        (fun (error, contact_map) l ->
                          match l with
                          | [ (site_type', state) ] when site_type' = site_type
                            ->
                            if state = Ckappa_sig.state_index_of_int 0 then
                              (* we ignore free sites *)
                              error, contact_map
                            else (
                              let error, dual =
                                Handler.dual parameters error kappa_handler
                                  ag_type site_type state
                              in
                              match dual with
                              | None ->
                                Exception.warn parameters error __POS__ Exit
                                  contact_map
                              | Some (ag_type', site_type', _) ->
                                Preprocess.add_link_in_contact_map parameters
                                  error (ag_type, site_type)
                                  (ag_type', site_type') contact_map
                            )
                          | [] | _ :: _ ->
                            Exception.warn parameters error __POS__ Exit
                              contact_map)
                        (error, contact_map) list_of_states
                    in
                    error, (handler, contact_map))
                | _ :: _ ->
                  Exception.warn parameters error __POS__ Exit
                    (handler, contact_map))
              site_map
              (error, (handler, contact_map)))
          ranges (handler, contact_map)
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      let kasa_state =
        Remanent_state.set_internal_contact_map Public_data.Medium contact_map
          kasa_state
      in
      error, dynamic, kasa_state

  let export_relation_properties_aux ~sort ~smash
      ~show_dep_with_dimmension_higher_than:dim_min decomposition domain_name
      parameters dynamic error handler_kappa site_correspondence fixpoint_result
      kasa_state =
    let handler = get_mvbdu_handler dynamic in
    (*convert result to list*)
    let error', handler, list =
      stabilise_bdu_update_map_gen_decomposition decomposition ~smash
        ~show_dep_with_dimmension_higher_than:dim_min parameters handler error
        handler_kappa site_correspondence fixpoint_result
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    (*store the information for relational properties*)
    let error', current_list =
      List.fold_left
        (fun (error, current_list) (agent_string, agent_type, _, translation) ->
          let error', current_list =
            Translation_in_natural_language
            .convert_views_internal_constraints_list
              ~show_dep_with_dimmension_higher_than:dim_min parameters
              handler_kappa error agent_string agent_type translation
              current_list
          in
          let error =
            Exception.check_point Exception.warn parameters error error' __POS__
              Exit
          in
          error, current_list)
        (error, []) list
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    (*------------------------------------------------------------------*)
    let internal_constraints_list =
      Remanent_state.get_internal_constraints_list kasa_state
    in
    let error, internal_constraints_list =
      match internal_constraints_list with
      | None -> Exception.warn parameters error __POS__ Exit []
      | Some l -> error, l
    in
    let error, current_list =
      if sort then
        Tools_kasa.sort_list
          (fun parameters error lemma ->
            Site_graphs.KaSa_site_graph.to_string parameters error
              lemma.Public_data.hyp)
          parameters error current_list
      else
        error, current_list
    in
    let pair_list = (domain_name, current_list) :: internal_constraints_list in
    let kasa_state =
      Remanent_state.set_internal_constraints_list pair_list kasa_state
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, kasa_state

  let export_relation_properties parameters dynamic error handler_kappa =
    let domain_name = "Views domain - relational properties" in
    export_relation_properties_aux ~sort:false ~smash:true
      ~show_dep_with_dimmension_higher_than:
        (if
           Remanent_parameters
           .get_hide_one_d_relations_from_cartesian_decomposition parameters
         then
           2
         else
           1)
      Ckappa_sig.Views_bdu.mvbdu_full_cartesian_decomposition domain_name
      parameters dynamic error handler_kappa

  let export_non_relation_properties parameters dynamic error handler_kappa =
    let domain_name = "Views domain - non relational properties" in
    export_relation_properties_aux ~sort:true ~smash:true
      ~show_dep_with_dimmension_higher_than:1
      Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction domain_name parameters
      dynamic error handler_kappa

  let export_views_properties_aux parameters error handler_kappa
      site_correspondence fixpoint_result dynamic kasa_state =
    (*non relational properties*)
    let error, dynamic, kasa_state =
      export_non_relation_properties parameters dynamic error handler_kappa
        site_correspondence fixpoint_result kasa_state
    in
    (*relational properties*)
    let error, dynamic, kasa_state =
      export_relation_properties parameters dynamic error handler_kappa
        site_correspondence fixpoint_result kasa_state
    in
    error, dynamic, kasa_state

  let export_views_properties static dynamic error kasa_state =
    let parameters = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let site_correspondence = get_site_correspondence_array static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let error, dynamic, kasa_state =
      export_views_properties_aux parameters error handler_kappa
        site_correspondence fixpoint_result dynamic kasa_state
    in
    error, dynamic, kasa_state

  let export_separating_edges static dynamic error kasa_state =
    match dynamic.local.separating_edges with
    | None -> error, dynamic, kasa_state
    | Some map ->
      let compil = get_compil static in
      let parameters = get_parameter static in
      let hide_reverse_rule =
        Remanent_parameters.get_hide_reverse_rule_without_label_from_dead_rules
          parameters
      in
      let original = hide_reverse_rule in
      let error, l =
        Mods.IntMap.fold
          (fun i c (error, l) ->
            let i = Ckappa_sig.rule_id_of_int i in
            let error, info =
              Handler.info_of_rule ~original ~with_rates:false parameters error
                compil i
            in
            let error, b1 = Handler.is_reverse parameters error compil i in
            let error, b2 = Handler.has_no_label parameters error compil i in
            let rule = Remanent_state.info_to_rule info in
            let rule =
              if b1 && b2 && hide_reverse_rule then
                Handler.hide rule
              else
                rule
            in
            error, (rule, c) :: l)
          map (error, [])
      in
      let kasa_state = Remanent_state.set_separating_transitions l kasa_state in
      error, dynamic, kasa_state

  let export_transition_system_length _static dynamic error kasa_state =
    match dynamic.local.transition_system_length with
    | None -> error, dynamic, kasa_state
    | Some l ->
      let kasa_state =
        Remanent_state.set_transition_system_length l kasa_state
      in
      error, dynamic, kasa_state

  let export static dynamic error kasa_state =
    (*export of contact map*)
    let error, dynamic, kasa_state =
      export_contact_map static dynamic error kasa_state
    in
    (*export of (non)relational properties*)
    let error, dynamic, kasa_state =
      export_views_properties static dynamic error kasa_state
    in
    let error, dynamic, kasa_state =
      export_separating_edges static dynamic error kasa_state
    in
    let error, dynamic, kasa_state =
      export_transition_system_length static dynamic error kasa_state
    in
    error, dynamic, kasa_state

  (**************************************************************************)

  let stabilize static dynamic error =
    let dim_min = 2 in
    let parameters = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let handler_kappa = get_kappa_handler static in
    let result = get_fixpoint_result dynamic in
    let site_correspondence = get_site_correspondence_array static in
    let error, handler, ranges =
      smash_map Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction
        ~show_dep_with_dimmension_higher_than:dim_min parameters handler error
        handler_kappa site_correspondence result
    in
    let dynamic = set_ranges ranges dynamic in
    error, set_mvbdu_handler handler dynamic, ()

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
