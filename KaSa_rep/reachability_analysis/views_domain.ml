(**
   * analyzer_sig.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
   * Last modification: Time-stamp: <Oct 13 2016>
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(* Before properly achieving separation of concepts. We introduce one
   monolithic domain that collect everything (as in the previous analyzer).*)

let direct_computation = false
let new_computation = false

let domain_name = "View domain"

let local_trace = false
(*  let compute_local_trace = false*)

module Domain =
struct

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : Bdu_static_views.bdu_analysis_static;
    }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)

  module AgentCV_map_and_set = Covering_classes_type.AgentCV_map_and_set
  module AgentIDCV_map_and_set = Covering_classes_type.AgentIDCV_map_and_set

  type local_dynamic_information =
    {
      fixpoint_result : Ckappa_sig.Views_bdu.mvbdu AgentCV_map_and_set.Map.t;
      domain_dynamic_information : Bdu_dynamic_views.bdu_analysis_dynamic;
      subviews: unit option ;
      ranges:
        Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t option ;
    }

  type dynamic_information =
    {
      local  : local_dynamic_information;
      global : Analyzer_headers.global_dynamic_information
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  (** bdu analysis static in static information*)
  let set_domain_static domain static =
    {
      static with
      domain_static_information = domain
    }

  let get_domain_static static = static.domain_static_information

  let lift f x = f (get_global_static_information x)

  let get_compilation_information static =
    lift Analyzer_headers.get_compilation_information static

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_bdu_common_static static = Analyzer_headers.get_bdu_common_static static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  let get_agent_name static = lift Analyzer_headers.get_agent_name static

  let get_side_effects static = lift Analyzer_headers.get_side_effects static

  let get_potential_side_effects static =
    lift Analyzer_headers.get_potential_side_effects static

  let get_covering_classes static =
    (get_domain_static static).Bdu_static_views.store_covering_classes

  let get_covering_classes_id static =
    (get_domain_static static).Bdu_static_views.store_covering_classes_id

  let get_pre_static static =
    (get_domain_static static).Bdu_static_views.store_pre_static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    {dynamic with global = gdynamic}

  (** handler *)
  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
      global = Analyzer_headers.set_mvbdu_handler handler
          (get_global_dynamic_information dynamic)
    }

  (** profiling *)
  let get_log_info dynamic =
    Analyzer_headers.get_log_info (get_global_dynamic_information dynamic)

  let set_log_info log_info dynamic =
    {
      dynamic with
      global = Analyzer_headers.set_log_info log_info
          (get_global_dynamic_information dynamic)
    }

  (** local dynamic information*)

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  (** fixpoint result local dynamic information*)
  let get_fixpoint_result dynamic =
    (get_local_dynamic_information dynamic).fixpoint_result

  let set_fixpoint_result result dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        fixpoint_result = result
      } dynamic

  let get_ranges dynamic =
    (get_local_dynamic_information dynamic).ranges

  let set_ranges ranges dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        ranges = Some ranges
      } dynamic

  (** bdu analysis dynamic in local dynamic information*)
  let get_domain_dynamic_information dynamic =
    (get_local_dynamic_information dynamic).domain_dynamic_information

  let set_domain_dynamic_information domain dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        domain_dynamic_information = domain
      } dynamic

  let get_store_update dynamic =
    (get_domain_dynamic_information dynamic).Bdu_dynamic_views.store_update

  let set_store_update update dynamic =
    set_domain_dynamic_information
      {
        (get_domain_dynamic_information dynamic) with
        Bdu_dynamic_views.store_update = update
      } dynamic

  let get_store_dual_contact_map dynamic =
    (get_domain_dynamic_information dynamic).Bdu_dynamic_views.store_dual_contact_map

  let set_store_dual_contact_map dual dynamic =
    set_domain_dynamic_information
      {
        (get_domain_dynamic_information dynamic) with
        Bdu_dynamic_views.store_dual_contact_map = dual
      } dynamic

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

  (*----------------------------------------------------------------------*)
  (*Instantiate of functions that store the static and dynamic information
    accordingly from the previous analyzer *)

  (**[get_bdu_false/true] from dynamic*)
  let get_mvbdu_false global_static dynamic error =
    let parameter = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_false =
      Ckappa_sig.Views_bdu.mvbdu_false
        parameter handler_bdu error
    in
    error,
    set_mvbdu_handler handler_bdu dynamic,
    bdu_false

  (** the initial build for mvbdu_true*)
  let get_mvbdu_true global_static dynamic error =
    let parameter = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true
        parameter handler_bdu error
    in
    error,
    set_mvbdu_handler handler_bdu dynamic,
    bdu_true

  (**************************************************************************)
  (** [scan_rule_set static] *)

  let scan_rule_set_static static dynamic error =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compiled = get_compil static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let potential_side_effects = get_potential_side_effects static in
    let log_info = get_log_info dynamic in
    let error, (handler_bdu, log_info, result) =
      Bdu_static_views.scan_rule_set
        parameter
        log_info
        handler_bdu
        error
        kappa_handler
        compiled
        potential_side_effects
    in
    let dynamic = set_log_info log_info dynamic in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let static = set_domain_static result static in
    error, static, dynamic

  let scan_rule_set_dynamic static dynamic error =
    let parameter = get_parameter static in
    let compiled = get_compil static in
    let kappa_handler = get_kappa_handler static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let store_pre_static = get_pre_static static in
    let covering_classes = get_covering_classes static in
    let covering_classes_id = get_covering_classes_id static in
    let potential_side_effects = get_potential_side_effects static in
    let log_info = get_log_info dynamic in
    let error, (handler_bdu, log_info, store_result) =
      Bdu_dynamic_views.scan_rule_set_dynamic
        parameter
        log_info
        error
        compiled
        kappa_handler
        handler_bdu
        store_pre_static
        covering_classes
        covering_classes_id
        potential_side_effects
    in
    let dynamic = set_log_info log_info dynamic in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let dynamic = set_domain_dynamic_information store_result dynamic in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    let log_info = Analyzer_headers.get_log_info dynamic in
    let error, log_info = StoryProfiling.StoryStats.add_event parameter error
        (StoryProfiling.Domain_initialization domain_name)
        None log_info
    in
    let dynamic = Analyzer_headers.set_log_info log_info dynamic in
    let error, init_bdu_analysis_static =
      Bdu_static_views.init_bdu_analysis_static parameter error
    in
    let init_global_static =
      {
        global_static_information = static;
        domain_static_information = init_bdu_analysis_static
      }
    in
    let init_fixpoint = AgentCV_map_and_set.Map.empty in
    let init_bdu_analysis_dynamic = Bdu_dynamic_views.init_bdu_analysis_dynamic
    in
    let init_global_dynamic =
      {
        global = dynamic;
        local =
          {
            fixpoint_result = init_fixpoint;
            domain_dynamic_information = init_bdu_analysis_dynamic;
            subviews = None ;
            ranges = None ;
          }}
    in
    let error, init_static, init_dynamic =
      scan_rule_set_static init_global_static init_global_dynamic error
    in
    let error, static, dynamic =
      scan_rule_set_dynamic init_static init_dynamic error
    in
    let log_info = get_log_info dynamic in
    let error, log_info = StoryProfiling.StoryStats.close_event parameter error
        (StoryProfiling.Domain_initialization domain_name)
        None log_info
    in
    let dynamic = set_log_info log_info dynamic in
    error, static, dynamic

  (** get type bdu_analysis_static*)
  let get_bdu_analysis_static static _dynamic error =
    let result = static.domain_static_information in
    error, result

  (**get type bdu_analysis_dynamic*)
  let get_bdu_analysis_dynamic _static dynamic error =
    let result = get_domain_dynamic_information dynamic in
    error, result

  let get_store_remanent_triple static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    let store_remanent_triple =
      result_static.Bdu_static_views.store_remanent_triple
    in
    error, store_remanent_triple

  (**************************************************************************)
  (**get type bdu_analysis_dynamic*)

  let get_store_covering_classes_modification_update_full _static dynamic error =
    let result = get_domain_dynamic_information dynamic in
    error, result.Bdu_dynamic_views.store_update

  (**************************************************************************)
  (* add updates_list into an event_list *)

  let dump_cv_label static dynamic error bool (agent_type, cv_id) = (*TODO: put title*)
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let error, site_correspondence =
      get_store_remanent_triple static dynamic error
    in
    if local_trace
    || Remanent_parameters.get_trace parameter
    || bool
    then
      let log = Remanent_parameters.get_logger parameter in
      let parameter_cv =
        Remanent_parameters.update_prefix parameter "Updating the views for"
      in
      let prefix = Remanent_parameters.get_prefix parameter_cv in
      (*---------------------------------------------------------*)
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
        | _ ->
          Exception.warn
            parameter error __POS__ Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      (*--------------------------------------------------------*)
      let error, site_correspondence =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_type site_correspondence
      in
      let error, site_correspondence =
        match site_correspondence with
        | None -> Exception.warn parameter error __POS__ Exit []
        | Some a -> error, a
      in
      (*get a list of sites in a covering class *)
      let error, site_correspondence =
        let rec aux list =
          match list with
          | [] -> Exception.warn  parameter error __POS__ Exit []
          | (h, list, _) :: _ when h = cv_id -> error, list
          | _ :: tail -> aux tail
        in aux site_correspondence
      in
      let () = Loggers.fprintf log "\t\t%s %s(" prefix agent_string in
      let error, _ =
        List.fold_left
          (fun (error, bool) site_type ->
             let error, site_string =
               try
                 Handler.string_of_site parameter error handler_kappa
                   agent_type site_type
               with
               | _ ->
                 Exception.warn
                   parameter error __POS__ Exit
                   (Ckappa_sig.string_of_site_name site_type)
             in
             let () =
               Loggers.fprintf log "%s%s" (if bool then "," else "") site_string
             in
             error, true)
          (error, false) site_correspondence
      in
      let () = Loggers.fprintf log ")" in
      let () = Loggers.print_newline log  in
      error
    else
      error

  (**************************************************************************)

  let updates_list2event_list ?title:(title="") static dynamic error agent_type
      cv_id event_list =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, store_covering_classes_modification_update_full =
      get_store_covering_classes_modification_update_full static dynamic error
    in
    let error, s1 =
      match
        Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
          parameter
          error
          (agent_type, cv_id)
          store_covering_classes_modification_update_full
      with
      | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*-----------------------------------------------------------------------*)
    (*print working list information*)
    let error =
      if local_trace
      || Remanent_parameters.get_dump_reachability_analysis_wl parameter
      then
        begin
          let log = Remanent_parameters.get_logger parameter in
          (*---------------------------------------------------------------*)
          let error, _agent_string =
            try
              Handler.string_of_agent parameter error kappa_handler agent_type
            with
            | _ -> Exception.warn
                     parameter error __POS__ Exit
                     (Ckappa_sig.string_of_agent_name agent_type)
          in
          (*---------------------------------------------------------------*)
          (*dump covering class label*)
          let error =
            if title <> ""
            then
              let error = (*TODO: different title*)
                dump_cv_label
                  static
                  dynamic
                  error
                  true
                  (agent_type, cv_id)
              in
              error
            else
              error
          in
          (*----------------------------------------------------------------*)
          let error =
            Ckappa_sig.Rule_map_and_set.Set.fold (fun rule_id error ->
                let compiled = get_compil static in
                let error, rule_id_string =
                  try
                    Handler.string_of_rule parameter error kappa_handler
                      compiled rule_id
                  with
                  | _ ->
                    Exception.warn
                      parameter error __POS__ Exit
                      (Ckappa_sig.string_of_rule_id rule_id)
                in
                let tab =
                  if title = "" then "\t" else "\t\t"
                in
                let () =
                  Loggers.fprintf log "%s%s(%s) should be investigated "
                    (Remanent_parameters.get_prefix parameter) tab
                    rule_id_string
                in
                let () = Loggers.print_newline log in error)
              s1 error
          in
          error
        end
      else error
    in
    (*-------------------------------------------------------------*)
    (*convert into an event list*)
    error,
    Ckappa_sig.Rule_map_and_set.Set.fold
      (fun rule_id event_list ->
         (Communication.Check_rule rule_id) :: event_list)
      s1 event_list

  (***************************************************************)

  let dump_view_diff static dynamic error (agent_type, cv_id) bdu_old bdu_union =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let error, site_correspondence =
      get_store_remanent_triple static dynamic error
    in
    if local_trace
    || Remanent_parameters.get_dump_reachability_analysis_diff parameter
    || Remanent_parameters.get_trace parameter
    then
      let prefix = Remanent_parameters.get_prefix parameter in
      let handler = get_mvbdu_handler dynamic in
      let error, handler, bdu_diff =
        Ckappa_sig.Views_bdu.mvbdu_xor
          parameter handler error bdu_old bdu_union
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      (*-----------------------------------------------------------------------*)
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
        | _ ->
          Exception.warn
            parameter error __POS__ Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      (*-----------------------------------------------------------------------*)
      (*list of sites in a covering class*)
      let error, site_correspondence =
        match Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get parameter
                error agent_type site_correspondence
        with
        | error, None ->
          Exception.warn
            parameter error __POS__ Exit []
        | error, Some a -> error, a
      in
      let error, site_correspondence =
        let rec aux list =
          match list with
          | [] ->
            Exception.warn parameter error __POS__ Exit []
          | (h, list, _) :: _ when h = cv_id -> error, list
          | _ :: tail -> aux tail
        in aux site_correspondence
      in
      (*-----------------------------------------------------------------------*)
      (*build a pair of coresspondence map:
        - map1: global -> local; map2: local -> global*)
      let error, (_map1, map2) =
        Bdu_static_views.new_index_pair_map parameter error site_correspondence
      in
      (*-----------------------------------------------------------------------
      *)
      (*  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter)  in*)
      (*-----------------------------------------------------------------------*)
      let error, dynamic =
        if local_trace
        || Remanent_parameters.get_trace parameter
        then
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%sINTENSIONAL DESCRIPTION:" prefix
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameter)
          in
          (*print bdu different: this will print in a format of bdu*)
          let () =
            Ckappa_sig.Views_bdu.print parameter bdu_diff
          in
          (*print a list of relations: this will print in a format readable*)
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%sEXTENSIONAL DESCRIPTION:" prefix
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameter)
          in
          error, dynamic
        else
          error, dynamic
      in
      (*this is a function to convert a bdu of diff into a list.
        return a pair: (bdu, and a pair of (site, state) list of list)*)
      let handler = get_mvbdu_handler dynamic in
      let error, handler, list =
        Ckappa_sig.Views_bdu.extensional_of_mvbdu
          parameter handler error bdu_diff
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      (*----------------------------------------------------------------------
        -*)
      (*print function for extentional description*)
      let error =
        List.fold_left
          (fun error l ->
             let error, bool =
               List.fold_left
                 (fun (error, bool) (site_type, state) ->
                    let error, site_type =
                      match Ckappa_sig.Site_map_and_set.Map.find_option
                              parameter error site_type map2
                      with
                      | error, None ->
                        Exception.warn
                          parameter error __POS__ Exit
                          Ckappa_sig.dummy_site_name
                      | error, Some i -> error, i
                    in
                    (*-----------------------------------------------------------------------*)
                    let error, site_string =
                      try
                        Handler.string_of_site parameter error handler_kappa
                          agent_type site_type
                      with
                      | _ ->
                        Exception.warn
                          parameter error __POS__ Exit (Ckappa_sig.string_of_site_name site_type)
                    in
                    let error, state_string =
                      try
                        Handler.string_of_state_fully_deciphered parameter error
                          handler_kappa agent_type site_type state
                      with
                      | _ ->
                        Exception.warn
                          parameter error __POS__ Exit
                          (Ckappa_sig.string_of_state_index state)
                    in
                    (*-----------------------------------------------------------------------*)
                    let () =
                      if bool
                      then
                        Loggers.fprintf (Remanent_parameters.get_logger parameter) ","
                      else
                        Loggers.fprintf (Remanent_parameters.get_logger parameter)
                          "\t\t\t%s%s(" prefix agent_string
                    in
                    let () = (*Print the information of views*)
                      Loggers.fprintf (Remanent_parameters.get_logger parameter)
                        "%s%s" site_string state_string
                    in
                    error, true
                 )
                 (error, false) l
             in
             (*-----------------------------------------------------------------------*)
             let () =
               if bool
               then
                 let () =
                   Loggers.fprintf (Remanent_parameters.get_logger parameter) ")" in
                 Loggers.print_newline (Remanent_parameters.get_logger parameter)
             in error)
          error list
      in
      let () =
        if list = []
        then ()
        else
          Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      error, dynamic
    else
      error, dynamic

  (**************************************************************************)

  let add_link ?title error static dynamic (agent_type, cv_id) bdu event_list =
    let parameter = get_parameter static in
    let log = Remanent_parameters.get_logger parameter in
    (*let error, store_remanent_triple =
      get_store_remanent_triple static dynamic error
      in*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store = get_fixpoint_result dynamic in
    let error, bdu_old =
      match
        Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
          parameter error
          (agent_type, cv_id)
          store
      with
      | error, None -> error, bdu_false
      | error, Some bdu -> error, bdu
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_union =
      Ckappa_sig.Views_bdu.mvbdu_or
        parameter handler error bdu_old bdu
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let updates_list = [] in
    (*-----------------------------------------------------------*)
    let error, dynamic, _title, is_new_views, updates_list =
      if Ckappa_sig.Views_bdu.equal bdu_old bdu_union
      then
        error, dynamic, title, false, updates_list
      else
        (*print different views*)
        let () =
          match title with
          | None -> ()
          | Some s ->
            if
              (local_trace
               || Remanent_parameters.get_dump_reachability_analysis_diff parameter
               || Remanent_parameters.get_trace parameter)
            then
              let () = Loggers.fprintf log "\t%s" s in
              let () = Loggers.print_newline log in
              let () = Loggers.print_newline log in
              ()
        in
        let error, dynamic =
          dump_view_diff
            static
            dynamic
            error
            (agent_type, cv_id)
            bdu_old
            bdu_union
        in
        let error, store =
          Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite parameter error
            (agent_type, cv_id) bdu_union store
        in
        let dynamic = set_fixpoint_result store dynamic in
        error, dynamic, None, true, (agent_type, cv_id) :: updates_list
    in
    (*-----------------------------------------------------------------------*)
    (*add updates_list into event list*)
    if is_new_views
    then
      (*print*)
      let () =
        if local_trace
        || Remanent_parameters.get_trace parameter
        || Remanent_parameters.get_dump_reachability_analysis_wl parameter
        then
          let () = Loggers.fprintf log "\tWake-up rules" in
          let () = Loggers.print_newline log in
          ()
      in
      let error, event_list =
        List.fold_left (fun (error, event_list) (agent_type, cv_id) ->
            updates_list2event_list
              ~title:"Dealing with"
              static
              dynamic
              error
              agent_type
              cv_id
              event_list
          ) (error, event_list) updates_list
      in
      error, dynamic, event_list
    else
      (*let () =
        let () =
        Loggers.fprintf log "\tInitial state is empty"
        in
        let () = Loggers.print_newline log in
        let () = Loggers.print_newline log in
        ()
        in*)
      error, dynamic, event_list

  (***************************************************************************)
  (*get map restriction from covering classes*)

  let get_pair_list static error agent triple_list =
    let parameter = get_parameter static in
    let error, get_pair_list =
      List.fold_left (fun (error, current_list) (cv_id, list, set) ->
          (*------------------------------------------------------------*)
          (*new index for site type in covering class*)
          let error, (map_new_index_forward, _) =
            Bdu_static_views.new_index_pair_map parameter error list
          in
          (*-------------------------------------------------------------*)
          let add site state (error, store) =
            let error, site' =
              match
                Ckappa_sig.Site_map_and_set.Map.find_option
                  parameter error site map_new_index_forward
              with
              | error, None ->
                Exception.warn
                  parameter error __POS__ Exit
                  Ckappa_sig.dummy_site_name
              | error, Some s -> error, s
            in
            Ckappa_sig.Site_map_and_set.Map.add
              parameter error site' state store
          in
          let error', map_res =
            Ckappa_sig.Site_map_and_set.Map.fold_restriction_with_missing_associations
              parameter error
              (fun site port ->
                 add site port.Cckappa_sig.site_state.Cckappa_sig.min)
              (*JF: we should check that port.site_state.min is equal to
                       port.site_state.max*)
              (fun site -> add site Ckappa_sig.dummy_state_index)
              set
              agent.Cckappa_sig.agent_interface
              Ckappa_sig.Site_map_and_set.Map.empty
          in
          let error =
            Exception.check_point
              Exception.warn  parameter error error'
              __POS__ Exit
          in
          error, ((cv_id, map_res) :: current_list)
        ) (error, []) triple_list
    in
    error, get_pair_list

  (***************************************************************************)
  (*build bdu restriction for initial state *)

  let bdu_build static dynamic error (pair_list: (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list) =
    let parameter = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_result =
      Bdu_static_views.build_bdu
        parameter
        handler
        error
        pair_list
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (**************************************************************************)

  let build_init_restriction static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_remanent_triple =
      get_store_remanent_triple static dynamic error
    in
    let error, (dynamic, event_list) =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameter error
        (fun parameter error _agent_id agent (dynamic, event_list) ->
           match agent with
           | Cckappa_sig.Unknown_agent _
           | Cckappa_sig.Ghost -> error, (dynamic, event_list)
           | Cckappa_sig.Dead_agent _ ->
             Exception.warn
               parameter error __POS__ Exit (dynamic, event_list)
           | Cckappa_sig.Agent agent ->
             let agent_type = agent.Cckappa_sig.agent_name in
             (*-------------------------------------------------------------*)
             let error, (dynamic, event_list) =
               match Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                       parameter error agent_type
                       store_remanent_triple
               with
               | error, Some l ->
                 let error, get_pair_list = get_pair_list static error agent l in
                 let error, (dynamic, event_list) =
                   List.fold_left (fun (error, (dynamic, event_list)) (cv_id, map_res) ->
                       let error, pair_list =
                         Ckappa_sig.Site_map_and_set.Map.fold
                           (fun site' state (error, current_list) ->
                              let pair_list = (site', state) :: current_list in
                              error, pair_list
                           ) map_res (error, [])
                       in
                       let error, dynamic, bdu_init =
                         bdu_build
                           static
                           dynamic
                           error
                           pair_list
                       in
                       (*----------------------------------------------------*)
                       let error, dynamic, event_list =
                         add_link
                           ~title:"Views in initial state"
                           error
                           static
                           dynamic
                           (agent_type, cv_id)
                           bdu_init
                           event_list
                       in
                       error, (dynamic, event_list)
                     ) (error, (dynamic, event_list)) get_pair_list
                 in
                 error, (dynamic, event_list)
               | error, None -> error, (dynamic, event_list)
             in
             error, (dynamic, event_list)
        ) init_state.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views (dynamic, [])
    in
    error, dynamic, event_list

  (**************************************************************)
  (**add initial state of kappa*)

  let add_initial_state static dynamic error init_state =
    let error, dynamic, event_list =
      build_init_restriction
        static
        dynamic
        error
        init_state
    in
    error, dynamic, event_list

  (****************************************************************)

  let get_store_proj_bdu_test_restriction static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_static_views.store_proj_bdu_test_restriction

  let get_store_proj_bdu_creation_restriction static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_static_views.store_proj_bdu_creation_restriction_map

  let get_store_proj_bdu_potential_restriction static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_static_views.store_proj_bdu_potential_restriction_map

  let get_store_modif_list_restriction_map static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_static_views.store_modif_list_restriction_map

  (*****************************************************************)

  exception False of Exception.method_handler * dynamic_information

  (****************************************************************)
  (*compute condition of bdu whether or not it is enable by doing the
    intersection of bdu_test and bdu_X*)

  let collect_bdu_enabled parameter error dynamic bdu_false fixpoint_result proj_bdu_test_restriction =
    let error, dynamic, _ =
      Covering_classes_type.AgentsCV_setmap.Map.fold
        (fun (agent_id, agent_type, cv_id) bdu_test (error, dynamic, map) ->
           (*------------------------------------------------------*)
           (*for each (agent_id, cv_id) a bdu*)
           let error, bdu_X =
             match
               Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                 parameter
                 error
                 (agent_type, cv_id)
                 fixpoint_result
             with
             | error, None -> error, bdu_false
             | error, Some bdu -> error, bdu
           in
           (*---------------------------------------------------------------------*)
           (*bdu intersection*)
           let handler = get_mvbdu_handler dynamic in
           let error, handler, bdu_inter =
             Ckappa_sig.Views_bdu.mvbdu_and
               parameter handler error bdu_test bdu_X
           in
           let dynamic = set_mvbdu_handler handler dynamic in
           if Ckappa_sig.Views_bdu.equal bdu_inter bdu_false
           then raise (False (error, dynamic))
           else
             let error, map =
               Covering_classes_type.AgentIDCV_map_and_set.Map.add parameter error
                 (agent_id, cv_id) bdu_inter map
             in
             error, dynamic, map
        ) proj_bdu_test_restriction
        (error, dynamic, Covering_classes_type.AgentIDCV_map_and_set.Map.empty)
    in
    error, dynamic

  (*****************************************************************)

  let get_new_site_name parameter error cv_id site_name site_correspondence =
    let error, site_correspondence =
      let rec aux list =
        match list with
        | [] ->
          Exception.warn parameter error __POS__ Exit []
        | (h, list, _) :: _ when h = cv_id -> error, list
        | _ :: tail -> aux tail
      in
      aux site_correspondence
    in
    let error, (map1, _map2) =
      Bdu_static_views.new_index_pair_map
        parameter
        error
        site_correspondence
    in
    let error, new_site_name =
      match
        Ckappa_sig.Site_map_and_set.Map.find_option
          parameter
          error
          site_name
          map1
      with
      | error, None ->
        Exception.warn
          parameter error __POS__ Exit
          Ckappa_sig.dummy_site_name
      | error, Some i -> error, i
    in
    error, new_site_name

  (*****************************************************************)

  let step_list_empty kappa_handler dynamic parameter error _rule_id agent_id agent_type site_name
      cv_list fixpoint_result proj_bdu_test_restriction bdu_false bdu_true site_correspondence =
    (*------------------------------------------------------------*)
    let error, dynamic, bdu =
      List.fold_left
        (fun (error, dynamic, bdu) cv_id ->
           let error, new_site_name =
             get_new_site_name
               parameter error cv_id site_name site_correspondence
           in
           let error, _new_site_string =
             try
               Handler.string_of_site parameter error kappa_handler
                 agent_type site_name
             with
             | _ ->
               Exception.warn
                 parameter error __POS__ Exit
                 (Ckappa_sig.string_of_site_name site_name)
           in
           (*print for test*)
           (*let _ =
             Loggers.fprintf (Remanent_parameters.get_logger parameter)
               "path:cv_id:%i:agent_type:%i:site_name:%i:%s:new_site_name:%i\n"
               (Covering_classes_type.int_of_cv_id cv_id)
               (Ckappa_sig.int_of_agent_name agent_type)
               (Ckappa_sig.int_of_site_name site_name)
               new_site_string
               (Ckappa_sig.int_of_site_name new_site_name)
             in*)
           (*---------------------------------------------------------------------*)
           (* fetch the bdu for the agent type and the cv_id in
              the current state of the iteration *)
           let error, bdu_X =
             match
               Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                 parameter
                 error
                 (agent_type, cv_id)
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
             Ckappa_sig.Views_bdu.mvbdu_and
               parameter handler error bdu_X bdu_test
           in
           (*let _ =
             Loggers.fprintf (Remanent_parameters.get_logger parameter) "BDU_test_fixpoint:\n" ;
             Ckappa_sig.Views_bdu.print parameter bdu_test_X
             in*)
           (* compute the projection over new_site_name *)
           let error, handler, singleton =
             Ckappa_sig.Views_bdu.build_variables_list
               parameter
               handler
               error
               [new_site_name]
           in
           let error, handler, bdu_proj =
             Ckappa_sig.Views_bdu.mvbdu_project_keep_only
               parameter
               handler
               error
               bdu_test_X
               singleton
           in
           (* rename new_site_name into 1 *)
           let error, handler, new_site_name_1 =
             Ckappa_sig.Views_bdu.build_renaming_list
               parameter
               handler
               error
               [new_site_name, site_name]
           in
           let error, handler, bdu_renamed =
             Ckappa_sig.Views_bdu.mvbdu_rename
               parameter
               handler
               error
               bdu_proj
               new_site_name_1
           in
           (* conjunction between bdu and bdu'*)
           let error, handler, bdu =
             Ckappa_sig.Views_bdu.mvbdu_and
               parameter
               handler
               error
               bdu
               bdu_renamed
           in
           let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
           error, dynamic, bdu)
        (error, dynamic, bdu_true)
        cv_list
    in
    (*---------------------------------------------------------------------*)
    let handler = Analyzer_headers.get_mvbdu_handler dynamic in
    let error, handler, list =
      Ckappa_sig.Views_bdu.extensional_of_mvbdu
        parameter
        handler
        error
        bdu
    in
    let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
    (*---------------------------------------------------------------------*)
    let error, state_list =
      List.fold_left
        (fun (error, output) list ->
           match list with
           | [_, state] -> (* the site name is fictitious, do not take it *)
             let error, _site_string =
               try
                 Handler.string_of_site parameter error kappa_handler
                   agent_type site_name
               with
               | _ ->
                 Exception.warn
                   parameter error __POS__ Exit
                   (Ckappa_sig.string_of_site_name site_name)
             in
             let error, _state_string =
               try
                 Handler.string_of_state_fully_deciphered parameter error kappa_handler
                   agent_type site_name state
               with
               | _ ->
                 Exception.warn
                   parameter error __POS__ Exit
                   (Ckappa_sig.string_of_state_index state)
             in
             (*let _ =
               Loggers.fprintf (Remanent_parameters.get_logger parameter)
                 "List of state in the precondition:\
                  rule_id:%i:agent_type:%i:site_type:%i:%s:%i:%s\n"
                 (Ckappa_sig.int_of_rule_id rule_id)
                 (Ckappa_sig.int_of_agent_name agent_type)
                 (Ckappa_sig.int_of_site_name site)
                 site_string
                 (Ckappa_sig.int_of_state_index state)
                 state_string
               in*)
             error, state :: output
           | _ ->
             Exception.warn
               parameter error __POS__ ~message:"state is empty" Exit output)
        (error, []) list
    in
    error, dynamic, Usual_domains.Val (List.rev state_list)

  (**************************************************************************)
  (*empty case of step list*)

  let precondition_empty_step_list kappa_handler parameter error dynamic rule_id path store_agent_name
      bdu_false bdu_true store_covering_classes_id site_correspondence
      fixpoint_result proj_bdu_test_restriction =
    let error, agent_type =
      match Ckappa_sig.RuleAgent_map_and_set.Map.find_option_without_logs
              parameter error
              (rule_id, path.Communication.agent_id)
              store_agent_name
      with
      | error, None ->
        Exception.warn
          parameter error __POS__
          ~message:"unknown agent type"
          Exit
          Ckappa_sig.dummy_agent_name
      | error, Some a -> error, a
    in
    (*------------------------------------------------------------*)
    let error, site_correspondence =
      match Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
              parameter
              error
              agent_type
              site_correspondence
      with
      | error, None ->
        Exception.warn  parameter error __POS__ Exit []
      | error, Some l -> error, l
    in
    (* compute the list of cv_id documenting site_name *)
    let error, cv_list =
      match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
              parameter
              error
              (agent_type, path.Communication.site) (*site:v*)
              store_covering_classes_id
      with
      | error, None ->
        Exception.warn  parameter error __POS__
          ~message:"the site does not belong to the last agent type of a path"
          Exit []
      | error, Some l -> error, l
    in
    (*---------------------------------------------------------------------*)
    let error, dynamic, new_answer =
      step_list_empty
        kappa_handler dynamic parameter error rule_id path.Communication.agent_id
        agent_type path.Communication.site cv_list fixpoint_result proj_bdu_test_restriction
        bdu_false bdu_true site_correspondence
    in
    error, dynamic, new_answer

  (***************************************************************)
  (*let compute precondition*)

  (* checking the binding information whether or not path belong to the
     current contact map*)

  (*TODO*)
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

  let precondition_typing
      parameter error kappa_handler rule_id step_list path
      store_agent_name dual_contact_map
    =
    let rec aux acc error =
      match acc with
      | [] -> error, Usual_domains.Any
      | step :: tl ->
        let error, agent_type =
          match
            Ckappa_sig.RuleAgent_map_and_set.Map.find_option_without_logs
              parameter
              error
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
            (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
               parameter
               error
               (agent_type, step.Communication.site_out) (*A.x*)
               kappa_handler.Cckappa_sig.states_dic)
            (fun error ->
               Exception.warn
                 parameter error __POS__ Exit (Ckappa_sig.Dictionary_of_States.init()))
        in
        (*---------------------------------------------------------*)
        (*Binding state: B.y*)
        let state =
          Ckappa_sig.C_Lnk_type (step.Communication.agent_type_in, step.Communication.site_in)
        in
        (*check if whether or not state is defined*)
        let error, b =
          Ckappa_sig.Dictionary_of_States.member
            parameter
            error
            (Ckappa_sig.Binding state)
            state_dic
        in
        if b
        then
          (*-----------------------------------------------------*)
          (*state is defined*)
          let error, answer_contact_map =
            match
              Ckappa_sig.Dictionary_of_States.allocate
                parameter
                error
                Ckappa_sig.compare_unit_state_index
                (Ckappa_sig.Binding state)
                ()
                Misc_sa.const_unit
                state_dic
            with
            | error, None ->
              Exception.warn
                parameter error __POS__ Exit  Usual_domains.Undefined
            | error, Some (state, _, _, _) ->
              match
                Ckappa_sig.AgentSiteState_map_and_set.Map.find_option_without_logs
                  parameter
                  error
                  (agent_type, step.Communication.site_out, state)
                  dual_contact_map
              with
              | error, None ->
                Exception.warn
                  parameter error __POS__ Exit Usual_domains.Undefined
              | error, Some _ -> (*recursive call*) aux tl error
          in
          error, answer_contact_map
        else
          (*state is not defined*) (*FIXME*)
          Exception.warn
            parameter error __POS__ Exit
            Usual_domains.Undefined
    in
    aux step_list error

  (*-----------------------------------------------------------*)
  (*outside the pattern*)

  let last parameters error l =
    match List.rev l with
    | x::_ -> error, Some x
    | [] ->
      Exception.warn parameters error __POS__
        ~message:"no element" Exit None

  let get_step_tl parameters error path =
    match path.Communication.relative_address with
    | [] ->
      Exception.warn
        parameters error __POS__
        ~message:"derefencing null pointer"
        Exit None
    | head::tail -> error, Some (head,tail)

  let get_tuple_pattern error path agent_type =
    let error,
        (agent_type', site_out, site_in, agent_type_in) =
      (*revert the path, and get the three cases*)
      match List.rev path.Communication.relative_address with
      | [] -> (*return dummy*)
        error,
        (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name,
         Ckappa_sig.dummy_site_name, Ckappa_sig.dummy_agent_name)
      | [x] ->
        (*agent_type' (h') is agent_type inside the pattern*)
        (*get information of the triple*)
        let site_in = x.Communication.site_in in
        let agent_type_in = x.Communication.agent_type_in in
        let site_out = x.Communication.site_out in
        error, (agent_type, site_out, site_in, agent_type_in)
      | h::h'::_ ->
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

  let precondition_outside_pattern
      parameter error dynamic kappa_handler path
      agent_type (*agent_type inside the pattern*)
      bdu_false bdu_true site_correspondence store_covering_classes_id
      fixpoint_result = (*CHECK ME*)
    (*get the information of the path,
      A - x - y - B - z
      (agent_type', site_out, site_in, agent_type_in, site_path)
      where (agent_type_in, site_in) is the information of the last agent,
      and (agent_type', site_in) is the information of the agent of the pattern
    *)
    let error, (agent_type', site_out, site_in, agent_type_in) =
      get_tuple_pattern
        error
        path
        agent_type
    in
    (*get the site path*)
    let site_path = path.Communication.site in
    (*get the information of state of the last agent
      (A.x:agent_type,site_out)*)
    let error, state_dic =
      Misc_sa.unsome
        (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
           parameter
           error
           (agent_type', site_out)
           kappa_handler.Cckappa_sig.states_dic)
        (fun error ->
           Exception.warn parameter error __POS__ Exit
             (Ckappa_sig.Dictionary_of_States.init()))
    in
    (*check binding state B,y *)
    let state = Ckappa_sig.C_Lnk_type (agent_type_in, site_in) in
    let error, b =
      Ckappa_sig.Dictionary_of_States.member
        parameter
        error
        (Ckappa_sig.Binding state)
        state_dic
    in
    if b
    then
      (*state is defined*)
      let error, (dynamic, new_answer) =
        match
          Ckappa_sig.Dictionary_of_States.allocate
            parameter
            error
            Ckappa_sig.compare_unit_state_index
            (Ckappa_sig.Binding state)
            ()
            Misc_sa.const_unit
            state_dic
        with
        | error, None -> (*inconsistent*)
          error, (dynamic, Usual_domains.Undefined)
        | error, Some (state, _ ,_, _) ->
          let error, cv_list =
            match
              Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameter
                error
                (agent_type_in, site_path)
                store_covering_classes_id
            with
            | error, None ->
              Exception.warn parameter error __POS__ Exit []
            | error, Some l -> error, l
          in
          let error, site_correspondence =
            match
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
                parameter
                error
                agent_type_in
                site_correspondence
            with
            | error, None ->
              Exception.warn parameter error __POS__ Exit []
            | error, Some l -> error, l
          in
          let error, dynamic, bdu =
            List.fold_left (fun (error, dynamic, bdu) cv_id ->
                let error, b =
                  List.fold_left (fun (error, bool) (h, _, _) ->
                      if h = cv_id
                      then error, true
                      else error, bool
                    ) (error, false) site_correspondence
                in
                if b
                then
                  (*it is in CV, *)
                  let error, new_site_name =
                    get_new_site_name
                      parameter
                      error
                      cv_id
                      site_path
                      site_correspondence
                  in
                  let error, bdu_X =
                    match
                      Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                        parameter error
                        (agent_type_in, cv_id)
                        fixpoint_result
                    with
                    | error, None -> error, bdu_false
                    | error, Some bdu -> error, bdu
                  in
                  let handler = Analyzer_headers.get_mvbdu_handler dynamic in
                  let error, handler, singleton =
                    Ckappa_sig.Views_bdu.build_variables_list
                      parameter
                      handler
                      error
                      [new_site_name]
                  in
                  let error, handler, list =
                    Ckappa_sig.Views_bdu.extensional_of_mvbdu
                      parameter
                      handler
                      error
                      bdu_X
                  in
                  let error, handler, new_bdu =
                    List.fold_left (fun (error, handler, bdu) pair_list ->
                        let error, new_pair_list =
                          List.fold_left (fun (error, current_list) (site, _) ->
                              error, (site, state) :: current_list
                            ) (error, []) pair_list
                        in
                        let error, handler, new_bdu =
                          Bdu_static_views.build_bdu
                            parameter
                            handler
                            error
                            new_pair_list
                        in
                        let error, handler, conj_bdu =
                          Ckappa_sig.Views_bdu.mvbdu_and
                            parameter
                            handler
                            error
                            bdu
                            new_bdu
                        in
                        error, handler, conj_bdu
                      ) (error, handler, bdu_true) list
                  in
                  let error, handler, bdu_conj =
                    Ckappa_sig.Views_bdu.mvbdu_and
                      parameter
                      handler
                      error
                      bdu_X
                      new_bdu
                  in
                  let error, handler, bdu_proj =
                    Ckappa_sig.Views_bdu.mvbdu_project_keep_only
                      parameter
                      handler
                      error
                      bdu_conj
                      singleton
                  in
                  let error, handler, new_site_name_1 =
                    Ckappa_sig.Views_bdu.build_renaming_list
                      parameter
                      handler
                      error
                      [new_site_name, site_path]
                  in
                  let error, handler, bdu_renamed =
                    Ckappa_sig.Views_bdu.mvbdu_rename
                      parameter
                      handler
                      error
                      bdu_proj
                      new_site_name_1
                  in
                  let error, handler, bdu =
                    Ckappa_sig.Views_bdu.mvbdu_and
                      parameter
                      handler
                      error
                      bdu
                      bdu_renamed
                  in
                  let dynamic =
                    Analyzer_headers.set_mvbdu_handler handler dynamic in
                  error, dynamic, bdu
                else
                  (*it is not in C, take the state of y*)
                  let error, new_site_name =
                    get_new_site_name
                      parameter
                      error
                      cv_id
                      site_path
                      site_correspondence
                  in
                  let error, bdu_X =
                    match
                      Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                        parameter error
                        (agent_type_in, cv_id)
                        fixpoint_result
                    with
                    | error, None -> error, bdu_false
                    | error, Some bdu -> error, bdu
                  in
                  let handler = Analyzer_headers.get_mvbdu_handler dynamic in
                  let error, handler, singleton =
                    Ckappa_sig.Views_bdu.build_variables_list
                      parameter
                      handler
                      error
                      [new_site_name]
                  in
                  let error, handler, bdu_proj =
                    Ckappa_sig.Views_bdu.mvbdu_project_keep_only
                      parameter
                      handler
                      error
                      bdu_X
                      singleton
                  in
                  let error, handler, new_site_name_1 =
                    Ckappa_sig.Views_bdu.build_renaming_list
                      parameter
                      handler
                      error
                      [new_site_name, Ckappa_sig.site_name_of_int 1]
                  in
                  let error, handler, bdu_renamed =
                    Ckappa_sig.Views_bdu.mvbdu_rename
                      parameter
                      handler
                      error
                      bdu_proj
                      new_site_name_1
                  in
                  let error, handler, bdu =
                    Ckappa_sig.Views_bdu.mvbdu_and
                      parameter
                      handler
                      error
                      bdu
                      bdu_renamed
                  in
                  let dynamic =
                    Analyzer_headers.set_mvbdu_handler handler dynamic
                  in
                  error, dynamic, bdu
              ) (error, dynamic, bdu_true) cv_list
          in
          let handler = Analyzer_headers.get_mvbdu_handler dynamic in
          let error, handler, list =
            Ckappa_sig.Views_bdu.extensional_of_mvbdu
              parameter
              handler
              error
              bdu
          in
          let dynamic = Analyzer_headers.set_mvbdu_handler handler dynamic in
          let error, state_list =
            List.fold_left (fun (error, output) list ->
                match list with
                | [_, state] -> error, state :: output
                | _ -> Exception.warn parameter error __POS__ Exit output
              ) (error, []) list
          in
          error, (dynamic, Usual_domains.Val (List.rev state_list))
      in
      error, (dynamic, new_answer)
    else
      (*state is not defined*)
      Exception.warn parameter error __POS__ Exit
        (dynamic, Usual_domains.Undefined)

  (*revert the list and then take the first two elements in the list, there
    are two cases, first case: there exists two elements inside the list,
    other case is there is only one element inside the list*)
  (*begin
    let last_step_opt =
      last parameter error path.Communication.relative_address
    in
    match last_step_opt with
    | error, None ->
      Exception.warn
        parameter error __POS__
        ~message:"empty path in precondition outside pattern"
        Exit (dynamic, Usual_domains.Any)
    | error, Some fst_step ->
      begin
        let fst_agent = fst_step.Communication.agent_type_in in
        let fst_site = fst_step.Communication.site_in in (*D.t*)
        let step_agent = step.Communication.agent_type_in in
        let step_site = step.Communication.site_in in

        (*In D, site t has the binding state B.w, get the information of the state B.w for t*)
        let error, state_dic =
          Misc_sa.unsome
            (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
               parameter
               error
               (fst_agent, fst_site) (*D.t*)
               kappa_handler.Cckappa_sig.states_dic)
            (fun error ->
               Exception.warn
                 parameter error __POS__ Exit
                 (Ckappa_sig.Dictionary_of_States.init()))
        in
        (*the state of site t is B.w*)
        let state = Ckappa_sig.C_Lnk_type (step_agent, step_site) in
        (*check that if the state B@w is defined or not*)
        let error, b =
          Ckappa_sig.Dictionary_of_States.member
            parameter
            error
            (Ckappa_sig.Binding state)
            state_dic
        in
        (*--------------------------------------------------------*)
        if b
        then
          (*state is defined, get the information about this state*)
          let error, (dynamic, new_answer) =
            match
              Ckappa_sig.Dictionary_of_States.allocate
                parameter
                error
                Ckappa_sig.compare_unit_state_index
                (Ckappa_sig.Binding state)
                ()
                Misc_sa.const_unit
                state_dic
            with
            | error, None -> (*inconsistent*)
              error, (dynamic, Usual_domains.Undefined)
            | error, Some (state, _, _, _) -> (*state of t is B@w*)
              (*for each covering class containing D@v, if t in covering class,
                take the state of v knowing that t has type B@w*)
              (*covering class containing D@v*)
              let error, cv_list =
                match
                  Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                    parameter
                    error
                    (fst_agent, path.Communication.site) (*D@v*)
                    store_covering_classes_id
                with
                | error, None ->
                  Exception.warn parameter error __POS__ Exit []
                | error, Some l -> error, l
              in
              (*get a triple (cv_id, list, set) of the last agent that containing t*)
              let error, site_correspondence =
                match
                  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameter
                    error
                    fst_agent (*D*)
                    site_correspondence
                with
                | error, None ->
                  Exception.warn parameter error __POS__ Exit []
                | error, Some l -> error, l
              in
              (*---------------------------------------------------*)
              let error, dynamic, bdu =
                List.fold_left
                  (fun (error, dynamic, bdu) cv_id ->
                     (*FIX ME: check whether or not t is in this covering class*)
                     let error, b =
                       List.fold_left
                         (fun (error, bool) (h, _list, _set)  ->
                           if h = cv_id
                           then
                             error, true
                           else
                             error, bool
                         ) (error, false) site_correspondence
                     in
                     if b
                     then
                       (*------------------------------------------------*)
                       (*t is in CV, take the states of v knowing that t has type B@w*)
                       let error, new_site_name =
                         get_new_site_name
                           parameter
                           error
                           cv_id
                           path.Communication.site (*state v in D*)
                           site_correspondence
                       in
                       (*fetch bdu of D*)
                       let error, bdu_X =
                         match
                           Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                             parameter
                             error
                             (fst_agent, cv_id)
                             fixpoint_result
                         with
                         | error, None -> error, bdu_false
                         | error, Some bdu -> error, bdu
                       in
                       let handler =
                         Analyzer_headers.get_mvbdu_handler dynamic
                       in
                       let error, handler, singleton =
                         Ckappa_sig.Views_bdu.build_variables_list
                           parameter
                           handler
                           error
                           [new_site_name]
                       in
                       (*build new bdu where state t = B@w, get bdu_X and change the type*)
                       let error, handler, list =
                         Ckappa_sig.Views_bdu.extensional_of_mvbdu
                           parameter
                           handler
                           error
                           bdu_X
                       in
                       let error, handler, new_bdu =
                         List.fold_left (fun (error, handler, bdu) pair_list ->
                             let error, new_pair_list =
                               List.fold_left
                                 (fun (error, current_list) (site, _) ->
                                    error, (site, state) :: current_list
                                 ) (error, []) pair_list
                             in
                             (*build bdu with state = B@w*)
                             let error, handler, new_bdu =
                               Bdu_static_views.build_bdu
                                 parameter
                                 handler
                                 error
                                 new_pair_list
                             in
                             let error, handler, conj_bdu =
                               Ckappa_sig.Views_bdu.mvbdu_and
                                 parameter
                                 handler
                                 error
                                 bdu
                                 new_bdu
                             in
                             error, handler, conj_bdu
                           ) (error, handler, bdu_true) list
                       in
                       (*to the conjunction between bdu_X and new_bdu*)
                       let error, handler, bdu_conj =
                         Ckappa_sig.Views_bdu.mvbdu_and
                           parameter
                           handler
                           error
                           bdu_X
                           new_bdu
                       in
                       (*do the projection of bdu_conj and singleton*)
                       let error, handler, bdu_proj =
                         Ckappa_sig.Views_bdu.mvbdu_project_keep_only
                           parameter
                           handler
                           error
                           bdu_conj
                           singleton
                       in
                       (*rename*)
                       let error, handler, new_site_name_1 =
                         Ckappa_sig.Views_bdu.build_renaming_list
                           parameter
                           handler
                           error
                           [new_site_name, path.Communication.site]
                       in
                       (*do the conjuntion between bdu and bdu final*)
                       let error, handler, bdu_renamed =
                         Ckappa_sig.Views_bdu.mvbdu_rename
                           parameter
                           handler
                           error
                           bdu_proj
                           new_site_name_1
                       in
                       let error, handler, bdu =
                         Ckappa_sig.Views_bdu.mvbdu_and
                           parameter
                           handler
                           error
                           bdu
                           bdu_renamed
                       in
                       let dynamic =
                         Analyzer_headers.set_mvbdu_handler handler dynamic
                       in
                       error, dynamic, bdu
                     else
                       (*---------------------------------------------*)
                       (*t is not in CV, take the state of v*)
                       let error, new_site_name =
                         get_new_site_name
                           parameter
                           error
                           cv_id
                           path.Communication.site (*state v in D*)
                           site_correspondence
                       in
                       (*fetch the bdu of D*)
                       let error, bdu_X =
                         match
                           Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                             parameter
                             error
                             (fst_agent, cv_id)
                             fixpoint_result
                         with
                         | error, None -> error, bdu_false
                         | error, Some bdu -> error, bdu
                       in
                       (*projection over new_site_name v*)
                       let handler = Analyzer_headers.get_mvbdu_handler dynamic in
                       let error, handler, singleton =
                         Ckappa_sig.Views_bdu.build_variables_list
                           parameter
                           handler
                           error
                           [new_site_name]
                       in
                       (*do the projection of v at this bdu*)
                       let error, handler, bdu_proj =
                         Ckappa_sig.Views_bdu.mvbdu_project_keep_only
                           parameter
                           handler
                           error
                           bdu_X
                           singleton
                       in
                       (*then rename v to 1*)
                       let error, handler, new_site_name_1 =
                         Ckappa_sig.Views_bdu.build_renaming_list
                           parameter
                           handler
                           error
                           [new_site_name, Ckappa_sig.site_name_of_int 1]
                       in
                       let error, handler, bdu_renamed =
                         Ckappa_sig.Views_bdu.mvbdu_rename
                           parameter
                           handler
                           error
                           bdu_proj
                           new_site_name_1
                       in
                       let error, handler, bdu =
                         Ckappa_sig.Views_bdu.mvbdu_and
                           parameter
                           handler
                           error
                           bdu
                           bdu_renamed
                       in
                       let dynamic =
                         Analyzer_headers.set_mvbdu_handler handler dynamic
                       in
                       error, dynamic, bdu
                  ) (error, dynamic, bdu_true) cv_list
              in
              (*--------------------------------------------------*)
              let handler = Analyzer_headers.get_mvbdu_handler dynamic in
              let error, handler, list =
                Ckappa_sig.Views_bdu.extensional_of_mvbdu
                  parameter
                  handler
                  error
                  bdu
              in
              let dynamic =
                Analyzer_headers.set_mvbdu_handler handler dynamic
              in
              (*--------------------------------------------------*)
              let error, state_list =
                List.fold_left (fun (error, output) list ->
                    match list with
                    | [_, state] -> error, state :: output
                    | _ -> Exception.warn
                             parameter error __POS__ Exit output
                  ) (error, []) list
              in
              error, (dynamic, Usual_domains.Val (List.rev state_list))
          in
          error, (dynamic, new_answer)
        else
          (*state is not defined*)
          (*give a warning*)
          Exception.warn
            parameter error __POS__ Exit (dynamic, Usual_domains.Undefined)
      end
    end*)

  (*---------------------------------------------------------------*)
  (*inside the pattern*)

  let precondition_inside_pattern
      parameter error dynamic kappa_handler
      path agent_type
      aux rule site_correspondence store_covering_classes_id
      fixpoint_result bdu_false bdu_true
    =
    (*---------------------------------------------------------*)
    (*inside the pattern, check the binding information in the lhs of the current agent*)
    let error, output =
      Communication.follow_path_inside_cc
        parameter error kappa_handler
        rule.Cckappa_sig.rule_lhs
        path
    in
    match output with
    | Communication.Cannot_exist ->
      error, (dynamic, Usual_domains.Undefined)
    | Communication.May_exist _ ->
      (*-----------------------------------------------------*)
      let error', (dynamic, new_answer) =
        precondition_outside_pattern
          parameter error dynamic kappa_handler
          path
          agent_type
          bdu_false
          bdu_true
          site_correspondence
          store_covering_classes_id
          fixpoint_result
      in
      let error =
        Exception.check_point
          Exception.warn parameter error error' __POS__ Exit
      in
      error, (dynamic, new_answer)
    (*--------------------------------------------------------*)
    | Communication.Located agent_id ->
      (*search inside this map which agent and site, A.x bind to.*)
      let next_path =
        {
          Communication.site = path.Communication.site ;
          Communication.agent_id = agent_id ;
          Communication.relative_address = [];}
      in
      let error, dynamic, new_answer = aux dynamic next_path in
      error, (dynamic, new_answer)

  (*-------------------------------------------------------------*)

  let print_test_answer parameter answer =
    match answer with
    | Usual_domains.Val l ->
      List.iter
        (fun i ->  Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "List: state:%i\n" (Ckappa_sig.int_of_state_index i)) l
    | Usual_domains.Undefined ->
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "Undefined\n"
    | Usual_domains.Any ->
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "Any\n"

  (*-------------------------------------------------------------*)

  let scan_bot
      ~also_scan_top:(also_scan_top:bool)
      pos parameters error elt string
    =
    match elt with
    | Usual_domains.Undefined ->
      let error, () =
        Exception.warn
          parameters error pos
          ~message:("bot generated while fetching the potential state of a site"^string)
          Exit ()
      in error
    | Usual_domains.Any when also_scan_top ->
      let error, () =
        Exception.warn
          parameters error pos
          ~message:("top generated while fetching the potential state of a site"^string)
          Exit ()
      in error
    | Usual_domains.Val _
    | Usual_domains.Any  -> error

  let scan_bot_warn
      ~also_scan_top:(also_scan_top:bool)
      (a,_,_,_) parameters error elt string
    =
    let () =
      match elt with
      | Usual_domains.Undefined ->
        if
          Remanent_parameters.get_dump_reachability_analysis_wl
            parameters
        then
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s%sbot generated while fetching the potential state of a site %s" (Remanent_parameters.get_prefix parameters) a string in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
      | Usual_domains.Any when also_scan_top ->
        if
          Remanent_parameters.get_dump_reachability_analysis_wl
            parameters
        then
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%stop generated while fetching the potential state of a site %s" (Remanent_parameters.get_prefix parameters) string in
          Loggers.print_newline
            (Remanent_parameters.get_logger parameters)
      | Usual_domains.Val _
      | Usual_domains.Any  -> ()
    in error

  let compute_pattern_navigation
      parameter error kappa_handler
      aux dynamic path rule step bdu_false bdu_true site_correspondence
      store_covering_classes_id fixpoint_result =
    let error, agent =
      match
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
          parameter
          error
          path.Communication.agent_id (*#1:A*)
          rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      with
      | error, None -> (*FIXME*)
        Exception.warn parameter error __POS__ Exit Cckappa_sig.Ghost
      | error, Some a -> error, a
    in
    let error, (dynamic, new_answer) =
      match agent with
      | Cckappa_sig.Ghost
      | Cckappa_sig.Unknown_agent _
      | Cckappa_sig.Dead_agent _ -> (*FIXME dead agent*)
        Exception.warn
          parameter error __POS__ Exit (dynamic, Usual_domains.Undefined)
      | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*search inside the pattern, check whether or not it is out
          of the pattern or in the pattern.*)
        let error, (dynamic, new_answer) =
          match
            Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
              parameter
              error
              step.Communication.site_out (*A.x: state*)
              agent.Cckappa_sig.agent_interface
          with
          | error, None ->
            (*------------------------------------------------------*)
            (*out of the pattern, take the last element in the
              relative_address, if one can take the agent type of the
              target, take site and collect the information one has about
              the potential state of this site in agents of this type. *)
            let error', (dynamic, new_answer) =
              precondition_outside_pattern
                parameter
                error
                dynamic
                kappa_handler
                path
                agent_type (*agent_type inside the pattern*)
                bdu_false
                bdu_true
                site_correspondence
                store_covering_classes_id
                fixpoint_result
            in
            let error =
              Exception.check_point
                Exception.warn parameter error error' __POS__ Exit
            in
            let error =
              scan_bot
                ~also_scan_top:false __POS__
                parameter error
                new_answer
                " in precondition outside a pattern"
            in
            error, (dynamic, new_answer)
          (*----------------------------------------------------*)
          (*There is some states, inside the pattern. Check
            port whether or not it is free/bound?*)
          | error, Some port ->
            (*check if it is free?*)
            match port.Cckappa_sig.site_free with
            | Some true ->
              (*then it is inconsistent, undefined*)
              (*Exception.warn
                parameter error __POS__ Exit
                ~message:"try to navigate through a free site"
                (dynamic, Usual_domains.Undefined)*)
              let () =
                if
                  (local_trace
                   || Remanent_parameters.get_dump_reachability_analysis_wl parameter
                   || Remanent_parameters.get_trace parameter)
                then
                  let () =   Loggers.fprintf
                      (Remanent_parameters.get_logger parameter)
                      "Try to neavigate through a free site: bottom reduction"
                  in
                  Loggers.print_newline (Remanent_parameters.get_logger parameter)
              in
              error, (dynamic, Usual_domains.Undefined)
            | None
            | Some false ->
              (*it is not free, check if it is fully defined, or incompelete,
                by looking into the bonds on the lhs*)
              (*get the information of the agent partner *)
              let agent_type_partner = step.Communication.agent_type_in in
              let site_x_partner = step.Communication.site_in in
              (*  let site_type_y_partner = path.Communication.site in*)
              (*get information of the agent*)
              let agent_id = path.Communication.agent_id in
              let agent_type = agent.Cckappa_sig.agent_name in
              let site_x = step.Communication.site_out in
              (*looking into the bonds of the agent*)
              match
                Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                  parameter
                  error
                  agent_id
                  rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds
              with
              | error, None ->
                (*it is incompelete, then it is outside the pattern*)
                let error', (dynamic, new_answer) =
                  precondition_outside_pattern
                    parameter
                    error
                    dynamic
                    kappa_handler
                    path
                    agent_type
                    bdu_false
                    bdu_true
                    site_correspondence
                    store_covering_classes_id
                    fixpoint_result
                in
                let error =
                  Exception.check_point
                    Exception.warn parameter error error' __POS__ Exit
                in
                error, (dynamic, new_answer)
              | error, Some map ->
                match
                  Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
                    parameter
                    error
                    site_x
                    map
                with
                | error, None ->
                  (*outside the pattern*)
                  let error', (dynamic, new_answer) =
                    precondition_outside_pattern
                      parameter
                      error
                      dynamic
                      kappa_handler
                      path
                      agent_type
                      bdu_false
                      bdu_true
                      site_correspondence
                      store_covering_classes_id
                      fixpoint_result
                  in
                  let error =
                    Exception.check_point
                      Exception.warn parameter error error' __POS__ Exit
                  in
                  error, (dynamic, new_answer)
                | error, Some site_add ->
                  (*there is a bond, check the following pattern is it well
                    defined*)
                  let agent_type' = site_add.Cckappa_sig.agent_type in (*B*)
                  let site_type' = site_add.Cckappa_sig.site in (*z?*)
                  (*if A.x is bound to B.x*)
                  if agent_type' = agent_type_partner &&
                     site_type' = site_x_partner
                  then
                    (*inside the pattern*)
                    let error', (dynamic, new_answer) =

                      precondition_inside_pattern
                        parameter error dynamic
                        kappa_handler path agent_type aux rule
                        site_correspondence
                        store_covering_classes_id
                        fixpoint_result
                        bdu_false
                        bdu_true
                    in
                    let error =
                      Exception.check_point
                        Exception.warn parameter error error' __POS__ Exit
                    in
                    error, (dynamic, new_answer)
                  else
                    (*outsite the pattern*)
                    let error', (dynamic, new_answer) =
                      precondition_outside_pattern
                        parameter
                        error
                        dynamic
                        kappa_handler
                        path
                        agent_type
                        bdu_false
                        bdu_true
                        site_correspondence
                        store_covering_classes_id
                        fixpoint_result
                    in
                    let error =
                      Exception.check_point
                        Exception.warn parameter error error' __POS__ Exit
                    in
                    error, (dynamic, new_answer)
        in
        error, (dynamic, new_answer)
    in
    error, (dynamic, new_answer)

  (*------------------------------------------------------------*)

  let compute_precondition_enable
      parameter error kappa_handler rule rule_id precondition
      bdu_false bdu_true dual_contact_map store_agent_name site_correspondence
      store_covering_classes_id fixpoint_result proj_bdu_test_restriction =
    let precondition =
      Communication.refine_information_about_state_of_sites_in_precondition
        precondition
        (fun parameters error dynamic (current_path:Communication.path) former_answer ->
           (*-----------------------------------------------------*)
           (*typing*)
           let error =
             scan_bot
               ~also_scan_top:false
               __POS__ parameters error
               former_answer
               " from overlying domain"
           in
           let error, answer_contact_map =
             precondition_typing
               parameter
               error
               kappa_handler
               rule_id
               current_path.Communication.relative_address
               current_path
               store_agent_name
               dual_contact_map
           in
           let error = (*FIXME*)
             scan_bot_warn
               ~also_scan_top:false
               __POS__ parameters error
               answer_contact_map
               " in the contact map"
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
                   compute_pattern_navigation
                     parameter error kappa_handler
                     aux dynamic path rule
                     step
                     bdu_false
                     bdu_true
                     site_correspondence
                     store_covering_classes_id
                     fixpoint_result
                 in
                 let error = (*FIXME*)
                   scan_bot
                     ~also_scan_top:false
                     __POS__ parameters error
                     new_answer
                     " while navigating"
                 in
                 let update_answer =
                   Usual_domains.glb_list new_answer former_answer in
                 error, dynamic, update_answer
               (*--------------------------------------------------*)
               (*empty relative_adress*)
               | [] ->
                 let error, dynamic, new_answer =
                   precondition_empty_step_list
                     kappa_handler parameter error dynamic
                     rule_id path store_agent_name bdu_false bdu_true store_covering_classes_id
                     site_correspondence fixpoint_result proj_bdu_test_restriction
                 in
                 let error =
                   scan_bot
                     ~also_scan_top:false
                     __POS__ parameters error
                     new_answer
                     " while navigating (empty path)"
                 in
                 (*let _ =
                   Loggers.fprintf (Remanent_parameters.get_logger parameter)
                     "-Pattern is empty\n";
                   in*)
                 (*do I need to do the intersection with former answer?*)
                 let update_answer =
                   Usual_domains.glb_list new_answer former_answer in
                 error, dynamic, update_answer
             in
             aux dynamic current_path
           in
           (*final intersection with contact map*)
           let update_answer =
             Usual_domains.glb_list answer_contact_map new_answer in
           error, dynamic,  update_answer
        )
    in
    error, precondition

  (****************************************************************)

  let is_enable_aux static dynamic error rule_id precondition =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let kappa_handler = get_kappa_handler static in
    let rules = compil.Cckappa_sig.rules in
    let error, rule =
      match
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameter error rule_id rules
      with
      | error, None ->
        let error, rule = Preprocess.empty_rule parameter error in
        Exception.warn parameter error __POS__
          ~message:"unknown rule" Exit rule
      | error, Some rule -> error, rule.Cckappa_sig.e_rule_c_rule
    in
    (*-----------------------------------------------------------*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
    (*------------------------------------------------------------*)
    let fixpoint_result = get_fixpoint_result dynamic in
    let dual_contact_map = get_store_dual_contact_map dynamic in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let store_agent_name = get_agent_name static in
    let error, site_correspondence =
      get_store_remanent_triple static dynamic error
    in
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
      (* JF: You do not use the result, of this function, I comment the function call *)
      let error, dynamic =
        collect_bdu_enabled
          parameter
          error
          dynamic
          bdu_false
          fixpoint_result
          proj_bdu_test_restriction
      in
      (*-----------------------------------------------------*)
      (*get a set of sites in a covering class: later with state list*)
      let error, precondition =
        compute_precondition_enable parameter error kappa_handler rule rule_id
          precondition bdu_false bdu_true dual_contact_map store_agent_name
          site_correspondence store_covering_classes_id fixpoint_result
          proj_bdu_test_restriction
      in
      error, (dynamic, precondition), true
    with
      False (error, dynamic) -> error, (dynamic, precondition), false

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
    if is_enable
    then
      (*the information about the dynamic contact map is available?, if
        yes, use them, otherwise use the static contact map instead*)
      error, dynamic, Some precondition
      (* TO DO, update the function state_of_site in precondition *)
    else
      error, dynamic, None

  (***********************************************************)
  (*deal with views*)

  let compute_bdu_update_aux static dynamic error bdu_test list_a bdu_X =
    let parameter = get_parameter static in
    let parameter_views = Remanent_parameters.update_prefix parameter "\t\t\t" in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_inter =
      Ckappa_sig.Views_bdu.mvbdu_and
        parameter_views handler error bdu_X bdu_test
    in
    (*redefine with modification list*)
    let error, handler, bdu_redefine =
      Ckappa_sig.Views_bdu.mvbdu_redefine
        parameter_views handler error bdu_inter list_a
    in
    (*do the union of bdu_redefine and bdu_X*)
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_or
        parameter_views handler error bdu_redefine bdu_X
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
    let parameter = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_result =
      Ckappa_sig.Views_bdu.mvbdu_or
        parameter handler error bdu_creation bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (***************************************************************)

  let compute_bdu_update_side_effects static dynamic error bdu_test list_a bdu_X =
    let error, dynamic, bdu_result =
      compute_bdu_update_aux static dynamic error bdu_test list_a bdu_X
    in
    error, dynamic, bdu_result

  (****************************************************************)

  let compute_views_test_enabled static dynamic error rule_id event_list =
    let parameter = get_parameter static in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
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
           let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
           let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
           let error, store_modif_list_restriction_map =
             get_store_modif_list_restriction_map static dynamic error
           in
           (*print*)
           (*let parameter_cv =
             Remanent_parameters.update_prefix parameter "\t\tUpdating the views for"
             in*)
           let error = (*TODO: different title*)
             dump_cv_label
               static
               dynamic
               error
               (Remanent_parameters.get_dump_reachability_analysis_diff parameter)
               (agent_type, cv_id)
               (*Bdu_fixpoint_iteration.dump_cv_label (*FIXME*)
                 (Remanent_parameters.get_dump_reachability_analysis_diff parameter)
                 parameter_cv
                 kappa_handler
                 error
                 store_remanent_triple
                 agent_type
                 cv_id*)
           in
           (*-----------------------------------------------------*)
           let store_result = get_fixpoint_result dynamic in
           let error, bdu_X =
             match Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                     parameter error (agent_type, cv_id) store_result
             with
             | error, None -> error, bdu_false
             | error, Some bdu -> error, bdu
           in
           let error, bdu_test =
             match Covering_classes_type.AgentsCV_setmap.Map.find_option
                     (agent_id, agent_type, cv_id) proj_bdu_test_restriction
             with
             | None -> error, bdu_true
             | Some bdu -> error, bdu
           in
           let error, dynamic, bdu_update =
             match Covering_classes_type.AgentsRuleCV_map_and_set.Map.find_option_without_logs
                     parameter error
                     (agent_id, agent_type, rule_id, cv_id)
                     store_modif_list_restriction_map
             with
             | error, None -> error, dynamic, bdu_X
             | error, Some list_a ->
               let error, dynamic, bdu_update =
                 compute_bdu_update_views static dynamic error bdu_test list_a bdu_X
               in
               error, dynamic, bdu_update
           in
           let error, dynamic, event_list =
             add_link
               ~title:"\t\t"
               error
               static
               dynamic
               (agent_type, cv_id)
               bdu_update
               event_list
           in
           error, dynamic, event_list
        ) proj_bdu_test_restriction (error, dynamic, event_list)
    in
    error, dynamic, event_list

  (**************************************************************************)
  (*deal with creation*)

  let compute_views_creation_enabled static dynamic error rule_id event_list =
    let parameter = get_parameter static in
    let error, store_bdu_creation =
      get_store_proj_bdu_creation_restriction static dynamic error
    in
    let error, bdu_creation_map =
      match Ckappa_sig.Rule_setmap.Map.find_option rule_id
              store_bdu_creation
      with
      | None -> error, Covering_classes_type.AgentCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      Covering_classes_type.AgentCV_setmap.Map.fold
        (fun (agent_type, cv_id) bdu_creation (error, dynamic, event_list) ->
           let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
           let fixpoint_result = get_fixpoint_result dynamic in
           let error, bdu_X =
             match Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                     parameter error (agent_type, cv_id) fixpoint_result
             with
             | error, None -> error, bdu_false
             | error, Some bdu -> error, bdu
           in
           let error, dynamic, bdu_update =
             compute_bdu_update_creation static dynamic error bdu_creation bdu_X
           in
           let error, dynamic, event_list =
             add_link
               ~title:"Dealing with creation"
               error
               static
               dynamic
               (agent_type, cv_id)
               bdu_update
               event_list
           in
           error, dynamic, event_list
        ) bdu_creation_map (error, dynamic, event_list)
    in
    error, dynamic, event_list

  (**************************************************************************)
  (*deal with side effects*)

  let compute_side_effects_enabled static dynamic error rule_id event_list =
    let parameter = get_parameter static in
    let error, store_bdu_potential =
      get_store_proj_bdu_potential_restriction static dynamic error
    in
    let error, proj_bdu_potential_restriction =
      match Ckappa_sig.Rule_setmap.Map.find_option rule_id
              store_bdu_potential
      with
      | None -> error, Covering_classes_type.AgentSiteCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      Covering_classes_type.AgentSiteCV_setmap.Map.fold
        (fun (agent_type, _new_site_id, cv_id) (bdu_test, list)
          (error, dynamic, event_list) ->
          let fixpoint_result = get_fixpoint_result dynamic in
          let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
          let error, bdu_X =
            match
              Covering_classes_type.AgentCV_map_and_set.Map.find_option_without_logs
                    parameter error (agent_type, cv_id) fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, dynamic, bdu_update =
            compute_bdu_update_side_effects static dynamic error bdu_test list bdu_X
          in
          let error, dynamic, event_list =
            add_link
              ~title:"Dealing with side effects"
              error
              static
              dynamic
              (agent_type, cv_id)
              bdu_update
              event_list
          in
          error, dynamic, event_list
        ) proj_bdu_potential_restriction (error, dynamic, event_list)
    in
    error, dynamic, event_list

  (**************************************************************************)
  (*compute enable in different cases*)

  let can_we_prove_this_is_the_first_application precondition =
    match
      Communication.is_the_rule_applied_for_the_first_time precondition
    with
    | Usual_domains.Sure_value b ->
      if b
      then true
      else false
    | Usual_domains.Maybe -> false

  let compute_views_enabled static dynamic error rule_id precondition =
    (*-----------------------------------------------------------------------*)
    (*deal with views*)
    let error, dynamic, event_list =
      compute_views_test_enabled
        static
        dynamic
        error
        rule_id
        []
    in
    (*-----------------------------------------------------------------------*)
    (*deal with creation*)
    let error, dynamic, event_list =
      let b = can_we_prove_this_is_the_first_application precondition in
      if b
      then
        (*if Sure_value is true then compute creation_enabled*)
        let error, dynamic, event_list =
          compute_views_creation_enabled
            static
            dynamic
            error
            rule_id
            event_list
        in
        error, dynamic, event_list
      else
        (*Sure_value is false*)
        error, dynamic, event_list
    in
    (*-----------------------------------------------------------------------*)
    (*deal with side effects*)
    let error, dynamic, event_list =
      compute_side_effects_enabled
        static
        dynamic
        error
        rule_id
        event_list
    in
    error, dynamic, event_list

  (**************************************************************)

  let apply_rule static dynamic error rule_id precondition =
    let error, dynamic, event_list =
      compute_views_enabled static dynamic error rule_id precondition
    in
    error, dynamic, (precondition, event_list)

  (**************************************************************************)
  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let apply_event static (error, dynamic, event_list) event =
    let parameter = get_parameter static in
    let side_effects = get_side_effects static in
    let store_update = get_store_update dynamic in
    let (half_break, remove) = side_effects in
    match event with
    | Communication.See_a_new_bond ((agent_type, site_type, _state),
                                    (_agent_type', _site_type', state')) ->
      (*-----------------------------------------------------------------------*)
      (* get the pairs (r, state) compatible with the second site:half_break *)
      let error, pair_list =
        match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameter error (agent_type, site_type) half_break with
        | error, None -> error, []
        | error, Some (_, l) -> error, l
      in
      (*-----------------------------------------------------------------------*)
      (*get rule_id in remove side effects *)
      let error, rule_list =
        match Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameter error (agent_type, site_type) remove with
        | error, None -> error, []
        | error, Some (_, l) -> error, l
      in
      (*---------------------------------------------------------------------*)
      (* store result with half_break action: for each add the rule r in
         update of (c) for any covering class that documents this second
         site *)
      let error, store_update_half_break =
        Covering_classes_type.AgentCV_map_and_set.Map.fold
          (fun (agent_type, cv_id) rule_id_set (error, store_result) ->
             let error, new_rule_id_set =
               List.fold_left (fun (error, store) (rule, state) ->
                   (*state is compatible with B.x*)
                   if state = state'
                   then
                     (* A rule may be added several time *)
                     let error, new_update =
                       Ckappa_sig.Rule_map_and_set.Set.add_when_not_in
                         parameter error rule store
                     in
                     error, new_update
                   else error, store
                 ) (error, rule_id_set) pair_list
             in
             let error, store_result =
               Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite
                 parameter error
                 (agent_type, cv_id)
                 new_rule_id_set
                 store_result
             in
             error, store_result
          ) store_update (error, Covering_classes_type.AgentCV_map_and_set.Map.empty)
      in
      (*---------------------------------------------------------------------*)
      (*store result with remove action*)
      let error, store_update_remove =
        Covering_classes_type.AgentCV_map_and_set.Map.fold
          (fun (agent_type, cv_id) rule_id_set (error, store_result) ->
             let error, new_rule_id_set =
               List.fold_left (fun (error, store) rule ->
                   let error, new_update =
                     Ckappa_sig.Rule_map_and_set.Set.add_when_not_in
                       parameter error rule store
                   in
                   error, new_update
                 ) (error, rule_id_set) rule_list
             in
             let error, store_result =
               Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite
                 parameter error
                 (agent_type, cv_id)
                 new_rule_id_set
                 store_result
             in
             error, store_result
          ) store_update (error, Covering_classes_type.AgentCV_map_and_set.Map.empty)
      in
      (*---------------------------------------------------------------------*)
      (*fold2*)
      let error, store_update =
        Covering_classes_type.AgentCV_map_and_set.Map.fold2
          parameter
          error
          (fun parameter error (agent_type, cv_id) rule_id_set store_result ->
             let error, store_result =
               Bdu_dynamic_views.add_link parameter error (agent_type, cv_id)
                 rule_id_set store_result
             in
             error, store_result
          )
          (fun parameter error (agent_type, cv_id) rule_id_set store_result ->
             let error, store_result =
               Bdu_dynamic_views.add_link parameter error (agent_type, cv_id)
                 rule_id_set store_result
             in
             error, store_result
          )
          (fun parameter error (agent_type, cv_id) s1 s2 store_result ->
             let error', union_set =
               Ckappa_sig.Rule_map_and_set.Set.union parameter error s1 s2
             in
             let error =
               Exception.check_point
                 Exception.warn  parameter error error' __POS__ Exit
             in
             let error, store_result =
               Bdu_dynamic_views.add_link parameter error (agent_type, cv_id)
                 union_set store_result
             in
             error, store_result
          )
          store_update_half_break
          store_update_remove
          store_update
      in
      (* update is in local dynamic information *)
      let dynamic = set_store_update store_update dynamic in
      (*-----------------------------------------------------------------------*)
      (* when doing so, add any such rules r in the event list with an
         event of kind Check_rule *)
      let store_update = get_store_update dynamic in
      let event_list =
        Covering_classes_type.AgentCV_map_and_set.Map.fold
          (fun _ rule_id_set event_list ->
             Ckappa_sig.Rule_map_and_set.Set.fold
               (fun rule_id event_list ->
                  (Communication.Check_rule rule_id) :: event_list
               ) rule_id_set event_list
          ) store_update event_list
      in
      error, dynamic, event_list
    | Communication.Dummy | Communication.Check_rule _
    | Communication.Modified_sites _ (*TODO?*) -> error, dynamic, event_list

  (**************************************************************************)

  let apply_event_list static dynamic error event_list =
    let error, dynamic, event_list =
      List.fold_left (fun (error, dynamic, event_list) event ->
          let error, dynamic, event_list =
            apply_event static
              (error, dynamic, event_list)
              event
          in
          error, dynamic, event_list
        ) (error, dynamic, []) event_list
    in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    match
      get_ranges dynamic
    with
    | None ->
      error, dynamic, kasa_state
    | Some ranges ->
      let kappa_handler = get_kappa_handler static in
      let handler = get_mvbdu_handler dynamic in
      let parameters = get_parameter static in
      let contact_map = Preprocess.init_contact_map in
      let error, (handler, contact_map) =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters
          error
          (fun parameters error ag_type site_map (handler, contact_map) ->
             let error, contact_map =
               Preprocess.declare_agent parameters error ag_type contact_map
             in
             Wrapped_modules.LoggedIntMap.fold
               (fun _ mvbdu (error,(handler, contact_map)) ->
                  let error, handler, list_of_states = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error mvbdu in
                  match list_of_states
                  with
                  | [] -> error, (handler, contact_map)
                  | [site_type,_]::_ ->
                    begin
                      let error, contact_map =
                        Preprocess.declare_site parameters error ag_type site_type contact_map
                      in
                      let error, site = Handler.translate_site parameters error kappa_handler ag_type site_type in
                      match
                        site
                      with
                      | Ckappa_sig.Internal _ ->
                        let error, contact_map =
                          List.fold_left
                            (fun (error, contact_map) l ->
                               match l
                               with
                               | [site_type',state]
                                 when site_type' = site_type ->
                                 Preprocess.add_internal_state_in_contact_map
                                   parameters error
                                   (ag_type,site_type) state contact_map
                               | [] | _::_ ->
                                 Exception.warn
                                   parameters error __POS__ Exit
                                   contact_map)
                            (error, contact_map) list_of_states
                        in
                        error, (handler, contact_map)
                      | Ckappa_sig.Binding _ ->
                        let error, contact_map =
                          List.fold_left
                            (fun (error, contact_map) l ->
                               match l
                               with
                               | [site_type',state]
                                 when site_type' = site_type ->
                                 if state = Ckappa_sig.state_index_of_int 0
                                 then (* we ignore free sites *)
                                   error, contact_map
                                 else
                                   begin
                                     let error, dual =
                                       Handler.dual parameters error kappa_handler ag_type site_type state
                                     in
                                     match
                                       dual
                                     with
                                     | None ->
                                       Exception.warn
                                         parameters error __POS__ Exit contact_map
                                     | Some (ag_type', site_type', _ ) ->
                                       Preprocess.add_link_in_contact_map
                                         parameters error
                                         (ag_type,site_type) (ag_type',site_type')
                                         contact_map
                                   end
                               | [] | _::_ ->
                                 Exception.warn
                                   parameters error __POS__ Exit
                                   contact_map)
                            (error, contact_map) list_of_states
                        in
                        error, (handler, contact_map)
                    end
                  | _::_ ->
                    Exception.warn
                      parameters error __POS__ Exit (handler, contact_map)
               )
               site_map
               (error, (handler, contact_map))
          )
          ranges
          (handler, contact_map)
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      let kasa_state = Remanent_state.set_internal_contact_map Remanent_state.Medium contact_map kasa_state in
      error, dynamic, kasa_state

  (**************************************************************************)

  (*let print_static_information static dynamic error loggers =
    let parameter = get_parameter static in
    let log = Remanent_parameters.get_logger parameter in
    let kappa_handler = get_kappa_handler static in
    let compiled = get_compil static in
    let error, result = get_bdu_analysis_static static dynamic error in
    let parameter = Remanent_parameters.update_prefix parameter "agent_type_" in
    let error =
      if local_trace
        || Remanent_parameters.get_trace parameter
        || Remanent_parameters.get_dump_reachability_analysis_static parameter
      then
          (*List.fold_left (fun error log ->*)
        let _ = Loggers.print_newline log in
        let _ = Loggers.fprintf log
          "Reachability analysis static information module...."
        in
        let _ = Loggers.print_newline log in
        let parameters_cv = Remanent_parameters.update_prefix parameter "" in
        begin
          if (Remanent_parameters.get_trace parameters_cv)
          then
    let _ =
              Loggers.print_newline log
    in
    let error =
              Print_static_views.print_result_static
                parameter
                error
                kappa_handler
                compiled
                result
            in error
          else error
        end
      (* ) error loggers*)
      else
        error
    in
    error, dynamic, ()*)

  (**************************************************************************)

  (*let print_dynamic_information static dynamic error loggers =
    let parameter = get_parameter static in
    let log = Remanent_parameters.get_logger parameter in
    let kappa_handler = get_kappa_handler static in
    let compiled = get_compil static in
    let error, result = get_bdu_analysis_dynamic static dynamic error in
    let error =
        (*List.fold_left (fun error log ->*)
      let () = Loggers.print_newline log in
      let _ =
        Print_bdu_analysis_dynamic.print_result_dynamic
          parameter
          error
          kappa_handler
          compiled
          result
      in
      error
    (* ) error loggers*)
    in
    error, set_domain_dynamic_information result dynamic, ()*)

  (************************************************************************************)
  (*main print of fixpoint*)

  let print_bdu_update_map parameter error handler_kappa result =
    AgentCV_map_and_set.Map.fold (fun (agent_type, cv_id) bdu_update error ->
        let error', agent_string =
          Handler.string_of_agent parameter error handler_kappa agent_type
        in
        let error =
          Exception.check_point Exception.warn
            parameter error error' __POS__ Exit in
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_type:%i:%s:cv_id:%i"
            (Ckappa_sig.int_of_agent_name agent_type)
            agent_string
            (Covering_classes_type.int_of_cv_id cv_id)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameter)
        in
        let () =
          Ckappa_sig.Views_bdu.print
            parameter bdu_update
        in
        error)
      result error

  (************************************************************************************)

  let smash_map decomposition
      ~show_dep_with_dimmension_higher_than:_dim_min
      parameter handler error _handler_kappa site_correspondence result =
    let error,handler,mvbdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true
        parameter handler error
    in
    Covering_classes_type.AgentCV_map_and_set.Map.fold
      (fun (agent_type, cv_id) bdu (error,handler,output) ->
         let error, handler, list =
           decomposition parameter handler error bdu
         in
         let error, site_correspondence =
           Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
             parameter error agent_type site_correspondence
         in
         let error, site_correspondence =
           match site_correspondence with
           | None -> Exception.warn parameter error __POS__ Exit []
         | Some a -> error, a
       in
       let error, site_correspondence =
         let rec aux list =
           match list with
           | [] -> Exception.warn parameter error __POS__ Exit []
           | (h, list, _) :: _ when h = cv_id -> error, list
           | _ :: tail -> aux tail
         in aux site_correspondence
       in
       let error, (_map1, map2) =
         Bdu_static_views.new_index_pair_map parameter error site_correspondence
       in
       let rename_site parameter error site_type =
         let error, site_type =
           match Ckappa_sig.Site_map_and_set.Map.find_option
                   parameter error site_type map2
           with
           | error, None -> Exception.warn  parameter error __POS__ Exit
                              Ckappa_sig.dummy_site_name_minus1
           | error, Some i -> error, i
         in
         error, site_type
       in
       List.fold_left
         (fun (error, handler, output) bdu ->
            begin
              let error, handler, lvar =
                Ckappa_sig.Views_bdu.variables_list_of_mvbdu
                  parameter handler error
                  bdu
              in
              (*list: ckappa_sig.c_site_name list*)
              let error, handler, list =
                Ckappa_sig.Views_bdu.extensional_of_variables_list
                  parameter handler error
                  lvar
              in
              (*asso take (key * key) list *)
              let error, asso =
                List.fold_left
                  (fun (error, list) i ->
                     let error, new_name =
                       rename_site parameter error i
                     in
                     error,(i, new_name) :: list)
                  (error, [])
                  (List.rev list)
              in
              let error,handler,hconsed_asso =
                Ckappa_sig.Views_bdu.build_renaming_list
                  parameter handler error asso
              in
              let error,handler,renamed_mvbdu =
                Ckappa_sig.Views_bdu.mvbdu_rename
                  parameter handler error bdu hconsed_asso
              in
              let error,handler,hconsed_vars =
                Ckappa_sig.Views_bdu.variables_list_of_mvbdu
                  parameter handler error renamed_mvbdu
              in
              let error, cv_map_opt =
                Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                  parameter error agent_type output
              in
              let error,cv_map =
                match
                  cv_map_opt
                with
                | None ->
                  error, Wrapped_modules.LoggedIntMap.empty
                | Some map -> error, map
              in
                let error, handler, cv_map' =
                  Ckappa_sig.Views_bdu.store_by_variables_list
                    Wrapped_modules.LoggedIntMap.find_default_without_logs
                    Wrapped_modules.LoggedIntMap.add_or_overwrite
                    mvbdu_true
                    Ckappa_sig.Views_bdu.mvbdu_and
                    parameter
                    handler
                    error
                    hconsed_vars
                    renamed_mvbdu
                    cv_map
                in
                let error,output =
                  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                    parameter error agent_type cv_map'
                    output
                in
                error, handler, output
              end)
           (error, handler, output)
           list)
      result
      (let error, agent_map =
         Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create parameter error 0
       in
       (error, handler, agent_map))

  (**************************************************************************)

  let stabilise_bdu_update_map_gen_decomposition decomposition
      ~smash:smash ~show_dep_with_dimmension_higher_than:dim_min
      parameter handler error handler_kappa site_correspondence
      result =
    if
      smash
    then
      let error,handler,output =
        smash_map
          decomposition
          ~show_dep_with_dimmension_higher_than:dim_min parameter handler error
          handler_kappa site_correspondence result
      in
      let error, (handler, list) =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameter
          error
          (fun parameter error agent_type map (handler,list) ->
             let error', agent_string =
               try
                 Handler.string_of_agent parameter error handler_kappa agent_type
               with
               | _ -> Exception.warn parameter error __POS__  Exit
                        (Ckappa_sig.string_of_agent_name agent_type)
             in
             let error =
               Exception.check_point
                 Exception.warn  parameter error error' __POS__ Exit
             in
             (*-----------------------------------------------------------------------*)
             Wrapped_modules.LoggedIntMap.fold
               (fun _ mvbdu (error,(handler,list))
                 ->
                   let error, handler =
                     if local_trace || Remanent_parameters.get_trace parameter
                     then
                       let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                           "INTENSIONAL DESCRIPTION:"
                       in
                       let () = Loggers.print_newline
                           (Remanent_parameters.get_logger parameter)
                       in
                       let () =
                         Ckappa_sig.Views_bdu.print
                           parameter mvbdu
                       in
                       let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                           "EXTENSIONAL DESCRIPTION:"
                       in
                       let () = Loggers.print_newline
                           (Remanent_parameters.get_logger parameter)
                       in
                       error, handler
                     else
                       error, handler
                   in
                   let error, (handler, translation) =
                     Translation_in_natural_language.translate
                       parameter handler error (fun _ e i -> e, i) mvbdu
                   in
                   (*-----------------------------------------------------------------------*)
                   error, (handler,(agent_string, agent_type, mvbdu,translation)::list)
               )
               map
               (error, (handler,list)))
          output (handler,[])
      in
      error, handler, List.rev list
    else
      begin
        let error, (handler, list) = Covering_classes_type.AgentCV_map_and_set.Map.fold
            (fun (agent_type, cv_id) bdu_update (error,(handler,list)) ->
               let error', agent_string =
                 try
                   Handler.string_of_agent parameter error handler_kappa agent_type
                 with
                 | _ ->
                   Exception.warn
                     parameter error __POS__ Exit
                     (Ckappa_sig.string_of_agent_name agent_type)
               in
               let error =
                 Exception.check_point
                   Exception.warn  parameter error error'
                   __POS__ Exit
               in
               (*-----------------------------------------------------------------------*)
               let () =
                 if local_trace || Remanent_parameters.get_trace parameter
                 then
                   let () =
                     Loggers.fprintf (Remanent_parameters.get_logger parameter)
                       "agent_type:%i:%s:cv_id:%i"
                       (Ckappa_sig.int_of_agent_name agent_type)
                       agent_string
                       (Covering_classes_type.int_of_cv_id cv_id)
                   in
                   Loggers.print_newline (Remanent_parameters.get_logger parameter)
               in
               (*-----------------------------------------------------------------------*)
               let error, site_correspondence =
                 Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.get
                   parameter error agent_type site_correspondence
               in
               let error, site_correspondence =
                 match site_correspondence with
                 | None -> Exception.warn  parameter error __POS__ Exit []
                 | Some a -> error, a
               in
               let error, site_correspondence =
                 let rec aux list =
                   match list with
                   | [] -> Exception.warn  parameter error __POS__ Exit []
                   | (h, list, _) :: _ when h = cv_id -> error, list
                   | _ :: tail -> aux tail
                 in aux site_correspondence
               in
               (*-----------------------------------------------------------------------*)
               let error,(_, map2) =
                 Bdu_static_views.new_index_pair_map parameter error site_correspondence
               in
               (*-----------------------------------------------------------------------*)
               let error, handler, list' =
                 decomposition parameter handler error bdu_update
               in
               (*-----------------------------------------------------------------------*)
               let error, (handler, list) =
                 List.fold_left
                   (fun (error, (handler, list)) mvbdu ->
                      let error, handler =
                        if local_trace || Remanent_parameters.get_trace parameter
                        then
                          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                              "INTENSIONAL DESCRIPTION:" in
                          let () = Loggers.print_newline
                              (Remanent_parameters.get_logger parameter)
                          in
                          let () =
                            Ckappa_sig.Views_bdu.print
                              parameter mvbdu
                          in
                          let () = Loggers.fprintf
                              (Remanent_parameters.get_logger parameter)
                              "EXTENSIONAL DESCRIPTION:"
                          in
                          let () = Loggers.print_newline
                              (Remanent_parameters.get_logger parameter)
                          in
                          error, handler
                        else
                          error, handler
                      in
                      let rename_site parameter error site_type =
                        let error, site_type =
                          match Ckappa_sig.Site_map_and_set.Map.find_option
                                  parameter error site_type map2
                          with
                          | error, None ->
                            Exception.warn
                              parameter error __POS__ Exit
                              Ckappa_sig.dummy_site_name_minus1
                          | error, Some i -> error, i
                        in
                        error, site_type
                      in
                      let error, (handler, translation) =
                        Translation_in_natural_language.translate
                          parameter
                          handler
                          error
                          rename_site
                          mvbdu
                      in
                      (*--------------------------------------------------------------------*)
                      error, (handler,(agent_string, agent_type, mvbdu,translation)::list)
                   )
                   (error, (handler,list))
                   list'
               in
               error, (handler,list))
            result (error, (handler,[]))
        in error, handler, (List.rev list)
      end

  let print_bdu_update_map_gen_decomposition decomposition
      ~smash:smash ~show_dep_with_dimmension_higher_than:dim_min
      parameter handler error handler_kappa site_correspondence  result =
    let error, handler, list = stabilise_bdu_update_map_gen_decomposition decomposition
        ~smash:smash ~show_dep_with_dimmension_higher_than:dim_min
        parameter handler error handler_kappa site_correspondence  result
    in
    let error =
      List.fold_left
        (fun error (agent_string, agent_type, _, translation) ->
           Translation_in_natural_language.print
             ~show_dep_with_dimmension_higher_than:dim_min parameter
             handler_kappa error agent_string agent_type translation
        )
        error
        list
    in
    error, handler

  (****************************************************************)
  (*non relational properties*)

  let print_bdu_update_map_cartesian_abstraction a b c d =
    print_bdu_update_map_gen_decomposition
      ~smash:true
      ~show_dep_with_dimmension_higher_than:1
      Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction
      a b c d

  (*****************************************************************)
  (*relational properties*)

  let print_bdu_update_map_cartesian_decomposition a b c d =
    print_bdu_update_map_gen_decomposition
      ~smash:true
      ~show_dep_with_dimmension_higher_than:
        (if Remanent_parameters.get_hide_one_d_relations_from_cartesian_decomposition a
         then 2
         else 1
        )
      Ckappa_sig.Views_bdu.mvbdu_full_cartesian_decomposition
      a b c d

  (*****************************************************************)

  let print_result_fixpoint_aux
      parameter handler error handler_kappa site_correspondence result (_static:static_information) =
    if Remanent_parameters.get_dump_reachability_analysis_result parameter
    then
      let error =
        if local_trace
        || (Remanent_parameters.get_trace parameter)
        then
          begin
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "" in
            let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "------------------------------------------------------------" in
            let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "* Fixpoint iteration :"
            in
            let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "------------------------------------------------------------"
            in
            let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
            let error =
              print_bdu_update_map
                parameter
                error
                handler_kappa
                result
            in
            error
          end
        else error
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------" in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "* Relational properties:"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let error, handler =
        print_bdu_update_map_cartesian_decomposition
          parameter
          handler
          error
          handler_kappa
          site_correspondence
          result
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------" in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "* Non relational properties:"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "------------------------------------------------------------"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let error, handler =
        print_bdu_update_map_cartesian_abstraction
          parameter
          handler
          error
          handler_kappa
          site_correspondence
          result
      in
      error, handler
    else error, handler

  (************************************************************************************)

  let print_fixpoint_result static dynamic error _loggers =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, store_remanent_triple = get_store_remanent_triple static dynamic error in
    let fixpoint_result = get_fixpoint_result dynamic in
    let handler = get_mvbdu_handler dynamic in
    let error, handler =
      if local_trace
      || Remanent_parameters.get_dump_reachability_analysis_result parameter
      then
        let error =
          let _ =
            print_result_fixpoint_aux
              parameter
              handler
              error
              kappa_handler
              store_remanent_triple
              fixpoint_result
              static
          in
          error
        in
        error, handler
      else error, handler
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, ()

  (**************************************************************************)

  let print static dynamic error loggers =
    (*print static information*)
    (*let error, dynamic, () =
      print_static_information static dynamic error loggers
      in
      (*print dynamic information*)
      let error, dynamic, () =
      print_dynamic_information static dynamic error loggers
      in*)
    (*print fixpoint result*)
    let parameter = get_parameter static in
    let error, dynamic =
      (* local traces *)
      if Remanent_parameters.get_compute_local_traces parameter
      then
        let handler_kappa = get_kappa_handler static in
        let handler = get_mvbdu_handler dynamic in
        let compil = get_compil static in
        let error, site_correspondence =  get_store_remanent_triple static dynamic error in
        let error, handler, output = smash_map
            (fun _parameter handler error a -> error, handler, [a])
            parameter handler error
            ~show_dep_with_dimmension_higher_than:1
            handler_kappa
            site_correspondence
            (get_fixpoint_result dynamic)
        in
        let error, log_info, handler =
          Agent_trace.agent_trace parameter (get_log_info dynamic) error handler (get_global_static_information static) handler_kappa compil output
        in
        error, set_mvbdu_handler handler (set_log_info log_info dynamic)
      else
        error, dynamic
    in
    let error, dynamic, () =
      print_fixpoint_result static dynamic error loggers
    in
    error, dynamic, ()

  let stabilize static dynamic error =
    let dim_min = 2 in
    let parameter = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let handler_kappa = get_kappa_handler static in
    let result = get_fixpoint_result dynamic in
    let error, site_correspondence =  get_store_remanent_triple static dynamic error in
    let error, handler, ranges =
      smash_map
        Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction
        ~show_dep_with_dimmension_higher_than:dim_min parameter handler error
        handler_kappa site_correspondence result
    in
    let dynamic = set_ranges ranges dynamic in
    error, set_mvbdu_handler handler dynamic, ()

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
