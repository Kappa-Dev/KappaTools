(**
   * analyzer_sig.mli
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
   * Last modification:
   *
   * Compute the relations between sites in the BDU data structures
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(* Before properly achieving separation of concepts. We introduce one
   monolithic domain that collect everything (as in the previous analyzer).*)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "views_domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;	
      domain_static_information : Bdu_static_views.bdu_analysis_static
    }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)

  module AgentCV_map_and_set = Covering_classes_type.AgentCV_map_and_set
  module AgentIDCV_map_and_set = Covering_classes_type.AgentIDCV_map_and_set

  type local_dynamic_information =
    {
      dead_rule       : bool array;
      fixpoint_result : Mvbdu_wrapper.Mvbdu.mvbdu AgentCV_map_and_set.Map.t;
      proj_fixpoint_result : Mvbdu_wrapper.Mvbdu.mvbdu AgentIDCV_map_and_set.Map.t;
      domain_dynamic_information : Bdu_dynamic_views.bdu_analysis_dynamic;
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

  (** handler *)
  let get_mvbdu_handler dynamic =
    Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)

  let set_mvbdu_handler handler dynamic =
    {
      dynamic with
        global = Analyzer_headers.set_mvbdu_handler handler
        (get_global_dynamic_information dynamic)
    }

  (** local dynamic information*)

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  (** dead rule local dynamic information*)
  let get_dead_rule dynamic = dynamic.dead_rule

  let set_dead_rule dead_rule dynamic =
    {
      dynamic with dead_rule = dead_rule
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

  (*get projection for fixpoint result*)
  let get_proj_fixpoint_result dynamic =
    (get_local_dynamic_information dynamic).proj_fixpoint_result

  let set_proj_fixpoint_result result dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          proj_fixpoint_result = result
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
      Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler_bdu error
    in
    error,
    set_mvbdu_handler handler_bdu dynamic,
    bdu_false

  (** the initial build for mvbdu_true*)
  let get_mvbdu_true global_static dynamic error =
    let parameter = get_parameter global_static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, handler_bdu, bdu_true =
      Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler_bdu error
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
    let error, (handler_bdu, result) =
      Bdu_static_views.scan_rule_set
        parameter
        handler_bdu
        error
        kappa_handler
        compiled
        potential_side_effects
    in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let static = set_domain_static result static in
    error, static, dynamic

  let scan_rule_set_dynamic static dynamic error =
    let parameter = get_parameter static in
    let compiled = get_compil static in
    let handler_bdu = get_mvbdu_handler dynamic in
    let store_pre_static = get_pre_static static in
    let covering_classes = get_covering_classes static in
    let covering_classes_id = get_covering_classes_id static in
    let potential_side_effects = get_potential_side_effects static in
    let error, (handler_bdu, store_result) =
      Bdu_dynamic_views.scan_rule_set_dynamic
        parameter
        error
        compiled
        handler_bdu
        store_pre_static
        covering_classes
        covering_classes_id
        potential_side_effects
    in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let dynamic = set_domain_dynamic_information store_result dynamic in
    error, static, dynamic

  (**************************************************************************)

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    let error, init_bdu_analysis_static =
      Bdu_static_views.init_bdu_analysis_static parameter error
    in
    let init_global_static =
      {
        global_static_information = static;
        domain_static_information = init_bdu_analysis_static
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let nrules = Handler.nrules parameter error kappa_handler in
    let init_dead_rule_array = Array.make nrules false in
    let init_fixpoint = AgentCV_map_and_set.Map.empty in
    let init_proj = AgentIDCV_map_and_set.Map.empty in
    let init_bdu_analysis_dynamic = Bdu_dynamic_views.init_bdu_analysis_dynamic
    in
    let init_global_dynamic =
      {
	global = dynamic;
	local =
	  { dead_rule = init_dead_rule_array;
	    fixpoint_result = init_fixpoint;
            proj_fixpoint_result = init_proj;
	    domain_dynamic_information = init_bdu_analysis_dynamic;
	  }}
    in
    let error, init_static, init_dynamic =
      scan_rule_set_static init_global_static init_global_dynamic error
    in
    let error, static, dynamic =
      scan_rule_set_dynamic init_static init_dynamic error
    in
    error, static, dynamic
										
  (** get type bdu_analysis_static*)
  let get_bdu_analysis_static static dynamic error =
    let result = static.domain_static_information in
    error, result

  (**get type bdu_analysis_dynamic*)
  let get_bdu_analysis_dynamic static dynamic error =
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

  let get_store_covering_classes_modification_update_full static dynamic error =
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
      (*-----------------------------------------------------------------------*)
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 56") Exit (string_of_int agent_type)
      in
      (*-----------------------------------------------------------------------*)
      let error, site_correspondence =
        Bdu_static_views.AgentMap.get parameter error agent_type site_correspondence
      in
      let error, site_correspondence =
        match site_correspondence with
        | None -> warn parameter error (Some "line 187") Exit []
        | Some a -> error, a
      in
      (*get a list of sites in a covering class *)
      let error, site_correspondence =
        let rec aux list =
	  match list with
	  | [] -> warn parameter error (Some "line 195") Exit []
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
		_ -> warn parameter error (Some "line 210") Exit (string_of_int site_type)
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

  (************************************************************************************)

  let updates_list2event_list ?title:(title="") static dynamic error agent_type cv_id
      event_list =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let error, store_covering_classes_modification_update_full =
      get_store_covering_classes_modification_update_full static dynamic error
    in
    let error, (_, s1) =
      match
        Bdu_dynamic_views.AgentCV_map_and_set.Map.find_option_without_logs
          parameter
          error
          (agent_type, cv_id)
          store_covering_classes_modification_update_full
      with
      | error, None -> error, ([], Cckappa_sig.Site_map_and_set.Set.empty)
      | error, Some (l, s) -> error, (l, s)
    in
  (*-----------------------------------------------------------------------*)
  (*print working list information*)
    let error =
      if local_trace
        || Remanent_parameters.get_dump_reachability_analysis_wl parameter
      then
        begin
	  let log = Remanent_parameters.get_logger parameter in
        (*-----------------------------------------------------------------------*)
	  let error, agent_string =
	    try
              Handler.string_of_agent parameter error kappa_handler agent_type
            with
              _ -> warn
                parameter error (Some "line 460") Exit (string_of_int agent_type)
	  in
        (*-----------------------------------------------------------------------*)
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
        (*-----------------------------------------------------------------------*)
	  let () =
            Cckappa_sig.Site_map_and_set.Set.iter (fun rule_id ->
              let compiled = get_compil static in
	      let error, rule_id_string =
	        try
		  Handler.string_of_rule parameter error kappa_handler
		    compiled rule_id
	        with
		  _ -> warn parameter error (Some "line 250") Exit (string_of_int rule_id)
	      in
	      let tab =
	        if title = "" then "\t" else "\t\t"
	      in
	      let () =
                Loggers.fprintf log "%s%s(%s) should be investigated "
                  (Remanent_parameters.get_prefix parameter) tab rule_id_string
              in
	      let () = Loggers.print_newline log in ())
	      s1 in
	  error
        end
      else error
    in
  (*-----------------------------------------------------------------------*)
  (*convert into an event list*)
    error,
    Cckappa_sig.Site_map_and_set.Set.fold
      (fun rule_id event_list ->
        (Communication.Check_rule rule_id) :: event_list)
      s1 event_list

  (**************************************************************************)

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
        Mvbdu_wrapper.Mvbdu.mvbdu_xor parameter handler error bdu_old bdu_union
      in
      let dynamic = set_mvbdu_handler handler dynamic in
      (*-----------------------------------------------------------------------*)
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 56") Exit (string_of_int agent_type)
      in
    (*-----------------------------------------------------------------------*)
    (*list of sites in a covering class*)
      let error, site_correspondence =
        match Bdu_static_views.AgentMap.get parameter
          error agent_type site_correspondence
        with
        | error, None -> warn parameter error (Some "73") Exit []
        | error, Some a -> error, a
      in
      let error, site_correspondence =
        let rec aux list =
	  match list with
	  | [] -> warn parameter error (Some "line 68") Exit []
	  | (h, list, _) :: _ when h = cv_id -> error, list
	  | _ :: tail -> aux tail
        in aux site_correspondence
      in
    (*-----------------------------------------------------------------------*)
    (*build a pair of coresspondence map:
      - map1: global -> local; map2: local -> global*)
    let error, (map1, map2) =
      Bdu_static_views.new_index_pair_map parameter error site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter)  in
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
          Mvbdu_wrapper.Mvbdu.print parameter bdu_diff
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
      Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error bdu_diff
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    (*-----------------------------------------------------------------------*)
    (*print function for extentional description*)
    let error =
      List.fold_left
	(fun error l ->
	 let error, bool =
	   List.fold_left
	     (fun (error, bool) (site_type, state) ->
               let error, site_type =
                 match Cckappa_sig.Site_map_and_set.Map.find_option
                   parameter error site_type map2
                 with
                 | error, None -> warn parameter error (Some "line 100") Exit (-1)
                 | error, Some i -> error, i
               in
               (*-----------------------------------------------------------------------*)
	       let error, site_string =
		 try
                   Handler.string_of_site parameter error handler_kappa
		     agent_type site_type
		 with
		   _ -> warn parameter error (Some "line 136")
                     Exit (string_of_int site_type)
	       in
	       let error, state_string =
                 try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type site_type state
		 with
		   _ -> warn parameter error (Some "line 146") Exit (string_of_int state)
               in
               (*-----------------------------------------------------------------------*)
               let () =
		 if bool
                 then Loggers.fprintf (Remanent_parameters.get_logger parameter) ","
		 else Loggers.fprintf (Remanent_parameters.get_logger parameter)
                   "\t%s%s(" prefix agent_string
               in
	       let () =
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
    in error, dynamic
  else error, dynamic

  (**************************************************************************)

  let add_link ?title error static dynamic (agent_type, cv_id) bdu event_list =
    let parameter = get_parameter static in
    let log = Remanent_parameters.get_logger parameter in
    let error, store_remanent_triple =
      get_store_remanent_triple static dynamic error
    in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let store = get_fixpoint_result dynamic in
    let error, bdu_old =
      match AgentCV_map_and_set.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store
      with
      | error, None -> error, bdu_false
      | error, Some bdu -> error, bdu
    in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    let updates_list = [] in
    (*-----------------------------------------------------------*)
    let error, dynamic, title, is_new_views, updates_list =
      if Mvbdu_wrapper.Mvbdu.equal bdu_old bdu_union
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
          AgentCV_map_and_set.Map.add_or_overwrite parameter error
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
      error, is_new_views, dynamic, event_list
    else
      (*let () =
        let () =
        Loggers.fprintf log "\tInitial state is empty"
        in
        let () = Loggers.print_newline log in
        let () = Loggers.print_newline log in
        ()
        in*)
      error, is_new_views, dynamic, event_list

  (***************************************************************************)
  (*get map restriction from covering classes*)

  let get_pair_list static error agent triple_list =
    let parameter = get_parameter static in
    (*let kappa_handler = get_kappa_handler static in*)
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
	      Cckappa_sig.Site_map_and_set.Map.find_option
		parameter error site map_new_index_forward
	    with
	    | error, None -> warn parameter error (Some "398") Exit 0
	    | error, Some s -> error, s
	  in
	  Cckappa_sig.Site_map_and_set.Map.add
	    parameter error site' state store
	in
	let error', map_res =
	  Cckappa_sig.Site_map_and_set.Map.fold_restriction_with_missing_associations
	    parameter error
	    (fun site port ->
              add site port.Cckappa_sig.site_state.Cckappa_sig.min)
	    (*JF: we should check that port.site_state.min is equal to
              port.site_state.max*)
	    (fun site -> add site 0)
	    set
	    agent.Cckappa_sig.agent_interface
	    Cckappa_sig.Site_map_and_set.Map.empty
	in
	let error = Exception.check warn parameter error error'
	  (Some "line 370") Exit
	in
	error, ((cv_id, map_res) :: current_list)
      ) (error, []) triple_list
    in
    error, get_pair_list

  (***************************************************************************)
  (*build bdu restriction for initial state *)

  let bdu_build static dynamic error pair_list =
    let parameter = get_parameter static in
    let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
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

  (************************************************************************************)

  let build_init_restriction static dynamic error init_state =
    let parameter = get_parameter static in
    let error, store_remanent_triple =
      get_store_remanent_triple static dynamic error
    in
    let error, (dynamic, event_list) =
      Cckappa_sig.Agent_id_storage_quick_nearly_inf_Imperatif.fold parameter error
        (fun parameter error agent_id agent (dynamic, event_list) ->
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, (dynamic, event_list)
          | Cckappa_sig.Dead_agent _ ->
            warn parameter error (Some "") Exit (dynamic, event_list)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*----------------------------------------------------------------*)
            let error, (dynamic, event_list) =
              match Bdu_static_views.AgentMap.unsafe_get parameter error agent_type
                store_remanent_triple
              with
              | error, Some l ->
                let error, get_pair_list = get_pair_list static error agent l in
                let error, (dynamic, event_list) =
                  List.fold_left (fun (error, (dynamic, event_list)) (cv_id, map_res) ->
                    let error, pair_list =
                      Cckappa_sig.Site_map_and_set.Map.fold
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
                    (*----------------------------------------------------------------*)
                    let error, is_new_views, dynamic, event_list =
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

  (**************************************************************************)
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

  (**************************************************************************)

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

  (**************************************************************************)

  exception False of Exception.method_handler * dynamic_information

  exception False' of Exception.method_handler *
      (Mvbdu_wrapper.Mvbdu.mvbdu AgentIDCV_map_and_set.Map.t)

  (*precondition?*)
  (*let error, precondition, _ =
    Communication.fold_over_potential_partners
    parameter
    error
    precondition
    agent_type
    site
    (fun parameter state (agent_type, site, state) (error, a) ->
    error, a
    )
    a
    in*)

  let is_enable_aux static dynamic error rule_id = (*REMOVE later*)
    let parameter = get_parameter static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let error, proj_bdu_test_restriction =
      match
        Bdu_static_views.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Bdu_static_views.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    try
      let error, dynamic =
        Bdu_static_views.AgentsCV_setmap.Map.fold
          (fun (agent_id, agent_type, cv_id) bdu_test (error, dynamic) ->
            let error, bdu_X =
              match
                AgentCV_map_and_set.Map.find_option_without_logs parameter
                  error (agent_type, cv_id) fixpoint_result
              with
              | error, None -> Printf.fprintf stdout "cv_id %i\n" cv_id; error, bdu_false
              | error, Some bdu -> error, bdu
            in
            let handler = get_mvbdu_handler dynamic in
            let error, handler, bdu_inter =
              Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
            in
            let dynamic = set_mvbdu_handler handler dynamic in
            if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
            then raise (False (error, dynamic))
            else error, dynamic
          ) proj_bdu_test_restriction (error, dynamic)
      in
      error, dynamic, true
    with
      False (error, dynamic) -> error, dynamic, false

  (**************************************************************************)
  (*projection the result of bdu_fixpoint from:
    [map (agent_type, cv_id) bdu] to [map (agent_id, cv_id) bdu]*)
        
  let proj_fixpoint_result static dynamic error rule_id =
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let parameter = get_parameter static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let handler = get_mvbdu_handler dynamic in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let error, proj_bdu_test_restriction =
      match
        Bdu_static_views.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Bdu_static_views.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    let error, dynamic =
      Bdu_static_views.AgentsCV_setmap.Map.fold 
        (fun (agent_id, agent_type, cv_id) bdu_test (error, dynamic) ->
          let error, map =
            Covering_classes_type.Project_agent.monadic_proj
              (fun parameter error (agent_type, cv_id) ->
                error, (agent_id, cv_id))
              parameter
              error
              bdu_false
              (fun parameter error bdu bdu' ->
                let error, handler, bdu =
                  Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu bdu'
                in
                error, bdu)
              fixpoint_result
          in
          let dynamic = set_proj_fixpoint_result map dynamic in
          error, dynamic
        ) proj_bdu_test_restriction (error, dynamic)
    in
    error, dynamic

  (**************************************************************************)

  let init_path =
    {
      Communication.agent_id = 0;
      Communication.relative_address = [];
      Communication.site = 0
    }

  let is_enable_aux' static dynamic error rule_id precondition =
    let parameter = get_parameter static in
    let fixpoint_result = get_fixpoint_result dynamic in
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let error, site_correspondence =
      get_store_remanent_triple static dynamic error
    in
    let error, proj_bdu_test_restriction =
      match
        Bdu_static_views.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Bdu_static_views.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    try
      let error, dynamic, map =
        Bdu_static_views.AgentsCV_setmap.Map.fold
          (fun (agent_id, agent_type, cv_id) bdu_test (error, dynamic, map) ->
            (*---------------------------------------------------------------------*)
            (*for each (agent_id, cv_id) a bdu*)
            let error, bdu_X =
              match
                AgentCV_map_and_set.Map.find_option_without_logs parameter
                  error (agent_type, cv_id) fixpoint_result
              with
              | error, None -> Printf.fprintf stdout "cv_id %i\n" cv_id; error, bdu_false
              | error, Some bdu -> error, bdu
            in
            (*---------------------------------------------------------------------*)
            (*bdu intersection*)
            let handler = get_mvbdu_handler dynamic in
            let error, handler, bdu_inter =
              Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
            in
            let dynamic = set_mvbdu_handler handler dynamic in
            if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
            then raise (False (error, dynamic))
            else 
              let error, map =
                AgentIDCV_map_and_set.Map.add parameter error
                  (agent_id, cv_id) bdu_inter map
              in
              error, dynamic, map
          ) proj_bdu_test_restriction (error, dynamic,AgentIDCV_map_and_set.Map.empty)
      in
      (*---------------------------------------------------------------------*)
      (*get a set of sites in a covering class: later with state list*)
(*      let precondition = *)
        (*AgentIDCV_map_and_set.Map.fold
          (fun (agent_id, cv_id) bdu precondition ->
            let error, handler, list =
              Mvbdu_warpper.Mvbdu.extensional_of_mvbdu parameter handler error bdu
            in*)
      let precondition =
        Communication.refine_information_about_state_of_site
          precondition
          (fun error path former_answer ->
            let step_list = path.Communication.relative_address in
            match
              step_list
            with
              _::_ -> error, former_answer
            | [] ->
              begin
                let agent_id = path.Communication.agent_id in
                let agent_type = 1 in (* to do, get the agent type from the agent id *)
                let site_name = path.Communication.site in
                let error, site_correspondence =
                  match Bdu_static_views.AgentMap.get parameter error
                    agent_type site_correspondence
                  with
                  | error, None -> warn parameter error (Some "") Exit []
                  | error, Some a -> error, a
                in
                let cv_list = [] (* compute the list of cv_id documenting site_name *)
                in
                let error, bdu =
                  List.fold_left
                    (fun (error, bdu) cv_id ->
                       let error, site_correspondence =
                         let rec aux list =
                           match list with
                           | [] -> warn parameter error (Some "") Exit []
                           | (h, list, _) :: _ when h = cv_id -> error, list
                           | _ :: tail -> aux tail
                         in
                         aux site_correspondence
                       in
                       let error, (map1, map2) =
                         Bdu_static_views.new_index_pair_map parameter error 
                           site_correspondence
                       in
                       let error, new_site_name =
                         match Cckappa_sig.Site_map_and_set.Map.find_option
                           parameter error site_name map2
                         with
                         | error, None -> warn parameter error (Some "") Exit (-1)
                         | error, Some i -> error, i
                       in
                       let error, bdu' = error, bdu_true (* fetch the bdu for the qgent type and the cv_id in the current state of the iteration *)
                       in
                       let error, bdu' = error, bdu' (* to do compute the projection over new_site_name *) in
                       let error, bdu' = error, bdu' (* to do. rename new_site_name into 1 *) in
                       let error, bdu = error, bdu (* to do conjunction between bdu and bdu'*) in
                       error, bdu)
                    (error, bdu_true)
                    cv_list
                in
                

                
(*              let error, handler, renamed_mvbdu =
                Mvbdu_wrapper.Mvbdu.mvbdu_rename parameter handler error 
                  bdu hconsed_asso
              in
              let error, handler, singleton =
                Mvbdu_wrapper.Mvbdu.build_variables_list parameter handler error
                  site_correspondence
              in
              let error, handler, proj_in =
                Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error
                  renamed_mvbdu singleton
              in*)
                let handler = get_mvbdu_handler dynamic in
                let error, handler, list =
                Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error
                  bdu
                in
                let dynamic = set_mvbdu_handler handler dynamic in
              let error, state_list =
                List.fold_left
                  (fun (error,output) list ->
                    match 
                      list
                    with
                  | [_,state] -> error,state::output
                  | _ -> 
                    warn parameter error (Some "line 1134") Exit output)
                  (error,[]) 
                  list
              in
              error, Usual_domains.Val (List.rev state_list)
              end
          )
      in   
      error, (dynamic, precondition), true
    with
      False (error, dynamic) -> error, (dynamic, precondition), false

                      (*let empty_path = 
                        {
                          Communication.agent_id = 0;
                          Communication.relative_address = [];
                          Communication.site = 0
                        }
                      in
                      let is_empty_path path =
                        if path = empty_path
                        then true
                        else false                       
                      in
                      if is_empty_path path
                      then error, usual
                      else*)
                        (*------------------------------------------------------------*)
                        (*do the bdu_rename for bdu*)
                      (*let handler = get_mvbdu_handler dynamic in
                        let error, handler, list =
                          Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error
                            bdu
                        in
                        let site_path = path.Communication.site in
                        let pair_list =
                          List.fold_left (fun current_list l ->
                            List.fold_left (fun current_list (site, state) ->
                              if site_path = site
                              then (site, state) :: current_list
                              else current_list
                            ) current_list l
                          ) [] list
                        in
                        let error, handler, list_a =
                          Mvbdu_wrapper.Mvbdu.build_association_list
                            parameter
                            handler
                            error
                            pair_list
                        in
                        let error, handler, bdu =
                          Mvbdu_wrapper.Mvbdu.mvbdu_rename parameter handler error
                            bdu list_a
                        in
                        (*------------------------------------------------------------*)
                        (*do the projection*)
                        let site_list =
                          List.fold_left (fun current_list l ->
                            List.fold_left (fun current_list (site, state) ->
                              if site = site_path 
                              then site :: current_list
                              else current_list
                            ) current_list l
                          ) [] list
                        in
                        let error, handler, singleton =
                          Mvbdu_wrapper.Mvbdu.build_variables_list parameter handler error
                            site_list
                        in
                        let error, handler, proj_in =
                          Mvbdu_wrapper.Mvbdu.mvbdu_project_keep_only parameter handler
                            error bdu singleton
                        in
                        let error, handler, list =
                          Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error
                            proj_in
                        in
                        (*store dynamic*)
                        (*let dynamic = set_mvbdu_handler handler dynamic in*)
                        let state_list =
                          List.fold_left (fun current_list l ->
                            let (_, state_list) = List.split l in
                            List.concat [state_list; current_list]
                          ) [] list
                         in
                        let precondition = Usual_domains.Val state_list in
                        error, precondition*)
(* )
                ) map precondition
            in
            (*---------------------------------------------------------------------*)
            (*FIXME: intersection of bdu and projection of bdu*)
            if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
            then raise (False (error, dynamic))
            else error, dynamic, map
          ) proj_bdu_test_restriction
          (error, dynamic, AgentIDCV_map_and_set.Map.empty)
      in
      error, dynamic, true, map
    with
      False (error, dynamic) -> error, dynamic, false, AgentIDCV_map_and_set.Map.empty*)

  (**************************************************************************)
  (*get contact_map from dynamic*)
  (* then use the functions get_potential_partner and/or
     fold_over_potential_partners in the views domain to use the incremental
     (dynamic) contact map *)
  (* instead of the static one *)

  let is_enabled static dynamic error rule_id precondition =
    let error, dynamic, is_enable =
      is_enable_aux
        static
        dynamic
        error
        rule_id
    in
    if is_enable
    then
      (*the information about the dynamic contact map is available?, if
        yes, use them, otherwise use the static contact map instead*)
      error, dynamic, Some precondition
    (* TO DO, update the function state_of_site in precondition *)
    else
      error, dynamic, None

  (**************************************************************************)
  (*deal with views*)

  let compute_bdu_update_aux static dynamic error bdu_test list_a bdu_X =
    let parameter = get_parameter static in
    let parameter_views = Remanent_parameters.update_prefix parameter "\t\t\t" in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_inter =
      Mvbdu_wrapper.Mvbdu.mvbdu_and parameter_views handler error bdu_X bdu_test
    in
    (*redefine with modification list*)
    let error, handler, bdu_redefine =
      Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter_views handler error bdu_inter list_a
    in
    (*do the union of bdu_redefine and bdu_X*)
    let error, handler, bdu_result =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter_views handler error bdu_redefine bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (**************************************************************************)

  let compute_bdu_update_views static dynamic error bdu_test list_a bdu_X =
    let error, dynamic, bdu_result =
      compute_bdu_update_aux
        static
        dynamic
        error
        bdu_test
        list_a
        bdu_X
    in
    error, dynamic, bdu_result

  (**************************************************************************)

  let compute_bdu_update_creation static dynamic error bdu_creation bdu_X =
    let parameter = get_parameter static in
    let handler = get_mvbdu_handler dynamic in
    let error, handler, bdu_result =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_creation bdu_X
    in
    let dynamic = set_mvbdu_handler handler dynamic in
    error, dynamic, bdu_result

  (**************************************************************************)

  let compute_bdu_update_side_effects static dynamic error bdu_test list_a bdu_X =
    let error, dynamic, bdu_result =
      compute_bdu_update_aux
        static
        dynamic
        error
        bdu_test
        list_a
        bdu_X
    in
    error, dynamic, bdu_result

  (**************************************************************************)

  let compute_views_test_enabled static dynamic error rule_id event_list =
    let parameter = get_parameter static in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let error, proj_bdu_test_restriction =
      match
        Bdu_static_views.Rule_setmap.Map.find_option rule_id
          store_proj_bdu_test_restriction
      with
      | None -> error, Bdu_static_views.AgentsCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      (*-----------------------------------------------------------------------*)
      (*deal with views*)
      Bdu_static_views.AgentsCV_setmap.Map.fold
        (fun (agent_id, agent_type, cv_id) _ (error, dynamic, event_list) ->
          let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
          let error, dynamic, bdu_true = get_mvbdu_true static dynamic error in
          let error, store_remanent_triple = get_store_remanent_triple static dynamic error
          in
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
          (*-----------------------------------------------------------------------*)
          let store_result = get_fixpoint_result dynamic in
          let error, bdu_X =
            match AgentCV_map_and_set.Map.find_option_without_logs
              parameter error (agent_type, cv_id) store_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, bdu_test =
            match Bdu_static_views.AgentsCV_setmap.Map.find_option
              (agent_id, agent_type, cv_id) proj_bdu_test_restriction
            with
            | None -> error, bdu_true
            | Some bdu -> error, bdu
          in
	  let error, dynamic, bdu_update =
            match Bdu_static_views.AgentsRuleCV_map_and_set.Map.find_option_without_logs
              parameter error
              (agent_id, agent_type, rule_id, cv_id)
              store_modif_list_restriction_map
	    with
            | error, None -> error, dynamic, bdu_X
	    | error, Some list_a ->
              let error, dynamic, bdu_update =
                compute_bdu_update_views
                  static
                  dynamic
                  error
                  bdu_test
                  list_a
                  bdu_X
              in
              error, dynamic, bdu_update
          in
          let error, is_new_views, dynamic, event_list =
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
      match Bdu_static_views.Rule_setmap.Map.find_option rule_id
        store_bdu_creation
      with
      | None -> error, Bdu_static_views.AgentCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      Bdu_static_views.AgentCV_setmap.Map.fold
        (fun (agent_type, cv_id) bdu_creation (error, dynamic, event_list) ->
          let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
          let fixpoint_result = get_fixpoint_result dynamic in
          let error, bdu_X =
            match AgentCV_map_and_set.Map.find_option_without_logs
              parameter error (agent_type, cv_id) fixpoint_result
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
          let error, dynamic, bdu_update =
            compute_bdu_update_creation
              static
              dynamic
              error
              bdu_creation
              bdu_X
          in
          let error, is_new_views, dynamic, event_list =
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
        )
        bdu_creation_map (error, dynamic, event_list)
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
      match Bdu_static_views.Rule_setmap.Map.find_option rule_id
        store_bdu_potential
      with
      | None -> error, Bdu_static_views.AgentSiteCV_setmap.Map.empty
      | Some map -> error, map
    in
    (*-----------------------------------------------------------------------*)
    let error, dynamic, event_list =
      Bdu_static_views.AgentSiteCV_setmap.Map.fold
        (fun (agent_type, new_site_id, cv_id)(bdu_test, list)
          (error, dynamic, event_list) ->
            let fixpoint_result = get_fixpoint_result dynamic in
            let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
            let error, bdu_X =
	      match AgentCV_map_and_set.Map.find_option_without_logs
                parameter error (agent_type, cv_id) fixpoint_result
	      with
	      | error, None -> error, bdu_false
	      | error, Some bdu -> error, bdu
	    in
	    let error, dynamic, bdu_update =
              compute_bdu_update_side_effects
                static
                dynamic
                error
                bdu_test
                list
                bdu_X
            in
            let error, is_new_views, dynamic, event_list =
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

  (**************************************************************************)
  (*  let dead_rule_array = dynamic.dead_rule in
      to be pushed in apply_rule in the rule domain
      (when we will split the domain concept-wise) *)

  let apply_rule static dynamic error rule_id precondition =
    let error, dynamic, event_list =
      compute_views_enabled static dynamic error rule_id precondition
    in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    (*TODO?*)
    error, dynamic, []

  let export static dynamic error kasa_state =
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
              Print_bdu_analysis_static.print_result_static (*FIXME*)
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
      let error = Exception.check warn parameter error error' (Some "line 95") Exit in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:cv_id:%i"
          agent_type agent_string cv_id
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      let () =
        Mvbdu_wrapper.Mvbdu.print parameter bdu_update
      in
      error)
      result error

(************************************************************************************)

  let smash_map decomposition
      ~show_dep_with_dimmension_higher_than:dim_min
      parameter handler error handler_kappa site_correspondence result =
    let error,handler,mvbdu_true =
      Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error
    in
    AgentCV_map_and_set.Map.fold
      (fun (agent_type, cv_id) bdu (error,handler,output) ->
        let error, handler, list =
          decomposition parameter handler error bdu
        in
        let error, site_correspondence =
          Bdu_static_views.AgentMap.get parameter error agent_type site_correspondence
        in
        let error, site_correspondence =
	  match site_correspondence with
	  | None -> warn parameter error (Some "line 58") Exit []
	  | Some a -> error, a
        in
        let error, site_correspondence =
	  let rec aux list =
	    match list with
	    | [] -> warn parameter error (Some "line 68") Exit []
	    | (h, list, _) :: _ when h = cv_id -> error, list
	    | _ :: tail -> aux tail
	  in aux site_correspondence
        in
        let error, (map1, map2) =
          Bdu_static_views.new_index_pair_map parameter error site_correspondence
        in
        let rename_site parameter error site_type =
          let error, site_type =
            match Cckappa_sig.Site_map_and_set.Map.find_option
              parameter error site_type map2
            with
            | error, None -> warn parameter error (Some "line 165") Exit (-1)
            | error, Some i -> error, i
          in
          error, site_type
        in
        List.fold_left
	  (fun (error, handler, output) bdu ->
	    begin
	      let error, handler, lvar =
	        Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu
		  parameter handler error
		  bdu
	      in
	      let error, handler, list =
	        Mvbdu_wrapper.Mvbdu.extensional_of_variables_list
		  parameter handler error
		  lvar
	      in
	      let error,asso =
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
	        Mvbdu_wrapper.Mvbdu.build_association_list parameter handler error asso
	      in
	      let error,handler,renamed_mvbdu =
	        Mvbdu_wrapper.Mvbdu.mvbdu_rename parameter handler error bdu hconsed_asso
	      in
	      let error,handler,hconsed_vars =
	        Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu
                  parameter handler error renamed_mvbdu
	      in
              let error, cv_map_opt =
	        Bdu_static_views.AgentMap.unsafe_get parameter error agent_type output
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
	        Mvbdu_wrapper.Mvbdu.store_by_variables_list
       		  Wrapped_modules.LoggedIntMap.find_default_without_logs
		  Wrapped_modules.LoggedIntMap.add_or_overwrite
		  mvbdu_true
		  Mvbdu_wrapper.Mvbdu.mvbdu_and
		  parameter
		  handler
		  error
		  hconsed_vars
		  renamed_mvbdu
		  cv_map
	      in
	      let error,output =
	        Bdu_static_views.AgentMap.set parameter error agent_type cv_map'
		  output
	      in
	      error, handler, output
	    end)
	  (error, handler, output)
	  list)
      result
      (let error, agent_map =
         Bdu_static_views.AgentMap.create parameter error 0
       in
       (error, handler, agent_map))

(**************************************************************************)

  let print_bdu_update_map_gen_decomposition decomposition
      ~smash:smash ~show_dep_with_dimmension_higher_than:dim_min
      parameter handler error handler_kappa site_correspondence result =
    if
      smash
    then
      let error,handler,output =
        smash_map
          decomposition
          ~show_dep_with_dimmension_higher_than:dim_min parameter handler error handler_kappa site_correspondence result
      in
      Bdu_static_views.AgentMap.fold
        parameter
        error
        (fun parameter error agent_type map (handler:Mvbdu_wrapper.Mvbdu.handler) ->
	  let error', agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 111") Exit (string_of_int agent_type)
	  in
	  let error = Exception.check warn parameter error error' (Some "line 110") Exit in
	  (*-----------------------------------------------------------------------*)
	  Wrapped_modules.LoggedIntMap.fold
	    (fun _ mvbdu (error,handler)
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
		  let () = Mvbdu_wrapper.Mvbdu.print parameter mvbdu in
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
	      let error =
	        Translation_in_natural_language.print
		  ~show_dep_with_dimmension_higher_than:dim_min parameter
		  handler_kappa error agent_string agent_type translation
	      in
	      error, handler
	    )
	    map
	    (error, handler))
        output handler
    else
      begin
        AgentCV_map_and_set.Map.fold
	  (fun (agent_type, cv_id) bdu_update (error,handler) ->
	    let error', agent_string =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type
              with
                _ -> warn parameter error (Some "line 111") Exit (string_of_int agent_type)
	    in
	    let error = Exception.check warn parameter error error'
              (Some "line 110") Exit
            in
            (*-----------------------------------------------------------------------*)
	    let () =
	      if local_trace || Remanent_parameters.get_trace parameter
	      then
	        let () =
		  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "agent_type:%i:%s:cv_id:%i"
		    agent_type agent_string cv_id
	        in
	        Loggers.print_newline (Remanent_parameters.get_logger parameter)
	    in
            (*-----------------------------------------------------------------------*)
	    let error, site_correspondence =
              Bdu_static_views.AgentMap.get parameter error agent_type site_correspondence
	    in
	    let error, site_correspondence =
	      match site_correspondence with
	      | None -> warn parameter error (Some "line 58") Exit []
	      | Some a -> error, a
	    in
	    let error, site_correspondence =
	      let rec aux list =
	        match list with
	        | [] -> warn parameter error (Some "line 68") Exit []
	        | (h, list, _) :: _ when h = cv_id -> error, list
	        | _ :: tail -> aux tail
	      in aux site_correspondence
	    in
            (*-----------------------------------------------------------------------*)
	    let error,(map1, map2) =
              Bdu_static_views.new_index_pair_map parameter error site_correspondence
	    in
            (*-----------------------------------------------------------------------*)
	    let error, handler, list =
              decomposition parameter handler error bdu_update
	    in
            (*-----------------------------------------------------------------------*)
	    let error, handler =
	      List.fold_left
	        (fun (error, handler) mvbdu ->
		  let error, handler =
		    if local_trace || Remanent_parameters.get_trace parameter
		    then
		      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                        "INTENSIONAL DESCRIPTION:" in
		      let () = Loggers.print_newline
                        (Remanent_parameters.get_logger parameter)
                      in
		      let () = Mvbdu_wrapper.Mvbdu.print parameter mvbdu in
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
		      match Cckappa_sig.Site_map_and_set.Map.find_option
                        parameter error site_type map2
                      with
		      | error, None -> warn parameter error (Some "line 165") Exit (-1)
		      | error, Some i -> error, i
		    in
		    error, site_type
		  in
		  let error, (handler, translation) =
		    Translation_in_natural_language.translate
		      parameter handler error rename_site mvbdu
		  in
	          (*--------------------------------------------------------------------*)
		  let error =
		    Translation_in_natural_language.print
		      ~show_dep_with_dimmension_higher_than:dim_min parameter
		      handler_kappa error agent_string agent_type translation
		  in error, handler
	        )
	        (error, handler)
	        list
	    in
	    error, handler)
	  result (error, handler)
      end

  (************************************************************************************)
  (*non relational properties*)

  let print_bdu_update_map_cartesian_abstraction a b c d =
    print_bdu_update_map_gen_decomposition
      ~smash:true
      ~show_dep_with_dimmension_higher_than:1
      Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction a b c d

  (************************************************************************************)
  (*relational properties*)

  let print_bdu_update_map_cartesian_decomposition a b c d =
    print_bdu_update_map_gen_decomposition
      ~smash:true
      ~show_dep_with_dimmension_higher_than:
      (if Remanent_parameters.get_hide_one_d_relations_from_cartesian_decomposition a
       then 2
       else 1
      )
      Mvbdu_wrapper.Mvbdu.mvbdu_full_cartesian_decomposition a b c d

  (************************************************************************************)

  let print_result_fixpoint_aux
      parameter handler error handler_kappa site_correspondence result =
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

  let print_fixpoint_result static dynamic error loggers =
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
          (*List.fold_left (fun error log ->*)
          let _ =
            print_result_fixpoint_aux
              parameter
              handler
              error
              kappa_handler
	      store_remanent_triple
	      fixpoint_result
          in
          error
        (* ) error loggers*)
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
    let error, dynamic, () =
      print_fixpoint_result static dynamic error loggers
    in
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)


end
