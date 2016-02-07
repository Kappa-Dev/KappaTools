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


module Domain =
  struct
      
    (* the type of the struct that contains all static information as in the
     previous version of the analysis *)
    
    type static_information =
      {
	global_static_information : Analyzer_headers.global_static_information;	   
	domain_static_information : Bdu_analysis_type.bdu_analysis_static
      }
	
    (*--------------------------------------------------------------------*)
    (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)
	
    type local_dynamic_information =
      {
	  dead_rule                  : bool array;
	  fixpoint_result            : 
            Mvbdu_wrapper.Mvbdu.mvbdu Bdu_analysis_type.Map_bdu_update.Map.t;
	  domain_dynamic_information : Bdu_analysis_type.bdu_analysis_dynamic
      }

    type dynamic_information =
      {
	local  : local_dynamic_information;
	global : Analyzer_headers.global_dynamic_information
      }
	
    (*--------------------------------------------------------------------*)
    (** explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)
	  
    let get_global_static_information static =
      static.global_static_information
	
    let lift f x = f (get_global_static_information x)
		     
    let get_compilation_information static =
      lift Analyzer_headers.get_compilation_information static
	   
    let get_kappa_handler static =
      lift Analyzer_headers.get_kappa_handler static
      	   
    let get_parameter static = lift Analyzer_headers.get_parameter static
				    
    let get_cc_code static = lift Analyzer_headers.get_cc_code static
				  
    let get_bdu_common_static global_static =
      Analyzer_headers.get_common_static global_static					

  (*--------------------------------------------------------------------*)
  (* explain how to extract the handler for mvbdu *)

    let get_global_dynamic_information dynamic = dynamic.global
    let get_local_dynamic_information dynamic = dynamic.local
    let get_mvbdu_handler dynamic = Analyzer_headers.get_mvbdu_handler (get_global_dynamic_information dynamic)
								       
    (* explain how to overwritte the previous handler *)

    let set_mvbdu_handler handler dynamic = 
      {
	dynamic with global = Analyzer_headers.set_mvbdu_handler handler (get_global_dynamic_information dynamic)
      }
	
    let set_local_dynamic_information local dynamic = {dynamic with local = local}

    let get_fixpoint_result dynamic = (get_local_dynamic_information dynamic).fixpoint_result
					
    let set_fixpoint_result result dynamic =
      set_local_dynamic_information {(get_local_dynamic_information dynamic) with fixpoint_result = result} dynamic
	
    let get_domain_dynamic_information dynamic = (get_local_dynamic_information dynamic).domain_dynamic_information
											
    let set_domain_dynamic_information domain dynamic =
      set_local_dynamic_information {(get_local_dynamic_information dynamic) with domain_dynamic_information = domain} dynamic 

  let get_dead_rule dynamic = dynamic.dead_rule
  let set_dead_rule dead_rule dynamic = {dynamic with dead_rule = dead_rule}

  let set_domain_static domain static =
    {
      static with
        domain_static_information = domain
    }
      
  (*--------------------------------------------------------------------*)
  (** intialization function of global static & dynamic information of this
      domain*)
      
  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    let error, init_bdu_analysis_static =
      Bdu_analysis_main.init_bdu_analysis_static parameter error
    in
    let compilation_result = Analyzer_headers.get_compilation_information static in
    let init_global_static_information =
    {
      global_static_information = static;
      domain_static_information = init_bdu_analysis_static
    }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let nrules = Handler.nrules parameter error kappa_handler in
    let init_dead_rule_array = Array.make nrules false in
    let init_fixpoint = Bdu_analysis_type.Map_bdu_update.Map.empty in
    let error, init_bdu_analysis_dynamic =
      Bdu_analysis_main.init_bdu_analysis_dynamic parameter error
    in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local =
	  { dead_rule = init_dead_rule_array;
	    fixpoint_result = init_fixpoint;
	    domain_dynamic_information = init_bdu_analysis_dynamic;
	  }}
    in
    error,
    init_global_static_information,
    init_global_dynamic_information

 
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
							    
    (*--------------------------------------------------------------------*)
    (** [add_initial_state static dynamic error state] takes an initial state
      and returns the information of the dynamic and a list of event*)
							    
    (*get map restriction from covering classes*)
    let get_pair_list parameter error agent triple_list =
      let error, get_pair_list =
	List.fold_left (fun (error, current_list) (cv_id, list, set) ->
			(*------------------------------------------------------------*)
			(*new index for site type in covering class*)
			let error, (map_new_index_forward, _) =
			  Bdu_build.new_index_pair_map parameter error list
			in
			(*-------------------------------------------------------------*)
			let add site state (error, store) = 
			  let error, site' =
			    match 
			      Cckappa_sig.Site_map_and_set.Map.find_option
				parameter error site map_new_index_forward
			    with
			    | error, None -> Bdu_build.warn parameter error (Some "398") Exit 0
			    | error, Some s -> error, s
			  in
			  Cckappa_sig.Site_map_and_set.Map.add
			    parameter error site' state store
			in
			let error', map_res =
			  Cckappa_sig.Site_map_and_set.Map.fold_restriction_with_missing_associations
			    parameter error 
			    (fun site port -> add site port.Cckappa_sig.site_state.Cckappa_sig.min)
			    (*JF: we should check that port.site_state.min is equal to
              port.site_state.max*)
			    (fun site -> add site 0)
			    set
			    agent.Cckappa_sig.agent_interface
			    Cckappa_sig.Site_map_and_set.Map.empty
			in
			let error = Exception.check Bdu_build.warn parameter error error'
						    (Some "line 370") Exit 
			in
			error, ((cv_id, map_res) :: current_list)
		       ) (error, []) triple_list
      in
      error, get_pair_list
	       
   (* let build_init_restriction parameter handler error bdu_false
			       init store_remanent_triple store =
      let add_link handler (agent_type, cv_id) bdu store =
	(*NOTE: build bdu_false later*)
	(*let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false 
        parameter handler error in*)
	let error, old_bdu =
          match Bdu_analysis_type.Map_init_bdu.Map.find_option_without_logs parameter error
									    (agent_type, cv_id) store 
          with
          | error, None -> error, bdu_false
          | error, Some bdu -> error, bdu
	in
	(* In the case when the agent is created twice, we take the union *)
	let error, handler, bdu_new = 
          Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error old_bdu bdu
	in
	let error, store =
          Bdu_analysis_type.Map_init_bdu.Map.add_or_overwrite
            parameter error (agent_type, cv_id) bdu_new store
	in
	error, ((cv_id, map_res) :: current_list)
      ) (error, []) triple_list
    in
    error, get_pair_list*)

  (* to do: singature of the function add_link *)
    let add_link ?string error static dynamic (agent_type, cv_id) bdu store (updates_list: (int*int) list) =
      (*this function should update the bdu associated with the pair (agent_type, cv_id) in the store*)
      (*If the bdu has changed, it should log the string (if given) *)
      (*If the bdu has changed, it should log the diff (if the parameters request it)*)
      (*If the bdu has changed, the pair (agent_type,cv_id) should be recorded in the list updates_list*)
      error dynamic,updates_list


  (* merge build_init_restiction and compute_bdu_fixpoint_init and avoid useless computation *)
  let build_init_restriction parameter handler error bdu_false
      init store_remanent_triple store =
    (* Use the unique add_link function instead *)
    let add_link handler (agent_type, cv_id) bdu store = (* add_link should also take a list to record with views have been updated *)
      (* the value error should has be given as an argument *)
      let error, old_bdu =
        match Bdu_analysis_type.Map_init_bdu.Map.find_option_without_logs parameter error
          (agent_type, cv_id) store 
        with
        | error, None -> error, bdu_false
        | error, Some bdu -> error, bdu
      in
      (* In the case when the agent is created twice, we take the union *)
      let error, handler, bdu_new = 
        Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error old_bdu bdu
      in
      (* We should test whether or not there are new views, in such a case promp them and update the recording list accordingly *)
      let error, store =
        Bdu_analysis_type.Map_init_bdu.Map.add_or_overwrite
          parameter error (agent_type, cv_id) bdu_new store
      in
      error, handler, store
    in
    let error, (handler, store) =
      Bdu_analysis_type.AgentMap.fold parameter error
        (fun parameter error agent_id agent (handler, store) -> (* also use a boolean to prompt "\tViews in initial state" at the first views *) 
          match agent with
          | Cckappa_sig.Unknown_agent _
          | Cckappa_sig.Ghost -> error, (handler, store)
          | Cckappa_sig.Dead_agent _ ->
            Bdu_build.warn parameter error (Some "373") Exit (handler, store)
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            Bdu_analysis_type.AgentMap.fold parameter error
              (fun parameter error agent_type' triple_list (handler, store) -> 
                if agent_type = agent_type' (* please use find instead of fold *)
                then
                  let error, get_pair_list =
                    get_pair_list parameter error agent triple_list
                  in
                  (*-----------------------------------------------------------------*)
                  let error, handler, store =
                    List.fold_left
		      (fun (error, handler, store) (cv_id,map_res) ->
		        let error, pair_list =
		          Cckappa_sig.Site_map_and_set.Map.fold
		            (fun site' state (error, current_list) ->
			      let pair_list = (site', state) :: current_list in
			      error, pair_list
		            ) map_res (error, [])
		        in
                        (*build bdu for initial state*)
		        let error, handler, bdu_init =
		          Bdu_build.build_bdu parameter handler error pair_list
		        in
		        let error, handler, store =
		          add_link handler (agent_type, cv_id) bdu_init store
		        in
		        error, handler, store)
		      (error, handler, store)
		      get_pair_list 
	          in
	          error, (handler, store)
                else
                  error, (handler, store)
              ) store_remanent_triple (handler, store)       
        ) init.Cckappa_sig.e_init_c_mixture.Cckappa_sig.views (handler, store)
    in
    error, (handler, store)
  
  let compute_bdu_fixpoint_init_map parameter error handler handler_kappa init
      store_remanent_triple 
      (*build bdu_false from handler dynamic*)
      bdu_false
      =
      let store_init = Bdu_analysis_type.Map_init_bdu.Map.empty in (* the map should not be empty, it should be the current map in dynamic *)
      let error, (handler, store_bdu_init_restriction_map) =
	build_init_restriction parameter handler error bdu_false init store_remanent_triple 
			       store_init
      in
      (*-----------------------------------------------------------------------*)
      let log = Remanent_parameters.get_logger parameter in
      let add_link parameter handler error correspondence (agent_type, cv_id) 
		   bdu store_result = 
	let error, bdu_old =
	  match Bdu_analysis_type.Map_bdu_update.Map.find_option_without_logs parameter error
									      (agent_type, cv_id) store_result
	  with
	  | error, None -> error, bdu_false
	  | error, Some bdu -> error, bdu
	in
	let error, handler, bdu_union =
	  Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
				       (*JF: this is a computation, thus you have to pass the handler *)
	in
	let parameter_views = Remanent_parameters.update_prefix parameter "\t" in
	(*print bdu different in views*)
	let error, handler =
	  Bdu_fixpoint_iteration.dump_view_diff parameter_views handler_kappa handler error
						correspondence agent_type cv_id bdu_old bdu_union
	in
	let error, result_map =
	  Bdu_analysis_type.Map_bdu_update.Map.add_or_overwrite parameter error
								(agent_type, cv_id) bdu_union store_result
	in
	error, handler, result_map
      in
      (*-----------------------------------------------------------------------*)
      let error, bool, handler, store_bdu_fixpoint_init_map = (* this fold is useless, the map has already been dealt with the function bdu_init_restriction *)
                                                              (* Please rather complete bdu_init_restriction with logging messages *)
	Bdu_analysis_type.Map_init_bdu.Map.fold
          (fun (agent_type, cv_id) bdu (error, bool, handler, store_result) ->
	   let () =
	     if not bool
		&&
	          (Bdu_fixpoint_iteration.local_trace
	           || Remanent_parameters.get_dump_reachability_analysis_diff parameter
	           || Remanent_parameters.get_trace parameter)
	     then
	       let () = Loggers.fprintf log "\tViews in initial state" in
	       let () = Loggers.print_newline log in
	       let () = Loggers.print_newline log in
	       ()
	   in
	   let error, handler, store_result =
             add_link parameter handler error
		      store_remanent_triple
		      (agent_type, cv_id) bdu store_result
           in
           error, true, handler, store_result
          )
          store_bdu_init_restriction_map
          (error, false, handler, Bdu_analysis_type.Map_bdu_update.Map.empty)
      in
       let () =
      if not bool
        &&
	  (Bdu_fixpoint_iteration.local_trace
	   || Remanent_parameters.get_dump_reachability_analysis_diff parameter
	   || Remanent_parameters.get_trace parameter)
      then
        let () =
	  Loggers.fprintf log "\tInitial state is empty"
        in
        let () = Loggers.print_newline log in
        let () = Loggers.print_newline log in
        ()
    in
    let () =
      if
        Bdu_fixpoint_iteration.local_trace
        || Remanent_parameters.get_trace parameter
        || Remanent_parameters.get_dump_reachability_analysis_wl parameter
      then
        let () = Loggers.fprintf log "\tWake-up rules" in
        let () = Loggers.print_newline log in
        ()
    in
    error, bool, handler, store_bdu_fixpoint_init_map

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
    
  (** [get_scan_rule_set static] *)
  let get_scan_rule_set (static: static_information) dynamic error =
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compiled = get_cc_code static in
    let error, handler_bdu = Boolean_mvbdu.init_remanent parameter error in
    let error, (handler_bdu, result) =
      Bdu_analysis_main.scan_rule_set
        parameter
        handler_bdu
        error
        kappa_handler
        compiled
        compiled.Cckappa_sig.rules
    in
    let dynamic = set_mvbdu_handler handler_bdu dynamic in
    let dynamic = set_domain_dynamic_information result.Bdu_analysis_type.store_bdu_analysis_dynamic dynamic in
    error,
    set_domain_static result.Bdu_analysis_type.store_bdu_analysis_static static, dynamic

    
  (** get type bdu_analysis_static*)
  let get_bdu_analysis_static (static:static_information) dynamic error =
    let error, static_information, dynamic_information =
      get_scan_rule_set static dynamic error
    in
    let result = static_information.domain_static_information in
    error, result

  (**get type bdu_analysis_dynamic*)
  let get_bdu_analysis_dynamic (static:static_information) dynamic error =
    let error, static_information, dynamic_information =
      get_scan_rule_set static dynamic error
    in
    let result = get_domain_dynamic_information dynamic in
    error, result

  let get_store_remanent_triple static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    let store_remanent_triple =
      result_static.Bdu_analysis_type.store_remanent_triple
    in
    error, store_remanent_triple

  (**add initial state of kappa*)
  let add_initial_state (static:static_information) dynamic error init_state =
    let parameter = get_parameter static in
    (*let handler_kappa = get_kappa_handler static in*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let error, store_remanent_triple = get_store_remanent_triple static dynamic error in
    let handler_bdu = get_mvbdu_handler dynamic in
    let store_init = Bdu_analysis_type.Map_init_bdu.Map.empty in (* we should not start from an empty map *)
                                                                 (* we should take the current state of the bdu_map in dynamic *)
    (*JF: this function should not call the fixpoint machinery, it should
      just abstract the chemical species provided in argument, and store it
      in the mvbdu map *)
    (*JF: Here you just need the part of the function, 
      that takes into account one chemical species *)
    (*JF: moreover, try not to seperate the field of the struct 
      (dynamic and static), pass each of them as a single argument, and return 
      the updated value of the struct dynamic, it will lighter your core *)    
    let error, (handler_bdu, store) =
      build_init_restriction
        parameter
        handler_bdu
        error
        bdu_false
        init_state
        store_remanent_triple
        store_init
    in
    error, set_mvbdu_handler handler_bdu dynamic, []
      
  let get_store_proj_bdu_test_restriction static dynamic error =
    let error, result_static = 
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_analysis_type.store_proj_bdu_test_restriction

  let get_store_proj_bdu_creation_restriction static dynamic error =
    let error, result_static = 
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_analysis_type.store_proj_bdu_creation_restriction_map

  let get_store_proj_bdu_potential_restriction static dynamic error =
    let error, result_static = 
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_analysis_type.store_proj_bdu_potential_restriction_map

  let get_triple_restriction_map static dynamic error rule_id =
    let parameter = get_parameter static in
    let error, store_proj_bdu_test_restriction =
      get_store_proj_bdu_test_restriction static dynamic error
    in
    let error, store_proj_bdu_creation_restriction =
      get_store_proj_bdu_creation_restriction static dynamic error
    in
    let error, store_proj_bdu_potential_restriction =
      get_store_proj_bdu_potential_restriction static dynamic error
    in
    let error,
      (proj_bdu_test_restriction_map,
       bdu_creation_restriction_map, bdu_and_list_potential_map) =
      Bdu_fixpoint_iteration.collect_map_views_creation_test_potential
        parameter
        error
        rule_id
        store_proj_bdu_test_restriction
        store_proj_bdu_creation_restriction
        store_proj_bdu_potential_restriction
    in
    error, 
    (proj_bdu_test_restriction_map,
     bdu_creation_restriction_map, bdu_and_list_potential_map)

  let get_store_modif_list_restriction_map static dynamic error =
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_analysis_type.store_modif_list_restriction_map

  let get_store_wl_creation static dynamic error = (* the value dynamic should not contain the working list, it is 
						      at the top of the hierarchy of modules *)
    let error, result_static =
      get_bdu_analysis_static static dynamic error
    in
    error, result_static.Bdu_analysis_type.store_wl_creation
    
  let get_store_covering_classes_modification_update_full static dynamic error =
    let error, result_dynamic =
      get_bdu_analysis_dynamic static dynamic error
    in
    error, result_dynamic.Bdu_analysis_type.store_covering_classes_modification_update_full

  let is_enabled (static:static_information) dynamic error rule_id precondition =
    let parameter = get_parameter static in
   (* let handler_kappa = get_kappa_handler static in*)
   (* let compiled = get_cc_code static in*)
    let error, dynamic, bdu_false = get_mvbdu_false static dynamic error in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, result_static = get_bdu_analysis_static static dynamic error in
    let error, result_dynamic = get_bdu_analysis_dynamic static dynamic error in
    let error, (proj_bdu_test_restriction, _, _) =
      get_triple_restriction_map static dynamic error rule_id
    in
    let fixpoint_result = get_fixpoint_result dynamic in 
    let error, handler_bdu, is_enable =
      Bdu_fixpoint_iteration.is_enable
        parameter
        handler_bdu
        error
        bdu_false
        rule_id
        proj_bdu_test_restriction
        fixpoint_result
    in
    if is_enable
    then 
      (* precondition is empty for the moment, we will add information when
         necessary *)
      error, set_mvbdu_handler handler_bdu dynamic, Some precondition
    else 
      error, dynamic, None

  (*
    let dead_rule_array = dynamic.dead_rule in 
      (* to be pushed in apply_rule in the rule domain 
    (when we will split the domain concept-wise) *)
      (*let dead_rule_array =                     

    dead_rule_array.(rule_id) <- true;
    dead_rule_array       
    in*) 
      (* From here: this code should not belong to the function is_enable, 
    it implements the application of the rule *)
      (* compute views that is enabled*) 
    let error, (handler_bdu, new_wl, store_new_result) =
    Bdu_fixpoint_iteration.compute_views_enabled
    parameter
    handler_bdu
    error
    handler_kappa
    compiled
    result_static.Bdu_analysis_type.store_remanent_triple
    bdu_true
    bdu_false
    rule_id
    bdu_creation_map
    result_static.Bdu_analysis_type.store_modif_list_restriction_map
    bdu_and_list_potential_map
    result_static.Bdu_analysis_type.store_wl_creation (*FIXME*)
    result_dynamic.Bdu_analysis_type.store_covering_classes_modification_update_full
    proj_bdu_test_restriction
    fixpoint_result (*FIXME*)
    in
    (*store*)
    let dynamic_information =
    {
    dynamic with
    mvbdu_handler = handler_bdu;
    dead_rule     = dead_rule_array;
    fixpoint_result = store_new_result
    }
    in
      (** To here *)
    error, dynamic_information, Some () (* precondition is empty for the moment, we will add information when necessary *)
    else
    error, dynamic, None*)
        
  (*update by setting handler_bdu*)
  (*let is_enabled static dynamic error rule_id =
    error, dynamic, None*)

  let apply_rule static dynamic error rule_id precondition =
    let parameter = get_parameter static in
    let handler_kappa = get_kappa_handler static in
    let compiled = get_cc_code static in
    (*FIXME: the order of getting handler_bdu from dynamic before bdu_false/true?*)
    let error, handler_bdu, bdu_false = get_mvbdu_false static dynamic error in
    let error, handler_bdu, bdu_true = get_mvbdu_true static dynamic error in
    let handler_bdu = get_mvbdu_handler dynamic in
    let error, store_remanent_triple = get_store_remanent_triple static dynamic error in
    let error, store_modif_list_restriction_map =
      get_store_modif_list_restriction_map static dynamic error
    in
    let error, store_wl_creation = get_store_wl_creation static dynamic error in (* this should not exist *)
    let error, store_covering_classes_modification_update_full =
      get_store_covering_classes_modification_update_full static dynamic error
    in
    let error, (proj_bdu_test_restriction, bdu_creation_map, bdu_and_list_potential_map) =
      get_triple_restriction_map static dynamic error rule_id
    in
    let fixpoint_result = get_fixpoint_result dynamic in
    let error, (handler_bdu, new_wl, store_new_result) =
      Bdu_fixpoint_iteration.compute_views_enabled
        parameter
        handler_bdu
        error
        handler_kappa
        compiled
        store_remanent_triple
        bdu_true
        bdu_false
        rule_id
        bdu_creation_map
        store_modif_list_restriction_map
        bdu_and_list_potential_map
        store_wl_creation (*FIXME: wl_tl*) (* there is working list at the level of abstraction *)
	                                   (* please output a list of events, to record the rule_id to be put in the 
					      wporking_list in the Analyzer.Analyzer *)
        store_covering_classes_modification_update_full
        proj_bdu_test_restriction
        fixpoint_result
    in
    let dynamic = set_mvbdu_handler handler_bdu dynamic
    in
    let dynamic = set_fixpoint_result store_new_result dynamic in
    error, dynamic, []   
      
  let rec apply_event_list static dynamic error event_list =
    error, dynamic, [] (* events enable communication between domains. At this moment, the global domain does not collect information *)

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state
      
  let print static dynamic error loggers =
    error, dynamic, ()

end

(**A functor that takes a module Analyzer.Analyzer, and implement a
   function: main, export, and print*)
(*module Analyzer_main =
  functor (Analyzer_domain:Analyzer.Analyzer) ->
    struct

      type static_information = Analyzer_domain.static_information
      type dynamic_information = Analyzer_domain.dynamic_information


      let fixpoint static dynamic error init_state =
        (*-------------------------------------------------*)
        (*active static information and dynamic information*)
        let error, static, dynamic =
         get_scan_rule_set static dynamic error
        in
        let error, result_static =
          get_bdu_analysis_static static dynamic error
        in
        let error, result_dynamic =
          get_bdu_analysis_dynamic static dynamic error
        in
        let parameter, handler_kappa, compiled =
          get_common_static static.global_static_information
        in
        let error, handler_bdu, bdu_false =
          get_mvbdu_false static_information.global_static_information
            dynamic_information error 
        in
        let error, handler_bdu, bdu_true =
          get_mvbdu_true static_information.global_static_information
            dynamic_information error 
        in
        (*-------------------------------------------------*)
        (*get initial_state *)
        let error, dynamic_with_init, _ =
          add_initial_state static dynamic init_state
        in
        let store_bdu_fixpoint_init_map = dynamic_with_init.fixpoint_result in
        let dead_rule_array = dynamic.dead_rule in
        (*-------------------------------------------------*)
        (*build working list*)
        let error, dynamic_with_working_list, rule_id_op =
          Analyzer.next_rule static dynamic_with_init error
        in
        let working_list = dynamic.rule_working_list in
        let wl_creation = result_static.store_wl_creation in
        (*-------------------------------------------------*)
        (*push init_fixpoint into working list*)
        let error, working_list_init_creation =
          Bdu_analysis_type.Map_bdu_update.Map.fold
            (fun (agent_type, cv_id) _ (error, wl_init_creation) ->
              Bdu_fixpoint_iteration.add_update_to_wl
                ~title:"Dealing with"
                parameter 
                error
                handler_kappa
                compiled
                result_static.Bdu_analysis_type.store_remanent_triple
                agent_type
                cv_id
                result_dynamic.Bdu_analysis_type.store_covering_classes_modification_update_full
                wl_init_creation
            )
            store_bdu_fixpoint_init_map
            (error, wl_creation)
        in
        let working_list_tail = dynamic_with_working_list.rule_working_list in
        (*-------------------------------------------------*)
        (*Loop function*)
        let rec aux acc_wl (error, dynamic_result) =
          if Fifo.IntWL.is_empty acc_wl
          then 
            error,
            {
              dynamic with
                mvbdu_handler = handler_bdu;
                fixpoint_result = store_bdu_fixpoint_init_map;
                dead_rule = dead_rule_array
            }
          else
            match rule_id_op with
            | Some rule_id ->
              (*-------------------------------------------------*)
              (*check if this rule is enable*)
              let error, dynamic_enable, is_enabled_op =
                is_enabled static dynamic_with_init error rule_id
              in
              (match is_enable_op with
              | Some true ->
                let error,
                  (proj_bdu_test_restriction, bdu_creation_map, bdu_and_list_potential_map) =
                  Bdu_fixpoint_iteration.collect_map_views_creation_test_potential
                    parameter
                    error
                    rule_id
                    result_static.Bdu_analysis_type.store_proj_bdu_test_restriction
                    result_static.Bdu_analysis_type.store_proj_bdu_creation_restriction_map
                    result_static.Bdu_analysis_type.store_proj_bdu_potential_restriction_map
                in
                (*compute views that is enabled*)
                let error, (handler_bdu, new_wl, store_new_result) =
                  Bdu_fixpoint_iteration.compute_views_enabled
                    parameter
                    handler_bdu
                    error
                    handler_kappa
                    compiled
                    result_static.Bdu_analysis_type.store_remanent_triple
                    bdu_true
                    bdu_false
                    rule_id
                    bdu_creation_map
                    result_static.Bdu_analysis_type.store_modif_list_restriction_map
                    bdu_and_list_potential_map
                    working_list_tail
                    result_dynamic.Bdu_analysis_type.store_covering_classes_modification_update_full
                    proj_bdu_test_restriction
                    store_bdu_fixpoint_init_map
                in
                aux new_wl (error, 
                            {
                              dynamic_information with
                                fixpoint_result = store_new_result;
                                mvbdu_handler = handler_bdu
                                dead_rule = dead_rule_array
                            })

              | None -> 
                aux working_list_tail (error, dynamic_result)
              ) 
            | None -> Bdu_fixpoint_iteration.warn parameter error (Some "") Exit 
              {
                dynamic_information with
                  mvbdu_handler = handler_bdu;
                  fixpoint_result = store_bdu_fixpoint_init_map
                  dead_rule = dead_rule_array
              }
        in 
        aux working_list_init_creation
          (error, 
           {
             dynamic_information with
               mvbdu_handler = handler_bdu;
               fixpoint_result = store_bdu_fixpoint_init_map;
               dead_rule = dead_rule_array
           })

      let print static dynamic error loggers = error, dynamic, ()


    end*)
