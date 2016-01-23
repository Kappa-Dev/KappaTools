(**
  * bdu_analysis_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 20th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Mvbdu_sig
open Covering_classes_type
open Memo_sig
open Boolean_mvbdu
open Bdu_build_type

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  let error', set =
    List.fold_left (fun (error,current_set) elt ->
      Site_map_and_set.Set.add parameter error elt current_set
    ) (error, Site_map_and_set.Set.empty) list
  in
  let error = Exception.check warn parameter error error' (Some "line 61") Exit in
  error, set

(************************************************************************************)
(* From each covering class, with their new index for sites, build
   (bdu_test, bdu_creation and list of modification).
   Note: not taking sites in the local, because it will be longer.
   - Convert type set of sites into map restriction
*)

let collect_remanent_triple parameter error store_remanent store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_type remanent store_result ->
      let store_dic = remanent.store_dic in
      (*-----------------------------------------------------------------*)
      let error, triple_list =
        Dictionary_of_Covering_class.fold
          (fun list _ cv_id (error, current_list) ->
            let error, set = list2set parameter error list in
            let triple_list = (cv_id, list, set) :: current_list in
            error, triple_list
          ) store_dic (error, [])
      in
      (*-----------------------------------------------------------------*)
      let error, store_result =
        AgentMap.set
          parameter
          error
          agent_type
          (List.rev triple_list)
          store_result
      in
      error, store_result
    ) store_remanent store_result

(*----------------------------------------------------------------------------*)
(*working list content only creation rule_id: this is the initial working list,
  this creation has no lhs.
  Only this case:
  'r1' -> A(x,y)
  will add 'r1' into a working list.

  - There is case when it has a lhs and created a new agent.
    For instance:
    'r1' A(x,y) -> D(x,y)
  will also add 'r1' into a working list. (lhs of D is Ghost. Rhs of A is Ghost)
*)

let collect_wl_creation parameter error rule_id rule store_result =
  (*add rule_id that has no lhs into a working list*)
  let error, wl_creation =
    List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
      let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
      match agent with
      | Some Dead_agent _ | Some Ghost  -> error, store_result
      | None ->  warn parameter error (Some "line 45") Exit store_result
      | Some Unknown_agent _
      | Some Agent _ ->
        let error, wl = Fifo.IntWL.push parameter error rule_id store_result in
        error, wl
    ) (error, store_result) rule.actions.creation
  in
  error, wl_creation

(************************************************************************************)

let build_bdu parameter handler error pair_list =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let error, handler, list_a =
    Mvbdu_wrapper.Mvbdu.build_association_list
      parameter
      handler
      error
      pair_list
  in
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter handler error bdu_true list_a
  in
  error, handler, bdu_result

(************************************************************************************)

let new_index_pair_map parameter error l = (*JF:  it should be computed only once *)
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error, map1 = Site_map_and_set.Map.add parameter error h k map1 in
      let error, map2 = Site_map_and_set.Map.add parameter error k h map2 in
      aux tl (k+1) map1 map2 error
  in
  let error', (map1, map2) =
    aux l 1 Site_map_and_set.Map.empty Site_map_and_set.Map.empty error
  in
  let error = Exception.check warn parameter error error' (Some "line 49") Exit in
  error, (map1, map2)

(************************************************************************************)

let collect_bdu_test_restriction_map parameter handler error rule_id rule
    store_remanent_triple store_result =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link (agent_id, agent_type, rule_id, cv_id) bdu store_result =
    (* JF: add_link should assign views uniquely *)
    let result_map =
      Map_test_bdu.Map.add (agent_id, agent_type, rule_id, cv_id) bdu store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_id agent (handler,store_result) ->
      match agent with
      | Unknown_agent _ | Ghost -> error, (handler, store_result)
      | Dead_agent (agent, _, _, _)
      | Agent agent ->
        let agent_type = agent.agent_name in
	let error, triple_list =
	  match
	    AgentMap.get parameter error agent_type store_remanent_triple
	  with
	  | error, None -> warn parameter error (Some "Line 136") Exit []
	  | error, Some x -> error, x
	in
	(*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
	if Site_map_and_set.Map.is_empty agent.agent_interface
	then
          (* IF the covering class is empty, put the bdu true since there is no test *)
	  let error, store_result =
	    add_link (agent_id, agent_type, rule_id, 0) bdu_true store_result
	  in
	  error, (handler, store_result)
	else
          let error, get_pair_list =
            List.fold_left (fun (error, current_list) (cv_id, list, set) ->
              (*-----------------------------------------------------------------*)
              (*new index for site type in covering class*)
              let error, (map_new_index_forward, _) =
		new_index_pair_map parameter error list
              in
              (*-----------------------------------------------------------------*)
              let error', map_res =
		Site_map_and_set.Map.fold_restriction
		parameter error
		  (fun site port (error, store_result) ->
                    let state = port.site_state.min in
                    let error, site' =
                      Site_map_and_set.Map.find_default parameter error
                        0 site map_new_index_forward
                    in
                    let error, map_res =
                      Site_map_and_set.Map.add parameter error
			site'
			state
			store_result
                    in
                    error, map_res
		  ) set agent.agent_interface Site_map_and_set.Map.empty
              in
	      let error = Exception.check warn parameter error error'
                (Some "line 178") Exit
              in
              error, (cv_id, map_res) :: current_list)
	      (error, []) triple_list
          in
          (*-----------------------------------------------------------------*)
          let error, handler, store_result =
            List.fold_left
              (fun (error, handler, store_result) (cv_id,map_res) ->
		if Site_map_and_set.Map.is_empty map_res
		then
		  error, handler, store_result
		else
		  begin
		    let error, pair_list =
		      Site_map_and_set.Map.fold
			(fun site' state (error, current_list) ->
			  let pair_list = (site', state) :: current_list in
			  error, pair_list
			) map_res (error, [])
		    in
		    (*build bdu_test*)
		    let error, handler, bdu_test =
		      build_bdu parameter handler error pair_list
		    in
		    let error, store_result =
		      add_link (agent_id, agent_type, rule_id, cv_id) bdu_test store_result
		    in
		    error, handler, store_result
		  end)

	      (error, handler, store_result) get_pair_list
        in
        error, (handler, store_result)
    ) rule.rule_lhs.views  (handler,store_result)

(*projection with (rule_id), from map (rule_id -> map (agent_id -> bdu)) *)

let collect_proj_bdu_test_restriction_map parameter handler error
    store_bdu_test_restriction_map =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  (*do the projection*)
  let (error, handler), store_result =
    Project2bdu_test.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_id, agent_type, rule_id, cv_id) -> rule_id)
      (fun (agent_id, agent_type, rule_id, cv_id) -> agent_id)
      bdu_true (*default value of bdu_test*)
      (fun parameter (error, handler) bdu bdu' ->
        let error, handler, bdu_union = Mvbdu_wrapper.Mvbdu.mvbdu_and
          parameter handler error bdu bdu'
        in
        (error, handler), bdu_union
      )
      store_bdu_test_restriction_map
  in
  (error, handler), store_result

(************************************************************************************)
(*creation rules*)

let collect_bdu_creation_restriction_map
    parameter handler error rule_id rule store_remanent_triple store_result   =
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link handler (agent_type, rule_id, cv_id) bdu store_result =
    let error, old_bdu =
      match
        Map_creation_bdu.Map.find_option
          (agent_type, rule_id, cv_id) store_result
      with
      | None -> error, bdu_false
      (*default value when there is no creation in this rule*)
      | Some bdu -> error, bdu
    in
    (* In the case when the agent is created twice, we take the union *)
    let error, handler, bdu_new =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error old_bdu bdu
    in
    let result_map =
      Map_creation_bdu.Map.add (agent_type, rule_id, cv_id) bdu_new store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type' triple_list (handler,store_result) ->
      List.fold_left (fun (error, (handler,store_result)) (agent_id, agent_type) ->
        let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
        match agent with
	| Some Unknown_agent _ | Some Dead_agent _
        | None -> warn parameter error (Some "168") Exit (handler,store_result)
	| Some Ghost -> error, (handler,store_result)
        | Some Agent agent ->
          if agent_type' = agent_type
          then
            (*-----------------------------------------------------------------*)
            (*get map restriction from covering classes*)
            let error, get_pair_list =
              List.fold_left (fun (error, current_list) (cv_id, list, set) ->
                (*-----------------------------------------------------------------*)
                (*new index for site type in covering class*)
		let error, (map_new_index_forward, _) =
                  new_index_pair_map parameter error list
                in
                (*-----------------------------------------------------------------*)
		let add site state (error, store_result) =
                  let error, site' =
                    match Site_map_and_set.Map.find_option parameter error
                      site map_new_index_forward
                    with
                    | error, None -> warn parameter error (Some "282") Exit 0
                    | error, Some s -> error, s
                  in
                  Site_map_and_set.Map.add parameter error site' state store_result
		in
                (*-----------------------------------------------------------------*)
		let error', map_res =
		  Site_map_and_set.Map.fold_restriction_with_missing_associations
                    parameter error
                    (fun site port -> add site port.site_state.min)
		    (fun site -> add site 0)
		    set
		    agent.agent_interface
		    Site_map_and_set.Map.empty
                in
		let error =
                  Exception.check warn parameter error error' (Some "line 212") Exit
                in
                error, (cv_id, map_res) :: current_list)
		(error, []) triple_list
            in
            (*-----------------------------------------------------------------*)
            (*fold a list and get a pair of site and state and rule_id*)
            let error, handler, store_result  =
              List.fold_left
                (fun (error, handler, store_result) (cv_id,map_res) ->
                  let error, pair_list =
                    Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
                        let pair_list = (site', state) :: current_list in
                        error, pair_list
                      ) map_res (error, [])
                  in
		  let error, handler, bdu_creation =
		    build_bdu parameter handler error pair_list
		  in
		   let error, handler, store_result =
		     add_link handler (agent_type, rule_id, cv_id) bdu_creation store_result
		   in
		   error, handler, store_result
                ) (error, handler, store_result) get_pair_list
            in
            error, (handler, store_result)
          else error, (handler, store_result)
      ) (error, (handler, store_result)) rule.actions.creation
    ) store_remanent_triple (handler, store_result)

(*projection with rule_id*)
(*FIXME: return handler*)

let collect_proj_bdu_creation_restriction_map parameter handler error
    store_bdu_creation_restriction_map =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let (error, handler), store_result =
    Project2bdu_creation.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_type, rule_id, cv_id) -> rule_id)
      (fun (agent_type, rule_id, cv_id) -> agent_type, cv_id)
      bdu_true
      (fun parameter (error, handler) bdu bdu' ->
        let error, handler, bdu_union = Mvbdu_wrapper.Mvbdu.mvbdu_and
          parameter handler error bdu bdu'
        in
        (error, handler), bdu_union
      )
      store_bdu_creation_restriction_map
  in
  (error, handler), store_result
		
(************************************************************************************)
(*build bdu in the case of initial state. Declare as %init in Kappa*)

let collect_bdu_init_restriction_map parameter handler error compil store_remanent_triple
    store =
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link handler (agent_type, cv_id) bdu store =
    let error, old_bdu =
      match Map_init_bdu.Map.find_option_without_logs parameter error
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
      Map_init_bdu.Map.add_or_overwrite parameter error (agent_type, cv_id) bdu_new store
    in
    error, handler, store
  in
  (*-----------------------------------------------------------------*)
  let error, (handler, store) =
    Int_storage.Nearly_inf_Imperatif.fold parameter error
      (fun parameter error index init (handler, store) ->
        AgentMap.fold parameter error
          (fun parameter error agent_id agent (handler, store) ->
            match agent with
            | Unknown_agent _ | Ghost -> error, (handler, store)
            | Dead_agent _ -> warn parameter error (Some "373") Exit (handler, store)
            | Agent agent ->
              let agent_type = agent.agent_name in
              AgentMap.fold parameter error
                (fun parameter error agent_type' triple_list (handler, store) ->
                  if agent_type = agent_type'
                  then
                    (*get map restriction from covering classes*)
                    let error,  get_pair_list =
                      List.fold_left (fun (error, current_list) (cv_id, list, set) ->
                        (*-----------------------------------------------------------------*)
                        (*new index for site type in covering class*)
			let error, (map_new_index_forward, _) =
			  new_index_pair_map parameter error list
			in
                        (*-----------------------------------------------------------------*)
			let add site state (error, store) =
			  let error, site' =
			    match Site_map_and_set.Map.find_option parameter error site
			      map_new_index_forward
			    with
			    | error, None -> warn parameter error (Some "398") Exit 0
			    | error, Some s -> error, s
                          in
		          Site_map_and_set.Map.add parameter error site' state store
			in
                        (*-----------------------------------------------------------------*)
			let error', map_res =
			  Site_map_and_set.Map.fold_restriction_with_missing_associations
			    parameter error
			    (fun site port -> add site port.site_state.min)
			    (* JF: we should check that port.site_state.min is
                               equal to port_site_state.max *)
			    (fun site -> add site 0)
			    set
			    agent.agent_interface
			    Site_map_and_set.Map.empty
			in
			let error = Exception.check warn parameter error error'
			  (Some "line 370") Exit
			in
			error, ((cv_id, map_res) :: current_list)
		      ) (error, []) triple_list
                    in
                    (*-----------------------------------------------------------------*)
                    let error, handler, store =
                      List.fold_left
		        (fun (error, handler, store) (cv_id,map_res) ->
		          let error, pair_list =
		            Site_map_and_set.Map.fold
		              (fun site' state (error, current_list) ->
			        let pair_list = (site', state) :: current_list in
			        error, pair_list
		              ) map_res (error, [])
		          in
                          (*build bdu for initial state*)
		          let error, handler, bdu_init =
		            build_bdu parameter handler error pair_list
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
          ) init.e_init_c_mixture.views (handler, store)
      ) compil.init (handler, store)
  in
  error, (handler, store)

(************************************************************************************)
(*modification rule with creation rules*)

let collect_modif_list_restriction_map
    parameter handler error rule_id rule store_remanent_triple store_result =
  let add_link (agent_id, agent_type, rule_id, cv_id) list_a store_result =
    (*the association must be unique *)
    let error, result_map =
      Map_modif_list.Map.add_or_overwrite parameter error
        (agent_id, agent_type, rule_id, cv_id) list_a store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_id agent_modif (handler, store_result) ->
      if Site_map_and_set.Map.is_empty agent_modif.agent_interface
      then error, (handler, store_result)
      else
        let agent_type = agent_modif.agent_name in
	let error, triple_list =
	  match
	    AgentMap.get parameter error agent_type store_remanent_triple
	  with
	  | error, None -> warn parameter error (Some "Line 476") Exit []
	  | error, Some x -> error, x
	in
        (*-----------------------------------------------------------------*)
        (*get map restriction from covering classes*)
        let error, get_pair_list =
          List.fold_left (fun (error, current_list) (cv_id, list, set) ->
            (*-----------------------------------------------------------------*)
            (*new index for site type in covering class*)
            let error, (map_new_index_forward, _) =
              new_index_pair_map parameter error list
            in
            (*-----------------------------------------------------------------*)
            let error', map_res =
              Site_map_and_set.Map.fold_restriction parameter error
                (fun site port (error, store_result) ->
                  let state = port.site_state.min in
                  let error, site' =
                    Site_map_and_set.Map.find_default_without_logs
                      parameter error 0 site map_new_index_forward
                  in
                  let error, map_res =
                    Site_map_and_set.Map.add parameter error
                      site'
                      state
                      store_result
                  in
                  error, map_res
                ) set agent_modif.agent_interface Site_map_and_set.Map.empty
            in
	    let error = Exception.check warn parameter error error' (Some "line 293") Exit
            in
            error, (cv_id, map_res) :: current_list
	  ) (error, []) triple_list
        in
        (*-----------------------------------------------------------------*)
        (*fold a list and get a pair of site and state and rule_id*)
        let error, handler, store_result =
          List.fold_left
            (fun (error, handler, store_result) (cv_id,map_res) ->
	      if Site_map_and_set.Map.is_empty map_res
	      then error, handler, store_result
	      else
		begin
                  (*get a list of pair (site, state) in a map of new indexes of site.*)
		  let error, pair_list =
                    Site_map_and_set.Map.fold
                      (fun site' state (error, current_list) ->
			let pair_list = (site', state) :: current_list in
			error, pair_list
                      ) map_res (error, [])
		  in
		  (*-----------------------------------------------------------------*)
		  (*build list_a*)
		  let error, handler, list_a =
		    Mvbdu_wrapper.Mvbdu.build_association_list
		      parameter
		      handler
		      error
		      pair_list
		  in
		  let error, store_result =
		    add_link (agent_id, agent_type, rule_id, cv_id) list_a store_result
		  in
		  error, handler, store_result
		end
	    )
	    (error, handler, store_result)
	    get_pair_list
	in error, (handler, store_result))
    rule.diff_direct (handler, store_result)

(************************************************************************************)
(*build bdu for potential side effects*)

let collect_bdu_potential_restriction_map_aux parameter handler error store_remanent_triple
    store_potential_side_effects store_result =
  let error, handler, bdu_false = Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error in
  (*-----------------------------------------------------------------*)
  let add_link handler (agent_type, new_site_type, rule_id, cv_id) bdu store_result =
    (*build a list_a*)
    let error, handler, list =
      Mvbdu_wrapper.Mvbdu.build_reverse_sorted_association_list
        parameter handler error [new_site_type, 0] (*state is 0*)
    in
    let result_map =
      Map_potential_bdu.Map.add
        (agent_type, new_site_type, rule_id, cv_id) (bdu, list) store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_type' triple_list (handler, store_result) ->
      (*map of potential partner side_effect with site is bond*)
      Bdu_analysis_static_type.Int2Map_potential_effect.Map.fold
        (fun (agent_type, rule_id) pair_list (error, (handler, store_result)) ->
	 if agent_type' = agent_type
         then
           let error, get_pair_list =
             List.fold_left (fun (error, current_list) (cv_id, list, set) ->
	       (*-----------------------------------------------------------------*)
               (*get new indexes for sites*)
	       let error, (map_new_index_forward, _) =
                 new_index_pair_map parameter error list
               in
	       (*-----------------------------------------------------------------*)
               let error', map_res =
                 List.fold_left
		   (fun (error, map_res) (site, state) ->
		     if Site_map_and_set.Set.mem site set
		     then
		       let error, site' =
			 Site_map_and_set.Map.find_default_without_logs
                           parameter error 0 site map_new_index_forward
		       in
		       let error, old =
			 Site_map_and_set.Map.find_default_without_logs
			   parameter error
			   []
			   site'
			   map_res
		       in
		       let error, map_res =
			 Site_map_and_set.Map.add_or_overwrite parameter error
			   site'
			   (state :: old)
			   map_res
                       in
                       error, map_res
		     else error, map_res
		   ) (error, Site_map_and_set.Map.empty) pair_list
               in
	       (*-----------------------------------------------------------------*)
               let error =
                 Exception.check warn parameter error error' (Some "line 630") Exit
	       in
	       error,
	       Site_map_and_set.Map.fold (fun site' list_state list ->
                 (cv_id, site', list_state) :: list) map_res current_list)
               (error, []) triple_list
           in
	   (*-----------------------------------------------------------------*)
           let error, handler, store_result =
             List.fold_left
	       (fun (error, handler, store_result) (cv_id, site', map_res) ->
		 let error, handler, bdu =
		   List.fold_left (fun (error, handler, bdu) state ->
		     (*---------------------------------------------------------------*)
                     (*build bdu_potential side effects*)
		     let error, handler, bdu_potential_effect =
		       build_bdu parameter handler error [site', state]
		     in
                     (*union of bdu and bdu effect*)
		     let error, handler, bdu =
		       Mvbdu_wrapper.Mvbdu.mvbdu_or
                         parameter handler error bdu bdu_potential_effect
		     in
		     error, handler, bdu)
		     (error, handler, bdu_false)
		     map_res
		 in
		 let error, handler, store_result =
		   add_link handler (agent_type, site', rule_id, cv_id) bdu store_result
		 in
		 error, handler, store_result
	       )
	       (error, handler, store_result)
	       get_pair_list
           in
	   error, (handler, store_result)
         else
	   error, (handler, store_result)
	) store_potential_side_effects (error, (handler, store_result))
    ) store_remanent_triple (handler, store_result)

(************************************************************************************)
(*build bdu_potential in the case of binding*)

let collect_bdu_potential_effect_restriction_map parameter handler error
    store_remanent_triple
    store_potential_side_effects store_result =
  let _, store_potential_bind = store_potential_side_effects in
  let error', (handler, store_result) =
    collect_bdu_potential_restriction_map_aux
      parameter
      handler
      error
      store_remanent_triple
      store_potential_bind
      store_result
  in
  let error =
    Exception.check warn parameter error error' (Some "line 675") Exit
  in
  error, (handler, store_result)

(************************************************************************************)
(*projection with rule_id*)

let collect_proj_bdu_potential_restriction_map parameter handler error
    store_bdu_potential_restriction_map =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  (*an empty hconsed list*)
  let error, handler, empty =
    Mvbdu_wrapper.Mvbdu.build_reverse_sorted_association_list parameter handler error [] in
  let (error, handler), store_result =
    Project2bdu_potential.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_type, new_site_name, rule_id, cv_id) -> rule_id)
      (fun (agent_type, new_site_name, rule_id, cv_id) -> agent_type, new_site_name, cv_id)
      (bdu_true, empty)
      (fun _ (error, handler) _ pair' -> (error, handler), pair')
      store_bdu_potential_restriction_map
  in
  (error, handler), store_result

(************************************************************************************)
(*REMOVE: used in is_enable*)

let collect_proj_bdu_views parameter handler error store_bdu_test_restriction_map =
  let error, handler, bdu_true = Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error in
  let (error, handler), store_result =
    Project2_bdu_views.proj2_monadic
      parameter
      (error, handler)
      (fun (agent_id, agent_type, rule_id, cv_id) -> rule_id)
      (fun (agent_id, agent_type, rule_id, cv_id) -> (agent_id, agent_type, cv_id))
      bdu_true
      (fun parameter (error, handler) bdu bdu' ->
        let error, handler, bdu_union = Mvbdu_wrapper.Mvbdu.mvbdu_and
          parameter handler error bdu bdu'
        in
        (error, handler), bdu_union
      )
      store_bdu_test_restriction_map
  in
  (error, handler), store_result

(*******************************************************************************)
(*PRINT SECTION*)

open Printf

(************************************************************************************)

let print_triple_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (id, _, set) :: tl ->
      fprintf stdout "Covering_class_id:%i\n" id;
      Site_map_and_set.Set.iter (fun site ->
        fprintf stdout "site_type:%i\n" site
      ) set; aux tl
  in aux l

let print_remanent_triple parameter error result =
  AgentMap.print error
    (fun error parameter triple_list ->
      let _ =
        print_triple_list triple_list
      in
      error
    ) parameter result

(************************************************************************************)
(*working list*)

let print_wl_creation parameter result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Working list creation:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Fifo.IntWL.print_wl parameter result

(************************************************************************************)
(*test rule*)

let print_bdu_test_restriction_map parameter error result =
  Map_test_bdu.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) bdu_test ->
      let () =
        Loggers.fprintf  (Remanent_parameters.get_logger parameter)
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_id agent_type rule_id cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_test
    ) result

let print_proj_bdu_test_restriction_map parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Bdu for the valuations of the views that are tested (projection per rule):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_final_test_bdu.Map.iter
    (fun rule_id map_b ->
      let () = Loggers.fprintf  (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_agent_id_test_bdu.Map.iter (fun agent_id bdu_test ->
        let () = Loggers.fprintf  (Remanent_parameters.get_logger parameter) "agent_id:%i" agent_id in
	let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
        Mvbdu_wrapper.Mvbdu.print parameter bdu_test
      ) map_b
    ) result

(************************************************************************************)
(*creation rule*)

let print_bdu_creation_restriction_map parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Bdu for the valuations of the views that are created (per rule, agent and covering class):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_creation_bdu.Map.iter
    (fun (agent_type, rule_id, cv_id) bdu_creation ->
      let () =
        Loggers.fprintf  (Remanent_parameters.get_logger parameter) "agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_type rule_id cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_creation
    ) result

let print_proj_bdu_creation_restriction_map parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Bdu for the valuations of the views that are created (per rule_id; projection function):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_final_creation_bdu.Map.iter
    (fun rule_id map_b ->
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_agent_type_creation_bdu.Map.iter
        (fun (agent_type, cv_id) bdu_creation ->
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:cv_id:%i"
            agent_type cv_id
          in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
          Mvbdu_wrapper.Mvbdu.print parameter bdu_creation
        ) map_b
    ) result

(************************************************************************************)
(*bdu initial state*)

let print_bdu_init_restriction_map parameter error result =
  Map_init_bdu.Map.iter
    (fun (agent_type, cv_id) bdu_init ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:covering_class_id:%i"
          agent_type cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_init
    ) result

(************************************************************************************)
(*modification list, this is list_a*)

let print_modif_list_restriction_map parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- List for update of the views due to modification (per rule, agent and covering class):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_modif_list.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) list_a ->
      let () =
        Loggers.fprintf  (Remanent_parameters.get_logger parameter)
          "agent_id:%i:agent_type:%i:rule_id:%i:covering_class_id:%i"
          agent_id agent_type rule_id cv_id
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Mvbdu_wrapper.Mvbdu.print_association_list parameter list_a
    ) result

(************************************************************************************)
(*bdu of potential partner in side effects*)

let print_bdu_potential_effect_restriction_map parameter error result =
  Map_potential_bdu.Map.iter
    (fun (agent_type, site_type, rule_id, cv_id) (bdu_potential,list) ->
      let () =
        Loggers.fprintf  (Remanent_parameters.get_logger parameter)
          "agent_type:%i:new_site_name:%i:rule_id:%i:covering_class_id:%i"
          agent_type site_type rule_id cv_id
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Mvbdu_wrapper.Mvbdu.print parameter bdu_potential;
      Mvbdu_wrapper.Mvbdu.print_association_list parameter list
    ) result

(*projection*)

let print_proj_bdu_potential_restriction_map parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Bdu for the valuations of the views that are created from potential partner in side effects (projection per rule):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_final_potential_bdu.Map.iter
    (fun rule_id map_b ->
      let () = Loggers.fprintf  (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_agent_type_potential_bdu.Map.iter
        (fun (agent_type, site_type, cv_id) (bdu_potential,list) ->
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_type:%i:new_site_name:%i:covering_class_id:%i"
            agent_type site_type cv_id
          in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
          Mvbdu_wrapper.Mvbdu.print parameter bdu_potential;
	  Mvbdu_wrapper.Mvbdu.print_association_list
            parameter list
        ) map_b
    ) result

(************************************************************************************)
(*projection function will be used in is_enable function*)

let print_proj_bdu_views parameter error result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "- Bdu for the valuation of the views that are tested (projection per rule):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Map_rule_id_views.Map.iter
    (fun rule_id map_b ->
      let () = Loggers.fprintf  (Remanent_parameters.get_logger parameter) "rule_id:%i" rule_id in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Map_triple_views.Map.iter
        (fun (agent_id, agent_type, cv_id) bdu ->
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_id:%i:agent_type:%i:cv_id:%i"
            agent_id agent_type cv_id in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
          Mvbdu_wrapper.Mvbdu.print parameter bdu
        ) map_b
    ) result
