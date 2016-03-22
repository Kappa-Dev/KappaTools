(**
   * agent_trace.ml
   * openkappa
   * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
   *
   * Creation:                      <2016-03-21 10:00:00 feret>
   * Last modification: Time-stamp: <2016-03-21 22:07:02 feret>
   * *
   * Compute the projection of the traces for each insighful
   * subset of site in each agent

   *
   * Copyright 2016 Institut National
   * de Recherche en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Agent_trace.ml") message exn (fun () -> default)

let agent_trace parameter error handler handler_kappa mvbdu_true compil   output =
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter
    error
    (fun parameter error agent_type map (handler:Ckappa_sig.Views_bdu.handler) ->
      let error', agent_string =
	try
	  Handler.string_of_agent parameter error handler_kappa agent_type
	with
	  _ -> warn parameter error (Some "line 30") Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      let error = Exception.check warn parameter error error' (Some "line 32") Exit in
      Wrapped_modules.LoggedIntMap.fold
	(fun _ mvbdu (error, handler) ->
	  let error, handler, sites = 
            Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error mvbdu 
          in
	  let error, handler, list =
            Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu 
          in
	  let error, handler, ext_list =
            Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error sites 
          in
	  let error, file_name =
	    List.fold_left
	      (fun (error, string) site ->
		let error, site_string =
                  Handler.string_of_site parameter error handler_kappa agent_type site 
                in
		error, string ^ "_" ^ site_string (* to be replaced with the kappa site *))
	      (error, ("Agent_trace_" ^ (agent_string)))
              ext_list
	  in
	  let fic = open_out (file_name ^ ".dot") in
	  let hash_of_association_list handler error list =
	    let error, handler, hconsed_list = 
              Ckappa_sig.Views_bdu.build_association_list parameter handler error list
            in
	    let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_list in
	    error, handler, hash
	  in
	  let print_key_of_asso handler error list =
	    let error, handler, hash = hash_of_association_list handler error list in
	    let () = Printf.fprintf fic "Node_%i" hash in
	    error, handler
	  in
	  let print_label_of_asso error list =
	    List.fold_left
	      (fun error (site_type, state) ->
		let error, state_string =
		  Handler.string_of_state_fully_deciphered parameter error
		    handler_kappa agent_type site_type state
		in
		let error = Exception.check warn parameter error error'
		  (Some "line 76") Exit in
		(*-----------------------------------------------------------*)
		let () =
		  Printf.fprintf fic "_%s" state_string
		in
		error
	      )
	      error list
	  in
	  let _ =
	    Printf.fprintf fic
	      "digraph G{\n"
	  in
	  let error, handler =
	    List.fold_left
	      (fun (error, handler) list ->
		let error, handler =
		  print_key_of_asso handler error list in
		let () = Printf.fprintf fic " [label=\"" in
		let error = print_label_of_asso error list in
		let () = Printf.fprintf fic "\"];\n" in
		error, handler)
	      (error, handler)
	      list
	  in
	  let max_site =
	    List.fold_left
	      (fun output e -> max output (Ckappa_sig.int_of_site_name e))
	      0
	      ext_list
	  in
	  let max_site = max_site + 1 in
	  let post_list =
            List.rev_map (fun s-> 
              (Ckappa_sig.site_name_of_int ((Ckappa_sig.int_of_site_name s) + max_site),
               Ckappa_sig.state_index_of_int (-1))) ext_list
          in
	  let post_list = List.rev post_list in
	  let error, handler, post_list =
	    Ckappa_sig.Views_bdu.build_association_list parameter handler error post_list
	  in
	  let error, handler, bdu_diag =
	    Ckappa_sig.Views_bdu.mvbdu_redefine
	      parameter
	      handler
	      error
	      mvbdu_true
	      post_list
	  in
	  let rules = compil.Cckappa_sig.rules in
	  let error, handler =
	    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
	      parameter
	      error
	      (fun parameter error r_id rule handler ->
		let error, rule_name =  error, "" in
                (*Handler.string_of_rule parameter error handler_kappa compil r_id in*)
		let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
		let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
		let error, handler =
		  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
		    parameter
		    error
		    (fun parameter error ag_id diff handler ->
		      begin
			if diff.Cckappa_sig.agent_name <> agent_type then
			  error, handler
			else
			  let error, handler, modif_list =
			    List.fold_left
			      (fun
				(error, handler, modif_list)
				site ->
				  match
				    Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
				      parameter error
				      site diff.Cckappa_sig.agent_interface
				  with error, None ->
				    error, handler, modif_list
				  | error, Some state ->
				    let interv = state.Cckappa_sig.site_state in
				    let min = interv.Cckappa_sig.min in
				    if min = interv.Cckappa_sig.max
				    then
				      error,
				      handler,
				      ((Ckappa_sig.site_name_of_int
					  ((Ckappa_sig.int_of_site_name site)+max_site)
					  ,min)::modif_list)
				    else (* error *)
				      error, handler, modif_list
			      )
			      (error, handler, [])
			      ext_list
			  in
			  match
			    modif_list
			  with
			    [] ->
			      error,handler
			  | _ ->
			    begin (* the view is modified by the rule *)
			      let modif_list = List.rev modif_list in
			      let views = test.Cckappa_sig.views in
			      let error, test = 
				match
				  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
				    parameter
				    error
				    ag_id
				    views
				with
				| error, Some (Cckappa_sig.Agent ag) -> error, ag.Cckappa_sig.agent_interface
				| error, _ -> error, Ckappa_sig.Site_map_and_set.Map.empty
			      in
			      let error, handler, update =
				Ckappa_sig.Views_bdu.build_association_list parameter handler error modif_list
			      in
			      let error, handler, mvbdu_test =
				let error, handler, test_list =
				  List.fold_left
				    (fun
				      (error, handler, test_list)
				      site ->
					match
					  Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
					    parameter error
					    site test
					with error, None ->
					  error, handler, test_list
					| error, Some state ->
					  let interv = state.Cckappa_sig.site_state in
					  let min = interv.Cckappa_sig.min in
					  if min = interv.Cckappa_sig.max (* this should not be mandatory *)
					  then
					    error,
					    handler,
					    (site,min)::test_list
					  else (* error *)
					    error, handler, test_list
				    )
				    (error, handler, [])
				    ext_list
				in
				let error,handler, test_list = 
				  Ckappa_sig.Views_bdu.build_association_list
				    parameter handler error test_list
				in
				Ckappa_sig.Views_bdu.mvbdu_redefine 
				  parameter handler error
				  mvbdu_true
				  test_list
			      in
			      let error, handler, mvbdu = 
				Ckappa_sig.Views_bdu.mvbdu_and
				  parameter
				  handler
				  error
				  mvbdu
				  mvbdu_test
			      in
			      let error, handler, mvbdu =
				Ckappa_sig.Views_bdu.mvbdu_and
				  parameter
				  handler
				  error
				  mvbdu
				  bdu_diag
			      in
			      let error, handler, mvbdu =
				Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu update
			      in
			      let error, handler, list_edges =
				Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
			      in
			      let good_pair s1 s2 =
				Ckappa_sig.int_of_site_name s2 =
				Ckappa_sig.int_of_site_name s1 + max_site
			      in
			      let post_site s =
				Ckappa_sig.int_of_site_name s >= max_site
			      in
			      let error =
				List.fold_left
				  (fun error list ->
				    let rec aux list output =
				      match
					list
				      with
					(t,_)::q when post_site t -> List.rev output,list
				      | t::q -> aux q (t::output)
				      | [] -> (* error *) List.rev output,list
				    in
				    let pre,post = aux list [] in
				    let rec aux pre pos output =
				      match pre,pos
				      with
				      | t::q,t'::q' when good_pair (fst t) (fst t')->
					if Ckappa_sig.int_of_state_index (snd t') = -1
					then aux q q' (t::output)
					else
					  aux q q' ((fst t,snd t')::output)
				      | (t,_)::_,(t',_)::q' when (not (good_pair t t')) ->
					aux pre q' output
				      | [],[] -> List.rev output
				      | [],_ -> (* error *) List.rev output
				      | _,[] -> (* error *) List.rev output
				    in
				    let post = aux pre post [] in
				    let error,  handler, hash_init = 
                                      hash_of_association_list handler error pre 
                                    in
				    let error, handler, hash_init' =
                                      hash_of_association_list handler error post
                                    in
				    if hash_init = hash_init' 
                                    then
				      error
				    else
				      let error, handler = print_key_of_asso handler error pre in
				      let () = Printf.fprintf fic " -> " in
				      let error, handler = print_key_of_asso handler error post in
				      let _ =
					Printf.fprintf fic " [label=\"%s\"];\n" rule_name in
				      error)
				  error
				  list_edges
			      in
			      error,handler
			    end
		      end
		    )
		    diff
		    handler
		in
		
		error, handler
	      )
	      rules
	      handler
	  in
	  let _ = Printf.fprintf fic "}\n" in
	  let _ = close_out fic in
	  error, handler
	)
	map
	(error, handler))
    output handler
