(**
  * agent_trace.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation:                      <2016-03-21 10:00:00 feret>
  * Last modification: Time-stamp: <2016-03-29 21:33:08 feret>
  * *
  * Compute the projection of the traces for each insighful
   * subset of site in each agent

  *
  * Copyright 2016 Institut National
  * de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let losange_reduction = false
let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Agent_trace.ml") message exn (fun () -> default)

type node_type = Concrete | Main_abstract | Secondary_abstract of int

type label = Ckappa_sig.c_rule_id * Ckappa_sig.c_agent_id
module Label =
  Map_wrapper.Make
    (SetMap.Make
       (struct
	   type t = label
	   let compare = compare
	 end))
module LabelMap = Label.Map
module Site =
  Map_wrapper.Make
    (SetMap.Make
       (struct
	 type t = Ckappa_sig.c_site_name
	 let compare = compare
	end))
module SiteSet = Site.Set
module Edge =
   Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int * label
         let compare = compare
        end
       ))
module EdgeSet = Edge.Set
type nodes_adj =
  {
    outgoing_transitions: (label * Wrapped_modules.LoggedIntMap.elt) list Wrapped_modules.LoggedIntMap.t ;
    in_out_labels: int list Wrapped_modules.LoggedIntMap.t ;
    creation: label list  Wrapped_modules.LoggedIntMap.t ;
    degradation: label list Wrapped_modules.LoggedIntMap.t ;
  }

type graph_with_losange_reduction =
  {
    main_nodes:  (Ckappa_sig.c_site_name * Ckappa_sig.state) list Wrapped_modules.LoggedIntMap.t;
    sub_nodes_in: ((Ckappa_sig.c_site_name * Ckappa_sig.state) list * int) Wrapped_modules.LoggedIntMap.t;
    sub_nodes_out: ((Ckappa_sig.c_site_name * Ckappa_sig.state) list * int) Wrapped_modules.LoggedIntMap.t;
    nodes_creation:  label list  Wrapped_modules.LoggedIntMap.t ;
    nodes_degradation: label list Wrapped_modules.LoggedIntMap.t ;
    transitions: (label  * int) list Wrapped_modules.LoggedIntMap.t ;
  }

let add_edge parameter error rule_id ag_id q q' label' nodes_adj =
  let error, old =
    match
      Wrapped_modules.LoggedIntMap.find_option_without_logs
	parameter error q nodes_adj.outgoing_transitions
    with
    | error, None -> error, []
    | error, Some l -> error, l
  in
  let error, outgoing = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error q (((rule_id,ag_id),q')::old) nodes_adj.outgoing_transitions in
  let nodes_adj = { nodes_adj with outgoing_transitions = outgoing } in
  let f error q nodes_adj =
    let error,old =
      match
	Wrapped_modules.LoggedIntMap.find_option_without_logs
	  parameter error q nodes_adj.in_out_labels
      with
      | error, None -> error, []
      | error, Some l -> error, l
    in
    let error, in_out = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error q (label'::old) nodes_adj.in_out_labels in
    error, {nodes_adj with in_out_labels = in_out }
  in
  let error, nodes_adj = f error q nodes_adj in
  let error, nodes_adj = f error q' nodes_adj in
  error, nodes_adj

let add_creation parameter error rule_id ag_id q' nodes_adj =
	   let error, old =
	     match
	       Wrapped_modules.LoggedIntMap.find_option_without_logs
		 parameter error q' nodes_adj.creation
	     with
	     | error, None -> error, []
	     | error, Some l -> error, l
	   in
	   let error, creation = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error q' ((rule_id,ag_id)::old) nodes_adj.creation in
	   let nodes_adj = { nodes_adj with creation = creation } in
	   let f error q nodes_adj =
	     let error,old =
	       match
		 Wrapped_modules.LoggedIntMap.find_option_without_logs
		   parameter error q nodes_adj.in_out_labels
	       with
			| error, None -> error, []
			| error, Some l -> error, l
	     in
	     let error, in_out = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error (-1) (q'::old) nodes_adj.in_out_labels in
	     error, {nodes_adj with in_out_labels = in_out }
	   in
	   let error, nodes_adj = f error q' nodes_adj in
	   error, nodes_adj

let dump_edge fic parameter error handler_kappa compil key key' label =
  let error, rule_name =
    if Remanent_parameters.get_show_rule_names_in_local_traces parameter
    then
      Handler.string_of_rule parameter error handler_kappa compil (fst label)
    else error,""
  in
  let () = Printf.fprintf fic "Node_%i -> Node_%i [label=\"%s\"];\n" key key' rule_name in
  error

let hash_of_variables_list parameter handler error list =
  let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_variables_list parameter handler error list in
  let hash = Ckappa_sig.Views_bdu.hash_of_variables_list hconsed_list in
  error, handler, hash

let hash_of_association_list parameter handler error list =
  let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error list in
  let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_list in
  error, handler, hash

let dump_key_of_asso fic parameter handler error list =
  let error, handler, hash = hash_of_association_list parameter handler error list in
  let () = Printf.fprintf fic "Node_%i" hash in
  error, handler

let print_label_of_asso fic parameter error handler_kappa agent_type agent_string list =
  let () = Printf.fprintf fic "%s(" agent_string  in
  let error,_ =
    List.fold_left
      (fun (error,bool) (site_type, state) ->
	let () =
	  if bool
	  then
	    Printf.fprintf fic ","
	in
	let error, site_string = Handler.string_of_site parameter error handler_kappa agent_type site_type in
	let error', state_string =
	  Handler.string_of_state_fully_deciphered parameter error
	    handler_kappa agent_type site_type state
	in
	let error = Exception.check warn parameter error error'
				    (Some "line 240") Exit in
	let () =
	  Printf.fprintf fic "%s%s" site_string state_string
	in
	error,true
      )
      (error,false) list
  in
  let () = Printf.fprintf fic ")" in
  error

let dump_graph_header fic =
   Printf.fprintf fic "digraph G{\n"

let compute_full_support parameter error ag_id rule =
  let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
  let error, agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
      parameter
      error
      ag_id
      test.Cckappa_sig.views
  in
  let view =
    match
      agent
    with
    | None
    | Some (Cckappa_sig.Dead_agent _)
    | Some (Cckappa_sig.Unknown_agent _) -> None
    | Some Cckappa_sig.Ghost ->
      begin
	let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
	let error, agent =
	  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
	    parameter
	    error
	    ag_id
	    diff
	in
	match
	  agent
	with
	| None -> None
	| Some ag -> Some ag
      end
    | Some (Cckappa_sig.Agent ag) -> Some ag
  in
  let error, list =
    match
      view
    with
    | Some v ->
      begin
	Ckappa_sig.Site_map_and_set.Map.fold
	  (fun site state (error,list) -> SiteSet.add parameter error site list)
	  v.Cckappa_sig.agent_interface
	  (error, SiteSet.empty)
      end
    | None -> error, SiteSet.empty
  in
  error, list

let build_support parameter error rules =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error r_id rule ->
	Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
	  parameter
	  error
	  (fun parameter error ag_id _  ->
	    let error, set = compute_full_support parameter error ag_id rule in
	    LabelMap.add parameter error (r_id,ag_id) set)
	  rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
	  )
      rules LabelMap.empty

      (* find a list of rules with pairwisely distinct support such that the support of each other rule meet each of the support of these rules *)

let smash parameter error support label_list =
  let error, list =
    List.fold_left
      (fun (error,l) label ->
	let error, set = LabelMap.find_option parameter error label support in
	match set with
	| None -> (* error *) error,l
	| Some set -> (error, (label,set,SiteSet.cardinal set)::l))
      (error, [])
      label_list
  in
  let list = List.sort (fun (_,_,a) (_,_,b) -> compare a b) list in
  let rec aux list partition not_in_the_partition =
    match list
    with
    | [] -> partition, not_in_the_partition
    | (label,set,_)::tail ->
      let error, bool =
	let rec aux set list error = 
	  match list with
	  | (_,set')::tail -> 
	      let error, inter = SiteSet.inter parameter error set set' in
	      if SiteSet.is_empty inter
	      then
		aux set tail error
	      else
		error, false
	  | [] -> error, true
	in
	aux set partition error
      in
      if bool
      then
	aux tail ((label,set)::partition) not_in_the_partition
      else
	aux tail partition ((label,set)::not_in_the_partition)
  in
  let partition,not_in_the_partition = aux list [] [] in
  let bool =
    List.for_all
      (fun (_,set) ->
	List.for_all
	  (fun (_,set') ->
	    not (SiteSet.is_empty (snd (SiteSet.inter parameter error set set'))) (* correct to trap errors *)
	  )
	  not_in_the_partition)
      partition
  in
  if bool
  then
    Some (partition, not_in_the_partition)
  else
    None

let agent_trace parameter error handler handler_kappa mvbdu_true compil output =
  let rules = compil.Cckappa_sig.rules in
  let error, support = build_support parameter error rules in
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter
    error
    (fun parameter error agent_type map (handler:Ckappa_sig.Views_bdu.handler) ->
      let error', agent_string =
	try
	  Handler.string_of_agent parameter error handler_kappa agent_type
	with
	  _ -> warn parameter error (Some "line 111") Exit
            (Ckappa_sig.string_of_agent_name agent_type)
      in
      let error = Exception.check warn parameter error error' (Some "line 1917") Exit in
      Wrapped_modules.LoggedIntMap.fold
	(fun _ mvbdu (error,handler) ->
	 let error, handler, sites = Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error mvbdu in
	 let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
	 let error, handler, ext_list =  Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error sites in
	 let error, file_name =
	   List.fold_left
	     (fun (error, string) site ->
	      let error, site_string = Handler.string_of_site parameter error handler_kappa agent_type site in
	      error, string^"_"^site_string)
	     (error, ((Remanent_parameters.get_local_trace_directory parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
             ext_list
	 in
	 let node_names = Wrapped_modules.LoggedIntMap.empty in
	 let node_type = Wrapped_modules.LoggedIntMap.empty in
	 let nodes_adj =
	   {outgoing_transitions = Wrapped_modules.LoggedIntMap.empty;
	    in_out_labels = Wrapped_modules.LoggedIntMap.empty;
	    creation = Wrapped_modules.LoggedIntMap.empty;
	    degradation = Wrapped_modules.LoggedIntMap.empty}
	 in
	 let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	 let fic = Remanent_parameters.open_out file_name in
	 let () = dump_graph_header fic in
	 let error, handler, node_names, node_types =
	   List.fold_left
	     (fun (error, handler,nodes,nodes') list ->
	      let error, handler, hash = hash_of_association_list parameter handler error list in
	      let error, nodes         = Wrapped_modules.LoggedIntMap.add parameter error hash list nodes in
	      let error, nodes'        = Wrapped_modules.LoggedIntMap.add parameter error hash Concrete  nodes' in
	      error, handler, nodes, nodes')
	     (error, handler,node_names,node_type)
	     list
	 in
	 let max_site =
	   List.fold_left
	     (fun output e -> max output (Ckappa_sig.int_of_site_name e))
	     0
	     ext_list
	 in
	 let max_site = max_site + 1 in
	 let post_list = List.rev_map (fun s-> (Ckappa_sig.site_name_of_int ((Ckappa_sig.int_of_site_name s)+max_site),Ckappa_sig.state_index_of_int (-1))) ext_list in
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
	 let error, (handler, nodes_adj) =
	   Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
	     parameter
	     error
	     (fun parameter error r_id rule (handler,nodes_adj) ->
	      let error, rule_name =
		if Remanent_parameters.get_show_rule_names_in_local_traces parameter
		then
		  Handler.string_of_rule parameter error handler_kappa compil r_id
		else error,""
	      in
	      let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
	      let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
	      let error, (handler, (nodes_adj:nodes_adj)) =
		Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
		  parameter
		  error
		  (fun parameter error ag_id diff (handler,nodes_adj) ->
		   begin
		     if diff.Cckappa_sig.agent_name <> agent_type then
		       error, (handler, nodes_adj)
		     else
		       let error, handler, modif_list_creation, modif_list =
			 List.fold_left
			   (fun
			       (error, handler, modif_list_creation, modif_list)
			       site ->
			     match
			       Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
				 parameter error
				 site diff.Cckappa_sig.agent_interface
			     with error, None ->
				  error, handler, modif_list_creation, modif_list
				| error, Some state ->
				   let interv = state.Cckappa_sig.site_state in
				   let min = interv.Cckappa_sig.min in
				   if min = interv.Cckappa_sig.max
				   then
				     error,
				     handler,
				     (site,min)::modif_list_creation,
						((Ckappa_sig.site_name_of_int
						    ((Ckappa_sig.int_of_site_name site)+max_site)
						 ,min)::modif_list)
				   else (* error *)
				     error, handler, modif_list_creation, modif_list
			   )
			   (error, handler, [], [])
			   ext_list
		       in
		       let views = test.Cckappa_sig.views in
		       let error, test =
			 match
			   Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
			     parameter
			     error
			     ag_id
			     views
			 with
			 | error, Some (Cckappa_sig.Agent ag) -> error, Some ag.Cckappa_sig.agent_interface
			 | error, _ -> error, None
		       in
		       match
			 test
		       with
		       | None ->
			  begin
			    let error, handler, update =
			      Ckappa_sig.Views_bdu.build_association_list parameter handler error modif_list_creation
			    in
			    let error, handler, mvbdu =
			      Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu update
			    in
			    let error, handler, list_edges =
			      Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
			    in
			    let error, handler, nodes_adj =
			      List.fold_left
				(fun (error, handler, nodes_adj) post ->
				 let error, handler, hash_init' = hash_of_association_list parameter handler error post in
				 let error, nodes_adj =
				   add_creation parameter error r_id ag_id hash_init' nodes_adj
				 in
				 error, handler, nodes_adj)
				(error, handler, nodes_adj)
				list_edges
			    in
			    error,(handler,nodes_adj)
			  end
		       | Some test ->
			  begin
			    match
			      modif_list
			    with
			    | [] ->
			       error,(handler,nodes_adj)
			    | _ ->
			       begin (* the view is modified by the rule *)
				 let modif_list = List.rev modif_list in
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
				 let error,handler, nodes_adj =
				   List.fold_left
				     (fun (error,handler,(nodes_adj:nodes_adj)) list ->
				      let rec aux list output =
					match
					  list
					with
					  (t,_)::q when post_site t -> List.rev output,list
					| t::q -> aux q (t::output)
					| [] -> (* error *) List.rev output,list
				      in
				      let pre,post = aux list [] in
				      let rec aux pre pos output update_list =
					match pre,pos
					with
					| t::q,t'::q' when good_pair (fst t) (fst t')->
					   if Ckappa_sig.int_of_state_index (snd t') = -1
					   then aux q q' (t::output) update_list
					   else
					     aux q q' ((fst t,snd t')::output) (t::t'::update_list)
					| (t,_)::_,(t',_)::q' ->
					   aux pre q' output update_list
					| [],[] -> List.rev output, List.rev update_list
					| [],_ -> (* error *) List.rev output, List.rev update_list
					| _,[] -> (* error *) List.rev output, List.rev update_list
				      in
				      let post,upd_list = aux pre post [] [] in
				      let error, handler, hash_init = hash_of_association_list parameter handler error pre in
				      let error, handler, hash_init' = hash_of_association_list parameter handler error post in
				      let error, handler, hash_upd = hash_of_association_list parameter handler error upd_list in
				      let error, nodes_adj =
					add_edge parameter error r_id ag_id hash_init hash_init' hash_upd nodes_adj
				      in
				      error, handler, nodes_adj)
				     (error,handler,nodes_adj)
				     list_edges
				 in
				 error,(handler,nodes_adj)
			       end
			  end
		   end
		  )
		  diff
		  (handler, nodes_adj)
	      in
	      error, (handler, nodes_adj)
	     )
	     rules
	     (handler, nodes_adj)
	 in
	 let error =
	   if losange_reduction
	   then
	     begin (* losange reduction *)
	       let empty_state =
		 {
		   main_nodes = Wrapped_modules.LoggedIntMap.empty ;
		   sub_nodes_in = Wrapped_modules.LoggedIntMap.empty ;
		   sub_nodes_out = Wrapped_modules.LoggedIntMap.empty ;
		   nodes_creation=  Wrapped_modules.LoggedIntMap.empty ;
		   nodes_degradation = Wrapped_modules.LoggedIntMap.empty ;
		   transitions = Wrapped_modules.LoggedIntMap.empty ;
		 }
	       in
	       let error,list,set =
		 Wrapped_modules.LoggedIntMap.fold
		   (fun i _ (error,list,set) ->
		    let error,set = Wrapped_modules.LoggedIntSet.add parameter error i set in
		    (error,i::list,set))
		   nodes_adj.creation
		   (error,[],Wrapped_modules.LoggedIntSet.empty)
	       in
	       let add_node_of_interest parameter error node list set =
		 if
		   Wrapped_modules.LoggedIntSet.mem node set
		 then
		   (error,list,set)
		 else
		   let error,set = Wrapped_modules.LoggedIntSet.add parameter error node set in
		   (error,node::list,set)
	       in
	       let rec aux error list set state =
		 match
		   list
		 with
		 | [] -> error, state
		 | h::t ->
		    begin
		      let error,list,set = add_node_of_interest parameter error h list set in (* Temporary loc to please the compiler *)
		      error, state
		    end
	       in
	       let error, state = aux error list set empty_state in
	       error
	     end (* losange reduction *)
	   else if
	     Remanent_parameters.get_use_por_in_local_traces parameter
	   then
	     let error, handler, map_classes =
	       Wrapped_modules.LoggedIntMap.fold
		 (fun
		     i l (error,handler,map_classes) ->
		   let l = List.sort compare l in
		   let rec aux list res =
		     match
		       list
		     with
		     | [] -> List.rev res
		     | t::t'::q when t=t' -> aux (t::q) res
		     | t::q -> aux q (t::res)
		   in
			    let l = aux l [] in
			    let l' = List.map Ckappa_sig.site_name_of_int l in
			    let error, handler, hash = hash_of_variables_list parameter handler error l' in
			    let error, old =
			      match
				Wrapped_modules.LoggedIntMap.find_option_without_logs parameter error hash map_classes
			      with
			      | error, None -> error, []
			      | error, Some l -> error, l
			    in
			    let error, map_classes =
			      Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error hash (i::old) map_classes
			    in error, handler, map_classes)
			  nodes_adj.in_out_labels
			  (error, handler, Wrapped_modules.LoggedIntMap.empty)
		      in
		      let error, node_types =
			Wrapped_modules.LoggedIntMap.fold
			  (fun i l (error,node_types) ->
			   match l with
			   | [] | [_] -> error,node_types
			   | t::q ->
			      let error,node_types = Wrapped_modules.LoggedIntMap.overwrite parameter error t Main_abstract node_types in
			      List.fold_left
				(fun (error,node_types) i ->
				 Wrapped_modules.LoggedIntMap.overwrite parameter error i (Secondary_abstract t) node_types)
				(error,node_types)
				q
			  )
			  map_classes
			  (error,node_types)
		      in
		      (* nodes *)
		      let error, handler =
			Wrapped_modules.LoggedIntMap.fold2z
			  parameter
			  error
			  (fun parameter error key list typ handler ->
			   match typ
			   with
			   | Concrete ->
			      let error, handler = dump_key_of_asso fic parameter handler error list in
			      let () = Printf.fprintf fic " [label=\"" in
			      let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
			      let () = Printf.fprintf fic "\"];\n" in
			      error, handler
			   | Main_abstract->
			      let error, handler  = dump_key_of_asso fic parameter handler error list in
			      let () = Printf.fprintf fic " [style=\"dotted\" label=\"\"];\n" in
			      error, handler
			   | _ -> error, handler)
			  node_names
			  node_types
			  handler
		      in
		      let () =
			Wrapped_modules.LoggedIntMap.iter
			  (fun key _  ->
			   Printf.fprintf fic "Init_%i [height=\"0cm\" width=\"0cm\" style=\"none\" label=\"\"];\n" key )
			
			  nodes_adj.creation
		      in
		      let kind_of error i =
			match
			  Wrapped_modules.LoggedIntMap.find_option
			    parameter error
			    i
			    node_types
			with error,None -> error, Concrete
			   | error,Some i -> error, i
		      in
		      let error,_ =
			Wrapped_modules.LoggedIntMap.fold
			  (fun key list (error,edge_set) ->
			   let error, key =
			     match kind_of error key
			      with
			      | error, Secondary_abstract key -> error,key
			      | error, Concrete
			      | error, Main_abstract -> error, key
			   in
			   List.fold_left
			     (fun (error,edge_set) (label,key') ->
			      let error, key' =
				match kind_of error key'
				with
				| error,Secondary_abstract key' ->  error, key'
				| error,Concrete | error,Main_abstract -> error, key'
			      in
			      if EdgeSet.mem (key,key',label) edge_set
			      then
				error, edge_set
			      else
				let error = dump_edge fic parameter error handler_kappa compil key key' label in
				EdgeSet.add parameter error (key,key',label) edge_set)
			     (error, edge_set)
			     list)
			  nodes_adj.outgoing_transitions
			  (error, EdgeSet.empty)
		      in
		      let error,_ =
			Wrapped_modules.LoggedIntMap.fold
			  (fun key list (error,edge_set) ->
			   let error, key =
			     match
			       kind_of error key
			     with
			     | error, Secondary_abstract key -> error, key
			     | error, Concrete
			     | error, Main_abstract -> error, key
			   in
			   List.fold_left
			     (fun (error,edge_set) (r_id) ->
			      if EdgeSet.mem (key,key,r_id) edge_set
			      then
				error, edge_set
			      else
				let error, rule_name =
				  if Remanent_parameters.get_show_rule_names_in_local_traces parameter
				  then
				    Handler.string_of_rule parameter error handler_kappa compil (fst r_id)
				  else error,""
				in
				let () = Printf.fprintf fic "Init_%i -> Node_%i [label=\"%s\"];\n" key key rule_name in
				EdgeSet.add parameter error (key,key,r_id) edge_set)
			     (error,edge_set)
			     list)
			  nodes_adj.creation
			  (error, EdgeSet.empty)
		      in
		      (* edges *)
		      error
		    else
		      (* nodes *)
		      let () =
			Wrapped_modules.LoggedIntMap.iter
			  (fun key _  ->
			   Printf.fprintf fic "Init_%i [width=\"0cm\" height=\"0cm\" style=\"none\" label=\"\"];\n" key )
			  nodes_adj.creation
		      in
		      let error, handler =
			Wrapped_modules.LoggedIntMap.fold
			  (fun key list (error, handler) ->
			      let error, handler = dump_key_of_asso fic parameter handler error list in
			      let () = Printf.fprintf fic " [label=\"" in
			      let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
			      let () = Printf.fprintf fic "\"];\n" in
			      error, handler)
			  node_names
			  (error, handler)
		      in
		      (* edges *)
		      let error =
			Wrapped_modules.LoggedIntMap.fold
			  (fun key list error ->
			    List.fold_left
			      (fun error (r_id,key') ->
				let error, rule_name =
				  if Remanent_parameters.get_show_rule_names_in_local_traces parameter
				  then
				    Handler.string_of_rule parameter error handler_kappa compil (fst r_id)
				  else error,""
				in
				let () = Printf.fprintf fic "Node_%i -> Node_%i [label=\"%s\"];\n" key key' rule_name in
				error)
			      error
			      list)
			  nodes_adj.outgoing_transitions
			  error
		      in
		      let error,_ =
			Wrapped_modules.LoggedIntMap.fold
			  (fun key list (error,edge_set) ->
			   List.fold_left
			     (fun (error,edge_set) (r_id) ->
			      if EdgeSet.mem (key,key,r_id) edge_set
			      then
				error, edge_set
			      else
				let error, rule_name =
				  if Remanent_parameters.get_show_rule_names_in_local_traces parameter
				  then
				    Handler.string_of_rule parameter error handler_kappa compil (fst r_id)
				  else error,""
				in
				let () = Printf.fprintf fic "Init_%i -> Node_%i [label=\"%s\"];\n" key key rule_name in
				EdgeSet.add parameter error (key,key,r_id) edge_set)
			     (error,edge_set)
			     list)
			  nodes_adj.creation
			  (error, EdgeSet.empty)
		       in
		       error
		  in
		  let _ = Printf.fprintf fic "}\n" in
		  let _ = close_out fic in
		  error, handler
		)
		map
		(error, handler))
    output handler
