(**
  * agent_trace.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation:                      <2016-03-21 10:00:00 feret>
  * Last modification: Time-stamp: <2016-04-01 15:51:14 feret>
  * *
  * Compute the projection of the traces for each insighful
   * subset of site in each agent

  *
  * Copyright 2016 Institut National
  * de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let sanity_test = false
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
module LabelSet = Label.Set
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
    nodes: Ckappa_sig.Views_bdu.mvbdu;
    state_to_mvbdu: Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t ;
    nodes_to_asso_list: (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list Wrapped_modules.LoggedIntMap.t ;
    nodes_name: (Exception.method_handler -> out_channel -> Exception.method_handler)  Wrapped_modules.LoggedIntMap.t ;
    nodes_type: node_type Wrapped_modules.LoggedIntMap.t ;
    outgoing_transitions: (label * Wrapped_modules.LoggedIntMap.elt) list Wrapped_modules.LoggedIntMap.t ;
    outgoing_transitions_map: Wrapped_modules.LoggedIntMap.elt LabelMap.t Wrapped_modules.LoggedIntMap.t ;
    in_out_labels: int list Wrapped_modules.LoggedIntMap.t ;
    creation: label list  Wrapped_modules.LoggedIntMap.t ;
    degradation: label list Wrapped_modules.LoggedIntMap.t ;
  }


let empty_nodes_adj mvbdu_false =
  {
    outgoing_transitions = Wrapped_modules.LoggedIntMap.empty;
    outgoing_transitions_map = Wrapped_modules.LoggedIntMap.empty;
    in_out_labels = Wrapped_modules.LoggedIntMap.empty;
    creation = Wrapped_modules.LoggedIntMap.empty;
    degradation = Wrapped_modules.LoggedIntMap.empty;
    nodes_type = Wrapped_modules.LoggedIntMap.empty;
    nodes = mvbdu_false ;
    nodes_name = Wrapped_modules.LoggedIntMap.empty;
    nodes_to_asso_list = Wrapped_modules.LoggedIntMap.empty;
    state_to_mvbdu = Wrapped_modules.LoggedIntMap.empty;
  }


type graph_with_losange_reduction =
  {
    nodes_adj: nodes_adj;
    n_macro_state: int; 
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
    already_visited: Ckappa_sig.Views_bdu.mvbdu;
    partially_visited: Ckappa_sig.Views_bdu.mvbdu LabelMap.t ;
  }

let fresh_macro_state los_state =
  let n = los_state.n_macro_state in
  n,
  {los_state with n_macro_state = n+1}

let copy_node parameter handler error q nodes_adj los_state =
  let nodes_adj' = los_state.nodes_adj in
  let error, mvbdu = Wrapped_modules.LoggedIntMap.find_option parameter error q nodes_adj.state_to_mvbdu in
  match 
    mvbdu
  with
  | None -> error, handler, los_state
  | Some mvbdu ->
    begin
      let error, handler, nodes = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu los_state.nodes_adj.nodes in
      let error, state_to_mvbdu = Wrapped_modules.LoggedIntMap.add parameter error q mvbdu los_state.nodes_adj.state_to_mvbdu in
      let error, nodes_to_asso_list = 
	let error, output = Wrapped_modules.LoggedIntMap.find_default parameter error [] q nodes_adj.nodes_to_asso_list in
	Wrapped_modules.LoggedIntMap.add parameter error q output los_state.nodes_adj.nodes_to_asso_list
      in
      let error, nodes_name = 
	let error, output = Wrapped_modules.LoggedIntMap.find_default parameter error (fun a _ -> a) q nodes_adj.nodes_name in
	Wrapped_modules.LoggedIntMap.add parameter error q output los_state.nodes_adj.nodes_name
      in
      let error, nodes_type = 
	let error, output = Wrapped_modules.LoggedIntMap.find_default parameter error Concrete q nodes_adj.nodes_type in
	Wrapped_modules.LoggedIntMap.add parameter error q output los_state.nodes_adj.nodes_type
      in
      error, 
      handler, 
      {los_state with nodes_adj =
	  {nodes_adj' 
       with
	 nodes = nodes ;
	 state_to_mvbdu = state_to_mvbdu ;
	 nodes_to_asso_list = nodes_to_asso_list ;
	 nodes_name = nodes_name ;
	 nodes_type = nodes_type 
	  }}
    end
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
   let error, old =
    match
      Wrapped_modules.LoggedIntMap.find_option_without_logs
	parameter error q nodes_adj.outgoing_transitions_map
    with
    | error, None -> error, LabelMap.empty
    | error, Some map -> error, map
  in
  let error, map =
    LabelMap.add parameter error (rule_id,ag_id) q' old
  in
  let error, outgoing_map = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error q map nodes_adj.outgoing_transitions_map in
  let nodes_adj = { nodes_adj with outgoing_transitions_map = outgoing_map } in
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

let add_concrete_node parameter handler handler_kappa error agent_type agent_string mvbdu list nodes_adj =
  let error, handler, inter = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu nodes_adj.nodes in
  let f error fic  =
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
  in
  if Ckappa_sig.Views_bdu.equal inter mvbdu
  then
    error, handler, nodes_adj
  else
    let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error list in
    let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_list in
    let error, handler, new_set = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error nodes_adj.nodes mvbdu in
    let error, name = error, "" in
    let error, new_nodes_name = Wrapped_modules.LoggedIntMap.add parameter error hash f nodes_adj.nodes_name in
    let error, new_nodes_type = Wrapped_modules.LoggedIntMap.add parameter error hash Concrete nodes_adj.nodes_type in
    let error, new_nodes_asso = Wrapped_modules.LoggedIntMap.add parameter error hash list nodes_adj.nodes_to_asso_list in
    let error, new_state_to_mvbdu = Wrapped_modules.LoggedIntMap.add parameter error hash mvbdu nodes_adj.state_to_mvbdu in
    error,
    handler,
    {
      nodes_adj with
      state_to_mvbdu = new_state_to_mvbdu ;
      nodes = new_set ;
      nodes_name = new_nodes_name ;
      nodes_type = new_nodes_type ;
      nodes_to_asso_list = new_nodes_asso ;
    }
      
let add_concrete_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu nodes_adj =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
  in
  match list
  with
  | [list] ->
     begin
       add_concrete_node parameter handler handler_kappa error  agent_type agent_string mvbdu list nodes_adj
     end
  | _ -> error, handler, nodes_adj (* error *)
  
let add_concrete_node_from_asso parameter handler handler_kappa error agent_type agent_string asso_list nodes_adj =  
   let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso_list in
   let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
   let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_list in
   add_concrete_node parameter handler handler_kappa error agent_type agent_string mvbdu asso_list nodes_adj 
  
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

let hash_of_mvbdu parameter handler error mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
  in
  match list
  with
  | [list] ->
     begin
       hash_of_association_list parameter handler error list
     end
  | _ -> error, handler, -1 (* error *)
  
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

let is_black_listed parameter handler error list state =
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true list in
  let error, handler, inter = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu state.already_visited in
  if Ckappa_sig.Views_bdu.equal inter mvbdu
  then
    error, handler, true
  else
    error, handler, false

let compute_full_support parameter error ag_id rule =
  let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
  let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
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
   let error, agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
      parameter
      error
      ag_id
      diff
  in
  let error, list' =
    match
      agent
    with
    | None -> error, SiteSet.empty
    | Some v ->
       begin
	 Ckappa_sig.Site_map_and_set.Map.fold
	   (fun site state (error,list) -> SiteSet.add parameter error site list)
	   v.Cckappa_sig.agent_interface
	   (error, SiteSet.empty)
      end
  in
  error, list, list'

let build_support parameter error rules =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error r_id rule ->
	Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
	  parameter
	  error
	  (fun parameter error ag_id _  ->
	    let error, set_test, set_mod  = compute_full_support parameter error ag_id rule in
	    LabelMap.add parameter error (r_id,ag_id) (set_test, set_mod))
	  rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
	  )
      rules LabelMap.empty

      (* find a list of rules with pairwisely distinct support *)
      (* when restricted_version = true *)
      (* it is also asked that the support of each other rule meet each of the support of the first list of  rules *)

let is_subset parameter error handler a b =
  let error, handler, c = Ckappa_sig.Views_bdu.mvbdu_and parameter error handler a b in
  error,Ckappa_sig.Views_bdu.equal a c

let smash parameter error ?restricted_version:(restricted_version=false) support label_list =
  let error, list =
    List.fold_left
      (fun (error,l) label ->
	let error, set  = LabelMap.find_option parameter error label support in
	match set with
	| None -> (* error *) error,l
	| Some (set,set') -> (error, (label,set,set',SiteSet.cardinal set,SiteSet.cardinal set')::l))
      (error, [])
      label_list
  in
  let list =
    List.sort
      (fun (_,_,_,a,a') (_,_,_,b,b') ->
       let cmp = compare a b in
       if cmp = 0 then compare a' b' else cmp)
      list
  in
  let rec aux list partition not_in_the_partition =
    match list
    with
    | [] -> partition, not_in_the_partition
    | (label,set_test,set_mod,_,_)::tail ->
      let error, bool =
	let rec aux set_test set_mod list error = 
	  match list with
	  | (_,set_test',set_mod')::tail -> 
	     let error, inter = SiteSet.inter parameter error set_test' set_mod in
	     let error, inter' = SiteSet.inter parameter error set_mod' set_test in
	      if SiteSet.is_empty inter && SiteSet.is_empty inter'
	      then
		aux set_test set_mod tail error
	      else
		error, false
	  | [] -> error, true
	in
	aux set_test set_mod partition error
      in
      if bool
      then
	aux tail ((label,set_test,set_mod)::partition) not_in_the_partition
      else
	aux tail partition ((label,set_test,set_mod)::not_in_the_partition)
  in
  let partition,not_in_the_partition = aux list [] [] in
  let bool =
    not restricted_version
    ||
      (List.for_all
	 (fun (_,set_test,set_mod) ->
	   List.for_all
	     (fun (_,set_test',set_mod') ->
	       not (SiteSet.is_empty (snd (SiteSet.inter parameter error set_mod set_test'))) (* correct to trap errors *)
	     )
	     not_in_the_partition)
	 partition)
  in
  if bool
  then
    error, Some (partition, not_in_the_partition)
  else
    error, None

let collect_concurrent parameter error p =
  List.fold_left
    (fun (error, labelset, siteset) (label,set) ->
     let error, labelset = LabelSet.add parameter error label labelset in
     let error, siteset = SiteSet.union parameter error siteset set in
     error, labelset, siteset)
    (error, LabelSet.empty, SiteSet.empty)
    p



let extend_partition parameter error handler mvbdu_false support nodes_adj initial_partition starting_state =
  let error, total_support_test, total_support_mod =
    List.fold_left
      (fun
	(error, set_test, set_mod) (_,set_test',set_mod') ->
	let error, test = SiteSet.union parameter error set_test set_test' in
	let error, mod_ = SiteSet.union parameter error set_mod set_mod' in
	error, test, mod_)
      (error, SiteSet.empty,SiteSet.empty)
      initial_partition
  in
  let _ = Printf.fprintf stdout "TOTAL SUPPORT\n" in
  let _ = SiteSet.iter (fun s -> Printf.fprintf stdout "%i," (Ckappa_sig.int_of_site_name s)) total_support_test in
  let _ = Printf.fprintf stdout "\n" in
  let _ = SiteSet.iter (fun s -> Printf.fprintf stdout "%i," (Ckappa_sig.int_of_site_name s)) total_support_mod in
  let _ = Printf.fprintf stdout "\n" in
  List.fold_left
    (fun 
      (error, handler, support_total_test, support_total_mod, accu) 
      (label,set_test,set_mod) ->
	let rec aux parameter error handler nodes_adj visited support_total_test support_total_mod support_local_test support_local_mod labelset to_be_visited              =
	  match to_be_visited with
	  | [] -> error, handler, support_local_test, support_local_mod, labelset
	  | state::tail -> 
	     begin
	       let error, handler, meet = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error state visited in
	       if not (Ckappa_sig.Views_bdu.equal meet state)
	       then (* fresh state, visite the outgoing transitions *)
		 let error, handler, visited = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error state visited in
		 let error, handler, hash = hash_of_mvbdu parameter handler error state in
		 let error, outgoing =
		   Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] hash nodes_adj.outgoing_transitions
		 in
		 let error, handler, to_be_visited, nodes_adj, support_local_test, support_local_mod, labelset =
		   List.fold_left
		     (fun (error,handler,to_be_visited,nodes_adj,support_local_test,support_local_mod,labelset) (label,q') ->
		      let error,(set_test,set_mod) = LabelMap.find_default  parameter error (SiteSet.empty,SiteSet.empty) label support in
		      let error, diff_test = SiteSet.minus parameter error set_test support_local_test in
		      let error, meet_test = SiteSet.inter parameter error diff_test support_total_mod in
		      let error, diff_mod = SiteSet.minus parameter error set_mod support_local_mod in
		      let error, meet_mod = SiteSet.inter parameter error diff_mod support_total_test in
		      if SiteSet.is_empty meet_test && SiteSet.is_empty meet_mod
		      then
			let error, mvbdu =
			  Wrapped_modules.LoggedIntMap.find_option
		            parameter
			    error
			    q'
			    nodes_adj.state_to_mvbdu
			in
			match
			  mvbdu
			with
			| None -> error, handler, to_be_visited, nodes_adj, support_local_test, support_local_mod, labelset
			| Some mvbdu -> 
			   let to_be_visited = mvbdu::to_be_visited in
			   let error, support_local_test = SiteSet.union parameter error set_test support_local_test in
			   let error, support_local_mod = SiteSet.union parameter error set_mod support_local_mod in
			   let error, labelset = LabelSet.add parameter error label labelset in
			   error, handler, to_be_visited, nodes_adj, support_local_test, support_local_mod, labelset
		      else
		      	error, handler, to_be_visited, nodes_adj, support_local_test, support_local_mod, labelset
		     )
		     (error,handler,tail,nodes_adj,support_local_test,support_local_mod,labelset)
		     outgoing
		 in
		 aux parameter error handler nodes_adj visited support_total_test support_total_mod support_local_test support_local_mod labelset to_be_visited
	       else
		 aux parameter error handler nodes_adj visited support_total_test support_total_mod support_local_test support_local_mod labelset tail
	    end
	in
	let error, handler, support_local_test, support_local_mod, labelset = 
	  aux parameter error handler nodes_adj mvbdu_false total_support_test total_support_mod set_test set_mod (LabelSet.singleton label) [starting_state] in
	let error, support_total_test =
	  SiteSet.union parameter error support_total_test support_local_test
	in
	let error, support_total_mod =
	  SiteSet.union parameter error support_total_mod support_local_mod
	in
	error, 
	handler, 
	support_total_test,
	support_total_mod,
	(labelset, support_local_test,support_local_mod)::accu)
    (error, handler, total_support_test, total_support_mod, [])
    initial_partition

let replay parameter error handler mvbdu_false support los_state partition starting_state =
   let error, total_support_test, total_support_mod =
    List.fold_left
      (fun
	(error, set_test, set_mod) (_,set_test',set_mod') ->
	let error, test = SiteSet.union parameter error set_test set_test' in
	let error, mod_ = SiteSet.union parameter error set_mod set_mod' in
	error, test, mod_)
      (error, SiteSet.empty,SiteSet.empty)
      partition
   in
   let error, handler, support_agent =
     Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error starting_state
   in
   let error, handler, los_state, exit_points =
     List.fold_left
       (fun (error, handler, los_state, exit_points) (set,support_test,support_mod) ->
	    error, handler, los_state, exit_points)
       (error, handler, los_state, Wrapped_modules.LoggedIntSet.empty)
       partition
   in
   error, handler, los_state
		     
let example ?restricted_version:(restricted_version=false) list nodes_adj =
  let f int_list =
    List.rev_map Ckappa_sig.site_name_of_int (List.rev int_list)
  in 
  let label i = (Ckappa_sig.rule_id_of_int i,Ckappa_sig.agent_id_of_int i) in
  let parameter =
        Remanent_parameters.get_parameters ()
  in
  let error = Exception.empty_error_handler in
  let input =
    List.rev_map
      (fun (lab, list) -> (label lab, f list))
      (List.rev list)
  in
  let set_of_list =
    List.fold_left
      (fun (error,set) elt -> SiteSet.add parameter error elt set)
      (error,SiteSet.empty)
  in
  let error,support,list =
    List.fold_left
      (fun (error,support,list) (label,sites) ->
	     let error,set = set_of_list sites in
	     let error, support = LabelMap.add parameter error label (set,set) support in
	     error,support,label::list)
      (error,LabelMap.empty,[])
      input
  in
  let error, output = smash ~restricted_version parameter error support list in
  let _ =
    match output with
    | None -> Printf.fprintf stdout "NONE\n"
    | Some (p,n_p) ->
       begin
	 let () = Printf.fprintf stdout "PARTITION: " in
	 let () = List.iter (fun ((s,_),_,_) -> Printf.fprintf stdout "%i" (Ckappa_sig.int_of_rule_id s)) p in
	 let () = Printf.fprintf stdout " SYNC: " in
     let () = List.iter (fun ((s,_),_,_) -> Printf.fprintf stdout "%i" (Ckappa_sig.int_of_rule_id s)) n_p  in
     let () = Printf.fprintf stdout "\n" in
     ()
       end
  in
  ()


let agent_trace parameter error handler handler_kappa mvbdu_true compil output =
  let rules = compil.Cckappa_sig.rules in
  let error, support = build_support parameter error rules in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true  parameter handler error in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  let () =
    if sanity_test
    then
      begin
	let nodes_adj = empty_nodes_adj mvbdu_false in
	let _ = example (*PARTITION: 321 SYNC: 4*)
		  [1, [1;2] ;
		   2, [4;5] ;
		   3, [7;8] ;
		   4, [1;4;7]] nodes_adj
	in
	let _ = example ~restricted_version:true (* NONE *)
			[1, [1;2] ;
			 2, [2;3] ;
			 3, [4;5]] nodes_adj
	in
	let _ = example ~restricted_version:false (* PARTITION 31, SYNC 2 *)
			[1, [1;2] ;
			 2, [2;3] ;
			 3, [4;5]] nodes_adj
	in
	()
      end
  in
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
	 let nodes_adj =
	   {
	     nodes_to_asso_list = Wrapped_modules.LoggedIntMap.empty ;
	     nodes_name = Wrapped_modules.LoggedIntMap.empty;
	     nodes = mvbdu_false;
	     state_to_mvbdu = Wrapped_modules.LoggedIntMap.empty;
	     nodes_type = Wrapped_modules.LoggedIntMap.empty;
	     outgoing_transitions = Wrapped_modules.LoggedIntMap.empty;
	    outgoing_transitions_map = Wrapped_modules.LoggedIntMap.empty;
	    in_out_labels = Wrapped_modules.LoggedIntMap.empty;
	    creation = Wrapped_modules.LoggedIntMap.empty;
	    degradation = Wrapped_modules.LoggedIntMap.empty}
	 in
	 let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	 let fic = Remanent_parameters.open_out file_name in
	 let () = dump_graph_header fic in
	 let error, handler, nodes_adj =
	   List.fold_left
	     (fun (error, handler,nodes_adj) list ->
	      add_concrete_node_from_asso parameter handler handler_kappa error agent_type agent_string list nodes_adj)
	     (error, handler,nodes_adj)
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
	       let _ = Printf.fprintf stdout "BEGIN\n" in
	       let empty_state =
		 {
		   partially_visited = LabelMap.empty ;
		   n_macro_state = 0 ;
		   macro_state_to_state = Wrapped_modules.LoggedIntMap.empty ;
		   already_visited = mvbdu_false ;
		   nodes_adj = {(empty_nodes_adj mvbdu_false) with creation = nodes_adj.creation} ;
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
	       let rec aux handler error list set los_state =
		 match
		   list
		 with
		 | [] -> error, handler, los_state
		 | h::t ->
		    begin
		      (* to do check that h is not in state *)
		      let error, mvbdu =
			Wrapped_modules.LoggedIntMap.find_default
			  parameter
			  error
			  mvbdu_false
			  h
			  nodes_adj.state_to_mvbdu
		      in
		      let error, handler, state' =
			Ckappa_sig.Views_bdu.mvbdu_or parameter handler error los_state.already_visited mvbdu
		      in
		      if Ckappa_sig.Views_bdu.equal los_state.already_visited state'
		      then
			aux handler error t set los_state
		      else
			let los_state = {los_state with already_visited = state'} in
			let error, handler, los_state = copy_node parameter handler error h nodes_adj los_state in
			let error, outgoing =
			  Wrapped_modules.LoggedIntMap.find_default_without_logs
			    parameter
			    error
			    []
			    h
			    nodes_adj.outgoing_transitions
			in
			let list_with_support =
			  List.rev_map fst (List.rev outgoing)
			in
			let error, output = smash parameter error support list_with_support in
			let error, los_state, dealt_label, output  =
			  match
			    output
			  with
			  | None ->
			    error, los_state, LabelSet.empty, []
			  | Some output ->
			   let error, handler, support_total_test, support_total_mod, output =
			     extend_partition
			       parameter
			       error
			       handler
			       mvbdu_false
			       support
			       nodes_adj
			       (fst output)
			       mvbdu
			   in
			   let error, handler, los_state =
			     replay parameter error handler error () los_state output mvbdu  in
			   (* to do*)
			   error, los_state, LabelSet.empty, output
			in
			let error, outgoing =
			  Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] h los_state.nodes_adj.outgoing_transitions
			in
			let error, handler, los_state, to_be_visited =
			  List.fold_left 
			    (fun (error, handler, los_state, to_be_visited) (label,q') -> 
			      if LabelSet.mem label dealt_label
			      then 
				error, handler, los_state, to_be_visited
			      else
				begin
				  let error, mvbdu_macro = LabelMap.find_default parameter error mvbdu_false label los_state.partially_visited in
				  let error, handler, mvbdu' =  Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu mvbdu_macro in
				  if Ckappa_sig.Views_bdu.equal mvbdu' mvbdu
				  then
				    error, handler, los_state, to_be_visited
				  else
				    let error, nodes_adj = add_edge parameter error (fst label) (snd label) h q' 
 0 (* to do *) los_state.nodes_adj in
				    let los_state = {los_state with nodes_adj = nodes_adj} in
				    let to_be_visited = q'::to_be_visited in
				    error, handler, los_state, to_be_visited 
				end)
			    (error, handler, los_state, t)
			    outgoing
			in
			aux handler error to_be_visited set los_state
		    end
	       in
	       let error, handler, state = aux handler error list set empty_state in
	       let nodes_adj = state.nodes_adj in 
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
			  nodes_adj.nodes_to_asso_list
			  (error, handler)
		      in
		      (* macro_steps *)
		      let rec aux k =
			if k=state.n_macro_state then ()
			else
			  let () = Printf.fprintf fic "Macro_%i [with=\"0cm\" height=\"0cm\" stype=\"none\" label=\"\"];\n" k
			  in aux (k+1)
		      in
		      let () = aux 0 in
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
		      let () =
			Wrapped_modules.LoggedIntMap.iter
			  (fun key ->
			   List.iter
			     (fun h ->
			      Printf.fprintf fic "Node_%i -> Macro_%i [style=\"dashed\" label-\"\"];\n" h key)
			  )
			  state.macro_state_to_state
		      in
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
			  (error,nodes_adj.nodes_type)
		      in
		      let nodes_adj = {nodes_adj with nodes_type = node_types} in
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
			  nodes_adj.nodes_to_asso_list
			  nodes_adj.nodes_type
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
			    nodes_adj.nodes_type
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
			  nodes_adj.nodes_to_asso_list
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
