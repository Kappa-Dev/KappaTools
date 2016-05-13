(**
  * agent_trace.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation:                      <2016-03-21 10:00:00 feret>
  * Last modification: Time-stamp: <2016-04-05 21:58:52 feret>
  * *
  * Compute the projection of the traces for each insighful
   * subset of site in each agent

  *
  * Copyright 2016 Institut National
  * de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let sanity_test = false

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Agent_trace.ml") message exn (fun () -> default)

type fst_label =
  Rule of Ckappa_sig.c_rule_id
| Init of int

type label = fst_label * Ckappa_sig.c_agent_id
module Label =
  Map_wrapper.Make
    (SetMap.Make
       (struct
	   type t = label
	   let compare = compare
	   let print _ _ = ()
	 end))
module LabelMap = Label.Map
module LabelSet = Label.Set
module Site =
  Map_wrapper.Make
    (SetMap.Make
       (struct
	 type t = Ckappa_sig.c_site_name
	 let compare = compare
	 let print _ _ = ()
	end))
module SiteSet = Site.Set
module Edge =
   Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int * label
         let compare = compare
	 let print _ _ = ()
        end
       ))
module EdgeSet = Edge.Set
type nodes_adj =
  {
    nodes: Ckappa_sig.Views_bdu.mvbdu;
    state_to_mvbdu: Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t ;
    nodes_to_asso_list: (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list Wrapped_modules.LoggedIntMap.t ;
    nodes_name: (Exception.method_handler -> out_channel -> Exception.method_handler)  Wrapped_modules.LoggedIntMap.t ;
    outgoing_transitions: (label * Wrapped_modules.LoggedIntMap.elt) list Wrapped_modules.LoggedIntMap.t ;
    ingoing_transitions: (label * Wrapped_modules.LoggedIntMap.elt) list Wrapped_modules.LoggedIntMap.t ;
    outgoing_transitions_map: Wrapped_modules.LoggedIntMap.elt LabelMap.t Wrapped_modules.LoggedIntMap.t ;
    in_out_labels: int list Wrapped_modules.LoggedIntMap.t ;
    creation: label list  Wrapped_modules.LoggedIntMap.t ;
    degradation: label list Wrapped_modules.LoggedIntMap.t ;
  }


let empty_nodes_adj mvbdu_false =
  {
    ingoing_transitions = Wrapped_modules.LoggedIntMap.empty;
    outgoing_transitions = Wrapped_modules.LoggedIntMap.empty;
    outgoing_transitions_map = Wrapped_modules.LoggedIntMap.empty;
    in_out_labels = Wrapped_modules.LoggedIntMap.empty;
    creation = Wrapped_modules.LoggedIntMap.empty;
    degradation = Wrapped_modules.LoggedIntMap.empty;
    nodes = mvbdu_false ;
    nodes_name = Wrapped_modules.LoggedIntMap.empty;
    nodes_to_asso_list = Wrapped_modules.LoggedIntMap.empty;
    state_to_mvbdu = Wrapped_modules.LoggedIntMap.empty;
  }


type graph_with_losange_reduction =
  {
    clusters: int list list ;
    nodes_adj: nodes_adj;
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
    already_visited: Ckappa_sig.Views_bdu.mvbdu;
    in_macro_states: Ckappa_sig.Views_bdu.mvbdu;
    macro_edges: Ckappa_sig.Views_bdu.mvbdu LabelMap.t
  }

let string_of_fst_label parameter error handler_kappa compil i =
  match
    i
  with
  | Rule r_id ->  Handler.string_of_rule parameter error handler_kappa compil r_id
  | Init i -> error, "Init"

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
      error,
      handler,
      {los_state with nodes_adj =
	  {nodes_adj'
       with
	 nodes = nodes ;
	 state_to_mvbdu = state_to_mvbdu ;
	 nodes_to_asso_list = nodes_to_asso_list ;
	 nodes_name = nodes_name ;
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
	parameter error q' nodes_adj.ingoing_transitions
    with
    | error, None -> error, []
    | error, Some l -> error, l
  in
  let error, ingoing = Wrapped_modules.LoggedIntMap.add_or_overwrite parameter error q (((rule_id,ag_id),q)::old) nodes_adj.outgoing_transitions in
  let nodes_adj = { nodes_adj with ingoing_transitions = ingoing } in
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

let add_node parameter handler handler_kappa error agent_type agent_string mvbdu list nodes_adj =
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
    let error, new_nodes_asso = Wrapped_modules.LoggedIntMap.add parameter error hash list nodes_adj.nodes_to_asso_list in
    let error, new_state_to_mvbdu = Wrapped_modules.LoggedIntMap.add parameter error hash mvbdu nodes_adj.state_to_mvbdu in
    error,
    handler,
    {
      nodes_adj with
      state_to_mvbdu = new_state_to_mvbdu ;
      nodes = new_set ;
      nodes_name = new_nodes_name ;
      nodes_to_asso_list = new_nodes_asso ;
    }

let add_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu nodes_adj =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
  in
  match list
  with
  | [list] ->
     begin
       add_node parameter handler handler_kappa error  agent_type agent_string mvbdu list nodes_adj
     end
  | _ ->
    let error, nodes_adj = warn parameter error (Some "line 233") Exit nodes_adj in
    error, handler, nodes_adj (* error *)

let add_node_from_asso parameter handler handler_kappa error agent_type agent_string asso_list nodes_adj =
   let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso_list in
   let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
   let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_list in
   add_node parameter handler handler_kappa error agent_type agent_string mvbdu asso_list nodes_adj

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

let is_macro_edge parameter handler error mvbdu_false label mvbdu state =
  let macro_edges = state.macro_edges in
  let error, mvbdu' = LabelMap.find_default parameter error mvbdu_false label macro_edges in
  Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error mvbdu mvbdu'

let dump_edge fic parameter error handler_kappa compil key key' label =
  let error, rule_name =
    if Remanent_parameters.get_show_rule_names_in_local_traces parameter
    then
      string_of_fst_label parameter error handler_kappa compil (fst label)
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
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_of_hconsed_asso parameter handler error list in
  Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error mvbdu state.already_visited

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
	    LabelMap.add parameter error (Rule r_id,ag_id) (set_test, set_mod))
	  rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
	  )
      rules LabelMap.empty

(* find a list of rules with pairwisely distinct support *)
(* when restricted_version = true *)
(* it is also asked that the support of each other rule meet each of the support of the first list of  rules *)

let smash parameter error ?restricted_version:(restricted_version=false) support label_list =
  let error, list =
    List.fold_left
      (fun (error,l) label ->
	let error, set  = LabelMap.find_option parameter error label support in
	match set with
	| None ->
	  warn parameter error (Some "line 438") Exit l
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
  match
    partition
  with
  | [] | [_] -> error, None
  | _::_::_ ->
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
	       then (* fresh state, visit the outgoing transitions *)
		 let error, handler, visited = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error state visited in
		 let error, handler, hash = hash_of_mvbdu parameter handler error state in
		 let error, outgoing =
		   Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] hash nodes_adj.outgoing_transitions
		 in
		 let error, ingoing =
		   Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] hash nodes_adj.ingoing_transitions
		 in
		 let g (error,handler,to_be_visited,nodes_adj,support_local_test,support_local_mod,labelset) (label,q') =
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
		 in
		 let error, handler, to_be_visited, nodes_adj, support_local_test, support_local_mod, labelset =
		   List.fold_left
		     g
		     (List.fold_left g
		     (error,handler,tail,nodes_adj,support_local_test,support_local_mod,labelset)
		     outgoing)
		     ingoing
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

let replay parameter error handler handler_kappa agent_type agent_string mvbdu_false mvbdu_true support nodes_adj los_state partition starting_state to_be_visited =
  let error, total_support_test, total_support_mod =
     List.fold_left
       (fun
	 (error, set_test, set_mod)
	 (_,set_test',set_mod') ->
	   let error, test = SiteSet.union parameter error set_test set_test' in
	   let error, mod_ = SiteSet.union parameter error set_mod set_mod' in
	   error, test, mod_)
       (error, SiteSet.empty,SiteSet.empty)
       partition
  in
  let error, handler, support_agent =
    Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error starting_state
  in
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error support_agent in
  let error, set =
    List.fold_left
      (fun (error,set) elt ->
	SiteSet.add parameter error elt set)
      (error, SiteSet.empty)
      list
  in
  let error, extension = SiteSet.minus parameter error set total_support_test in
  let error, partition =
    List.fold_left
      (fun (error, list) (a,set,set') ->
	let error, set = SiteSet.union parameter error set extension in
	let error, set' = SiteSet.union parameter error set' extension in
	(error, (a,set,set')::list))
      (error, [])
      (List.rev partition)
  in
  let error, handler, los_state, mvbdu_list, labelset =
    List.fold_left
      (fun (error, handler, los_state, mvbdu_list, labelset) (set,support_test,support_mod) ->
	let error, labelset = LabelSet.union parameter error labelset set in
	let error, support = SiteSet.union parameter error support_test support_mod in
	let support_list = SiteSet.fold (fun  a l -> a::l) support [] in
	let error, handler, proj_list = Ckappa_sig.Views_bdu.build_variables_list parameter handler error support_list in
	let rec visit handler error working_list mvbdu cluster los_state =
	 match
	   working_list
	 with
	 | [] -> error, handler, mvbdu, cluster, los_state
	 | t::working_list ->
	   let error, handler, bool = Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error t mvbdu in
	    if bool
	    then (* state has already been visited *)
	       visit handler error working_list mvbdu cluster los_state
	    else
	      begin (* fresh site *)
		let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error t mvbdu in
		let error, handler, hash = hash_of_mvbdu parameter handler error t in
		let error, handler, proj_state = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error t proj_list in
		let nodes_adj' = los_state.nodes_adj in
		let error, handler, nodes_adj' = add_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string proj_state nodes_adj' in
		let los_state = {los_state with nodes_adj = nodes_adj'} in
		let error, handler, hash_proj = hash_of_mvbdu parameter handler error proj_state in
		let cluster = hash_proj::cluster in
		let error, outgoing =
		  Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] hash nodes_adj.outgoing_transitions
		in
		let error, ingoing =
		  Wrapped_modules.LoggedIntMap.find_default_without_logs parameter error [] hash nodes_adj.ingoing_transitions
		in
		let error, working_list =
		  List.fold_left
		    (fun (error, working_list) (label, q') ->
		     if LabelSet.mem label set
		     then
		       let error, q' = Wrapped_modules.LoggedIntMap.find_default parameter error mvbdu_false q' nodes_adj.state_to_mvbdu in
		       error, q'::working_list
		     else
		       error, working_list
		    )
		    (error, working_list)
		    ingoing
		in
		let error, handler, working_list, mvbdu, los_state =
		  List.fold_left
		    (fun (error, handler, working_list, mvbdu, los_state) (label, q') ->
		     if LabelSet.mem label set
		     then
		       let error, t' = Wrapped_modules.LoggedIntMap.find_default parameter error mvbdu_false q' nodes_adj.state_to_mvbdu in
		       let error, handler, proj_state' = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error t' proj_list in
		       let error, handler, hash_proj' = hash_of_mvbdu parameter handler error proj_state' in
		       let nodes_adj' = los_state.nodes_adj in
		       let error, nodes_adj' = add_edge parameter error (fst label) (snd label) hash_proj hash_proj' (-1) nodes_adj' in
		       let los_state = {los_state with nodes_adj = nodes_adj' } in
		       error, handler, t'::working_list, mvbdu, los_state
		     else
		       error, handler, working_list, mvbdu, los_state
		    )
		    (error, handler, working_list, mvbdu, los_state)
		    outgoing
		in
		visit handler error working_list mvbdu cluster los_state
	      end
       in
       let error, handler, mvbdu, cluster, los_state = visit handler error [starting_state] mvbdu_false [] los_state in
       let los_state = {los_state with clusters = cluster::los_state.clusters} in
       let error, handler, mvbdu_proj = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error mvbdu proj_list in
       error, handler, los_state, mvbdu_proj::mvbdu_list, labelset)
      (error, handler, los_state, [], LabelSet.empty)
      partition
  in
  let error, handler, macro_state =
     List.fold_left
       (fun (error, handler, macro_state) mvbdu ->
	Ckappa_sig.Views_bdu.mvbdu_and parameter handler error macro_state mvbdu)
       (error, handler, mvbdu_true)
       mvbdu_list
   in
  let macro_edges = los_state.macro_edges in
  let error, handler, macro_edges =
    LabelSet.fold
      (fun label (error, handler, macro_edges) ->
	let error, old = LabelMap.find_default parameter error mvbdu_false label macro_edges in
	let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error old macro_state in
	let error, macro_edges = LabelMap.add_or_overwrite parameter error label mvbdu macro_edges in
	error, handler, macro_edges)
      labelset
      (error, handler, macro_edges)
  in
  let los_state = {los_state with macro_edges = macro_edges} in
   let error, handler, in_macro_states =
     Ckappa_sig.Views_bdu.mvbdu_or parameter handler error macro_state los_state.in_macro_states
   in
   let los_state = { los_state with in_macro_states = in_macro_states } in
   let error, handler, micro_states = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error macro_state in
   let error, handler, los_state, to_be_visited =
     List.fold_left
       (fun (error, handler, los_state, to_be_visited) (asso: (Ckappa_sig.Views_bdu.key * 'a) list)->
	let error, handler, hconsed_asso = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso in
	let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_asso in
	let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_asso in
	let error, outgoing = Wrapped_modules.LoggedIntMap.find_default parameter error [] hash nodes_adj.outgoing_transitions in
	let out_transitions = List.filter (fun (x,_) -> not (LabelSet.mem x labelset)) outgoing in
	let error, ingoing = Wrapped_modules.LoggedIntMap.find_default parameter error [] hash nodes_adj.ingoing_transitions in
	let in_transitions = List.filter (fun (x,_) -> not (LabelSet.mem x labelset)) ingoing in
	let error, creation = Wrapped_modules.LoggedIntMap.find_default parameter error [] hash nodes_adj.creation in
	let creation = List.filter (fun x-> not (LabelSet.mem x labelset)) creation in
	let error, degradation = Wrapped_modules.LoggedIntMap.find_default parameter error [] hash nodes_adj.degradation in
	let degradation = List.filter (fun x-> not (LabelSet.mem x labelset)) degradation in

	if out_transitions = [] && in_transitions = [] && degradation = [] && creation = []
	then
	  error, handler, los_state, to_be_visited
	else
	  let to_be_visited =
	    if out_transitions = [] && degradation = []
	    then
	      to_be_visited
	    else
	      hash::to_be_visited
	  in
	  let error, handler, list =
	    List.fold_left
	      (fun (error, handler, list)
		   (_,support_test,support_mod)
	       ->
	       let error, support = SiteSet.union parameter error support_test support_mod in
	       let support_list = SiteSet.fold (fun  a l -> a::l) support [] in
	       let error, handler, proj_list = Ckappa_sig.Views_bdu.build_variables_list parameter handler error support_list in
	       let error, handler, mac_state = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error mvbdu proj_list in
	       let error, handler, hash_mac_state = hash_of_mvbdu parameter handler error mac_state in
	       error, handler, hash_mac_state::list)
	      (error, handler, [])
	      partition
	  in
	  let error, macro_state_to_state = Wrapped_modules.LoggedIntMap.add parameter error hash list los_state.macro_state_to_state in
	  let los_state = { los_state with macro_state_to_state = macro_state_to_state } in
	  error, handler, los_state, to_be_visited)
       (error, handler, los_state, to_be_visited)
       micro_states
   in
   error, handler, los_state, labelset, to_be_visited

let example ?restricted_version:(restricted_version=false) list nodes_adj =
  let f int_list =
    List.rev_map Ckappa_sig.site_name_of_int (List.rev int_list)
  in
  let label i = ((Rule (Ckappa_sig.rule_id_of_int i)),Ckappa_sig.agent_id_of_int i) in
  let parameter =
        Remanent_parameters.get_parameters ~called_from:Remanent_parameters_sig.KaSa ()
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
	 let () =
	   List.iter
	     (fun x ->
	       match x with
	       | ((Rule s,_),_,_) -> Printf.fprintf stdout "%i" (Ckappa_sig.int_of_rule_id s)
	       | _ -> ()) p in
	 let () = Printf.fprintf stdout " SYNC: " in
	 let () = List.iter (fun x ->
	   match x with
	   | ((Rule s,_),_,_) -> Printf.fprintf stdout "%i" (Ckappa_sig.int_of_rule_id s)
	   | _ -> ()) n_p
	 in
	 let () = Printf.fprintf stdout "\n" in
	 ()
       end
  in
  ()

let create parameter handler error r_id ag_id agent_type ext_list update nodes_adj =
  let error, list =
    error, List.fold_left
	(fun list i -> (i,Ckappa_sig.state_index_of_int 0)::list)
	[]
	ext_list
  in
  let error, handler, mvbdu =
    Ckappa_sig.Views_bdu.mvbdu_of_reverse_sorted_association_list parameter handler error list
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


let agent_trace parameter error handler handler_kappa mvbdu_true compil output =
  let rules = compil.Cckappa_sig.rules in
  let init = compil.Cckappa_sig.init in
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
    (fun parameter error agent_type map handler ->
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
	 let nodes_adj = empty_nodes_adj mvbdu_false in
	 let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	 let fic = Remanent_parameters.open_out file_name in
	 let () = dump_graph_header fic in
	 let error, handler, nodes_adj =
	   List.fold_left
	     (fun (error, handler,nodes_adj) list ->
	      add_node_from_asso parameter handler handler_kappa error agent_type agent_string list nodes_adj)
	     (error, handler,nodes_adj)
	     list
	 in
	 let error, (handler, nodes_adj) =
	   Int_storage.Nearly_inf_Imperatif.fold
	     parameter
	     error
	     (fun parameter error i_id init (handler,nodes_adj) ->
	       let mixture = init.Cckappa_sig.e_init_c_mixture in
	       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
		 parameter
		 error
		 (fun parameter error ag_id view (handler,nodes_adj) ->
		    match
		      view
		    with
		    | Cckappa_sig.Agent agent ->
			if agent.Cckappa_sig.agent_name <> agent_type
			then
			  error, (handler, nodes_adj)
			else
			  begin
			     let error, handler, list =
			       List.fold_left
				 (fun
				   (error, handler, list)
				   site ->
				     match
				       Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
					 parameter error
					 site agent.Cckappa_sig.agent_interface
				     with error, None ->
				       error, handler, list
				     | error, Some state ->
				       let interv = state.Cckappa_sig.site_state in
				       let min = interv.Cckappa_sig.min in
				       if min = interv.Cckappa_sig.max
				       then
					 error,
					 handler,
					 (site,min)::list
				       else
					 let error, () = warn parameter error (Some "line 933") Exit () in
					 error, handler, list
				 )
				 (error, handler, [])
				 ext_list
			     in
			     let error, handler, update =
			       Ckappa_sig.Views_bdu.build_association_list
				 parameter handler error list
			     in
			     create parameter handler error (Init i_id) ag_id agent_type ext_list update nodes_adj
			  end
		    | Cckappa_sig.Ghost
		    | Cckappa_sig.Dead_agent _
		    | Cckappa_sig.Unknown_agent _ ->
		      warn parameter error (Some "line 948") Exit (handler,nodes_adj)
		 )
		 mixture.Cckappa_sig.views
		 (handler, nodes_adj)
	     )
	     init
	     (handler, nodes_adj)
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
	 let error, handler, bdu_diag =
	   Ckappa_sig.Views_bdu.mvbdu_of_association_list
             parameter
             handler
             error
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
		  string_of_fst_label parameter error handler_kappa compil (Rule r_id)
		else error,""
	      in
	      let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
	      let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
	      let error, (handler, nodes_adj) =
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
				   else
				     let error, () = warn parameter error (Some "line 933") Exit () in
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
			    let error, (handler, nodes_adj) =
			      create parameter handler error (Rule r_id) ag_id agent_type ext_list update nodes_adj
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
						 let error, () = warn parameter error (Some "line 1012") Exit () in
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
					add_edge parameter error (Rule r_id) ag_id hash_init hash_init' hash_upd nodes_adj
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
	   if Remanent_parameters.get_use_macrotransitions_in_local_traces parameter
	   then
	     begin (* losange reduction *)
	       let empty_state =
		 {
		   macro_edges = LabelMap.empty ;
		   clusters = [] ;
		   in_macro_states = mvbdu_false ;
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
	       let rec aux handler error list set los_state =
		 match
		   list
		 with
		 | [] -> error, handler, los_state
		 | h::t ->
		    begin
		      (* is h already visited ? *)
		      let error, mvbdu =
			Wrapped_modules.LoggedIntMap.find_default
			  parameter
			  error
			  mvbdu_false
			  h
			  nodes_adj.state_to_mvbdu
		      in
		      let error, handler, bool  =
			Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error mvbdu los_state.already_visited
		      in
		      if bool
		      then (* if yes, ignore h ? *)
			aux handler error t set los_state
		      else
			let error, handler, state' = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error los_state.already_visited mvbdu in
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
			let error, handler, output =
			   (* is h already involved in a macro-state ? *)
			  let error, handler, bool =
			    Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error mvbdu los_state.in_macro_states in
			  if bool
			  then (* if so, do not apply POR reduction on it ? *)
			    error, handler, None
			  else
			    let error, output = smash parameter error support list_with_support in
			    error, handler, output
			in
			let error, handler, los_state, dealt_label, output, t  =
			  match
			    output
			  with
			  | None ->
			     error, handler, los_state, LabelSet.empty, [], t
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
			     let error, handler, los_state, dealt_labels, t =
			       replay parameter error handler handler_kappa agent_type
				      agent_string mvbdu_false mvbdu_true support_total_test
				      nodes_adj los_state output mvbdu t
			     in
			     error, handler, los_state, dealt_labels, output, t
			in
			(* deal with the successors of h *)
			let error, handler, los_state, to_be_visited =
			  List.fold_left
			    (fun (error, handler, los_state, to_be_visited) (label,q') ->
			     (* detect wether a state is already dealt with by another macro-state *)
			     if LabelSet.mem label dealt_label
			     then
				error, handler, los_state, to_be_visited
			      else
			       begin
				 let error, handler, bool = is_macro_edge parameter handler error mvbdu_false label mvbdu los_state  in
				 if bool
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
	         (* clusters *)
	       let error, handler, blacklist, _ =
		 List.fold_left
		   (fun (error, handler, blacklist, n) list ->
		     let () = Printf.fprintf fic "subgraph cluster_%i {\n" n in
		     let error, handler, blacklist =
		       List.fold_left
			 (fun
			   (error, handler, blacklist)
			   k ->
			     let error, list = Wrapped_modules.LoggedIntMap.find_default parameter error [] k nodes_adj.nodes_to_asso_list in
			     let error, blacklist = Wrapped_modules.LoggedIntSet.add parameter error k blacklist in
			     let error, handler = dump_key_of_asso fic parameter handler error list in
			     let () = Printf.fprintf fic " [label=\"" in
			     let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
			     let () = Printf.fprintf fic "\"];\n" in
			     error, handler, blacklist)
			 (error, handler, blacklist)
			 list
		     in
		     let () = Printf.fprintf fic "}\n" in
		     (error, handler, blacklist, n+1))
		   (error, handler, Wrapped_modules.LoggedIntSet.empty, 0)
		   state.clusters
	       in
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
		     if Wrapped_modules.LoggedIntSet.mem key blacklist
		     then
		       error, handler
		     else
  let error, handler = dump_key_of_asso fic parameter handler error list in
		     let () = Printf.fprintf fic " [label=\"" in
		     let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
		     let () = Printf.fprintf fic "\"];\n" in
		     error, handler)
		   nodes_adj.nodes_to_asso_list
		   (error, handler)
		      in
		      (* macro_steps *)
		      let error =
			Wrapped_modules.LoggedIntMap.fold
			  (fun k _ error ->
			   let () = Printf.fprintf fic "Macro_%i [width=\"0cm\" height=\"0cm\" stype=\"none\" label=\"\"];\n" k
			   in error)
			  state.macro_state_to_state
			  error
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
				   string_of_fst_label parameter error handler_kappa compil (fst r_id)
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
				    string_of_fst_label parameter error handler_kappa compil (fst r_id)
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
			  (fun key l ->
			   if l = []
			   then
			     ()
			   else
			     let () = Printf.fprintf fic "Macro_%i -> Node_%i [style=\"dotted\" label=\"\"];\n" key key in
			     List.iter
			       (fun h ->
				Printf.fprintf fic "Macro_%i -> Node_%i [style=\"dashed\" label=\"\"];\n" key h )
			       l
			  )
			  state.macro_state_to_state
		      in
		      error
	     end (* losange reduction *)
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
				    string_of_fst_label parameter error handler_kappa compil (fst r_id)
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
				    string_of_fst_label parameter error handler_kappa compil (fst r_id)
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
