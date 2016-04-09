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

type intensional_set_of_transitions =
  {
    nsites: int;
    transitions: Ckappa_sig.Views_bdu.mvbdu;
    creation: Ckappa_sig.Views_bdu.mvbdu;
    degradation: Ckappa_sig.Views_bdu.mvbdu;
  }
    
type extensional_representation =
  {
    nodes: Ckappa_sig.Views_bdu.mvbdu list;
    state_to_mvbdu: Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t ;
    edges: (Ckappa_sig.Views_bdu.mvbdu * label * Ckappa_sig.Views_bdu.mvbdu) list ;
    nodes_creation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    nodes_degradation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    nodes_to_asso_list: (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list Wrapped_modules.LoggedIntMap.t ;
    nodes_name: (Exception.method_handler -> out_channel -> Exception.method_handler)  Wrapped_modules.LoggedIntMap.t ;
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
    already_visited: Ckappa_sig.Views_bdu.mvbdu;
    in_macro_states: Ckappa_sig.Views_bdu.mvbdu;
  }

let empty_transition n mvbdu_false =
  {nsites = n ;
   transitions = mvbdu_false;
   creation = mvbdu_false;
   degradation = mvbdu_false}
    
let empty_transition_system n mvbdu_false =
  {
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [] ;
    nodes_name = Wrapped_modules.LoggedIntMap.empty;
    nodes_to_asso_list = Wrapped_modules.LoggedIntMap.empty;
    state_to_mvbdu = Wrapped_modules.LoggedIntMap.empty;
    already_visited = mvbdu_false;
    in_macro_states = mvbdu_false;
    macro_state_to_state = Wrapped_modules.LoggedIntMap.empty
  }
    
let add_edge r_id ag_id q q' _ transition_system =
  {transition_system with edges = (q,(r_id,ag_id),q')::transition_system.edges}

let add_creation parameter handler error r_id ag_id mvbdu intensional =
 let n = intensional.nsites in
  let state_r_id = Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_rule_id r_id) in
  let state_ag_id = Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id ag_id)
  in
  let site_r_id = Ckappa_sig.site_name_of_int (2*n+1) in
  let site_ag_id = Ckappa_sig.site_name_of_int (2*n+2) in
  let error, handler, asso = Ckappa_sig.Views_bdu.build_association_list parameter handler error [site_r_id,state_r_id;
												  site_ag_id,state_ag_id] in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu asso in
  let error, handler, creation = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu intensional.creation in
  error, handler, 
  {intensional with creation = creation}
    
let shift_site op site internal =
  Ckappa_sig.site_name_of_int (op (Ckappa_sig.int_of_site_name site) internal.nsites)
let shift_site_minus site internal = shift_site (-) site internal
let shift_site_plus site internal = shift_site (+) site internal
							  
let convert_label (r,a) =
  (Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_rule_id r),Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id a))

let hash_of_association_list parameter handler error list =
  let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error list in
  let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_list in
  error, handler, hash
    
let mvbdu_of_association_list_gen gen parameter handler error asso_list =
  let error, handler, hconsed_list = gen parameter handler error asso_list in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
  Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_list

let mvbdu_of_association_list parameter handler error asso = mvbdu_of_association_list_gen Ckappa_sig.Views_bdu.build_association_list parameter handler error asso
let mvbdu_of_reverse_order_association_list parameter handler error asso = mvbdu_of_association_list_gen Ckappa_sig.Views_bdu.build_reverse_sorted_association_list parameter handler error asso

let transitions_starting_from parameter handler error asso internal = 
  mvbdu_of_association_list parameter handler error asso
			    
let transitions_ending_in parameter handler error asso internal =
  let asso = List.rev_map (fun (site,state) -> shift_site_plus site internal,state) (List.rev asso) in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
  List.fold_left
    (fun (error, handler, mvbdu) (site,state) ->
     let error, handler, case1 = mvbdu_of_association_list parameter handler error [site,state] in
     let error, handler, case21 = mvbdu_of_association_list parameter handler error [site,Ckappa_sig.state_index_of_int (-1)] in
     let error, handler, case22 = mvbdu_of_association_list parameter handler error [shift_site_minus site internal,state] in
     let error, handler, case2 = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error case21 case22 in
     let error, handler, mvbdu' = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error case1 case2 in
       Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu mvbdu')
    (error, handler, mvbdu_true)
    asso
  
let transitions_via parameter handler error labellist internal =
  let n = internal.nsites in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  let site_rid = Ckappa_sig.site_name_of_int (2*n+1) in
  let site_agid = Ckappa_sig.site_name_of_int (2*n+2) in
  List.fold_left
    (fun (error, handler, mvbdu) label ->
     let r_id, ag_id = convert_label label in
     let error, handler, mvbdu' = mvbdu_of_reverse_order_association_list parameter handler error [site_agid,ag_id;site_rid,r_id] in
     Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu mvbdu')
      (error, handler, mvbdu_false)
      labellist

let restrict_gen f parameter handler error x internal =
  let error, handler, mvbdu = f parameter handler x internal in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu internal.transitions in
  {internal with transitions = mvbdu}

let label_set_of_transitions parameter handler error internal =
    let n = internal.nsites in
    let site_rid  = Ckappa_sig.site_name_of_int (2*n+1) in
    let site_agid = Ckappa_sig.site_name_of_int (2*n+2) in
    let error, handler, varlist = Ckappa_sig.Views_bdu.build_reverse_sorted_variables_list parameter handler error [site_agid;site_rid] in
    let error, handler, mvbdu_proj = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error internal.transitions varlist in
    let error, handler, l = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu_proj in
    let error, set =
      List.fold_left
	(fun (error,set) list ->
	 match list with
	 | [x,x';y,y'] when x=site_rid && y=site_agid ->
	    LabelSet.add parameter error
			 (Ckappa_sig.rule_id_of_int (Ckappa_sig.int_of_state_index x'),
			  Ckappa_sig.agent_id_of_int (Ckappa_sig.int_of_state_index y'))
			 set
	 | _ -> warn parameter error (Some "line 124") Exit set)
	(error,LabelSet.empty)
	l
    in error, handler, set

let translate_gen f parameter error handler site_rid site_agid list internal output = 
  match
    List.rev list 
  with
  | (x,x')::(y,y')::l when x=site_agid && y = site_rid ->
     let label = Ckappa_sig.rule_id_of_int (Ckappa_sig.int_of_state_index y'),
		 Ckappa_sig.agent_id_of_int (Ckappa_sig.int_of_state_index x') in
     let asso = f l in
     let error, handler, mvbdu = mvbdu_of_association_list parameter handler error asso in
     error, (handler,(label,mvbdu)::output)
  | _ -> warn parameter error (Some "line 146") Exit (handler,output)			 

let get_label parameter error handler site_rid site_agid list internal output =
  match
    List.rev list 
  with
  | (x,x')::(y,y')::_ when x=site_agid && y = site_rid ->
     let label = Ckappa_sig.rule_id_of_int (Ckappa_sig.int_of_state_index y'),
		 Ckappa_sig.agent_id_of_int (Ckappa_sig.int_of_state_index x') in
      error, (handler,label::output)
  | _ -> warn parameter error (Some "line 146") Exit (handler,output)
	      
let translate_back parameter error handler site_rid site_agid list internal output =
  translate_gen (fun l -> List.rev_map (fun (x,y) -> (shift_site_minus x internal,y)) (List.rev l))
		parameter error handler site_rid site_agid list internal output
    
let translate_direct parameter error handler site_rid site_agid list internal output =
  translate_gen (fun l -> l) parameter error handler site_rid site_agid list internal output
		
let outgoing parameter handler error mvbdu internal =
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu internal.transitions in
  let n = internal.nsites in
  let site_rid  = Ckappa_sig.site_name_of_int (2*n+1) in
  let site_agid = Ckappa_sig.site_name_of_int (2*n+2) in
  let int_list = Misc_sa.list_0_n (n-1) in
  let site_list = List.map Ckappa_sig.site_name_of_int (List.rev int_list) in
  let error, handler, varlist = Ckappa_sig.Views_bdu.build_reverse_sorted_variables_list parameter handler error site_list in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu varlist in
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  let error, (handler, output) =
    List.fold_left
      (fun (error,(handler,output)) list -> 
	     translate_back parameter error handler site_rid site_agid list internal output)
      (error,(handler,[])) list
  in
  error, handler, output

let half_trans parameter handler error mvbdu trans_set n =
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu trans_set in
  let site_rid  = Ckappa_sig.site_name_of_int (n+1) in
  let site_agid = Ckappa_sig.site_name_of_int (n+2) in
  let int_list = Misc_sa.list_0_n (n-1) in
  let site_list = List.map Ckappa_sig.site_name_of_int (List.rev int_list) in
  let error, handler, varlist = Ckappa_sig.Views_bdu.build_reverse_sorted_variables_list parameter handler error site_list in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu varlist in
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  let error, (handler, output) =
    List.fold_left
      (fun (error,(handler,output)) list -> 
	     get_label parameter error handler site_rid site_agid list trans_set output)
      (error,(handler,[])) list
  in
  error, handler, output

let creation parameter handler error mvbdu internal =
  half_trans parameter handler error mvbdu internal.creation internal.nsites
let degradation parameter handler error mvbdu internal =
  half_trans parameter handler error mvbdu internal.degradation internal.nsites
	     
let ingoing parameter handler error mvbdu internal =
  let n = internal.nsites in
  let site_rid  = Ckappa_sig.site_name_of_int (2*n+1) in
  let site_agid = Ckappa_sig.site_name_of_int (2*n+2) in
  let int_list = Misc_sa.list_0_n (n-1) in
  let rename_list =
    List.rev_map
      (fun i -> (Ckappa_sig.site_name_of_int i,
		 Ckappa_sig.site_name_of_int (i+n)))
      int_list
  in
  let error, handler, hconsed_rename_list = Ckappa_sig.Views_bdu.build_renaming_list parameter handler error rename_list in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_rename parameter handler error mvbdu hconsed_rename_list in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu internal.transitions in
  let site_list = List.rev_map snd rename_list in
  let error, handler, varlist = Ckappa_sig.Views_bdu.build_reverse_sorted_variables_list parameter handler error site_list in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu varlist in
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  let error, (handler, output) =
    List.fold_left
      (fun (error,(handler,output)) list -> 
	     translate_direct parameter error handler site_rid site_agid list internal output)
      (error,(handler,[])) list
  in
  error, handler, output

		    


let label_to_state_pair label =
  (Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_rule_id (fst label)),
   Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id (snd label)))

	   
(*let add_concrete_node parameter handler handler_kappa error agent_type agent_string mvbdu list nodes_adj =
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
    }*)
      
let add_concrete_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu transition_system =
  error, handler, {transition_system with nodes = mvbdu::transition_system.nodes}
 
  (*
let add_concrete_node_from_asso parameter handler handler_kappa error agent_type agent_string asso_list nodes_adj =  
   let error, handler, hconsed_list = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso_list in
   let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
   let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_list in
   add_concrete_node parameter handler handler_kappa error agent_type agent_string mvbdu asso_list nodes_adj 
 *)  	   
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

let build_asso_of_mvbdu parameter handler error mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu
  in
  match list
  with
  | [list] ->
     begin
       error, handler, list
     end
  | _ ->
     let error, (handler,list) = 
       warn parameter error (Some "line 385") Exit (handler,[])
     in
     error, handler, list
		     
let hash_of_mvbdu parameter handler error mvbdu =
  let error, handler, asso = build_asso_of_mvbdu parameter handler error mvbdu in
  hash_of_association_list parameter handler error asso
  		    
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
	| None -> warn parameter error (Some "line 483") Exit l
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

let extend_partition parameter error handler mvbdu_false support internal initial_partition starting_state =
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
	let rec aux parameter error handler internal visited support_total_test support_total_mod support_local_test support_local_mod labelset to_be_visited              =
	  match to_be_visited with
	  | [] -> error, handler, support_local_test, support_local_mod, labelset
	  | state::tail -> 
	     begin
	       let error, handler, meet = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error state visited in
	       if not (Ckappa_sig.Views_bdu.equal meet state)
	       then (* fresh state, visite the outgoing transitions *)
		 let error, handler, visited = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error state visited in
		 let error, handler, outgoing =
		   outgoing parameter handler error state internal
		 in
		 let error, handler, ingoing =
		   ingoing parameter handler error state internal
		 in
		 let g (error,handler,to_be_visited,internal,support_local_test,support_local_mod,labelset) (label,mvbdu) =
		   let error,(set_test,set_mod) = LabelMap.find_default  parameter error (SiteSet.empty,SiteSet.empty) label support in
		    let error, diff_test = SiteSet.minus parameter error set_test support_local_test in
		    let error, meet_test = SiteSet.inter parameter error diff_test support_total_mod in
		    let error, diff_mod = SiteSet.minus parameter error set_mod support_local_mod in
		    let error, meet_mod = SiteSet.inter parameter error diff_mod support_total_test in
		    if SiteSet.is_empty meet_test && SiteSet.is_empty meet_mod
		    then
		      let to_be_visited = mvbdu::to_be_visited in
		      let error, support_local_test = SiteSet.union parameter error set_test support_local_test in
		      let error, support_local_mod = SiteSet.union parameter error set_mod support_local_mod in
		      let error, labelset = LabelSet.add parameter error label labelset in
			error, handler, to_be_visited, internal, support_local_test, support_local_mod, labelset
		    else
		      error, handler, to_be_visited, internal, support_local_test, support_local_mod, labelset
		 in
		 let error, handler, to_be_visited, internal, support_local_test, support_local_mod, labelset =
		   List.fold_left
		     g
		     (List.fold_left g
		     (error,handler,tail,internal,support_local_test,support_local_mod,labelset)
		     outgoing)
		     ingoing
		 in
		 aux parameter error handler internal visited support_total_test support_total_mod support_local_test support_local_mod labelset to_be_visited
	       else
		 aux parameter error handler internal visited support_total_test support_total_mod support_local_test support_local_mod labelset tail
	    end
	in
	let error, handler, support_local_test, support_local_mod, labelset = 
	  aux parameter error handler internal mvbdu_false total_support_test total_support_mod set_test set_mod (LabelSet.singleton label) [starting_state] in
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

let replay parameter error handler handler_kappa agent_type agent_string mvbdu_false mvbdu_true support internal transition_system partition starting_state to_be_visited =
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
  let error, handler, transisition_system, mvbdu_list, labelset =
    List.fold_left
      (fun (error, handler, transition_system, mvbdu_list,labelset) (set,support_test,support_mod) ->
       let error, labelset = LabelSet.union parameter error labelset set in
       let error, support = SiteSet.union parameter error support_test support_mod in
       let support_list = SiteSet.fold (fun  a l -> a::l) support [] in
       let error, handler, proj_list = Ckappa_sig.Views_bdu.build_variables_list parameter handler error support_list in
       let rec visit handler error working_list mvbdu transition_system =
	 match
	   working_list
	 with
	 | [] -> error, handler, mvbdu, transition_system
	 | t::working_list ->
	    let error, handler, union = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error t mvbdu in
	    if Ckappa_sig.Views_bdu.equal mvbdu union
	    then (* state has already been visited *)
	       visit handler error working_list mvbdu transition_system
	    else
	      begin (* fresh site *)
		let mvbdu = union in
		let error, handler, hash = hash_of_mvbdu parameter handler error t in
		let error, handler, proj_state = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error t proj_list in
		let error, handler, transition_system  = add_concrete_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string proj_state transition_system in
		(*	let error, handler, hash_proj = hash_of_mvbdu parameter handler error proj_state in*)
		let error, handler, outgoing = (* to do filter transitions before_hand *)
		  outgoing parameter handler error proj_state internal 
		in
		let error, handler, ingoing =
		  ingoing parameter handler error proj_state internal
			  	in
		let error, working_list =
		  List.fold_left
		    (fun (error, working_list) (label, q') ->
		     if LabelSet.mem label set (* to do make this test useless *)
		     then
		       error, q'::working_list
		     else
		       error, working_list
		    )
		    (error, working_list)
		    ingoing
		in
		let error, handler, working_list, mvbdu, transition_system =
		  List.fold_left
		    (fun (error, handler, working_list, mvbdu, transisiton_system) (label, t') ->
		     if LabelSet.mem label set
		     then
		       let error, handler, proj_state' = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error t' proj_list in
		       (*   let error, handler, hash_proj' = hash_of_mvbdu parameter handler error proj_state' in*)
		       let transition_system = add_edge (fst label) (snd label) proj_state proj_state' (-1) transition_system in
		       error, handler, t'::working_list, mvbdu, transition_system
		     else
		       error, handler, working_list, mvbdu, transition_system)
		    (error, handler, working_list, mvbdu, transition_system)
		    outgoing
		in
		visit handler error working_list mvbdu transition_system
	      end
       in
       let error, handler, mvbdu, transition_system = visit handler error [starting_state] mvbdu_false transition_system in
       let error, handler, mvbdu_proj = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error mvbdu proj_list in
       error, handler, transition_system, mvbdu_proj::mvbdu_list, labelset)
      (error, handler, transition_system, [], LabelSet.empty)
      partition
   in
   let error, handler, macro_state =
     List.fold_left
       (fun (error, handler, macro_state) mvbdu ->
	Ckappa_sig.Views_bdu.mvbdu_and parameter handler error macro_state mvbdu)
       (error, handler, mvbdu_true)
       mvbdu_list
   in
   let error, handler, in_macro_states =
     Ckappa_sig.Views_bdu.mvbdu_or parameter handler error macro_state transition_system.in_macro_states
   in
   let transition_system  = { transition_system with in_macro_states = in_macro_states } in
   let error, handler, micro_states = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error macro_state in
   let error, handler, transition_system, to_be_visited =
     List.fold_left 
       (fun (error, handler, transition_system, to_be_visited) (asso: (Ckappa_sig.Views_bdu.key * 'a) list)->
	let error, handler, hconsed_asso = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso in
	let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error mvbdu_true hconsed_asso in
	let hash = Ckappa_sig.Views_bdu.hash_of_association_list hconsed_asso in
	let error, handler, outgoing = (* to do filter transitions before_hand *)
		  outgoing parameter handler error mvbdu internal 
	in
	let out_transitions = List.filter (fun (x,_) -> not (LabelSet.mem x labelset)) outgoing in
	let error, handler, ingoing = (* to do filter transitions before_hand *)
	  ingoing parameter handler error mvbdu internal 
	in
	let in_transitions = List.filter (fun (x,_) -> not (LabelSet.mem x labelset)) ingoing in
	let error, handler, creation =
	  creation parameter handler error mvbdu internal
	in
	let creation = List.filter (fun x-> not (LabelSet.mem x labelset)) creation in
	let error, handler, degradation =
	  degradation parameter handler error mvbdu internal
	in
	let degradation = List.filter (fun x-> not (LabelSet.mem x labelset)) degradation in
	if out_transitions = [] && in_transitions = [] && degradation = [] && creation = []
	then
	  error, handler, transition_system, to_be_visited
	else
	  let to_be_visited =
	    if out_transitions = [] && degradation = []
	    then
	      to_be_visited
	    else
	      mvbdu::to_be_visited
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
	  let error, macro_state_to_state = Wrapped_modules.LoggedIntMap.add parameter error hash list transition_system.macro_state_to_state in
	  let transition_system = { transition_system with macro_state_to_state = macro_state_to_state } in
	  error, handler, transition_system, to_be_visited)
       (error, handler, transition_system, to_be_visited)
       micro_states
   in
   error, handler, transition_system, labelset, to_be_visited
		     
let example ?restricted_version:(restricted_version=false) list nodes_adj =
  let f int_list =
    List.rev_map Ckappa_sig.site_name_of_int (List.rev int_list)
  in 
  let label i = (Ckappa_sig.rule_id_of_int i,Ckappa_sig.agent_id_of_int i) in
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
	 let error, handler, ext_list =  Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error sites in
	 let max_site = List.fold_left (fun n i -> max n (Ckappa_sig.int_of_site_name i)) 0 ext_list in
	 let max_site = max_site +1 in
	 let site_rid = Ckappa_sig.site_name_of_int (2*max_site +1) in
	 let site_agid = Ckappa_sig.site_name_of_int (2*max_site +2) in
	 let error, file_name =
	   List.fold_left
	     (fun (error, string) site ->
	      let error, site_string = Handler.string_of_site parameter error handler_kappa agent_type site in
	      error, string^"_"^site_string)
	     (error, ((Remanent_parameters.get_local_trace_directory parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
             ext_list
	 in
	 let transition_system = empty_transition_system max_site mvbdu_false in
	 let intensional = empty_transition max_site mvbdu_false in
	 let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	 let fic = Remanent_parameters.open_out file_name in
	 let () = dump_graph_header fic in
	 let error, (handler, intensional) =
	   Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
	     parameter
	     error
	     (fun parameter error r_id rule (handler, intensional) ->
	     let error, rule_name =
		if Remanent_parameters.get_show_rule_names_in_local_traces parameter
		then
		  Handler.string_of_rule parameter error handler_kappa compil r_id
		else error,""
	      in
	      let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
	      let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
	      let error, (handler, intensional) =
		Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
		  parameter
		  error
		  (fun parameter error ag_id diff (handler, intensional) ->
		   begin
		     if diff.Cckappa_sig.agent_name <> agent_type then
		       error, (handler, intensional)
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
			    let error, handler, intensional =
			      add_creation parameter handler error r_id ag_id mvbdu intensional
			    in
			    error,(handler,intensional)
			  end
		       | Some test ->
			  begin
			    match
			      modif_list
			    with
			    | [] ->
			       error,(handler,intensional)
			    | _ ->
			       begin
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
				   List.fold_left
				     (fun (error, handler, mvbdu) site ->
				      let site' = shift_site_plus site intensional in
				       match
					 Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
					   parameter error
					   site diff.Cckappa_sig.agent_interface
				       with error, None ->
					 (* copy the state of site in site' *)
					 begin
					   let error, handler, var_interest = Ckappa_sig.Views_bdu.build_variables_list parameter handler error [site] in
					   let error, handler, mvbdu_case = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error mvbdu var_interest in
					   let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu_case in
					   let error, handler, mvbdu' = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
					   (* non monotonic renaming is painful, it should be a primitive of the mvbdu domain *)
					     List.fold_left
					       (fun (error, handler, mvbdu') list ->
						match
						  list
						with
						  [site'',state''] when site'' = site ->
						  let error, handler, conj = mvbdu_of_association_list parameter handler error [site,state'';site',state''] in
						  let error, handler, mvbdu'' = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu conj in
						  Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu' mvbdu''
						| _ ->
						let error, mvbdu' = warn parameter error (Some "line 253") Exit mvbdu' in
						error, handler, mvbdu')
					       (error, handler, mvbdu') list
				     	 end		      
					  | error, Some state ->
					     (* define the state of site' as state *)
					     begin
					       let interv = state.Cckappa_sig.site_state in
					       let min = interv.Cckappa_sig.min in
					       let error,() =
						 if min = interv.Cckappa_sig.max
						 then
						   error,()
						 else
					       	   warn parameter error (Some "line 1093") Exit ()
					       in
					       let error, handler, conj = mvbdu_of_association_list parameter handler error [site',min] in
					       Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu conj
					     end
				     )
				     (error, handler, mvbdu)
				     ext_list
				 in
				 let n = intensional.nsites in
				 let r_id',ag_id' = convert_label (r_id,ag_id) in
				 let label_update = [Ckappa_sig.site_name_of_int (2*n+1),r_id';Ckappa_sig.site_name_of_int (2*n+2),ag_id'] in
				 let error, handler, update =
				   mvbdu_of_association_list parameter handler error label_update
				 in
				 let error, handler, mvbdu =
				   Ckappa_sig.Views_bdu.mvbdu_and parameter handler error update mvbdu
				 in
				 let error, handler, mvbdu =
				   Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu intensional.transitions
				 in
				 let intensional = {intensional with transitions = mvbdu} in
				 error,(handler,intensional)
			       end
			  end
		   end
		  )
		  diff
		  (handler, intensional)
	      in
	      error, (handler, intensional)
	     )
	     rules
	     (handler, intensional)
	 in
	 let _ = Printf.fprintf stdout "LOSANGE REDUCTION\n%!" in
	 let error =
	   begin (* losange reduction *)
	     let error, mvbdu = error, intensional.creation in
	     let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
	     let error, handler, list,transition_system =
	       List.fold_left
		 (fun  (error, handler, list,transition_system) mvbdu ->
		  let error, (handler, list') = translate_direct parameter error handler site_rid site_agid mvbdu transition_system [] in
		  let list, transition_system =
		    List.fold_left
		      (fun (list, transition_system)
			   (label,q) ->
		       let transition_system = {transition_system with nodes_creation = (q,label)::transition_system.nodes_creation} in
		       let list = q::list in
		       list,transition_system)
		      (list, transition_system)
		      list'
		  in
		  (error,handler,list,transition_system))
		 (error,handler,[],transition_system)
		 list
	     in
	     let rec aux handler error list transition_system =
	       match
		 list
		 with
		 | [] -> error, handler, transition_system
		 | mvbdu ::t ->
		    begin
		      (* is h already visited ? *)
		     let error, handler, state' =
			Ckappa_sig.Views_bdu.mvbdu_or parameter handler error transition_system.already_visited mvbdu
		      in
		      if Ckappa_sig.Views_bdu.equal transition_system.already_visited state'
		      then (* if yes, ignore h ? *)
			aux handler error t (*set*) transition_system
		      else
			let transition_system = {transition_system with already_visited = state'} in
			let error, handler, transition_system = add_concrete_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu transition_system in
			let error, handler, outgoing =
			  outgoing 
			    parameter
			    handler
			    error
			    mvbdu
			    intensional
			in
			let list_with_support =
			  List.rev_map fst (List.rev outgoing)
			in
			let error, handler, output =
			  (* is h already involved in a macro-state ? *)
			  let error, handler, macro_state' =
			    Ckappa_sig.Views_bdu.mvbdu_or parameter handler error transition_system.in_macro_states mvbdu
			  in
			  if Ckappa_sig.Views_bdu.equal transition_system.in_macro_states macro_state'
			  then (* if so, do not apply POR reduction on it ? *)
			    error, handler, None
			  else
			    let error, output = smash parameter error support list_with_support in
			    error, handler, output
			in
			let error, transition_system, dealt_label, output, t  =
			  match
			    output
			  with
			  | None ->
			     error, transition_system, LabelSet.empty, [], t
			  | Some output ->
			     let error, handler, support_total_test, support_total_mod, output =
			       extend_partition
				 parameter
				 error
				 handler
				 mvbdu_false
				 support
				 intensional
				 (fst output)
				 mvbdu
			     in
			     let error, handler, transition_system, dealt_labels, t =
			       replay parameter error handler handler_kappa agent_type
				      agent_string mvbdu_false mvbdu_true support_total_test
				      intensional
				      transition_system
				      output
				      mvbdu
				      t
			     in
			     error, transition_system, dealt_labels, output, t
			in
			(* deal with the successors of h *)
			let error, handler, transition_system, to_be_visited =
			  List.fold_left 
			    (fun (error, handler, transition_system, to_be_visited) (label,q') -> 
			     (* to do, detect wether a state is already dealt with by another macro-state *)
			     if LabelSet.mem label dealt_label
			     then 
			       error, handler, transition_system, to_be_visited
			     else
			       begin
				 (*  let error, mvbdu_macro = LabelMap.find_default parameter error mvbdu_false label los_state.partially_visited in
				  let error, handler, mvbdu' =  Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu mvbdu_macro in
				  if Ckappa_sig.Views_bdu.equal mvbdu' mvbdu
				  then
				    error, handler, transition_system, to_be_visited
				  else*)
				 let transition_system = add_edge (fst label) (snd label) mvbdu q' 
								 0 (* to do *) transition_system in
				 let to_be_visited = q'::to_be_visited in
				 error, handler, transition_system, to_be_visited 
			       end)
			    (error, handler, transition_system, t)
			    outgoing
			in
			aux handler error to_be_visited (*set*) transition_system
		    end
	     in
	     let error, handler, transition_system = aux handler error list (*set*) transition_system in
	     (* nodes -> Initial *)
	     let error,handler =
	       List.fold_left
		 (fun (error, handler) (mvbdu,label) ->
		  let error, handler, key = hash_of_mvbdu parameter handler error mvbdu in
		  let () = Printf.fprintf fic "Init_%i [width=\"0cm\" height=\"0cm\" style=\"none\" label=\"\"];\n" key in
		  error, handler)
		 (error, handler)
		 transition_system.nodes_creation
	     in
	     (* nodes -> regular *)
	     let error, handler =
	       List.fold_left
		 (fun (error, handler) mvbdu ->
		  let error, handler, list = build_asso_of_mvbdu parameter handler error mvbdu in
		  let error, handler = dump_key_of_asso fic parameter handler error list in
		  let () = Printf.fprintf fic " [label=\"" in
		  let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
		  let () = Printf.fprintf fic "\"];\n" in
		  error, handler)
		 (error, handler)
		 transition_system.nodes
	     in
	   (*  (* macro_steps *)
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
	     in*)
	     error
	   end (* losange reduction *)
	 in
	 let _ = Printf.fprintf fic "}\n" in
	 let _ = close_out fic in
	 error, handler
	)
	map
	(error, handler))
    output handler
    
