(**
  * agent_trace.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation:                      <2016-03-21 10:00:00 feret>
  * Last modification: Time-stamp: <2016-04-12 16:15:46 feret>
  * *
  * Compute the projection of the traces for each insighful
   * subset of site in each agent

  *
  * Copyright 2016 Institut National
  * de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Agent_trace_even_sparser.ml") message exn (fun () -> default)

type fst_label =
  | Rule of Ckappa_sig.c_rule_id
  | Init of int
	      
type label = fst_label * Ckappa_sig.c_agent_id

let int_of_fst_label i =
  match i with
  | Rule r -> Ckappa_sig.int_of_rule_id r
  | Init i -> -(i+1)

let int_pair_of_label (i,j) = int_of_fst_label i, Ckappa_sig.int_of_agent_id j

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

type extensional_representation =
  {
    nodes: Ckappa_sig.Views_bdu.mvbdu list;
    edges: (Ckappa_sig.Views_bdu.mvbdu * label * Ckappa_sig.Views_bdu.mvbdu) list ;
    nodes_creation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    nodes_degradation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
  }


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

let empty_transition_system =
  {
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [] ;
    macro_state_to_state = Wrapped_modules.LoggedIntMap.empty;
  }

let add_node q transition_system = {transition_system with nodes = q::transition_system.nodes}
				      
let add_edge q q' label transition_system =
  {transition_system with edges = (q,label,q')::transition_system.edges}

let convert_label (r,a) =
  let fst =
    match r with Rule r ->
      (Ckappa_sig.int_of_rule_id r)
	       | Init i -> -(i+1)
  in
  Ckappa_sig.state_index_of_int fst,Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id a)


let add_creation parameter error r_id ag_id mvbdu transition_system =
  error, {transition_system with nodes_creation = (mvbdu,(r_id,ag_id))::transition_system.nodes_creation}
  
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

let dump_mvbdu fic parameter handler error handler_kappa agent_type agent_string mvbdu =
   let error, handler, list = build_asso_of_mvbdu parameter handler error mvbdu in
   let error, handler = dump_key_of_asso fic parameter handler error list in
   let () = Printf.fprintf fic " [label=\"" in
   let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
   let () = Printf.fprintf fic "\"];\n" in
   error, handler

let label_to_state_pair label =
  (Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_rule_id (fst label)),
   Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id (snd label)))

let add_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu transition_system =
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  error, handler, {transition_system with nodes = mvbdu::transition_system.nodes}

let dump_graph_header fic =
   Printf.fprintf fic "digraph G{\n"

let bdu_of_view parameter handler error test =
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true parameter handler error in
  List.fold_left
    (fun (error,handler,mvbdu) (site,state) ->
      let lub = Ckappa_sig.int_of_state_index state.Cckappa_sig.site_state.Cckappa_sig.min in
      let glb = Ckappa_sig.int_of_state_index state.Cckappa_sig.site_state.Cckappa_sig.max in
      let rec aux k output =
          if k>glb then output
          else
            let error, handler, mvbdu =  output in
            let error, handler, mvbdu' = mvbdu_of_association_list parameter handler error [site,Ckappa_sig.state_index_of_int k] in
            aux (k+1) (Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu mvbdu')
      in
      let error, handler, mvbdu' = aux lub (error,handler,mvbdu_false) in
      Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu mvbdu'
      )
    (error, handler, mvbdu_true)
    test

let asso_of_view parameter handler error view =
  let error, list =
    List.fold_left
      (fun (error,list) (site,state) ->
       let lub = state.Cckappa_sig.site_state.Cckappa_sig.min in
       let glb = state.Cckappa_sig.site_state.Cckappa_sig.max in
       if lub = glb
       then
	 error, (site,lub)::list
       else
	 warn parameter error (Some "line 350") Exit list
      )
      (error, [])
      view
  in
  Ckappa_sig.Views_bdu.build_association_list parameter handler error list

		  
let compute_full_support parameter handler error ag_id rule =
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
	    parameter error ag_id diff
	in
	match
	  agent
	with
	| None -> None
	| Some ag -> Some ag
      end
  | Some (Cckappa_sig.Agent ag) -> Some ag
  in
  let parse error v =
    match
      v
    with
    | Some v ->
      begin
	let error, list, list' =
	  Ckappa_sig.Site_map_and_set.Map.fold
	    (fun site state (error,list,list') ->
	     let error, list = SiteSet.add parameter error site list in
             error, list,(site,state)::list')
	    v.Cckappa_sig.agent_interface
	    (error, SiteSet.empty,[])
	in error, Some (v.Cckappa_sig.agent_name,list,list')
      end
    | None -> error, None
  in
  match parse error view
  with
  | error, None -> error, None
  | error, Some (name, list,list') -> 
     begin
       let error, agent =
	 Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
	   parameter
	   error
	   ag_id
	   diff
       in
       match parse error agent
       with
       | error, None -> error, None
       | error, Some (_, list'',list''') -> 
	  let error, handler, list' = bdu_of_view parameter handler error list' in
	  let error, handler, list''' = asso_of_view parameter handler error list''' in
	  error, Some (name, list, list', list'',list''')
     end

let build_support parameter handler error rules =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error r_id rule ->
	       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
	          parameter
	          error
	          (fun parameter error ag_id _  map ->
              match
                compute_full_support parameter handler error ag_id rule
              with
                error, None -> error, map
                | error, Some (agent_name, set_test, asso_test, set_mod, asso_mod) ->
                let error, old_map =
                  Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                    parameter
                    error
                    LabelMap.empty
                    agent_name
                    map
                in
                let error, new_map =
                    LabelMap.add parameter error (Rule r_id,ag_id) (set_test, asso_test, set_mod, asso_mod) old_map
                in
                Ckappa_sig.Agent_map_and_set.Map.add parameter error agent_name new_map map)
	         rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
	  )
      rules Ckappa_sig.Agent_map_and_set.Map.empty

let commute parameter error label1 label2 support =
  let error, opt1 =
    LabelMap.find_option parameter error label1 support
  in
  let error, opt2=
    LabelMap.find_option parameter error label2 support
  in
  match opt1,opt2 with
  | None, _ | _,None -> error, true
  | Some (test1,_, act1,_), Some (test2, _,  act2, _) ->
    let error, inter1 = SiteSet.inter parameter error test1 act2 in
    if SiteSet.is_empty inter1
    then
      let error, inter2 = SiteSet.inter parameter error test2 act1 in
      if SiteSet.is_empty inter2
      then
        error, true
      else
        error, false
    else
      error, false


let can_be_concurrent parameter handler error mvbdu mvbdu1 mvbdu2 =
  let error, handler, mvbdu_and = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu1 mvbdu2 in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu_and mvbdu in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  error, handler, Ckappa_sig.Views_bdu.equal mvbdu_false mvbdu

let concurrent_sites parameter handler error mvbdu support =
  LabelMap.fold
    (fun label (test,asso,act,asso_modif) (error, handler, map')->
      let error, handler, sites_in_conflict =
        LabelMap.fold
          (fun label' (test,asso',act,asso_modif') (error,handler,accu) ->
            let error, is_commute = commute parameter error label label' support in
            if is_commute
            then
              let error, handler, can_be_concurrent = can_be_concurrent parameter handler error mvbdu asso asso' in
              if can_be_concurrent
              then
                error, handler, accu
              else
                let error, accu = SiteSet.union parameter error accu act in
                error, handler, accu
            else
              error, handler, accu)
        support
        (error,handler,SiteSet.empty)
      in
      let ext_list = SiteSet.elements sites_in_conflict in
      let error, handler, hconsed =
        Ckappa_sig.Views_bdu.build_variables_list
          parameter handler error ext_list
      in
      let error, map' = LabelMap.add parameter error label (sites_in_conflict,hconsed) map' in
      error, handler, map')
    support
    (error, handler, LabelMap.empty)

let is_subset parameter error handler a b =
  let error, handler, c = Ckappa_sig.Views_bdu.mvbdu_and parameter error handler a b in
  error,Ckappa_sig.Views_bdu.equal a c

let agent_trace parameter error handler handler_kappa mvbdu_true compil output =
  let rules = compil.Cckappa_sig.rules in
  let init = compil.Cckappa_sig.init in
  let error, support = build_support parameter handler error rules in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true  parameter handler error in
  let error, handler, empty = Ckappa_sig.Views_bdu.build_variables_list parameter handler error [] in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter
    error
    (fun parameter error agent_type map (handler:Ckappa_sig.Views_bdu.handler) ->
     let error, support = Ckappa_sig.Agent_map_and_set.Map.find_default parameter error LabelMap.empty agent_type support in
     let error', agent_string =
       try
	 Handler.string_of_agent parameter error handler_kappa agent_type
       with
       | _ -> warn parameter error (Some "line 111") Exit
              (Ckappa_sig.string_of_agent_name agent_type)
     in
     let error = Exception.check warn parameter error error' (Some "line 1917") Exit in
     Wrapped_modules.LoggedIntMap.fold
       (fun _ mvbdu (error,handler) ->
	let error, handler, sites = Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error mvbdu in
	let error, handler, ext_list =  Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error sites in
        let error, site_set =
          List.fold_left
            (fun (error, set) site -> SiteSet.add parameter error site set)
              (error, SiteSet.empty)
              ext_list
        in
        let error, handler, support =
          LabelMap.fold
            (fun label (test,asso_test,modif,asso_modif) (error,handler,map) ->
             let error, test = SiteSet.inter parameter error test site_set in
	     let error, handler, asso_test = Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameter handler error asso_test sites in
	     let error, modif = SiteSet.inter parameter error modif site_set in
	     let error, handler, asso_modif = Ckappa_sig.Views_bdu.extensional_of_association_list parameter handler error asso_modif in
	     let asso_modif = List.filter (fun (a,_) -> SiteSet.mem a site_set) asso_modif in
	     let error, handler, asso_modif = Ckappa_sig.Views_bdu.build_association_list parameter handler error asso_modif in
	     let error, map = LabelMap.add parameter error label (test,asso_test,modif,asso_modif) map in
	    error, handler, map)
            support
            (error, handler, LabelMap.empty)
        in
        let error, handler, concurrent_sites = concurrent_sites parameter handler error mvbdu support in
        let error, file_name =
	  List.fold_left
	    (fun (error, string) site ->
	     let error, site_string = Handler.string_of_site parameter error handler_kappa agent_type site in
	     error, string^"_"^site_string)
	    (error, ((Remanent_parameters.get_local_trace_directory parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
	    ext_list
	in
	let transition_system = empty_transition_system in
	let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	let fic = Remanent_parameters.open_out file_name in
	let () = dump_graph_header fic in
	let error, handler, transition_system =
	  LabelMap.fold
	    (fun label (test,asso,modif,asso_modif) (error, handler, transition_system) ->
	     let error, concurrent_site =
	       LabelMap.find_default parameter error (SiteSet.empty,empty) label concurrent_sites
	     in
	     let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu (snd concurrent_site) in
	     let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu asso in
	     let error, handler, pre = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
	     let error, handler, transition_system =
	       List.fold_left
		 (fun (error, handler, transition_system) list ->
		  let error, handler, pre = mvbdu_of_association_list parameter handler error list in
		  let error, handler, post = Ckappa_sig.Views_bdu.mvbdu_redefine parameter handler error pre asso_modif in
		  let transition_system = add_node pre transition_system in
		  let transition_system = add_node post transition_system in
		  let transition_system = add_edge pre post label transition_system in
		  error, handler, transition_system)
		 (error, handler, transition_system)
		 pre
	     in
	     error, handler, transition_system)
	    support
	    (error, handler, transition_system)
	in
	(* nodes -> Initial *)
	let error,handler =
	  List.fold_left
	    (fun (  error, handler) (mvbdu,label) ->
	     let error, handler, key = hash_of_mvbdu parameter handler error mvbdu in
	     let () =   Printf.fprintf fic "Init_%i [width=\"0cm\" height=\"0cm\" style=\"none\" label=\"\"];\n" key in
	     error, handler)
	    (error, handler)
	    transition_system.nodes_creation
	in
	(* nodes -> regular *)
	let error, handler =
	  List.fold_left
	    (fun (error, handler) ->
	     dump_mvbdu fic parameter handler error handler_kappa agent_type agent_string)
	    (error, handler)
	    transition_system.nodes
	in
	(* macro_steps *)
	let error =
	  Wrapped_modules.LoggedIntMap.fold
	    (fun k _ error ->
	     let () = Printf.fprintf fic "Macro_%i [width=\"0cm\" height=\"0cm\" stype=\"none\" label=\"\"];\n" k
	     in error)
	    transition_system.macro_state_to_state
	    error
	in
	(* edges  *)
	let error, handler =
	  List.fold_left
	    (fun (error, handler) (q,label,q') ->
	     let error, handler, key = hash_of_mvbdu parameter handler error q in
	     let error, handler, key' = hash_of_mvbdu parameter handler error q' in
	     let error, rule_name =
	       if  Remanent_parameters.get_show_rule_names_in_local_traces parameter
	       then
		 begin
		   match
		     fst label
		   with
		   | Rule r -> Handler.string_of_rule parameter error handler_kappa compil r
		   | _ -> warn parameter error (Some "line 1412") Exit ""
		 end
	       else
		 error, ""
	     in
	     let () = Printf.fprintf fic "Node_%i -> Node_%i [label=\"%s\"];\n" key key' rule_name in
	     error,handler)
	    (error,handler)
	    transition_system.edges
	in
	let error,_ =
	  List.fold_left
	    (fun (error, handler) (q,label) ->
	     let error, handler, key = hash_of_mvbdu parameter handler error q in
	     let error, rule_name =
	       if  Remanent_parameters.get_show_rule_names_in_local_traces parameter
	       then
		 begin
		   match
		     fst label
		   with
		   | Rule r -> Handler.string_of_rule parameter error handler_kappa compil r
		   | _ -> warn parameter error (Some "line 1412") Exit ""
		 end
	       else
		 error, ""
	     in
	     let () = Printf.fprintf fic "Init_%i -> Node_%i [label=\"%s\"];\n" key key rule_name in
	     error, handler)
	    (error,handler)
	    transition_system.nodes_creation
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
	    transition_system.macro_state_to_state
	in
	
	let _ = Printf.fprintf fic "}\n" in
	let _ = close_out fic in
	error, handler
       )
       map
       (error, handler))
    output handler
    
