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

let allow_losange_reduction = true

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Agent_trace.ml") message exn (fun () -> default)

type fst_label =
  Rule of Ckappa_sig.c_rule_id
| Init of int
type label = fst_label * Ckappa_sig.c_agent_id

let int_of_fst_label i =
  match i with
  | Rule r -> Ckappa_sig.int_of_rule_id r
  | Init i -> -(i+1)

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
    hconsed_renaming: Ckappa_sig.Views_bdu.hconsed_renaming_list;
    hconsed_renaming_back: Ckappa_sig.Views_bdu.hconsed_renaming_list;
    hconsed_sites_precondition: Ckappa_sig.Views_bdu.hconsed_variables_list;
    hconsed_sites_postcondition:Ckappa_sig.Views_bdu.hconsed_variables_list;
    diag_precondition: Ckappa_sig.Views_bdu.mvbdu;
    diag_postcondition: Ckappa_sig.Views_bdu.mvbdu;
    hconsed_sites_label: Ckappa_sig.Views_bdu.hconsed_variables_list;
    sites_precondition: Ckappa_sig.c_site_name list;
    sites_postcondition: Ckappa_sig.c_site_name list;
    site_rule_id: Ckappa_sig.c_site_name;
    site_agent_id: Ckappa_sig.c_site_name;
    forward_transitions: Ckappa_sig.Views_bdu.mvbdu;
    backward_transitions: Ckappa_sig.Views_bdu.mvbdu;
    creation: Ckappa_sig.Views_bdu.mvbdu;
    degradation: Ckappa_sig.Views_bdu.mvbdu;
    reachables: Ckappa_sig.Views_bdu.mvbdu;
    mvbdu_default_value: Ckappa_sig.Views_bdu.mvbdu;
  }

type extensional_representation =
  {
    nodes: Ckappa_sig.Views_bdu.mvbdu list;
    state_to_mvbdu: Ckappa_sig.Views_bdu.mvbdu Wrapped_modules.LoggedIntMap.t ;
    edges: (Ckappa_sig.Views_bdu.mvbdu * label * Ckappa_sig.Views_bdu.mvbdu) list ;
    nodes_creation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    nodes_degradation: (Ckappa_sig.Views_bdu.mvbdu * label) list ;
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
    already_visited: Ckappa_sig.Views_bdu.mvbdu;
    in_macro_states: Ckappa_sig.Views_bdu.mvbdu;
    in_macro_edges: Ckappa_sig.Views_bdu.mvbdu LabelMap.t ; (* to do, remove the map, put the label in the mvbdu *)
  }

let shift_site op site n =
  Ckappa_sig.site_name_of_int (op (Ckappa_sig.int_of_site_name site) n)
let shift_site_minus site n = shift_site (-) site n
let shift_site_plus site n = shift_site (+) site n


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


let dummy_state = Ckappa_sig.state_index_of_int (-1)

let empty_transition parameter handler error mvbdu =
   let error, handler, sites = Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error mvbdu in
   let error, handler, ext_list =  Ckappa_sig.Views_bdu.extensional_of_variables_list parameter handler error sites in
   let def_list = List.rev_map (fun i -> (i,Ckappa_sig.state_index_of_int 0)) ext_list in
   let error, handler, mvbdu_default_value = mvbdu_of_reverse_order_association_list parameter handler error def_list in
   let max_site = List.fold_left (fun n i -> max n (Ckappa_sig.int_of_site_name i)) 0 ext_list in
   let max_site = max_site +1 in
   let n = max_site in
   let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
   let sites_precondition = ext_list in
   let sites_postcondition = List.rev_map (fun i -> shift_site_plus i n)  sites_precondition in
   let renaming = List.rev_map (fun i -> i,shift_site_plus i n) sites_precondition in
   let renaming_back = List.rev_map (fun i -> shift_site_plus i n,i) sites_precondition in
   let diag_precondition = List.rev_map (fun x -> x,dummy_state) (List.rev sites_precondition) in
   let diag_postcondition = List.rev_map (fun x -> x,dummy_state) sites_postcondition in
   let error, handler, renaming = Ckappa_sig.Views_bdu.build_renaming_list parameter handler error renaming in
   let error, handler, renaming_back = Ckappa_sig.Views_bdu.build_renaming_list parameter handler error renaming_back in
   let error, handler, hconsed_sites_precondition = Ckappa_sig.Views_bdu.build_variables_list parameter handler error sites_precondition in
   let error, handler, hconsed_sites_postcondition = Ckappa_sig.Views_bdu.build_variables_list parameter handler error sites_postcondition in
   let error, handler, diag_precondition = mvbdu_of_association_list parameter handler error diag_precondition in
   let error, handler, diag_postcondition = mvbdu_of_association_list parameter handler error diag_postcondition in
   let site_rule_id = Ckappa_sig.site_name_of_int (-2) (*(2*n)*) in
   let site_agent_id = Ckappa_sig.site_name_of_int (-1) (*(2*n+1)*) in
   let error, handler, hconsed_sites_label = Ckappa_sig.Views_bdu.build_variables_list parameter handler error [site_rule_id;site_agent_id] in
   error,
   handler,
   {
     nsites = n ;
     reachables = mvbdu ;
     hconsed_renaming = renaming ;
     hconsed_renaming_back = renaming_back;
     sites_precondition = sites_precondition ;
     sites_postcondition = sites_postcondition ;
     diag_precondition = diag_precondition ;
     diag_postcondition = diag_postcondition ;
     hconsed_sites_precondition = hconsed_sites_precondition ;
     hconsed_sites_postcondition = hconsed_sites_postcondition ;
     hconsed_sites_label = hconsed_sites_label ;
     site_rule_id = site_rule_id ;
     site_agent_id = site_agent_id ;
     forward_transitions = mvbdu_false;
     backward_transitions = mvbdu_false;
     degradation = mvbdu_false;
     creation = mvbdu_false;
     mvbdu_default_value = mvbdu_default_value ;
   }

let empty_transition_system n mvbdu_false =
  {
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [] ;
    state_to_mvbdu = Wrapped_modules.LoggedIntMap.empty;
    already_visited = mvbdu_false;
    in_macro_states = mvbdu_false;
    macro_state_to_state = Wrapped_modules.LoggedIntMap.empty;
    in_macro_edges = LabelMap.empty ;
  }

let add_edge r_id ag_id q q' _ transition_system =
  {transition_system with edges = (q,(r_id,ag_id),q')::transition_system.edges}

let convert_label (r,a) =
  let fst =
    match r with Rule r ->
      (Ckappa_sig.int_of_rule_id r)
	       | Init i -> -(i+1)
  in
  Ckappa_sig.state_index_of_int fst,Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id a)
let mvbdu_of_label parameter handler error intensional (r,a) =
  let (r,a) = convert_label (r,a) in
  let asso = [intensional.site_agent_id,a;intensional.site_rule_id,r] in
 let error, handler, mvbdu = mvbdu_of_reverse_order_association_list parameter handler error asso in
  error, handler, mvbdu

let add_creation parameter handler error r_id ag_id mvbdu intensional =
  let error, handler, mvbdu_label = mvbdu_of_label parameter handler error intensional (r_id,ag_id) in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu mvbdu_label in
  let error, handler, creation = Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu intensional.creation in
  error, handler,
  {intensional with creation = creation}

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

let transitions_via_label_list parameter handler error labellist internal =
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  let site_rid = internal.site_rule_id in
  let site_agid = internal.site_agent_id in
  List.fold_left
    (fun (error, handler, mvbdu) label ->
     let r_id, ag_id = convert_label label in
     let error, handler, mvbdu' = mvbdu_of_reverse_order_association_list parameter handler error [site_agid,ag_id;site_rid,r_id] in
     Ckappa_sig.Views_bdu.mvbdu_or parameter handler error mvbdu mvbdu')
      (error, handler, mvbdu_false)
      labellist

let translate_gen f parameter error handler site_rid site_agid list internal output =
  match
    list
  with
  | (y,y')::(x,x')::l when x=internal.site_agent_id && y = internal.site_rule_id ->
     let y' = Ckappa_sig.int_of_state_index y' in
     let r_id =
       if y'<0
       then Init (-(y'+1))
       else Rule (Ckappa_sig.rule_id_of_int y')
     in
     let label = r_id,
		 Ckappa_sig.agent_id_of_int (Ckappa_sig.int_of_state_index x') in
     let asso = f l in
     let error, handler, mvbdu = mvbdu_of_association_list parameter handler error asso in
     error, (handler,(label,mvbdu)::output)
  | _ -> warn parameter error (Some "line 146") Exit (handler,output)

let get_label parameter error handler site_rid site_agid list internal output =
  match
    list
  with
  | (y,y')::(x,x')::_ when x=site_agid && y = site_rid ->
     let y' = Ckappa_sig.int_of_state_index y' in
     let r_id =
       if y'<0
       then Init (-(y'+1))
       else Rule (Ckappa_sig.rule_id_of_int y')
     in
     let label =
        r_id,
		    Ckappa_sig.agent_id_of_int (Ckappa_sig.int_of_state_index x') in
      error, (handler,label::output)
  | _ -> warn parameter error (Some "line 146") Exit (handler,output)

let translate_back parameter error handler site_rid site_agid list internal output =
  translate_gen (fun l -> List.rev_map (fun (x,y) -> (shift_site_minus x internal.nsites,y)) (List.rev l))
		parameter error handler site_rid site_agid list internal output

let translate_direct parameter error handler site_rid site_agid list internal output =
  translate_gen (fun l -> l) parameter error handler site_rid site_agid list internal output

let correct_state state state' =
  if state=dummy_state then state' else state

let ingoing_outgoing_gen get_sites get_transitions shift_mvbdu translate filter parameter handler error mvbdu internal =
  let error, handler, asso_list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  let error, asso_list =
    match asso_list
    with
    | [a] -> error, a
    | _ -> warn parameter error (Some "line 352") Exit []
  in
  let error, handler, shifted_mvbdu = shift_mvbdu parameter handler error mvbdu in
  let transitions = get_transitions internal in
  let error, handler, mvbdu_transitions = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error shifted_mvbdu transitions in
  let varlist = get_sites internal in
  let error, handler, mvbdu_other_side = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu_transitions varlist in
  let error, handler, other_side_list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu_other_side in
  let error, (handler, output) =
    List.fold_left
      (fun (error,(handler,output)) list ->
	     translate parameter error handler internal.site_rule_id internal.site_agent_id list internal output)
      (error,(handler,[])) other_side_list
  in
  let error, handler, output =
    List.fold_left
      (fun (error,handler,output) (label,mvbdu) ->
       let error, handler, asso = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
       let error, asso =
	 match
	   asso
	 with
	   [a] -> error, a
	 | _ -> warn parameter error (Some "line 372") Exit []
       in
       let rec aux error list1 list2 listrep =
	 match
	   list1,list2
	 with
	   (a,b)::t,(a',b')::t' when a=a' -> aux error t t' ((a,correct_state b b')::listrep)
	 | [],[] -> error, listrep
	 | _ -> warn parameter error (Some "line 356") Exit (List.rev listrep)
       in
       let error, new_asso = aux error asso asso_list [] in
       let error, handler, mvbdu' = mvbdu_of_association_list parameter handler error new_asso in
       let error, handler, bool = filter parameter handler error mvbdu' internal in
       if bool then
	 error, handler, (label,mvbdu')::output
       else
	 error, handler, output
      )
      (error,handler,[])
      (List.rev output)
  in
  error, handler, output

let outgoing parameter handler error mvbdu internal =
  ingoing_outgoing_gen
    (fun internal -> internal.hconsed_sites_precondition)
    (fun internal -> internal.forward_transitions)
    (fun parameter handler error x -> error, handler, x)
    translate_back
    (fun _ handler error _ _ -> error, handler, true)
    parameter handler error mvbdu internal

let ingoing parameter handler error mvbdu internal =
  ingoing_outgoing_gen
    (fun internal -> internal.hconsed_sites_postcondition)
    (fun internal -> internal.backward_transitions)
    (fun parameter handler error x ->
      Ckappa_sig.Views_bdu.mvbdu_rename parameter handler error x internal.hconsed_renaming)
    translate_direct
    (fun parameter handler error mvbdu internal ->
      Ckappa_sig.Views_bdu.mvbdu_subseteq parameter handler error mvbdu internal.reachables)
    parameter handler error mvbdu internal

let half_trans parameter handler error mvbdu trans_set intensional  =
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu trans_set in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameter handler error mvbdu intensional.hconsed_sites_precondition in
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  let error, (handler, output) =
    List.fold_left
      (fun (error,(handler,output)) list ->
	     get_label parameter error handler intensional.site_rule_id intensional.site_agent_id list trans_set output)
      (error,(handler,[])) list
  in
  error, handler, output

let creation parameter handler error mvbdu internal =
  half_trans parameter handler error mvbdu internal.creation internal
let degradation parameter handler error mvbdu internal =
  half_trans parameter handler error mvbdu internal.degradation internal

let label_to_state_pair label =
  (Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_rule_id (fst label)),
   Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id (snd label)))

let add_node_from_mvbdu parameter handler handler_kappa error agent_type agent_string mvbdu transition_system =
  let error, handler, list = Ckappa_sig.Views_bdu.extensional_of_mvbdu parameter handler error mvbdu in
  error, handler, {transition_system with nodes = mvbdu::transition_system.nodes}

let dump_graph_header fic =
   Printf.fprintf fic "digraph G{\n"

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
  let error, list,list' =
    match
      view
    with
    | Some v ->
      begin
	Ckappa_sig.Site_map_and_set.Map.fold
	  (fun site state (error,list,list') ->
      let error, list = SiteSet.add parameter error site list in
        error, list,(site,state)::list')
	  v.Cckappa_sig.agent_interface
	  (error, SiteSet.empty,[])
      end
    | None -> error, SiteSet.empty, []
  in
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
    | None -> error, None
    | Some v ->
       begin
       let name = v.Cckappa_sig.agent_name in
       let error, list'' =
	 Ckappa_sig.Site_map_and_set.Map.fold
	   (fun site state (error,list) -> SiteSet.add parameter error site list)
	   v.Cckappa_sig.agent_interface
	   (error, SiteSet.empty)
     in
     error, Some (name, list, list', list'')
      end

let build_support parameter error rules =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error r_id rule ->
	       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
	          parameter
	          error
	          (fun parameter error ag_id _  map ->
              match
                compute_full_support parameter error ag_id rule
              with
                error, None -> error, map
                | error, Some (agent_name, set_test, asso, set_mod) ->
                let error, old_map =
                  Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                    parameter
                    error
                    LabelMap.empty
                    agent_name
                    map
                in
                let error, new_map =
                    LabelMap.add parameter error (Rule r_id,ag_id) (set_test, asso, set_mod) old_map
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
  | Some (test1,asso1, act1), Some (test2, asso2, act2) ->
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

let can_be_concurrent parameter handler error mvbdu test1 test2 =
  let error, handler, mvbdu1 = bdu_of_view parameter handler error test1 in
  let error, handler, mvbdu2 = bdu_of_view parameter handler error test2 in
  let error, handler, mvbdu_and = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu1 mvbdu2 in
  let error, handler, mvbdu = Ckappa_sig.Views_bdu.mvbdu_and parameter handler error mvbdu_and mvbdu in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  error, handler, Ckappa_sig.Views_bdu.equal mvbdu_false mvbdu

let concurrent_sites parameter handler error mvbdu support =
  LabelMap.fold
    (fun label (test,asso,act) (error, handler, map')->
      let error, handler, sites_in_conflict =
        LabelMap.fold
          (fun label' (test,asso',act) (error,handler,accu) ->
            let error, is_commute = commute parameter error label label' support in
            if is_commute
            then
              let error, handler, can_be_concurrent = can_be_concurrent parameter handler error mvbdu asso asso' in
              if can_be_concurrent
              then
                error, handler, accu
              else
                let _ = Printf.fprintf stdout "Commute %i %i ; " (int_of_fst_label (fst label)) (Ckappa_sig.int_of_agent_id (snd label))
                in
                let _ = Printf.fprintf stdout "-- %i %i ; \n" (int_of_fst_label (fst label')) (Ckappa_sig.int_of_agent_id (snd label'))
                in
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
  let error, support = build_support parameter error rules in
  let error, handler, mvbdu_true = Ckappa_sig.Views_bdu.mvbdu_true  parameter handler error in
  let error, handler, mvbdu_false = Ckappa_sig.Views_bdu.mvbdu_false parameter handler error in
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameter
    error
    (fun parameter error agent_type map (handler:Ckappa_sig.Views_bdu.handler) ->
      let error, support = Ckappa_sig.Agent_map_and_set.Map.find_default parameter error
                              LabelMap.empty agent_type support
      in
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
          let error, support =
              LabelMap.fold
                (fun label (test,asso,modif) (error,map) ->
                  let error, test = SiteSet.inter parameter error test site_set in
                  let error, modif = SiteSet.inter parameter error modif site_set in
                  LabelMap.add parameter error label (test,asso,modif) map)
                support
                (error, LabelMap.empty)
          in
          let error, handler, concurrent_sites = concurrent_sites parameter handler error mvbdu support in
          let _ =
            LabelMap.iter
              (fun (rule_id,agent_id) (set,_) ->
                  let _ = Printf.fprintf stdout "%i %i ; " (int_of_fst_label rule_id) (Ckappa_sig.int_of_agent_id agent_id)
  in
                   let _ =
                      SiteSet.iter
                        (fun site -> Printf.fprintf stdout "%i;" (Ckappa_sig.int_of_site_name site))
                        set
                  in
                  let _ = Printf.fprintf stdout "\n" in
                  ())
                  concurrent_sites
                in
          let error, file_name =
	         List.fold_left
	          (fun (error, string) site ->
	             let error, site_string = Handler.string_of_site parameter error handler_kappa agent_type site in
	              error, string^"_"^site_string)
	          (error, ((Remanent_parameters.get_local_trace_directory parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
            ext_list
	        in
	        let transition_system = empty_transition_system 0 (*max_site*) mvbdu_false in
	        let file_name = file_name^(Remanent_parameters.ext_format (Remanent_parameters.get_local_trace_format parameter)) in
	        let fic = Remanent_parameters.open_out file_name in
	        let () = dump_graph_header fic in


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
