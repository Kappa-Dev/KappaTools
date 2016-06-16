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

module SiteMap = Site.Map
module SiteSet = Site.Set
module SitePair =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = Ckappa_sig.c_site_name * Ckappa_sig.c_site_name
         let compare = compare
         let print _ _ = ()
       end))

module SitePairSet = SitePair.Set

type extensional_representation =
  {
    title: string;
    agent_type: Ckappa_sig.c_agent_name;
    agent_string: string;
    nodes: Ckappa_sig.Views_intbdu.mvbdu list;
    edges: (Ckappa_sig.Views_intbdu.mvbdu * label * Ckappa_sig.Views_intbdu.mvbdu) list ;
    nodes_creation: (Ckappa_sig.Views_intbdu.mvbdu * label) list ;
    nodes_degradation: (Ckappa_sig.Views_intbdu.mvbdu * label) list ;
    subframe: Mods.IntSet.t Mods.IntMap.t ;
    nodes_in_bdu: Mods.IntSet.t
  }

let mvbdu_of_association_list_gen gen  asso_list =
  let hconsed_list = gen asso_list in
  let mvbdu_true = Ckappa_sig.Views_intbdu.mvbdu_true () in
  Ckappa_sig.Views_intbdu.mvbdu_redefine mvbdu_true hconsed_list

let mvbdu_of_association_list asso =
  mvbdu_of_association_list_gen
    Ckappa_sig.Views_intbdu.build_association_list
    asso

let mvbdu_of_reverse_order_association_list asso =
  mvbdu_of_association_list_gen
    Ckappa_sig.Views_intbdu.build_reverse_sorted_association_list
    asso

let empty_transition_system title agent_string agent_name =
  {
    title = title;
    agent_type = agent_name;
    agent_string = agent_string;
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [] ;
    subframe = Mods.IntMap.empty;
    nodes_in_bdu = Mods.IntSet.empty
  }

let hash_of_association_list list =
  let hconsed_list =
    Ckappa_sig.Views_intbdu.build_association_list list
  in
  let hash = Ckappa_sig.Views_intbdu.hash_of_association_list hconsed_list in
  hash

let build_asso_of_mvbdu parameter error mvbdu =
  let list =
    Ckappa_sig.Views_intbdu.extensional_of_mvbdu mvbdu
  in
  match list
  with
  | [list] ->
     begin
       error, list
     end
  | _ ->
     let error, list =
       warn parameter error (Some "line 385") Exit []
     in
     error, list

let hash_of_mvbdu parameter error mvbdu =
  let error, asso =
    build_asso_of_mvbdu parameter error mvbdu
  in
  error, hash_of_association_list asso

let add_node parameter error q transition_system =
  let bdu_set = transition_system.nodes_in_bdu in
  let error, hash = hash_of_mvbdu parameter error q in
  if
    Mods.IntSet.mem hash bdu_set
  then
    error, transition_system
  else
    error,
    {
      transition_system
      with
        nodes = q::transition_system.nodes;
        nodes_in_bdu = Mods.IntSet.add hash bdu_set
    }

let add_edge q q' label transition_system =
  {transition_system with edges = (q,label,q')::transition_system.edges}

let convert_label (r,a) =
  Ckappa_sig.state_index_of_int (int_of_fst_label r),
  Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id a)

let add_creation parameter error r_id ag_id mvbdu transition_system =
  let error, transition_system =
    add_node parameter error mvbdu transition_system
  in
  error,
  {transition_system
   with
    nodes_creation = (mvbdu,(r_id,ag_id))::transition_system.nodes_creation
  }

let dump_edge logger parameter error handler_kappa compil key key' label =
  let error, rule_name =
    if Remanent_parameters.get_show_rule_names_in_local_traces parameter
    then
      Handler.string_of_rule parameter error handler_kappa compil (fst label)
    else error,""
  in
  let () =
    Graph_loggers.print_edge logger ("Node_"^key) ("Node_"^key') ~directives:[Graph_loggers.Label rule_name] in
  error


let string_key_of_asso list =
  let hash = hash_of_association_list list in
  Printf.sprintf "Node_%i" hash

let string_label_of_asso parameter error handler_kappa transition_system list =
  let string = transition_system.agent_string^"("  in
  let error,string,_ =
    List.fold_left
      (fun (error,string,bool) (site_type, state) ->
         let string =
           if bool
           then
             string^","
           else
             string
         in
         let error, site_string =
           Handler.string_of_site parameter error handler_kappa
             transition_system.agent_type site_type
         in
         let error', state_string =
           Handler.string_of_state_fully_deciphered parameter error
             handler_kappa transition_system.agent_type site_type state
         in
         let error =
           Exception.check warn parameter error error'
             (Some "line 240") Exit
         in
         error,
         string^site_string^state_string,
         true
      )
      (error,string,false) list
  in
  error, string^")"

let dump_mvbdu logger parameter error handler_kappa transition_system mvbdu =
   let error, list = build_asso_of_mvbdu parameter error mvbdu in
   let key = string_key_of_asso list in
   let error,label = string_label_of_asso parameter error handler_kappa transition_system list in
   let () = Graph_loggers.print_node logger key
       ~directives:([Graph_loggers.Label label])
   in
   error

let add_node_from_mvbdu _parameter _handler_kappa error _agent_type _agent_string mvbdu transition_system =
  error,
  {
    transition_system
    with
      nodes = mvbdu::transition_system.nodes}

let bdu_of_view test =
  let mvbdu_false = Ckappa_sig.Views_intbdu.mvbdu_false () in
  let mvbdu_true = Ckappa_sig.Views_intbdu.mvbdu_true () in
  List.fold_left
    (fun mvbdu (site,state) ->
      let lub =
        Ckappa_sig.int_of_state_index
          state.Cckappa_sig.site_state.Cckappa_sig.min
      in
      let glb =
        Ckappa_sig.int_of_state_index
          state.Cckappa_sig.site_state.Cckappa_sig.max
      in
      let rec aux k mvbdu =
          if k>glb then mvbdu
          else
            let mvbdu' =
              mvbdu_of_association_list
                [site,Ckappa_sig.state_index_of_int k] in
            aux (k+1) (Ckappa_sig.Views_intbdu.mvbdu_or mvbdu mvbdu')
      in
      let mvbdu' = aux lub mvbdu_false in
      Ckappa_sig.Views_intbdu.mvbdu_and mvbdu mvbdu'
      )
    mvbdu_true
    test

let asso_of_view_in_list parameter error view =
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
  error, Ckappa_sig.Views_intbdu.build_association_list list

let asso_of_view_in_map parameter error map =
  asso_of_view_in_list parameter error
		       (Ckappa_sig.Site_map_and_set.Map.fold
            (fun site state list -> (site,state)::list)
            map
            []
         )


let restrict_asso asso set =
  let asso =
    Ckappa_sig.Views_intbdu.extensional_of_association_list
      asso
  in
  let asso = List.filter (fun (a,_) -> SiteSet.mem a set) asso in
  Ckappa_sig.Views_intbdu.build_association_list asso


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
    | Some Cckappa_sig.Ghost
    | Some (Cckappa_sig.Dead_agent _)
    | Some (Cckappa_sig.Unknown_agent _) -> None
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
        in
        error,
        Some (v.Cckappa_sig.agent_name,list,list')
      end
    | None -> error, None
  in
  let error, agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
      parameter error ag_id diff
  in
  let error, test_opt = parse error view in
  match parse error agent
  with
  | error, None -> error, None, None
  | error, Some (name, list'',list''') ->
    begin
      let error, list''' =
        asso_of_view_in_list parameter error list'''
      in
	    match test_opt with
	    | None -> error, None, Some (name,list'',list''')
	    | Some (_,list,list') ->
       let list' = bdu_of_view list' in
	       error, Some (name,list,list',list'',list'''), None
	  end

let build_support parameter error rules =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameter error
      (fun parameter error r_id rule ->
	       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
	          parameter error
	          (fun parameter error ag_id _  (map, creation) ->
              match
                compute_full_support parameter error ag_id rule
              with
              | error, None, None ->
                error, (map, creation)
              | error, None, Some (agent_name, _, asso) ->
                let error, old_list =
                  Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                    parameter error [] agent_name creation
                in
                let error, creation =
                  Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
                    parameter error agent_name
                    (((Rule r_id,ag_id),asso)::old_list) creation
                in
                error, (map, creation)
              | error, Some _, Some _ ->
                warn
                  parameter error (Some "line 326") Exit
                  (map, creation)
              | error,
                Some (agent_name, set_test, asso_test, set_mod, asso_mod), None ->
                let error, old_map =
                  			Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                       parameter error LabelMap.empty agent_name map
                in
                let error, new_map =
                  LabelMap.add
                    parameter error
                    (Rule r_id,ag_id) (set_test, asso_test, set_mod, asso_mod) old_map
                in
                let error, map = Ckappa_sig.Agent_map_and_set.Map.add
                    parameter error agent_name new_map map
                in
                error, (map, creation))
	          rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      )
      rules
      (Ckappa_sig.Agent_map_and_set.Map.empty, Ckappa_sig.Agent_map_and_set.Map.empty)

let commute parameter error label1 label2 support =
  let error, opt1 = LabelMap.find_option parameter error label1 support in
  let error, opt2 = LabelMap.find_option parameter error label2 support in
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


let cannot_be_concurrent mvbdu mvbdu1 mvbdu2 =
  let mvbdu_and =
    Ckappa_sig.Views_intbdu.mvbdu_and mvbdu1 mvbdu2
  in
  let mvbdu =
    Ckappa_sig.Views_intbdu.mvbdu_and mvbdu_and mvbdu
  in
  let mvbdu_false =
    Ckappa_sig.Views_intbdu.mvbdu_false ()
  in
  Ckappa_sig.Views_intbdu.equal mvbdu_false mvbdu

let concurrent_sites parameter error mvbdu support =
  LabelMap.fold
    (fun label (_test,asso,_act,_asso_modif) (error, map')->
      let error, sites_in_conflict =
        LabelMap.fold
          (fun label' (_test,asso',act,_asso_modif') (error, accu) ->
            let error, is_commute = commute parameter error label label' support in
            if is_commute
            then
              if
                cannot_be_concurrent mvbdu asso asso'
              then
                error, accu
              else
                let error, accu = SiteSet.union parameter error accu act in
                error, accu
            else
              error, accu)
        support
        (error,SiteSet.empty)
      in
      let ext_list = SiteSet.elements sites_in_conflict in
      let hconsed =
        Ckappa_sig.Views_intbdu.build_variables_list ext_list
      in
      let error, map' =
        LabelMap.add parameter error label (sites_in_conflict,hconsed) map'
      in
      error, map')
    support
    (error, LabelMap.empty)

let trivial_concurrent_sites _ error _ support =
  let hconsed = Ckappa_sig.Views_intbdu.build_variables_list [] in
  error, LabelMap.map (fun _ -> SiteSet.empty, hconsed) support

let compute_var_set transition_system =
  List.fold_left
    (fun (map_interface, map_domain) mvbdu ->
       let varlist = Ckappa_sig.Views_intbdu.variables_list_of_mvbdu mvbdu in
       let hash = Ckappa_sig.Views_intbdu.hash_of_variables_list varlist in
       let map_interface = Mods.IntMap.add hash varlist map_interface in
       let old_list = Mods.IntMap.find_default [] hash map_domain in
       let map_domain = Mods.IntMap.add hash (mvbdu::old_list) map_domain in
       map_interface, map_domain)
    (Mods.IntMap.empty, Mods.IntMap.empty)
    transition_system.nodes

(*let merge_list list1 list2 =
  let rec aux list1 list2 rep =
    match
      list1,list2
    with
      [], _ -> (List.rev rep)@list2
    | _ ,[] -> (List.rev rep)@list1
    | h::q,h'::q' ->
      if h=h'
      then
        aux q q' (h::rep)
      else if h<h' then aux q list2 (h::rep)
      else aux list1 q' (h'::rep)
  in
  aux list1 list2 []*)

let distrib_list list1 list2 =
  let rec aux list1 list2 list12 list1wo2 list2wo1 =
    match
      list1,list2
    with
    | [],_ -> List.rev list12,List.rev list1wo2,(List.rev list2wo1)@list2
    | _,[] -> List.rev list12,(List.rev list1wo2)@list1,List.rev list2wo1
    | h::q,h'::q' ->
      if h=h'
      then
        aux q q' (h::list12) list1wo2 list2wo1
      else
      if h<h'
      then
        aux q list2 list12 (h::list1wo2) list2wo1
      else
        aux list1 q' list12 list1wo2 (h'::list2wo1)
  in
  aux list1 list2 [] [] []

let distrib_list list1 list2 =
  let lista , listb, listc = distrib_list list1 list2 in
  let f list =
    let () =
      List.iter (fun s -> Printf.fprintf stdout "%i;" (Ckappa_sig.int_of_site_name s)) list
    in
    let () = Printf.fprintf stdout "\n" in
    ()
  in
  let () = Printf.fprintf stdout "DISTRIB\n" in
  let () = f list1 in
  let () = f list2 in
  let () = f lista in
  let () = f listb in
  let () = f listc in
  let () = Printf.fprintf stdout "\n" in
  lista,listb,listc

let powerset  map_interface =
  let list =
    Mods.IntMap.fold
      (fun hash hconsed list ->
         (hash,hconsed)::list
      )
      map_interface
      []
  in
  let list =
    List.sort
      (fun (a,b) (c,d) ->
         compare
           (Ckappa_sig.Views_intbdu.nbr_variables d) (Ckappa_sig.Views_intbdu.nbr_variables b)
      )
      list
  in
  let rec aux to_use already_built =
    match
      to_use
    with
    | [] -> already_built
    | (hash,hconsed)::tail ->
      begin
        let already_built =
          List.fold_left
            (fun list (hash',hconsed',proof) ->
               let hconsed'' =
                 Ckappa_sig.Views_intbdu.merge_variables_lists hconsed hconsed'
               in
               let hash'' = Ckappa_sig.Views_intbdu.hash_of_variables_list hconsed'' in
               if hash'<>hash''
               then
                 (hash'',hconsed'',(hash,hconsed)::proof)::(hash',hconsed',proof)::list
               else
                 (hash',hconsed',proof)::list)
            []
            already_built
        in
        aux tail already_built
      end
  in
  let hconsed = Ckappa_sig.Views_intbdu.build_variables_list [] in
  let hash = Ckappa_sig.Views_intbdu.hash_of_variables_list hconsed in
  let list = aux list [hash,hconsed,[]] in
  let extended_map =
    List.fold_left
      (fun extended_map (hash,_hconsed,proof) ->
         let old_list = Mods.IntMap.find_default [] hash extended_map in
         let new_list =
           match proof with
           | [] | [_] -> old_list
           | _ -> proof::old_list
         in
         Mods.IntMap.add hash new_list extended_map)
      Mods.IntMap.empty
      list
  in
  Mods.IntMap.fold
    (fun hash list map ->
       let list =
         List.filter
           (fun l -> not (List.exists (fun (a,_) -> a=hash) l))
           list
       in
       match list with
       | [] | [_] -> map
       | _ -> Mods.IntMap.add hash list map)
    extended_map
    Mods.IntMap.empty

let add_singular parameter error transition_system =
    let map1,map2 = compute_var_set transition_system in
    let ambiguous_node = powerset map1 in
    Mods.IntMap.fold
      (fun _ proof_list (error, transition_system) ->
         let rec aux map2 proof_list (error, transition_system)=
           match proof_list with
           | [] -> error, transition_system
           | proof::tail ->
             let error, transition_system =
               List.fold_left
                 (fun (error, transition_system) proof' ->
                    let gen map2 proof =
                      List.fold_left
                        (fun mvbdu (hashvarlist,_) ->
                           let mvbdu' =
                             List.fold_left
                               (fun mvbdu'' mvbdu''' ->
                                  Ckappa_sig.Views_intbdu.mvbdu_or mvbdu'' mvbdu'''
                               )
                               (Ckappa_sig.Views_intbdu.mvbdu_false ())
                               (Mods.IntMap.find_default [] hashvarlist map2)
                           in
                           Ckappa_sig.Views_intbdu.mvbdu_and
                             mvbdu'
                             mvbdu)
                        (Ckappa_sig.Views_intbdu.mvbdu_true ())
                        proof
                    in
                    let mvbdu =
                      Ckappa_sig.Views_intbdu.mvbdu_and
                        (gen map2 proof)
                        (gen map2 proof')
                    in
                    let error, transition_system =
                      if
                        Remanent_parameters.get_add_singular_microstates
                          parameter
                      then
                        let mvbdu_list =
                          Ckappa_sig.Views_intbdu.extensional_of_mvbdu mvbdu
                        in
                        List.fold_left
                          (fun (error, transition_system) asso ->
                             add_node parameter error
                               (Ckappa_sig.Views_intbdu.mvbdu_of_association_list
                                  asso)
                               transition_system
                          )
                          (error, transition_system)
                          mvbdu_list
                      else
                        error, transition_system
                    in
                    let error, transition_system =
                      if
                        Remanent_parameters.get_add_singular_macrostates
                          parameter
                      then
                        List.fold_left
                          (fun (error, transition_system) (_,hconsed) ->
                             let length = Ckappa_sig.Views_intbdu.nbr_variables hconsed in
                             let list_var = Ckappa_sig.Views_intbdu.extensional_of_variables_list hconsed in
                             List.fold_left
                               (fun (error, transition_system) (_,hconsed') ->

                                  let length' = Ckappa_sig.Views_intbdu.nbr_variables hconsed' in
                                  let hconsed'' =
                                    Ckappa_sig.Views_intbdu.merge_variables_lists hconsed hconsed'
                                  in
                                  let length'' =
                                    Ckappa_sig.Views_intbdu.nbr_variables hconsed''
                                  in
                                  if length' = length'' || length = length''
                                  then
                                    (error, transition_system)
                                  else
                                    let list_var' = Ckappa_sig.Views_intbdu.extensional_of_variables_list hconsed' in
                                    let lista,listb,listc = distrib_list list_var list_var' in
                                    List.fold_left
                                      (fun (error, transition_system) list_var ->
                                         let restricted_mvbdu = Ckappa_sig.Views_intbdu.mvbdu_project_keep_only mvbdu hconsed in
                                         let mvbdu_list =
                                           Ckappa_sig.Views_intbdu.extensional_of_mvbdu restricted_mvbdu
                                         in
                                         List.fold_left
                                           (fun (error, transition_system) asso ->
                                              add_node parameter
                                                error
                                                (Ckappa_sig.Views_intbdu.mvbdu_of_association_list asso)
                                                transition_system
                                           )
                                           (error, transition_system)
                                           mvbdu_list)
                                      (error, transition_system)
                                      [lista;listb;listc]

                               )
                               (error, transition_system)
                               proof
                          )
                          (error, transition_system)
                          proof'
                      else
                        error, transition_system
                    in
                    error, transition_system
                 )
                 (error, transition_system) tail
             in
             aux map2 tail (error, transition_system)
         in
         aux map2 proof_list (error, transition_system))
      ambiguous_node
      (error, transition_system)


let merge_neighbour parameter error concurrent_sites =
  LabelMap.fold
    (fun label (sites_in_conflict, _hconsed) (error, map) ->
       let error, set, _list =
         LabelMap.fold
           (fun label' (sites_in_conflict', _hconsed') (error, set, list) ->
              let error, diff1 = SiteSet.minus parameter error sites_in_conflict sites_in_conflict' in
              let error, diff2 = SiteSet.minus parameter error sites_in_conflict' sites_in_conflict in
              if SiteSet.is_singleton diff1 && SiteSet.is_singleton diff2
              then
                let a1 = SiteSet.min_elt diff1 in
                let a2 = SiteSet.min_elt diff2 in
                match a1,a2
                with
                | Some a1, Some a2 ->
                  let error, set = SitePairSet.add parameter error (a1,a2) set in
                  error, set, (label'::list)
                | None, _ | _, None ->
                  let error, set =
                    warn parameter error (Some "line 477") Exit set
                  in
                  error, set, list
              else
                error, set, list)
           concurrent_sites
           (error, SitePairSet.empty,[])
       in
       if SitePairSet.is_singleton set
       then
         begin
           match SitePairSet.min_elt set
           with
           | None ->
             warn parameter error (Some "line 494") Exit map
           | Some (a1,_a2) ->
             let error, sites_in_conflict = SiteSet.remove parameter error a1 sites_in_conflict in
             let ext_list = SiteSet.elements sites_in_conflict in
             let hconsed =
               Ckappa_sig.Views_intbdu.build_variables_list ext_list
             in
               LabelMap.overwrite parameter error label (sites_in_conflict,hconsed) map
         end
       else
       error, map)
    concurrent_sites
    (error, concurrent_sites)

let concurrent_sites parameter error mvbdu support =
  if Remanent_parameters.get_use_macrotransitions_in_local_traces parameter
  then
    let error, concurrent = concurrent_sites parameter error mvbdu support in
    if Remanent_parameters.get_ignore_local_losanges parameter
    then
      merge_neighbour parameter error concurrent
    else
      error, concurrent
  else
    trivial_concurrent_sites parameter error mvbdu support

let add key im map =
  let old_set = Mods.IntMap.find_default Mods.IntSet.empty key map in
  Mods.IntMap.add key (Mods.IntSet.add im old_set) map

let addsub small big transition_system =
  {
    transition_system
    with
      subframe = add small big transition_system.subframe;
    }

let add_whether_subframe parameter error mvbdu1 mvbdu2 transition_system =
  if
    Ckappa_sig.Views_intbdu.equal mvbdu1 mvbdu2
  then
    error, transition_system
  else
    let mvbdu_lub = Ckappa_sig.Views_intbdu.mvbdu_or mvbdu1 mvbdu2 in
    if Ckappa_sig.Views_intbdu.equal mvbdu_lub mvbdu2 then
      let error, hash1 = hash_of_mvbdu parameter error mvbdu1 in
      let error, hash2 = hash_of_mvbdu parameter error mvbdu2 in
      error, addsub hash1 hash2 transition_system
    else
      error, transition_system

let check_all_subframes parameter error transition_system =
  List.fold_left
    (fun (error, transition_system) mvbdu1 ->
       List.fold_left
         (fun (error, transition_system) mvbdu2 ->
            add_whether_subframe parameter error mvbdu1 mvbdu2 transition_system
         )
         (error, transition_system)
         transition_system.nodes
    )
    (error, transition_system)
    transition_system.nodes

let reduce_subframes transition_system =
  {
    transition_system
      with
        subframe =
          Mods.IntMap.map
            (fun set ->
               Mods.IntSet.filter
                 (fun i ->
                    Mods.IntSet.for_all
                      (fun j ->
                         not (Mods.IntSet.mem i (Mods.IntMap.find_default
                                                   Mods.IntSet.empty j transition_system.subframe)))
                      set)
                 set)
            transition_system.subframe
  }


let print logger parameter compil handler_kappa handler error transition_system =
  let () = Graph_loggers.print_graph_preamble logger transition_system.title in
  (* nodes -> Initial *)
  let error,handler =
    List.fold_left
      (fun (error, handler) (mvbdu,_label) ->
       let error, key = hash_of_mvbdu parameter error mvbdu in
       let () =
         Graph_loggers.print_node logger
           ("Init_"^(string_of_int key))
           ~directives:
         [
           Graph_loggers.Width 0;
           Graph_loggers.Height 0;
           Graph_loggers.Shape Graph_loggers.Invisible;
           Graph_loggers.Label ""
         ]
       in
       error, handler)
    (error, handler)
    transition_system.nodes_creation
  in
  (* nodes -> regular *)
  let error =
    List.fold_left
      (fun error ->
         dump_mvbdu logger parameter error handler_kappa transition_system)
      error
      transition_system.nodes
  in
  (* edges  *)
  let error, handler =
    List.fold_left
      (fun (error, handler) (q,label,q') ->
         let error, key = hash_of_mvbdu parameter
             error q
         in
         let error, key' = hash_of_mvbdu parameter error q' in
         let error, rule_name =
           if  Remanent_parameters.get_show_rule_names_in_local_traces parameter
           then
             begin
               match
                 fst label
               with
               | Rule r ->
                 Handler.string_of_rule
                   parameter error handler_kappa compil r
               | Init _ ->
                 warn parameter error (Some "line 1412") Exit ""
             end
           else
             error, ""
         in
         let () =
           Graph_loggers.print_edge
             logger
             ("Node_"^(string_of_int key))
             ("Node_"^(string_of_int key'))
             ~directives:
               [
                 Graph_loggers.Label rule_name
               ]
         in
         error,handler)
      (error,handler)
      transition_system.edges
  in
let error,_ =
  List.fold_left
    (fun (error, handler) (q,label) ->
       let error, key = hash_of_mvbdu parameter error q in
       let error, rule_name =
         if
           Remanent_parameters.get_show_rule_names_in_local_traces
             parameter
         then
           begin
             match
               fst label
             with
             | Rule r ->
               Handler.string_of_rule
                 parameter error handler_kappa compil r
             | Init _ -> warn parameter error (Some "line 1412") Exit ""
           end
         else
           error, ""
       in
       let () =
         Graph_loggers.print_edge
           logger
           ("Init_"^(string_of_int key))
           ("Node_"^(string_of_int key))
           ~directives:
             [
               Graph_loggers.Label rule_name ;
             ]
       in
       error, handler)
    (error,handler)
    transition_system.nodes_creation
in
let () =
  Mods.IntMap.iter
    (fun key l ->
       if Mods.IntSet.is_empty l
       then
         ()
       else
         let k = "Node_"^(string_of_int key) in
         let l = List.rev (Mods.IntSet.fold (fun i list -> ("Node_"^(string_of_int i))::list) l []) in
         Graph_loggers.print_one_to_n_relation logger ~style_one:Graph_loggers.Dotted ~style_n:Graph_loggers.Dashed k l)
    transition_system.subframe
in
let () = Graph_loggers.print_graph_foot logger in
error

let agent_trace parameter log_info error handler handler_kappa compil output =
  let () = Ckappa_sig.Views_intbdu.import_handler handler in
  let rules = compil.Cckappa_sig.rules in
  let init = compil.Cckappa_sig.init in
  let error, (support, creation) =
    build_support parameter error rules
  in
  let error, init =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error i_id init init_map ->
       let mixture = init.Cckappa_sig.e_init_c_mixture in
       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
         parameter error
         (fun parameter error ag_id view init_map ->
            match
              view
            with
            | Cckappa_sig.Agent agent ->
              let error, old_list =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs parameter error [] agent.Cckappa_sig.agent_name init_map
              in
              let error, asso =
                asso_of_view_in_map
                  parameter error agent.Cckappa_sig.agent_interface
              in
              let error, new_map =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
                  parameter error agent.Cckappa_sig.agent_name
                  ((((Init i_id),ag_id),asso)::old_list) init_map
              in
              error, (new_map)
            | Cckappa_sig.Ghost
            | Cckappa_sig.Dead_agent _
            | Cckappa_sig.Unknown_agent _ ->
              	     warn parameter error (Some "line 948") Exit init_map
         )
         mixture.Cckappa_sig.views
         init_map)
      init
      creation
  in
  let empty = Ckappa_sig.Views_intbdu.build_variables_list [] in
  let error, log_info =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
      parameter
      error
      (fun parameter error agent_type map log_info ->
         let error, support =
           Ckappa_sig.Agent_map_and_set.Map.find_default parameter error
             LabelMap.empty agent_type support
         in
         let error', agent_string =
           try
             Handler.string_of_agent parameter error handler_kappa agent_type
           with
           | _ -> warn parameter error (Some "line 111") Exit
                    (Ckappa_sig.string_of_agent_name agent_type)
         in
         let error =
           Exception.check warn parameter error error' (Some "line 1917") Exit
         in
         Wrapped_modules.LoggedIntMap.fold
           (fun _ mvbdu (error, log_info) ->
              try
                begin
                  let sites =
                    Ckappa_sig.Views_intbdu.variables_list_of_mvbdu
                      mvbdu
                  in
                  let ext_list =
                    Ckappa_sig.Views_intbdu.extensional_of_variables_list
                      sites
                  in
                  let def_list =
                    List.rev_map
                      (fun i -> (i,Ckappa_sig.state_index_of_int 0))
                      ext_list
                  in
                  let mvbdu_default_value =
                    mvbdu_of_reverse_order_association_list
                      def_list
                  in
                  let error, site_set =
                    List.fold_left
                      (fun (error, set) site ->
                         SiteSet.add parameter error site set)
                      (error, SiteSet.empty)
                      ext_list
                  in
                  let error, support =
                    LabelMap.fold
                      (fun label (test, asso_test, modif, asso_modif)
                        (error, map) ->
                        let error, test =
                          SiteSet.inter parameter error test site_set
                        in
                        let asso_test =
                          Ckappa_sig.Views_intbdu.mvbdu_project_keep_only
                            asso_test sites
                        in
                        let error, modif =
                          SiteSet.inter parameter error modif site_set
                        in
                        if SiteSet.is_empty modif
                        then   error, map
                        else
                          let asso_modif =
                            Ckappa_sig.Views_intbdu.extensional_of_association_list
                              asso_modif
                          in
                          let asso_modif =
                            List.filter
                              (fun (a,_) -> SiteSet.mem a site_set) asso_modif
                          in
                          let asso_modif =
                            Ckappa_sig.Views_intbdu.build_association_list
                              asso_modif
                          in
                          let error, map =
                            LabelMap.add parameter error label (test,asso_test,modif,asso_modif) map
                          in
                          error, map)
                      support
                      (error, LabelMap.empty)
                  in
                  let error, concurrent_sites =
                    concurrent_sites parameter error mvbdu support
                  in
                  let error, concurrent_sites =
                    merge_neighbour parameter error concurrent_sites
                  in
                  let error, file_name =
                    List.fold_left
                      (fun (error, string) site ->
                         let error, site_string =
                           Handler.string_of_site_in_file_name
                             parameter error handler_kappa agent_type site
                         in
                         error, string^"."^site_string)
                      (error,
                       ((Remanent_parameters.get_local_trace_directory
                           parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
                      ext_list
                  in
                  let transition_system = empty_transition_system file_name agent_string agent_type in
                  let file_name =
                    file_name^(Remanent_parameters.ext_format
                                 (Remanent_parameters.get_local_trace_format
                                  parameter))
                  in
                  let fic = Remanent_parameters.open_out file_name in
                  let error, init_list =
                    Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                      parameter error [] agent_type init
                  in
                  let error, transition_system =
                (* initial/creation states *)
                    List.fold_left
                      (fun (error, transition_system) ((i_id,ag_id),asso)
                        ->
                          let update =
                            restrict_asso asso site_set
                          in
                          let mvbdu =
                            Ckappa_sig.Views_intbdu.mvbdu_redefine
                              mvbdu_default_value update
                          in
                          let error, transition_system =
                            add_creation
                              parameter error
                              i_id ag_id mvbdu transition_system
                          in
                          error, transition_system)
                      (error, transition_system)
                      init_list
                  in
                  let error, transition_system =
	  (* regular transitions *)
                    LabelMap.fold
                      (fun label (_test,asso,modif,asso_modif) (error, transition_system)
                    ->
                      if SiteSet.is_empty modif
                      then
                        error, transition_system
                      else
                        let error, concurrent_site =
                          LabelMap.find_default
                            parameter error (SiteSet.empty,empty) label
                            concurrent_sites
                        in
                        let mvbdu =
                          Ckappa_sig.Views_intbdu.mvbdu_project_abstract_away
                            mvbdu (snd concurrent_site)
                        in
                        let mvbdu =
                          Ckappa_sig.Views_intbdu.mvbdu_and
                            mvbdu asso
                        in
                        let pre =
                          Ckappa_sig.Views_intbdu.extensional_of_mvbdu
                            mvbdu
                        in
                        let error, transition_system =
                          List.fold_left
                            (fun (error, transition_system) list ->
                               let pre =
                                 mvbdu_of_association_list
                                   list
                               in
                               let post =
                                 Ckappa_sig.Views_intbdu.mvbdu_redefine
                                   pre asso_modif
                               in
                               let error, transition_system =
                                 add_node parameter error pre
                                   transition_system
                               in
                               let error, transition_system =
                                 add_node parameter error post
                                  transition_system

                              in
                              let transition_system =
                                add_edge pre post label transition_system
                              in
                              error, transition_system)
                            (error, transition_system)
                            pre
                        in
                        error, transition_system)
                  support
                  (error, transition_system)
                  in
                  let error, transition_system =
                    if Remanent_parameters.get_add_singular_macrostates
                        parameter
                       ||
                       Remanent_parameters.get_add_singular_microstates parameter
                    then
                      add_singular parameter error transition_system
                    else
                      error, transition_system
                  in
                  let error, transition_system =
                    check_all_subframes parameter error transition_system
                  in
                  let transition_system =
                    reduce_subframes transition_system
                  in
                  let format =
                    match Remanent_parameters.get_local_trace_format parameter with
                    | Remanent_parameters_sig.DOT -> Loggers.DOT
                    | Remanent_parameters_sig.HTML -> Loggers.HTML_Graph
                  in
                  let logger =
                    Loggers.open_logger_from_channel fic
                      ~mode:format
                  in
                  let error = print logger parameter compil handler_kappa () error transition_system in
                  let _ = close_out fic in
                  error, log_info
            end
          with Sys.Break -> error, log_info
       )
       map
       (error, log_info))
       output
       log_info
  in
  match
    Ckappa_sig.Views_intbdu.export_handler error
  with
  | error, Some h -> error, log_info, h
  | error, None ->
    let error, h = warn parameter error (Some "line 813") Exit handler in
    error, log_info, h
