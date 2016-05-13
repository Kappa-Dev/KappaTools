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
module SiteSet = Site.Set

type extensional_representation =
  {
    nodes: Ckappa_sig.Views_intbdu.mvbdu list;
    edges: (Ckappa_sig.Views_intbdu.mvbdu * label * Ckappa_sig.Views_intbdu.mvbdu) list ;
    nodes_creation: (Ckappa_sig.Views_intbdu.mvbdu * label) list ;
    nodes_degradation: (Ckappa_sig.Views_intbdu.mvbdu * label) list ;
    macro_state_to_state: int list Wrapped_modules.LoggedIntMap.t ;
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

let empty_transition_system =
  {
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [] ;
    macro_state_to_state = Wrapped_modules.LoggedIntMap.empty;
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

let dump_edge fic parameter error handler_kappa compil key key' label =
  let error, rule_name =
    if Remanent_parameters.get_show_rule_names_in_local_traces parameter
    then
      Handler.string_of_rule parameter error handler_kappa compil (fst label)
    else error,""
  in
  let () = Printf.fprintf fic "Node_%i -> Node_%i [label=\"%s\"];\n" key key' rule_name in
  error


let dump_key_of_asso fic list =
  let hash = hash_of_association_list list in
  let () = Printf.fprintf fic "Node_%i" hash in
  ()

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
         let error, site_string =
           Handler.string_of_site parameter error handler_kappa agent_type site_type
         in
         let error', state_string =
           Handler.string_of_state_fully_deciphered parameter error
             handler_kappa agent_type site_type state
         in
         let error =
           Exception.check warn parameter error error'
             (Some "line 240") Exit
         in
         let () =
           Printf.fprintf fic "%s%s" site_string state_string
         in
         error,true
      )
      (error,false) list
  in
  let () = Printf.fprintf fic ")" in
  error

let dump_mvbdu fic parameter error handler_kappa agent_type agent_string mvbdu =
   let error, list = build_asso_of_mvbdu parameter error mvbdu in
   let () = dump_key_of_asso fic list in
   let () = Printf.fprintf fic " [label=\"" in
   let error = print_label_of_asso fic parameter error handler_kappa agent_type agent_string list in
   let () = Printf.fprintf fic "\"];\n" in
   error

let add_node_from_mvbdu parameter handler_kappa error agent_type agent_string mvbdu transition_system =
  error,
  {
    transition_system
    with
      nodes = mvbdu::transition_system.nodes}

let dump_graph_header fic =
   Printf.fprintf fic "digraph G{\n"

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
    (fun label (test,asso,act,asso_modif) (error, map')->
      let error, sites_in_conflict =
        LabelMap.fold
          (fun label' (test,asso',act,asso_modif') (error, accu) ->
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


let agent_trace parameter error handler handler_kappa mvbdu_true compil output =
  (*let saved_handler = ref handler in
  let restore_handler () = !saved_handler int_pair_of_label in
    let save_handler handler = saved_handler:=handler in*)
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
  let error =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.iter
      parameter
      error
      (fun parameter error agent_type map  ->
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
           (fun _ mvbdu (error:Exception.method_handler) ->
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
                            LabelMap.add parameter error label                                                       (test,asso_test,modif,asso_modif) map
                          in
                          error, map)
                      support
                      (error, LabelMap.empty)
                  in
                  let error, concurrent_sites =
                    concurrent_sites parameter error mvbdu support
                  in
                  let error, file_name =
                    List.fold_left
                      (fun (error, string) site ->
                         let error, site_string =
                           Handler.string_of_site
                             parameter error handler_kappa agent_type site
                         in
                         error, string^"_"^site_string)
                      (error,
                       ((Remanent_parameters.get_local_trace_directory
                           parameter)^(Remanent_parameters.get_local_trace_prefix parameter)^(agent_string)))
                      ext_list
                  in
                  let transition_system = empty_transition_system in
                  let file_name =
                    file_name^(Remanent_parameters.ext_format
                                 (Remanent_parameters.get_local_trace_format
                                  parameter))
                  in
                  let fic = Remanent_parameters.open_out file_name in
                  let () = dump_graph_header fic in
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
                      (fun label (test,asso,modif,asso_modif) (error, transition_system)
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
              (* nodes -> Initial *)
              let error,handler =
                List.fold_left
                  (fun (  error, handler) (mvbdu,label) ->
                     let error, key = hash_of_mvbdu parameter error mvbdu in
                     let () =
                       Printf.fprintf fic
                         "Init_%i [width=\"0cm\" height=\"0cm\" style=\"none\" label=\"\"];\n" key
                     in
                     error, handler)
                  (error, handler)
                  transition_system.nodes_creation
              in
              (* nodes -> regular *)
              let error =
                List.fold_left
                  (fun error ->
                     dump_mvbdu fic parameter error handler_kappa
                       agent_type agent_string)
                  error
                  transition_system.nodes
              in
              (* macro_steps *)
              let error =
                Wrapped_modules.LoggedIntMap.fold
                  (fun k _ error ->
                     let () =
                       Printf.fprintf fic
                         "Macro_%i [width=\"0cm\" height=\"0cm\" stype=\"none\" label=\"\"];\n" k
                     in error)
                  transition_system.macro_state_to_state
                  error
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
                           | _ ->
                             warn parameter error (Some "line 1412") Exit ""
                         end
                       else
                         error, ""
                     in
                     let () =
                       Printf.fprintf fic
                         "Node_%i -> Node_%i [label=\"%s\"];\n" key
                         key' rule_name
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
                           | _ -> warn parameter error (Some "line 1412") Exit ""
                         end
                       else
                         error, ""
                     in
                     let () =
                       Printf.fprintf fic "Init_%i -> Node_%i [label=\"%s\"];\n" key key rule_name
                     in
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
                       let () =
                         Printf.fprintf fic "Macro_%i -> Node_%i [style=\"dotted\" label=\"\"];\n" key key
                       in
                       List.iter
                         (fun h ->
                            Printf.fprintf fic "Macro_%i -> Node_%i [style=\"dashed\" label=\"\"];\n"
                              key h ) l)
                  transition_system.macro_state_to_state
              in
              let _ = Printf.fprintf fic "}\n" in
              let _ = close_out fic in
              error
            end
          with ExceptionDefn_with_no_dep.UserInterrupted _ ->
            error
       )
       map
       error)
      output
  in
  match
    Ckappa_sig.Views_intbdu.export_handler error
  with
  | error, Some h -> error, h
  | error, None ->
    warn parameter error (Some "line 813") Exit handler
