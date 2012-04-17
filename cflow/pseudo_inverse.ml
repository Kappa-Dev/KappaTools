 (**
   * pseudo_inverse.ml 
   *
   * Cut pseudo inverse events: a module for KaSim 
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * Jean Krivine, Université Paris-Diderot, CNRS 
   * 
   * KaSim
   * Jean Krivine, Université Paris Dederot, CNRS 
   *  
   * Creation: 17/04/2012
   * Last modification: 17/04/2012
   * * 
   * Some parameter references can be tuned thanks to command-line options
   * other variables has to be set before compilation   
   *  
   * Copyright 2011,2012 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)



 module type Cut_pseudo_inverse =
   sig
     module Po:Po_cut.Po_cut 
     module A:LargeArray.GenArray 

     val cut: (Po.K.refined_step list -> Po.K.H.error_channel * Po.K.refined_step list * int) Po.K.H.with_handler 
   end

 module Po_cut = 
   (struct 

     module Po=Po_cut.Po_cut 
     module A = Mods.DynArray 

     type predicate_info = 
       | Here of Po.K.agent_id  
       | Bound_site of Po.K.agent_id * Po.K.site_name
       | Internal_state of Po.K.agent_id * Po.K.site_name 
           
     module PredicateMap = Map.Make (struct type t = predicate_info let compare = compare end) 

     type step_id = int 

     type predicate_value = 
       | Internal_state_is of Po.K.internal_state
       | Undefined (** the wire does not exist yet *)
       | Present   (** for agent presence *)
       | Free      (** for binding sites *)
       | Bound_to of Po.K.agent_id * Po.K.agent_name * Po.K.site_name   (** for binding sites *)
           
     type pseudo_inv_blackboard = 
      {
        pseudo_steps_by_column: (step_id * predicate_value * bool) list PredicateMap.t ;
        pseudo_nsteps: step_id ; 
        pseudo_predicates_of_event: predicate_info  list A.t ;
        pseudo_is_remove_action: bool A.t ;
        pseudo_event: (Po.K.refined_step) option A.t; 
        pseudo_predicate_id_list_related_to_predicate_id: (predicate_info list) PredicateMap.t ; 
      }

     let predicates_of_action parameter handler error blackboard action = 
       match action with 
         | Po.K.Create (ag,interface) -> 
           let ag_id = Po.K.agent_id_of_agent ag in
           let predicate_id = Here ag_id in   
           let list1,list2 = 
             List.fold_left 
               (fun (list1,list2) (s_id,opt) -> 
                 let predicate_id = Bound_site(ag_id,s_id) in 
                 let list1 = (predicate_id,Free)::list1 in
                 let list2 = predicate_id::list2 in 
                 match opt 
                 with 
                   | None -> list1,list2
                   | Some x -> 
                     let predicate_id = Internal_state (ag_id,s_id) in 
                     (predicate_id,Internal_state_is x)::list1,
                     predicate_id::list2
               )
               ([predicate_id,Present],[predicate_id])
               interface
           in 
           list1,list2,false
         | Po.K.Mod_internal (site,int)  -> 
           let predicate_id = Internal_state (Po.K.agent_id_of_site site,Po.K.site_name_of_site site) in 
           [predicate_id,Internal_state_is int],[],false
         | Po.K.Bind_to (s1,s2) -> 
           let ag_id1 = Po.K.agent_id_of_site s1 in 
           let ag_id2 = Po.K.agent_id_of_site s2 in 
           let agent_name2 = Po.K.agent_name_of_site s2 in 
           let site_id1 = Po.K.site_name_of_site s1 in 
           let site_id2 = Po.K.site_name_of_site s2 in 
           let predicate_id1 = Bound_site (ag_id1,site_id1) in 
           [predicate_id1,Bound_to (ag_id2,agent_name2,site_id2)],[],false
         | Po.K.Bind (s1,s2) -> 
           let ag_id1 = Po.K.agent_id_of_site s1 in 
           let ag_id2 = Po.K.agent_id_of_site s2 in 
           let agent_name1 = Po.K.agent_name_of_site s1 in 
           let agent_name2 = Po.K.agent_name_of_site s2 in 
           let site_id1 = Po.K.site_name_of_site s1 in 
           let site_id2 = Po.K.site_name_of_site s2 in 
           let predicate_id1 = Bound_site (ag_id1,site_id1) in 
           let predicate_id2 = Bound_site (ag_id2,site_id2) in 
           [predicate_id1,Bound_to (ag_id2,agent_name2,site_id2);
            predicate_id2,Bound_to (ag_id1,agent_name1,site_id1)],[],false
         | Po.K.Unbind (s1,s2) ->
           let ag_id1 = Po.K.agent_id_of_site s1 in 
           let ag_id2 = Po.K.agent_id_of_site s2 in 
           let site_id1 = Po.K.site_name_of_site s1 in 
           let site_id2 = Po.K.site_name_of_site s2 in 
           let predicate_id1 = Bound_site (ag_id1,site_id1) in 
           let predicate_id2 = Bound_site (ag_id2,site_id2) in 
           [predicate_id1,Free;predicate_id2,Free],[],false
         | Po.K.Free s -> 
           let ag_id = Po.K.agent_id_of_site s in 
           let site_id = Po.K.site_name_of_site s in 
           let predicate_id = Bound_site (ag_id,site_id) in     
           [predicate_id,Free],[],false
         | Po.K.Remove ag -> 
           let ag_id = Po.K.agent_id_of_agent ag in 
           let predicate_id = Here ag_id in 
           let set = 
             try 
               PredicateMap.find
                 predicate_id 
                 blackboard.pseudo_predicate_id_list_related_to_predicate_id
             with 
               | Not_found -> [] 
           in 
           let list = 
             List.fold_left 
               (fun list predicateid -> 
                 (predicateid,Undefined)::list)
               ([predicate_id,Undefined])
               set 
           in   
           list,[],true

     let no_remove parameter handler error blackboard eid = 
       try 
         A.get blackboard.pseudo_is_remove_action eid 
       with 
         | Not_found -> false 

     let n_actions list = 
       let rec aux l k = 
         match l 
         with 
           | [] -> k 
           | (_,_,true)::q ->  aux q (k+1)
           | (_,_,false)::q -> aux q k 
       in aux list 0 
               
     let same_length parameter handler error blackboard eid1 eid2 = 
       n_actions eid1 = n_actions eid2 

     let check pid eid1 eid2 blackboard = true 
       
     let predicates_of_test parameter handler error blackboard test = 
       match test
       with 
         | Po.K.Is_Here (agent) ->
           let ag_id = Po.K.agent_id_of_agent agent in 
           let predicate_id = Here ag_id in 
           [predicate_id]
         | Po.K.Has_Internal(site,int) -> 
           let predicate_id = Internal_state (Po.K.agent_id_of_site site,Po.K.site_name_of_site site) in 
           [predicate_id]
         | Po.K.Is_Free s -> 
           let ag_id = Po.K.agent_id_of_site s in 
           let site_id = Po.K.site_name_of_site s in 
           let predicate_id = Bound_site (ag_id,site_id) in     
           [predicate_id]
         | Po.K.Is_Bound_to  (s1,s2) -> 
           let ag_id1 = Po.K.agent_id_of_site s1 in 
           let ag_id2 = Po.K.agent_id_of_site s2 in 
           let agent_name1 = Po.K.agent_name_of_site s1 in 
           let agent_name2 = Po.K.agent_name_of_site s2 in 
           let site_id1 = Po.K.site_name_of_site s1 in 
           let site_id2 = Po.K.site_name_of_site s2 in 
           let predicate_id1 = Bound_site (ag_id1,site_id1) in 
           let predicate_id2 = Bound_site (ag_id2,site_id2) in 
           [predicate_id1;predicate_id2]
         | Po.K.Is_Bound s -> 
           let ag_id = Po.K.agent_id_of_site s in 
           let site_id = Po.K.site_name_of_site s in 
           let predicate_id = Bound_site (ag_id,site_id) in 
           [predicate_id]   
         | Po.K.Has_Binding_type (s,btype) ->
           let ag_id = Po.K.agent_id_of_site s in 
           let site_id = Po.K.site_name_of_site s in
           let agent_name = Po.K.agent_of_binding_type btype in 
           let site_name = Po.K.site_of_binding_type btype in 
           let predicate_id = Bound_site (ag_id,site_id) in 
           [predicate_id]

  

  let add_step parameter handler error step blackboard = 
    let pre_event = blackboard.pseudo_event in 
    let test_list = Po.K.tests_of_refined_step step in 
    let action_list,side_effect = Po.K.actions_of_refined_step step in
    let build_map list map = 
      List.fold_left 
        (fun map (id,value) -> PredicateMap.add id value map)
        map 
        list 
    in 
    let build_map_test list map = 
      List.fold_left 
        (fun map id -> PredicateMap.add id true map )
        map 
        list
    in 
    let add_state pid (test,action) map = 
      let test',action' = 
        try 
          PredicateMap.find pid map  
        with 
          | Not_found -> false,None 
      in 
      let test = test or test' in 
      let action = 
        match action' 
        with 
          | None -> action 
          | _ -> action' 
      in 
      let map = PredicateMap.add pid (test,action) map in 
      map 
    in 
    let unambiguous_side_effects = [] in (* TO DO *)
      
    let test_map = 
      List.fold_left 
        (fun map test -> 
          let test_list = predicates_of_test parameter handler error blackboard test in
          build_map_test test_list map)
        PredicateMap.empty
        test_list in 
    let error,blackboard,action_map,test_map,is_remove_action = 
      List.fold_left 
        (fun (error,blackboard,action_map,test_map,bool) action -> 
          let action_list,test_list,bool' = predicates_of_action parameter handler error blackboard action in 
          error,blackboard,build_map action_list action_map,build_map_test test_list test_map,bool or bool')
        (error,blackboard,PredicateMap.empty,test_map,false)
        (action_list) in 
    let merged_map = 
      PredicateMap.merge 
        (fun _ test action -> 
          let test = 
            match test 
            with 
              | None -> false 
              | Some x -> x 
          in Some (test,action))
        test_map
        action_map 
    in 
    let merged_map = 
      List.fold_left 
        (fun map (pid,_,(test,action)) -> 
          add_state pid ((test:bool),action) map)
        merged_map
        unambiguous_side_effects
    in 
    let side_effect = 
      List.fold_left 
        (fun list (_,a,_) -> 
          match a 
          with 
            | None -> list
            | Some a -> a::list)
        []
        unambiguous_side_effects 
    in 
    let nsid = blackboard.pseudo_nsteps + 1 in 
    let _ = A.set blackboard.pseudo_event nsid (Some step) in 
    let pre_steps_by_column,list  = 
      PredicateMap.fold 
        (fun id (test,action) (map,list) -> 
          begin 
            let old_list = 
              try 
                PredicateMap.find id map 
              with 
                | Not_found -> [] 
            in 
            let old_value = 
              match 
                old_list
              with 
                | (_,v,_)::_ -> v
                | [] -> Undefined 
            in 
            let new_value = 
              match action 
              with 
                | None -> old_value 
                | Some i -> i 
            in 
            let bool_action = 
              match action
              with 
                | None -> false
                | Some _ -> true 
            in 
            (PredicateMap.add id ((nsid,new_value,bool_action)::old_list) map,
             (id,new_value)::list)
          end)
        merged_map
        (blackboard.pseudo_steps_by_column,[])
    in 
    let pseudo_is_remove_action = 
      if is_remove_action 
      then 
        let _ = A.set blackboard.pseudo_is_remove_action nsid true in () 
    in 
    let pseudo_predicates_of_event = A.set blackboard.pseudo_predicates_of_event nsid (List.rev_map fst (List.rev list)) in 
    let blackboard = 
      { 
        blackboard with 
          pseudo_event = pre_event ;
          pseudo_steps_by_column = pre_steps_by_column; 
          pseudo_nsteps = nsid;
      }
    in 
    error,blackboard

  let cut parameter handler error list = error,list,0 
 
    
    end:Cut_pseudo_inverse)
