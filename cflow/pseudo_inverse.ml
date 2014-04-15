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
   * Last modification: 21/11/2013
   * * 
   * Some parameter references can be tuned thanks to command-line options
   * other variables has to be set before compilation   
   *  
   * Copyright 2011,2012, 2013 Institut National de Recherche en Informatique 
   * et en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)



 module type Cut_pseudo_inverse =
   sig
     module Po:Po_cut.Po_cut 
     module A:LargeArray.GenArray 

     val cut: (Po.K.refined_step list -> Po.K.H.error_channel * ((Po.K.refined_step*bool) list) * int) Po.K.H.with_handler 
     val do_not_cut: (Po.K.refined_step list -> Po.K.H.error_channel * ((Po.K.refined_step*bool) list) * int) Po.K.H.with_handler 
   end

 module Pseudo_inv = 
   (struct 

     module Po=Po_cut.Po_cut 
     module A = Mods.DynArray 

     type predicate_info = 
       | Here of Po.K.agent_id  
       | Bound_site of Po.K.agent_id * Po.K.site_name
       | Internal_state of Po.K.agent_id * Po.K.site_name 

     let string_of_predicate_info pi = 
       match 
         pi 
       with 
         | Here ag -> "Here "^(string_of_int ag)
         | Bound_site (ag,s) -> "Bound_state "^(string_of_int ag)^" "^(string_of_int s)
         | Internal_state (ag,s) -> "Internal_state "^(string_of_int ag)^" "^(string_of_int s)
           
     module PredicateMap = Map.Make (struct type t = predicate_info let compare = compare end) 

     type step_id = int 

     type predicate_value = 
       | Internal_state_is of Po.K.internal_state
       | Undefined (** the wire does not exist yet *)
       | Present   (** for agent presence *)
       | Free      (** for binding sites *)
        | Bound_to of Po.K.agent_id * Po.K.agent_name * Po.K.site_name   (** for binding sites *)

      let string_of_predicate_value pi = 
        match 
          pi 
        with
          | Internal_state_is s -> (string_of_int s)
          | Undefined -> "#Undef" 
          | Present -> "#Here"
          | Free -> "#Free" 
          | Bound_to (ag,ag_name,s) -> 
            "Bound_to "^(string_of_int ag)^" "^(string_of_int ag_name)^" "^(string_of_int s)

      type pseudo_inv_blackboard = 
       {
         steps_by_column: (step_id * predicate_value * bool) list PredicateMap.t ;
         init_state: predicate_value PredicateMap.t ; 
         nsteps: step_id ; 
         predicates_of_event: predicate_info  list A.t ;
         is_remove_action: bool A.t ;
         weak_actions: step_id list;
         modified_predicates_of_event: int A.t ;
         event: (Po.K.refined_step (** step_id list*)) option A.t; 
         predicate_id_list_related_to_predicate_id: (predicate_info list) PredicateMap.t ; 
       }

      let init_blackboard n = 
        {
          init_state = PredicateMap.empty ; 
          weak_actions= []; 
          steps_by_column = PredicateMap.empty ; 
          nsteps = -1 ; 
          predicates_of_event = A.create  n [] ;
          is_remove_action = A.create n false ;
          modified_predicates_of_event = A.create n 0 ; 
          event = A.create n None ; 
          predicate_id_list_related_to_predicate_id = PredicateMap.empty ; 
       }


      let print_blackboard parameter handler error blackboard = 
        let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "Blackboard for removing pseudo inverse element\n" in 
        let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "n_events: %i\n" blackboard.nsteps in 
        let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "Steps_by_column:\n" in 
        let _ = 
          PredicateMap.iter 
            (fun pred list -> 
              let _ = 
                Printf.fprintf parameter.Po.K.H.out_channel_err "%s: " (string_of_predicate_info pred)
              in 
              let _ = 
                List.iter 
                  (fun (eid,value,bool) -> 
                    Printf.fprintf parameter.Po.K.H.out_channel_err "(%i,%s%s)," eid (string_of_predicate_value value) (if bool then "(Mod)" else ""))
                  list 
              in 
              Printf.fprintf parameter.Po.K.H.out_channel_err "\n")
            blackboard.steps_by_column 
        in 
        let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "Events:\n" in 
        let rec aux k = 
          if k=blackboard.nsteps 
          then error
          else 
            let event = 
              try 
                A.get blackboard.event k 
              with _ -> 
                let _ = Printf.fprintf stderr "ERREUR %i 123\n" k in 
                raise Exit 
            in 
            let _ = 
              match 
                event 
              with 
                | None -> error
                | Some event -> 
                  begin 
                    try 
                      let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "Event %i\n" k in 
                      let error  = Po.K.print_refined_step parameter handler error event in 
                      let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "Predicates: " in 
                      let list = A.get blackboard.predicates_of_event k in 
                      let _ = List.iter (fun pid -> Printf.fprintf parameter.Po.K.H.out_channel_err "%s," (string_of_predicate_info pid)) list in 
                      let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "\n" in 
                      let bool = A.get blackboard.is_remove_action k in 
                      let _ = 
                        if bool 
                        then 
                          Printf.fprintf parameter.Po.K.H.out_channel_err "contain a deletion\n" in 
                      let int = A.get blackboard.modified_predicates_of_event k in 
                      let _ = Printf.fprintf parameter.Po.K.H.out_channel_err "%i modified predicates \n " int in
                      error
                    with _ -> error
                  end 
            in 
            aux (k+1)
        in 
        let error = aux 0 in 
        error 

      let p b = 
        let _ = 
          if b then Printf.fprintf stderr "TRUE\n" 
          else Printf.fprintf stderr "FALSE\n" 
        in 
        b 

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
            list1,list2,false,true
          | Po.K.Mod_internal (site,int)  -> 
            let predicate_id = Internal_state (Po.K.agent_id_of_site site,Po.K.site_name_of_site site) in 
            [predicate_id,Internal_state_is int],[],false,false
          | Po.K.Bind_to (s1,s2) -> 
            let ag_id1 = Po.K.agent_id_of_site s1 in 
            let ag_id2 = Po.K.agent_id_of_site s2 in 
            let agent_name2 = Po.K.agent_name_of_site s2 in 
            let site_id1 = Po.K.site_name_of_site s1 in 
            let site_id2 = Po.K.site_name_of_site s2 in 
            let predicate_id1 = Bound_site (ag_id1,site_id1) in 
            [predicate_id1,Bound_to (ag_id2,agent_name2,site_id2)],[],false,false
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
             predicate_id2,Bound_to (ag_id1,agent_name1,site_id1)],[],false,false
          | Po.K.Unbind (s1,s2) ->
            let ag_id1 = Po.K.agent_id_of_site s1 in 
            let ag_id2 = Po.K.agent_id_of_site s2 in 
            let site_id1 = Po.K.site_name_of_site s1 in 
            let site_id2 = Po.K.site_name_of_site s2 in 
            let predicate_id1 = Bound_site (ag_id1,site_id1) in 
            let predicate_id2 = Bound_site (ag_id2,site_id2) in 
            [predicate_id1,Free;predicate_id2,Free],[],false,false
          | Po.K.Free s -> 
            let ag_id = Po.K.agent_id_of_site s in 
            let site_id = Po.K.site_name_of_site s in 
            let predicate_id = Bound_site (ag_id,site_id) in     
            [predicate_id,Free],[],false,false
          | Po.K.Remove ag -> 
            let ag_id = Po.K.agent_id_of_agent ag in 
            let predicate_id = Here ag_id in 
            let set = 
              try 
                PredicateMap.find
                  predicate_id 
                  blackboard.predicate_id_list_related_to_predicate_id
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
            list,[],true,false

      let no_remove parameter handler error blackboard eid = 
        not (A.get blackboard.is_remove_action eid)

      let same_length parameter handler error blackboard eid1 eid2 = 
        (A.get blackboard.modified_predicates_of_event eid1)
          =
        (A.get blackboard.modified_predicates_of_event eid2)

      let clean t column blackboard = 
        match column 
        with 
          | [] -> column,blackboard 
          | head::tail -> 
            let rec aux list bool = 
              match list 
              with 
                | (eid,_,false)::q ->
                  begin
                    if eid = -1 
                    then 
                      list,bool
                    else 
                      match 
                        A.get blackboard.event eid 
                      with 
                        | None -> aux q true
                        | _ -> list,bool
                  end
                | _ -> list,bool 
            in 
            let list,bool = aux tail false in 
            if bool 
            then 
              let column = head::list in 
              let blackboard = 
                {
                  blackboard 
                 with 
                   steps_by_column = 
                    PredicateMap.add t column  blackboard.steps_by_column  }
              in column,blackboard 
            else
              column,blackboard 

      let check parameter handler error blackboard = 
        let eid = blackboard.nsteps in 
        let predicate_list = A.get blackboard.predicates_of_event eid in 
        let rec scan predicate_list = 
          match 
            predicate_list 
          with 
            | [] -> error,None,blackboard,[] 
            | t::q -> 
            begin
              let column = 
                try 
                  PredicateMap.find t blackboard.steps_by_column 
                with 
                  | Not_found -> []
              in 
              let column,blackboard = clean t column blackboard in 
              match 
                column 
              with 
                | (a,_,false)::_ -> scan q 
                | (a,x,true)::(b,_,true)::(_,y,_)::_ -> 
                  if a=eid && x=y
                  then 
                    error,Some (a,b),blackboard,q
                  else 
                    error,None,blackboard,q
                | _ -> error,None,blackboard,q
            end
        in 
        let error,candidates,blackboard,q = scan predicate_list in 
        match 
          candidates 
        with
          | None -> error,None 
          | Some (eida,eidb) -> 
            if 
              no_remove parameter handler error blackboard eidb
              && same_length parameter handler error blackboard eida eidb
              && 
                List.for_all 
                (fun pid -> 
                  let column = 
                    try 
                      PredicateMap.find pid  blackboard.steps_by_column  
                    with 
                      | Not_found -> []
                  in 
                  let column,blackboard = clean pid column blackboard in 
                  match 
                    column 
                  with 
                    | (a,_,false)::_ -> true
                    | (a,x,true)::(b,_,true)::(_,y,_)::_ -> 
                      if a=eida && b=eidb && x=y
                      then true
                      else false 
                    | _ -> false)
                q
            then error,Some (eida,eidb)
            else error,None

(*      let check _ _ error _ = error,None *)

      let pop parameter handler error blackboard eid = 
        let predicate_list = A.get blackboard.predicates_of_event eid in 
        let rec aux l error blackboard = 
          match l 
          with 
            | [] -> error,blackboard
            | pid::tail -> 
              let list = 
                try 
                  PredicateMap.find pid blackboard.steps_by_column 
                with 
                  | Not_found -> raise Exit
              in 
              begin 
                let list = 
                  match 
                    list
                  with 
                    | (a,_,_)::q when a = eid -> q
                    | _ -> list
                in 
                aux tail error 
                  {blackboard 
                   with 
                     steps_by_column = 
                      PredicateMap.add pid list blackboard.steps_by_column
                  }
              end
        in 
        let error,blackboard = aux predicate_list error blackboard in 
        let _ = A.set blackboard.event eid None in 
        error,blackboard 

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
            let predicate_id = Bound_site (ag_id,site_id) in 
            [predicate_id]



   let add_step parameter handler error step blackboard = 
     let get_init_state pid = 
       PredicateMap.find pid blackboard.init_state 
     in 
     let pre_event = blackboard.event in 
     let error,test_list = Po.K.tests_of_refined_step parameter handler error step in 
     let error,(action_list,_) = Po.K.actions_of_refined_step parameter handler error step in
     let side_effect = Po.K.get_kasim_side_effects (step) in 
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
       let test = test || test' in 
       let action = 
         match action' 
         with 
           | None -> action 
           | _ -> action' 
       in 
       let map = PredicateMap.add pid (test,action) map in 
       map 
     in 
     let unambiguous_side_effects = side_effect in 

     let test_map = 
       List.fold_left 
         (fun map test -> 
           let test_list = predicates_of_test parameter handler error blackboard test in
           build_map_test test_list map)
         PredicateMap.empty
         test_list in 
     let error,blackboard,action_map,test_map,is_remove_action,is_create_action = 
       List.fold_left 
         (fun (error,blackboard,action_map,test_map,bool,bool_creation) action -> 
           let action_list,test_list,bool',bool_creation' = predicates_of_action parameter handler error blackboard action in 
           error,blackboard,build_map action_list action_map,build_map_test test_list test_map,bool || bool',bool_creation || bool_creation')
         (error,blackboard,PredicateMap.empty,test_map,false,false)
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
       Mods.Int2Set.fold 
         (fun (a,b)  map  -> 
           let pid = Bound_site(a,b) in 
           add_state pid (false,Some Free) map)
         unambiguous_side_effects
         merged_map 
     in 
     let is_strong_action,pid_list = 
       PredicateMap.fold
         (fun pid (test,action) (bool,list) -> 
           bool 
           (*or (test=(Some Undefined)) *)
           ||
             (match action
              with 
                None -> false
              | Some action ->
                try not ((get_init_state pid = action) || action = Undefined)
                with Not_found -> false),
             match action 
             with 
             | None | Some Undefined -> list 
             | _ -> pid::list)
         merged_map (false,[])
     in 
     let is_strong_action = 
       if is_create_action then true 
       else is_strong_action
     in 
     let nsid = blackboard.nsteps + 1 in 
     let _ = A.set blackboard.event nsid (Some step) in 
     let n_modifications,pre_steps_by_column,init_state,list  = 
       PredicateMap.fold 
         (fun id (test,action) (n_modifications,map,init_state,(*init_event,*)list) -> 
           begin 
             let init_state =
               match 
                 action
               with 
               | None -> init_state
               | Some action -> 
                   begin 
                     try 
                       let _ = PredicateMap.find id init_state in 
                       init_state
                     with 
                       Not_found -> 
                         PredicateMap.add id action init_state
                   end
             in 
             let old_list = 
               try 
                 PredicateMap.find id map 
               with 
               | Not_found -> [-1,Undefined,false] 
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
             let n_modifications,bool_action = 
               match action
               with 
               | None -> n_modifications,false
               | Some _ -> (n_modifications+1),true 
             in 
             n_modifications,
             PredicateMap.add id ((nsid,new_value,bool_action)::old_list) map,
             init_state,
             (id,new_value)::list
           end)
         merged_map
         (0,blackboard.steps_by_column,blackboard.init_state,[])
     in 
    let _ = 
      if is_remove_action 
      then 
        let _ = A.set blackboard.is_remove_action nsid true in () 
    in 
    let _ = A.set blackboard.predicates_of_event nsid (List.rev_map fst (List.rev list)) in 
    let _ = A.set blackboard.modified_predicates_of_event nsid n_modifications in 
    let blackboard = 
      if is_strong_action 
      then 
        blackboard 
      else 
        {blackboard with weak_actions = nsid::blackboard.weak_actions}
    in 
    let blackboard = 
      { 
        blackboard with 
          init_state = init_state ;
          event = pre_event ;
          steps_by_column = pre_steps_by_column; 
          nsteps = nsid;
      }
    in 
    error,blackboard

      

  let cut parameter handler error list = 
    let n = List.length list in 
    let blackboard = init_blackboard n in 
    let error,blackboard,n_cut = 
      List.fold_left 
        (fun (error,blackboard,n_cut) step ->  
          let error,blackboard = add_step parameter handler error step blackboard in 
          let error,to_pop = check parameter handler error blackboard in 
          match 
            to_pop 
          with 
            | None -> error,blackboard,n_cut 
            | Some (e1,e2) -> 
              let error,blackboard = pop parameter handler error blackboard e1 in 
              let error,blackboard = pop parameter handler error blackboard e2 in 
              (error,blackboard,n_cut+2) )
        (error,blackboard,0)
        list 
    in 
    let list = 
      let rec aux k list list_weak = 
        if k=(-1) 
        then list
        else 
          let list_weak,bool = 
            match list_weak 
            with 
              t::q -> if t=k then q,true else list_weak,false
            | [] -> [],false
          in 
          match A.get blackboard.event k 
          with 
            | Some a -> 
              aux (k-1) ((a,bool)::list) list_weak 
            | None -> aux (k-1) list list_weak
      in aux (blackboard.nsteps) [] blackboard.weak_actions
    in 
    error,list,n_cut
    
  let do_not_cut parameter handler error list = 
    let n = List.length list in 
    let blackboard = init_blackboard n in 
    let error,blackboard,n_cut = 
      List.fold_left 
        (fun (error,blackboard,n_cut) step ->  
          let error,blackboard = add_step parameter handler error step blackboard in 
          error,blackboard,n_cut)
        (error,blackboard,0)
        list 
    in 
    let list = 
      let rec aux k list list_weak = 
        if k=(-1) 
        then list
        else 
          let list_weak,bool = 
            match list_weak 
            with 
              t::q -> if t=k then q,true else list_weak,false
            | [] -> [],false
          in 
          match A.get blackboard.event k 
          with 
            | Some a -> 
              aux (k-1) ((a,bool)::list) list_weak 
            | None -> aux (k-1) list list_weak
      in aux (blackboard.nsteps) [] blackboard.weak_actions
    in 
    error,list,n_cut
    end:Cut_pseudo_inverse)
