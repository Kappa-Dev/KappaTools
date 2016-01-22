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
     module A:GenArray.GenArray

     val cut:
       (Po.K.refined_step list, ( (Po.K.refined_step list) * int)) Po.K.H.unary 
   end

 module Pseudo_inv = 
   (struct 

     module Po=Po_cut.Po_cut 
     module A = Mods.DynArray 
     module CPredicateMap = Predicate_maps.QPredicateMap
     module PredicateMap = Predicate_maps.PredicateMap       
   
     type predicate_info = Predicate_maps.predicate_info
     type step_id = int 

   
     let string_of_predicate_info = Predicate_maps.string_of_predicate_info 

      let string_of_predicate_value pi = 
        match 
          pi 
        with
          | Predicate_maps.Internal_state_is s -> (string_of_int s)
          | Predicate_maps.Undefined -> "#Undef" 
          | Predicate_maps.Present -> "#Here"
          | Predicate_maps.Free -> "#Free" 
          | Predicate_maps.Bound_to (ag,ag_name,s) -> 
            "Bound_to "^(string_of_int ag)^" "^(string_of_int ag_name)^" "^(string_of_int s)

      type pseudo_inv_blackboard = 
       {
         steps_by_column: (step_id * Predicate_maps.predicate_value * bool) list CPredicateMap.t ;
         nsteps: step_id ; 
         predicates_of_event: predicate_info list A.t ;
         is_remove_action: bool A.t ;
         modified_predicates_of_event: int A.t ;
         event: (Po.K.refined_step (** step_id list*)) option A.t; 
	 agent_list: int list ;
       }

      let init_blackboard n_steps handler = 
        {
          steps_by_column = Po.K.H.get_predicate_map handler;
          nsteps = -1 ; 
          predicates_of_event = A.make  n_steps [] ;
          is_remove_action = A.make n_steps false ;
          modified_predicates_of_event = A.create n_steps  0 ; 
          event = A.make n_steps  None ; 
	  agent_list = [];
       }


      let print_blackboard parameter handler error blackboard = 
        let _ = Format.fprintf parameter.Po.K.H.out_channel_err "Blackboard for removing pseudo inverse element\n" in 
        let _ = Format.fprintf parameter.Po.K.H.out_channel_err "n_events: %i\n" blackboard.nsteps in 
        let _ = Format.fprintf parameter.Po.K.H.out_channel_err "Steps_by_column:\n" in 
        let _ = 
          CPredicateMap.iter 
            (fun pred list -> 
              let _ = 
                Format.fprintf parameter.Po.K.H.out_channel_err "%s: " (string_of_predicate_info pred)
              in 
              let _ = 
                List.iter 
                  (fun (eid,value,bool) -> 
                    Format.fprintf parameter.Po.K.H.out_channel_err "(%i,%s%s)," eid (string_of_predicate_value value) (if bool then "(Mod)" else ""))
                  list 
              in 
              Format.fprintf parameter.Po.K.H.out_channel_err "\n")
            blackboard.steps_by_column 
        in 
        let _ = Format.fprintf parameter.Po.K.H.out_channel_err "Events:\n" in 
        let rec aux k = 
          if k=blackboard.nsteps 
          then error
          else 
            let event = 
              try 
                A.get blackboard.event k 
              with _ -> 
                let _ = Format.eprintf "ERREUR %i 123\n" k in 
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
                      let () = Format.fprintf
				 parameter.Po.K.H.out_channel_err
				 "@[<v>Event %i@,%a@]@." k
				 (Po.K.print_refined_step ~compact:false ~handler:handler) event in
                      let _ = Format.fprintf parameter.Po.K.H.out_channel_err "Predicates: " in 
                      let list = A.get blackboard.predicates_of_event k in 
                      let _ = List.iter (fun pid -> Format.fprintf parameter.Po.K.H.out_channel_err "%s," (string_of_predicate_info pid)) list in 
                      let _ = Format.fprintf parameter.Po.K.H.out_channel_err "\n" in 
                      let bool = A.get blackboard.is_remove_action k in 
                      let _ = 
                        if bool 
                        then 
                          Format.fprintf parameter.Po.K.H.out_channel_err "contain a deletion\n" in 
                      let int = A.get blackboard.modified_predicates_of_event k in 
                      let _ = Format.fprintf parameter.Po.K.H.out_channel_err "%i modified predicates \n " int in
                      error
                    with _ -> error
                  end 
            in 
            aux (k+1)
        in 
        let error = aux 0 in 
        error 

      let predicates_of_action parameter handler error blackboard action  = 
        match action with 
          | Instantiation.Create (ag,interface) -> 
            let ag_id = Po.K.agent_id_of_agent ag in
            let predicate_id = Predicate_maps.Here ag_id in   
            let list1,list2 = 
              List.fold_left 
                (fun (list1,list2) (s_id,opt) -> 
                  let predicate_id = Predicate_maps.Bound_site(ag_id,s_id) in 
                  let list1 = (predicate_id,Predicate_maps.Free)::list1 in
                  let list2 = predicate_id::list2 in 
                  match opt 
                  with 
                    | None -> list1,list2
                    | Some x -> 
                      let predicate_id = Predicate_maps.Internal_state (ag_id,s_id) in 
                      (predicate_id,Predicate_maps.Internal_state_is x)::list1,
                      predicate_id::list2
                )
                ([predicate_id,Predicate_maps.Present],[predicate_id])
                interface
            in 
            {blackboard with agent_list = ag_id::blackboard.agent_list},list1,list2,false,true
          | Instantiation.Mod_internal (site,int)  -> 
            let predicate_id = Predicate_maps.Internal_state (Po.K.agent_id_of_site site,Po.K.site_name_of_site site) in 
            blackboard,[predicate_id,Predicate_maps.Internal_state_is int],[],false,false
          | Instantiation.Bind_to (s1,s2) -> 
            let ag_id1 = Po.K.agent_id_of_site s1 in 
            let ag_id2 = Po.K.agent_id_of_site s2 in 
            let agent_name2 = Po.K.agent_name_of_site s2 in 
            let site_id1 = Po.K.site_name_of_site s1 in 
            let site_id2 = Po.K.site_name_of_site s2 in 
            let predicate_id1 = Predicate_maps.Bound_site (ag_id1,site_id1) in 
            blackboard,[predicate_id1,Predicate_maps.Bound_to (ag_id2,agent_name2,site_id2)],[],false,false
          | Instantiation.Bind (s1,s2) -> 
            let ag_id1 = Po.K.agent_id_of_site s1 in 
            let ag_id2 = Po.K.agent_id_of_site s2 in 
            let agent_name1 = Po.K.agent_name_of_site s1 in 
            let agent_name2 = Po.K.agent_name_of_site s2 in 
            let site_id1 = Po.K.site_name_of_site s1 in 
            let site_id2 = Po.K.site_name_of_site s2 in 
            let predicate_id1 = Predicate_maps.Bound_site (ag_id1,site_id1) in 
            let predicate_id2 = Predicate_maps.Bound_site (ag_id2,site_id2) in 
            blackboard,
	    [
	      predicate_id1,Predicate_maps.Bound_to (ag_id2,agent_name2,site_id2);
              predicate_id2,Predicate_maps.Bound_to (ag_id1,agent_name1,site_id1)
	    ],
	    [],false,false
          | Instantiation.Free s -> 
            let ag_id = Po.K.agent_id_of_site s in 
            let site_id = Po.K.site_name_of_site s in 
            let predicate_id = Predicate_maps.Bound_site (ag_id,site_id) in     
            blackboard,[predicate_id,Predicate_maps.Free],[],false,false
          | Instantiation.Remove ag -> 
            let ag_id = Po.K.agent_id_of_agent ag in 
            let predicate_id = Predicate_maps.Here ag_id in 
            blackboard,[predicate_id,Predicate_maps.Undefined],[],true,false

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
                    CPredicateMap.add t column  blackboard.steps_by_column  }
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
                CPredicateMap.find_default
		  [] t blackboard.steps_by_column in
              let column,blackboard = clean t column blackboard in 
              match 
                column 
              with 
                | (_,_,false)::_ -> scan q 
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
                   CPredicateMap.find_default
		     [] pid blackboard.steps_by_column in
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


      let pop parameter handler error blackboard eid = 
        let predicate_list = A.get blackboard.predicates_of_event eid in 
        let rec aux l error blackboard = 
          match l 
          with 
            | [] -> error,blackboard
            | pid::tail -> 
               let list =
                 match CPredicateMap.find_option pid blackboard.steps_by_column with
		 | Some x -> x
		 | None -> raise Exit in
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
                      CPredicateMap.add pid list blackboard.steps_by_column
                  }
              end
        in 
        let error,blackboard = aux predicate_list error blackboard in 
        let _ = A.set blackboard.event eid None in 
        error,blackboard 

      let predicates_of_test parameter handler error blackboard test = 
        match test
        with 
          | Instantiation.Is_Here (agent) ->
            let ag_id = Po.K.agent_id_of_agent agent in 
            let predicate_id = Predicate_maps.Here ag_id in 
            [predicate_id]
          | Instantiation.Has_Internal(site,int) -> 
            let predicate_id = Predicate_maps.Internal_state (Po.K.agent_id_of_site site,Po.K.site_name_of_site site) in 
            [predicate_id]
          | Instantiation.Is_Free s -> 
            let ag_id = Po.K.agent_id_of_site s in 
            let site_id = Po.K.site_name_of_site s in 
            let predicate_id = Predicate_maps.Bound_site (ag_id,site_id) in     
            [predicate_id]
          | Instantiation.Is_Bound_to  (s1,s2) -> 
            let ag_id1 = Po.K.agent_id_of_site s1 in 
            let ag_id2 = Po.K.agent_id_of_site s2 in 
            let site_id1 = Po.K.site_name_of_site s1 in 
            let site_id2 = Po.K.site_name_of_site s2 in 
            let predicate_id1 = Predicate_maps.Bound_site (ag_id1,site_id1) in 
            let predicate_id2 = Predicate_maps.Bound_site (ag_id2,site_id2) in 
            [predicate_id1;predicate_id2]
          | Instantiation.Is_Bound s -> 
            let ag_id = Po.K.agent_id_of_site s in 
            let site_id = Po.K.site_name_of_site s in 
            let predicate_id = Predicate_maps.Bound_site (ag_id,site_id) in 
            [predicate_id]   
          | Instantiation.Has_Binding_type (s,_) ->
            let ag_id = Po.K.agent_id_of_site s in 
            let site_id = Po.K.site_name_of_site s in
            let predicate_id = Predicate_maps.Bound_site (ag_id,site_id) in 
            [predicate_id]


     


   let add_step parameter handler info error step blackboard =
     let pre_event = blackboard.event in 
     let error,info,test_list = Po.K.tests_of_refined_step parameter handler info error step in 
     let error,info,(action_list,_) = Po.K.actions_of_refined_step parameter handler info error step in
     let side_effect = Po.K.get_kasim_side_effects (step) in 
     let build_map list map = 
       List.fold_left 
         (fun map (id,value) -> Predicate_maps.PredicateMap.add id value map)
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
         PredicateMap.find_default (false,None) pid map in
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
           let blackboard,action_list,test_list,bool',bool_creation' = predicates_of_action parameter handler error blackboard action in 
           error,blackboard,build_map action_list action_map,build_map_test test_list test_map,bool || bool',bool_creation || bool_creation')
         (error,blackboard,PredicateMap.empty,test_map,false,false)
         (action_list) in 
     let error,merged_map = 
       PredicateMap.monadic_fold2
	 parameter error
         (fun _ e key test action acc ->
	  e,PredicateMap.add key (test,Some action) acc)
         (fun _ e key test acc ->
	  e,PredicateMap.add key (test,None) acc)
         (fun _ e key action acc ->
	  e,PredicateMap.add key (false,Some action) acc)
         test_map
         action_map
	 PredicateMap.empty in
     let merged_map =
       List.fold_right
         (fun ((a,_),b) map ->
          let pid = Predicate_maps.Bound_site(a,b) in add_state pid (false,Some Predicate_maps.Free) map)
         unambiguous_side_effects merged_map in
     let nsid = blackboard.nsteps + 1 in 
     let _ = A.set blackboard.event nsid (Some step) in 
     let n_modifications,pre_steps_by_column(*,init_state*),list  = 
       PredicateMap.fold 
         (fun id (test,action) (n_modifications,map(*,init_state*),list) -> 
          begin 
            let old_list =
              CPredicateMap.find_default [-1,Predicate_maps.Undefined,false] id map in
            let old_value = 
              match 
                old_list
              with 
              | (_,v,_)::_ -> v
              | [] -> Predicate_maps.Undefined 
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
            CPredicateMap.add id ((nsid,new_value,bool_action)::old_list) map,
            (id,new_value)::list
          end)
         merged_map
         (0,blackboard.steps_by_column,[])
     in 
     let _ = 
      if is_remove_action 
      then 
        let _ = A.set blackboard.is_remove_action nsid true in () 
    in 
    let _ = A.set blackboard.predicates_of_event nsid (List.rev_map fst (List.rev list)) in 
    let _ = A.set blackboard.modified_predicates_of_event nsid n_modifications in 
    let blackboard = 
      { 
        blackboard with 
          event = pre_event ;
          steps_by_column = pre_steps_by_column; 
          nsteps = nsid;
      }
    in 
    error,info,blackboard

      

  let cut parameter handler info error list = 
    let n_steps = List.length list in 
    let blackboard = init_blackboard n_steps handler in 
    let error,info,blackboard,n_cut = 
      List.fold_left 
        (fun (error,info,blackboard,n_cut) step ->  
          let error,info,blackboard = add_step parameter handler info error step blackboard in 
          let error,to_pop = check parameter handler error blackboard in 
          match 
            to_pop 
          with 
            | None -> error,info,blackboard,n_cut 
            | Some (e1,e2) -> 
              let error,blackboard = pop parameter handler error blackboard e1 in 
              let error,blackboard = pop parameter handler error blackboard e2 in 
              (error,info,blackboard,n_cut+2) )
        (error,info,blackboard,0)
        list 
    in 
    let list = 
      let rec aux k list = 
        if k=(-1) 
        then list
        else 
          match A.get blackboard.event k 
          with 
            | Some a -> 
              aux (k-1) (a::list) 
            | None -> aux (k-1) list 
      in aux (blackboard.nsteps) [] 
    in 
    let tab = blackboard.steps_by_column in 
    let _ = 
      List.iter 
	(CPredicateMap.recycle tab) 
	blackboard.agent_list
    in 
    error,info, (list,n_cut)
    
    end:Cut_pseudo_inverse)
