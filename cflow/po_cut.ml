 (**
   * po_cut.ml 
   *
   * Cut concurrent events: a module for KaSim 
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * Jean Krivine, Université Paris-Diderot, CNRS 
   * 
   * KaSim
   * Jean Krivine, Université Paris Dederot, CNRS 
   *  
   * Creation: 16/04/2012
   * Last modification: 02/08/2013
   * * 
   * Some parameter references can be tuned thanks to command-line options
   * other variables has to be set before compilation   
   *  
   * Copyright 2011,2012 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)

 module type Po_cut =
   sig
     module K:Kappa_instantiation.Cflow_signature

     val cut: (K.refined_step list,(K.refined_step list * int )) K.H.unary
   end

 module Po_cut = 
   (struct 

     module K=Kappa_instantiation.Cflow_linker

     type predicate_info = 
       | Here of K.agent_id  
       | Bound_site of K.agent_id * Instantiation.site_name
       | Internal_state of K.agent_id * Instantiation.site_name

     module PSM = SetMap.Make (struct type t = predicate_info let compare = compare end)
     module PS = PSM.Set

     let created_predicates_of_action action = 
       match action with 
         | Instantiation.Create (ag,interface) -> 
           let ag_id = K.agent_id_of_agent ag in
           List.fold_left 
             (fun list (s_id,opt) -> 
               let list = Bound_site(ag_id,s_id) :: list in 
               match opt 
               with 
                 | None -> list 
                 | Some _ ->  (Internal_state (ag_id,s_id))::list 
             )
             [Here ag_id]
             interface
         | Instantiation.Bind _ | Instantiation.Bind_to _ | Instantiation.Remove _ | Instantiation.Free _ | Instantiation.Mod_internal _ -> []

     let predicates_of_action action = 
       match action with 
         | Instantiation.Create (ag,interface) -> 
           let ag_id = K.agent_id_of_agent ag in
             List.fold_left 
               (fun list (s_id,opt) -> 
                 let list = (Bound_site(ag_id,s_id))::list in 
                 match opt 
                 with 
                   | None -> list 
                   | Some _ ->  (Internal_state (ag_id,s_id))::list 
               )
               [Here ag_id]
               interface
         | Instantiation.Mod_internal (site,_)  -> 
           [Internal_state (K.agent_id_of_site site,K.site_name_of_site site)]
         | Instantiation.Bind_to (s1,s2)  | Instantiation.Bind (s1,s2) ->
           [Bound_site (K.agent_id_of_site s1,K.site_name_of_site s1);Bound_site (K.agent_id_of_site s2,K.site_name_of_site s2)]
         | Instantiation.Free s ->
           [Bound_site (K.agent_id_of_site s,K.site_name_of_site s)]
         | Instantiation.Remove _ -> []

     let predicates_of_test test = 
       match test
       with 
         | Instantiation.Is_Here (agent) ->
           [Here (K.agent_id_of_agent agent)]
         | Instantiation.Has_Internal(site,_) -> 
           [Internal_state (K.agent_id_of_site site,K.site_name_of_site site)]
         | Instantiation.Is_Free s | Instantiation.Is_Bound s | Instantiation.Has_Binding_type (s,_) -> 
           [Bound_site (K.agent_id_of_site s,K.site_name_of_site s)]
         | Instantiation.Is_Bound_to  (s1,s2) -> 
           [Bound_site (K.agent_id_of_site s1,K.site_name_of_site s1);Bound_site (K.agent_id_of_site s2,K.site_name_of_site s2)]

     let predicates_of_side_effects sides =
       List.map (fun ((ag_id,_),s_id) -> Bound_site(ag_id,s_id)) sides

     let cut parameter handler info error event_list = 
       let seen_predicates = PS.empty in 
       let _,event_list,n = 
         List.fold_left 
           (fun (seen,kept,n_cut) event -> 
             let rec keep l = 
               match l 
               with 
                 | [] -> false 
                 | t0::q0 -> 
                   let rec aux1 l = 
                     match l 
                     with 
                       | [] -> keep q0  
                       | t1::q1 ->  
                         if PS.mem t1 seen 
                         then true 
                         else aux1 q1
                   in 
                     aux1 (predicates_of_action t0)
             in 
             let rec keep2 l = 
               match l 
               with 
                 | [] -> false 
                 | t::q -> 
                   if PS.mem t seen 
                   then 
                     true 
                   else 
                     keep2 q 
             in 
             let error,info,(action_list,_) = K.actions_of_refined_step parameter handler info error event in 
             let seen =   
               List.fold_left 
                 (fun seen action -> 
                   List.fold_left 
                     (fun seen elt -> PS.remove elt seen)
                     seen
                     (created_predicates_of_action action)
                 )
                 seen action_list
             in 
             let error,info,(actions,_) = K.actions_of_refined_step parameter handler info error event in  
             if (K.is_obs_of_refined_step event)
               || (keep actions)
               || (keep2 (predicates_of_side_effects (K.get_kasim_side_effects event)))
             then 
               begin
                 let kept = event::kept in 
                 let error,info,tests = K.tests_of_refined_step parameter handler info error event in 
                 let seen = 
                   List.fold_left 
                     (fun seen test -> 
                       List.fold_left 
                         (fun seen predicate_info -> PS.add predicate_info seen)
                         seen 
                         (predicates_of_test test)
                     )
                     seen 
                     tests
                 in 
                 (seen,kept,n_cut)
               end 
             else
               (seen,kept,n_cut+1)
           )
           (seen_predicates,[],0) 
           (List.rev event_list) 
       in 
       error,info,(event_list,n)
        
   end:Po_cut)
