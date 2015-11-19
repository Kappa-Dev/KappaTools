(**
  * generic_branch_and_cup_solver.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 19/07/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let log_steps = false

module type Solver = 
  (sig 
    module PH:Propagation_heuristics.Blackboard_with_heuristic

    val compress: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.update_order list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard * PH.B.assign_result  * PH.B.result list) PH.B.PB.CI.Po.K.H.with_handler
      
    val detect_independent_events: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.PB.step_id list) PH.B.PB.CI.Po.K.H.with_handler

    val filter: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val sub: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.PB.CI.Po.K.refined_step list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val clean: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard) PH.B.PB.CI.Po.K.H.with_handler

    val translate: (PH.B.blackboard -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.refined_step list * PH.B.result) PH.B.PB.CI.Po.K.H.with_handler 
    val translate_result: PH.B.result -> PH.B.PB.CI.Po.K.refined_step list 

   end)


module Solver = 
struct 
  module PH= Propagation_heuristics.Propagation_heuristic 
(*Blackboard_with_heuristic*)
    
  let combine_output o1 o2 = 
    if PH.B.is_ignored o2 then o1 else o2 
      
  let rec propagate parameter handler error log_info instruction_list propagate_list blackboard = 
    let bool,log_info = PH.B.tick log_info in 
    let _ = 
      if bool then
        PH.B.PB.CI.Po.K.P.dump_complete_log
	  parameter.PH.B.PB.CI.Po.K.H.out_channel_profiling log_info
    in
    match instruction_list 
    with 
      | t::q ->
        begin 
          let error,log_info,blackboard,instruction_list,propagate_list,assign_result = PH.apply_instruction parameter handler error log_info blackboard t q propagate_list in 
          if PH.B.is_failed assign_result 
          then 
            error,log_info,blackboard,assign_result 
          else 
            propagate parameter handler error log_info instruction_list propagate_list blackboard
        end
      | [] -> 
        begin
          match propagate_list 
          with 
            | t::q -> 
              let error,log_info,blackboard,instruction_list,propagate_list,assign_result = PH.propagate parameter handler error log_info blackboard t instruction_list q in 
                    if PH.B.is_failed assign_result 
                    then 
                      error,log_info,blackboard,assign_result 
                    else 
                      propagate parameter handler error log_info instruction_list propagate_list blackboard
            | [] -> error,log_info,blackboard,PH.B.success
        end 
          
  type choices = 
    {current:PH.update_order list ;
     stack:PH.update_order list list }
        
  let branch_choice_list choice_list = {current = [] ; stack = choice_list.current::choice_list.stack}
  let update_current choice_list list = {choice_list with current = list} 
  let pop_next_choice parameter handler error stack = 
    match stack.current
    with 
      | t::q -> error,(t,{stack with current=q})
      | [] -> 
        let error_list,error = PH.B.PB.CI.Po.K.H.create_error parameter handler error (Some "generic_branch_and_cut_solver.ml") None (Some "cut_choice_list") (Some "107") (Some "Empty choice stack") (failwith "Empty choice list in pop_next_choice") in 
        PH.B.PB.CI.Po.K.H.raise_error parameter handler error_list error (PH.dummy_update_order,stack)

  let no_more_choice stack = 
    match stack.current
    with 
      | [] -> true
      | _ -> false 

  let backtrack parameter handler error log_info blackboard choice_list = 
    let rec backtrack_aux error log_info blackboard choice_list = 
      match choice_list.current
      with 
        | [] -> 
          begin 
            match choice_list.stack 
            with 
            | [] -> error,log_info,blackboard,None 
            | t::q -> 
              let choice_list = {current = t ; stack = q } in 
              let error,log_info,blackboard =  PH.B.reset_last_branching parameter handler error log_info blackboard in 
              backtrack_aux error log_info blackboard choice_list
        end 
      | _ -> error,log_info,blackboard,Some choice_list
    in 
    let error,log_info,blackboard = PH.B.reset_last_branching parameter handler error log_info blackboard in 
    backtrack_aux error log_info blackboard choice_list 
 

  let empty_choice_list = 
    {stack=[];current=[]}
		       
  let rec iter parameter handler error log_info blackboard choice_list story_list = 
    let error,bool = PH.B.is_maximal_solution parameter handler error blackboard in
    if bool 
    then 
      (* SUCCESS *)
      let error,list = 
          PH.B.translate_blackboard parameter handler error blackboard 
      in
       if PH.B.PB.CI.Po.K.H.get_all_stories_per_obs parameter
       then
	 begin 
	   let story_list = list::story_list in	   
	   let error,() =
	     match choice_list.current
	     with
	     | _::_,_ ->
		let error_list,error = PH.B.PB.CI.Po.K.H.create_error parameter handler error (Some "generic_branch_and_cut_solver.ml") None (Some "iter") (Some "__LOC__") (Some "In case of success, the current list of choices should be empty") (failwith "In case of success, the current list of choices should be empty")
		in 
		PH.B.PB.CI.Po.K.H.raise_error parameter handler error_list error ()
	     | _ -> error,()
	   in 
           let choice_list =
	     match choice_list.stack
	     with [] -> choice_list
		| t::q -> { current=t;stack=q}
	   in 
	   let error,log_info,blackboard,choice_list = backtrack parameter handler error log_info blackboard choice_list in 	  
	   begin 
             match choice_list 
             with 
             | Some choice_list -> iter parameter handler error log_info blackboard choice_list story_list (*(update_first_story first_story list)*)
             | None -> error,log_info,blackboard,story_list
	   end
	 end 
       else
	 error,log_info,blackboard,[list] 
    else
      let error,choice_list = 
        if no_more_choice choice_list 
        then 
          let error,list = PH.next_choice parameter handler error blackboard in
          error,update_current choice_list list
        else
          error,choice_list 
      in 
      let error,log_info,blackboard = PH.B.branch parameter handler error log_info blackboard in
      let error,(choice,choice_list) = pop_next_choice parameter handler error choice_list in 
      let error,log_info,blackboard,output = propagate parameter handler error log_info [choice] [] blackboard in
      if PH.B.is_failed output 
      then 
        let error,log_info,blackboard,choice_list = backtrack parameter handler error log_info blackboard choice_list in 
        begin 
          match choice_list 
          with 
          | Some choice_list -> iter parameter handler error log_info blackboard choice_list story_list  
            | None -> error,log_info,blackboard,story_list
        end
      else 
        iter parameter handler error log_info blackboard (branch_choice_list choice_list) story_list  
            
  let detect_independent_events parameter handler error log_info blackboard list_eid = 
    let error,log_info,blackboard,events_to_keep = PH.B.cut parameter handler error log_info blackboard list_eid  in 
    error,log_info,events_to_keep 

  let translate parameter handler error blackboard list =
    let list' = List.rev_map (fun k -> PH.B.get_event blackboard k,PH.B.side_effect_of_event blackboard k) (List.rev list) in 
    let list = List.rev_map fst (List.rev list') in 
    error,list,list'
    
  let translate_result result = 
    List.rev_map fst @@ List.rev result 

  let clean parameter handler error log_info blackboard = 
    PH.B.reset_init parameter handler error log_info blackboard 

  let filter parameter handler error log_info blackboard events_to_keep = 
    let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
    let error,log_info,blackboard = PH.B.branch parameter handler error log_info blackboard in
    let events_to_remove = 
      let n_events = PH.B.get_n_eid blackboard in 
      let rec aux k list sol = 
        if k=n_events 
        then 
          List.rev sol 
        else 
          match 
            list
          with 
            | t::q -> 
              if t=k 
              then aux (k+1) q sol 
              else aux (k+1) list (k::sol)
            | [] -> 
              aux (k+1) list (k::sol)
      in 
      aux 0 events_to_keep []
    in 
    let error,forbidden_events = PH.forbidden_events parameter handler error events_to_remove in
    let _ =
      if log_steps then
        Format.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err "Start cutting@."
    in
    let error,log_info,blackboard,output = 
      propagate parameter handler error log_info forbidden_events [] blackboard  
    in 
    let log_info = PH.B.PB.CI.Po.K.P.set_concurrent_event_deletion_time log_info in 
    let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
    error,log_info,blackboard 

  let sub parameter handler error log_info to_keep = 
    let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
    let error,log_info,blackboard = PH.B.import parameter handler error log_info to_keep in 
    let log_info = PH.B.PB.CI.Po.K.P.set_concurrent_event_deletion_time log_info in 
    let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
    error,log_info,blackboard 
    
  let compress parameter handler error log_info blackboard list_order =
    let error,log_info,blackboard = PH.B.branch parameter handler error log_info blackboard in 
    let log_info = PH.B.PB.CI.Po.K.P.set_concurrent_event_deletion_time log_info in 
    let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
    let _ =
      if log_steps then
        Format.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err
		       "After Causal Cut  %i @." (PH.B.get_n_unresolved_events blackboard)
    in 
    let error,log_info,blackboard,output = 
      propagate parameter handler error log_info list_order [] blackboard 
    in 
    let _ =
      if log_steps then
        Format.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err
		       "After observable propagation  %i @." (PH.B.get_n_unresolved_events blackboard)
    in
    let error,log_info,blackboard,story_list = iter parameter handler error log_info blackboard empty_choice_list [] 
    in 
    let output =
      match
	story_list
      with
	[] -> PH.B.fail
      | _ -> PH.B.success
    in 
    error,log_info,blackboard,output,story_list 


end 
  
