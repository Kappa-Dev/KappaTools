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
  * Last modification: 18/06/2012
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

    val compress: (PH.B.PB.CI.Po.K.P.log_info -> PH.B.blackboard -> PH.update_order list -> PH.B.PB.step_id list -> PH.B.PB.CI.Po.K.H.error_channel * PH.B.PB.CI.Po.K.P.log_info * PH.B.blackboard * PH.B.assign_result * PH.B.result option * PH.B.result option) PH.B.PB.CI.Po.K.H.with_handler
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
      if bool 
      then 
        let _ = PH.B.PB.CI.Po.K.P.dump_complete_log parameter.PH.B.PB.CI.Po.K.H.out_channel_profiling log_info in 
        let _ = flush parameter.PH.B.PB.CI.Po.K.H.out_channel_profiling
        in () 
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
          
	  
  let rec branch_over_assumption_list parameter handler error log_info list blackboard = 
    match list 
    with 
	  | [] ->
            error,log_info,blackboard,PH.B.fail
	  | head::tail -> 
	    begin
	      let error,log_info,blackboard = PH.B.branch parameter handler error log_info blackboard in
	      let error,log_info,blackboard,output = propagate parameter handler error log_info [head] [] blackboard in
	      if PH.B.is_failed output 
              then 
                let error,log_info,blackboard = PH.B.reset_last_branching parameter handler error log_info blackboard in 
                branch_over_assumption_list parameter handler error log_info tail blackboard 
              else 
                let error,log_info,blackboard,output = iter parameter handler error log_info blackboard in 
                if PH.B.is_failed output 
                then 
                  let error,log_info,blackboard = PH.B.reset_last_branching parameter handler error log_info blackboard in 
                  branch_over_assumption_list parameter handler error log_info tail blackboard 
                else 
                  error,log_info,blackboard,output 
	    end
	      
  and iter parameter handler error log_info blackboard = 
    let error,bool = PH.B.is_maximal_solution parameter handler error blackboard in
    if bool 
    then 
      error,log_info,blackboard,PH.B.success 
    else
      let error,list = PH.next_choice parameter handler error blackboard in
      branch_over_assumption_list parameter handler error log_info list blackboard 
    
  let compress parameter handler error log_info blackboard list_order list_eid =
    let error,log_info,blackboard = PH.B.branch parameter handler error log_info blackboard in 
    let error,log_info,blackboard,events_to_keep = PH.B.cut parameter handler error log_info blackboard list_eid  in 
    let to_keep = 
      List.rev_map (fun k -> PH.B.get_event blackboard k) (List.rev events_to_keep) in
    let result_wo_compression = 
      List.rev_map (fun k -> (k,PH.B.PB.CI.Po.K.empty_side_effect)) (List.rev to_keep)  in 
    let save_blackboard = blackboard in 
    let bool = false in (* if true cut the current blackboard, otherwise wuild a new one *)
    let error,log_info,blackboard,list_order,list_eid = 
      if bool 
      then 
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
          if log_steps 
          then 
            let _ = Printf.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err "Start cutting\n" in 
            let _ = 
              flush parameter.PH.B.PB.CI.Po.K.H.out_channel_err
            in 
            ()
        in 
        let error,log_info,blackboard,output = 
          propagate parameter handler error log_info forbidden_events [] blackboard  
        in 
        error,log_info,blackboard,list_order,list_eid 
      else 
        let error,log_info,blackboard = PH.B.import parameter handler error log_info to_keep in 
        let error,list = PH.forced_events parameter handler error blackboard in 
        match list 
        with 
          | (list_order,list_eid,info)::q -> error,log_info,blackboard,list_order,list_eid
          | _ -> error,log_info,blackboard,[],[]  
     in 
     let result_wo_compression = 
       if 
         Parameter.get_causal_trace parameter.PH.B.PB.CI.Po.K.H.compression_mode 
       then 
         Some result_wo_compression 
       else 
         None 
     in 
     let log_info = PH.B.PB.CI.Po.K.P.set_concurrent_event_deletion_time log_info in 
     let log_info = PH.B.PB.CI.Po.K.P.set_step_time log_info in 
     let _ = 
       if log_steps 
       then 
         let _ = Printf.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err "After Causal Cut  %i \n" (PH.B.get_n_unresolved_events blackboard) in 
         let _ = 
           flush parameter.PH.B.PB.CI.Po.K.H.out_channel 
         in 
         ()
     in 
     let error,log_info,blackboard,output = 
       propagate parameter handler error log_info list_order [] blackboard 
     in 
     let _ = 
       if log_steps 
       then 
         let _ = Printf.fprintf parameter.PH.B.PB.CI.Po.K.H.out_channel_err "After observable propagation  %i \n" (PH.B.get_n_unresolved_events blackboard) in 
         let _ = 
           flush parameter.PH.B.PB.CI.Po.K.H.out_channel 
         in ()
     in 
     let error,log_info,blackboard,output = iter parameter handler error log_info blackboard 
     in 
     let log_info' = PH.B.PB.CI.Po.K.P.copy log_info in 
     let error,list = 
       if PH.B.is_failed output 
       then error,None 
       else 
         let error,list = 
           PH.B.translate_blackboard parameter handler error blackboard 
         in 
         error,Some list 
     in 
     let error,log_info,blackboard = 
       if bool 
       then 
        PH.B.reset_init parameter handler error log_info' blackboard 
       else
         error,log_info,blackboard 
     in 
     error,log_info,save_blackboard,output,result_wo_compression,list 
end 
  
