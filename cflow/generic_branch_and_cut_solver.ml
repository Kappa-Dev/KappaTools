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
  * Last modification: 19/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)


module Solver = 
struct 
  module PH= Propagation_heuristics.Propagation_heuristic 
(*Blackboard_with_heuristic*)
    
  let combine_output o1 o2 = 
    if PH.B.is_ignored o2 then o1 else o2 
      
  let rec propagate parameter handler error instruction_list propagate_list blackboard = 
    match instruction_list 
    with 
      | t::q ->
        begin 
          let error,blackboard,instruction_list,propagate_list,assign_result = PH.apply_instruction parameter handler error blackboard t q propagate_list in 
          if PH.B.is_failed assign_result 
          then 
            error,blackboard,assign_result 
          else 
            propagate parameter handler error instruction_list propagate_list blackboard
        end
      | [] -> 
        begin
          match propagate_list 
          with 
            | t::q -> 
              let error,blackboard,instruction_list,propagate_list,assign_result = PH.propagate parameter handler error blackboard t instruction_list q in 
                    if PH.B.is_failed assign_result 
                    then 
                      error,blackboard,assign_result 
                    else 
                      propagate parameter handler error instruction_list propagate_list blackboard
            | [] -> error,blackboard,PH.B.success
        end 
          
	  
  let rec branch_over_assumption_list parameter handler error list blackboard = 
    match list 
    with 
	  | [] -> 
            error,blackboard,PH.B.fail
	  | head::tail -> 
	    begin
	      let error,blackboard = PH.B.branch parameter handler error blackboard in
	      let error,blackboard,output = propagate parameter handler error [head] [] blackboard in
	      if PH.B.is_failed output 
              then 
                let error,blackboard = PH.B.reset_last_branching parameter handler error blackboard in 
                branch_over_assumption_list parameter handler error tail blackboard 
              else 
                let error,blackboard,output = iter parameter handler error blackboard in 
                if PH.B.is_failed output 
                then 
                  let error,blackboard = PH.B.reset_last_branching parameter handler error blackboard in 
                  branch_over_assumption_list parameter handler error tail blackboard 
                else 
                  error,blackboard,output 
	    end
	      
  and iter parameter handler error blackboard = 
    let error,bool = PH.B.is_maximal_solution parameter handler error blackboard in
    if bool 
    then 
      error,blackboard,PH.B.success 
    else
      let error,list = PH.next_choice parameter handler error blackboard in
      branch_over_assumption_list parameter handler error list blackboard 
    
  let compress parameter handler error blackboard list_order list_eid =
    let error,blackboard = PH.B.branch parameter handler error blackboard in 
    let error,blackboard,result_wo_compression = PH.B.cut parameter handler error blackboard list_eid  in 
    let error,blackboard,output = 
      propagate parameter handler error list_order [] blackboard 
    in 
    let error,blackboard,output = iter parameter handler error blackboard 
    in 
    error,blackboard,output,result_wo_compression
end 
  
